-- G-Transformer Cohort Setup Script
-- This script creates the proper Sepsis-3 cohort matching the G-Transformer paper criteria:
-- 1. Sepsis-3 patients (SOFA >= 2 + suspicion of infection)
-- 2. Exclude cardiac, vascular, trauma surgery patients
-- 3. Exclude patients with missing pre-ICU fluid records
--
-- Run this after the MIMIC-IV derived tables are created.
-- Dependencies: mimiciv_derived.sofa must be populated first
--
-- Usage:
--   1. First populate SOFA (if empty): see populate_sofa.sql or run manually
--   2. Then run: psql -d mimiciv -f gtransformer_cohort.sql

-- ============================================================================
-- STEP 1: Ensure SOFA table is populated
-- ============================================================================
DO $$
DECLARE
    sofa_count INTEGER;
BEGIN
    SELECT COUNT(*) INTO sofa_count FROM mimiciv_derived.sofa;
    IF sofa_count = 0 THEN
        RAISE NOTICE '';
        RAISE NOTICE '============================================================';
        RAISE NOTICE 'ERROR: SOFA table is empty!';
        RAISE NOTICE '============================================================';
        RAISE NOTICE 'Please run populate_sofa.sql first (takes 10-30 minutes):';
        RAISE NOTICE '  psql -d mimiciv -f mimicwizard/installation/populate_sofa.sql';
        RAISE NOTICE '';
        RAISE EXCEPTION 'SOFA table must be populated before creating G-Transformer cohort';
    ELSE
        RAISE NOTICE 'SOFA table has % rows - OK', sofa_count;
    END IF;
END $$;

-- ============================================================================
-- STEP 1b: Ensure supporting indexes exist for performant cohort creation
-- ============================================================================
CREATE INDEX IF NOT EXISTS idx_inputevents_stay_id ON mimiciv_icu.inputevents (stay_id);

-- ============================================================================
-- STEP 2: Rebuild sepsis3 table from SOFA and suspicion_of_infection
-- ============================================================================
TRUNCATE TABLE mimiciv_derived.sepsis3;
INSERT INTO mimiciv_derived.sepsis3
WITH sofa AS (
  SELECT
    stay_id,
    starttime,
    endtime,
    respiration_24hours AS respiration,
    coagulation_24hours AS coagulation,
    liver_24hours AS liver,
    cardiovascular_24hours AS cardiovascular,
    cns_24hours AS cns,
    renal_24hours AS renal,
    sofa_24hours AS sofa_score
  FROM mimiciv_derived.sofa
  WHERE sofa_24hours >= 2
), s1 AS (
  SELECT
    soi.subject_id,
    soi.stay_id,
    soi.ab_id,
    soi.antibiotic,
    soi.antibiotic_time,
    soi.culture_time,
    soi.suspected_infection,
    soi.suspected_infection_time,
    soi.specimen,
    soi.positive_culture,
    starttime,
    endtime,
    respiration,
    coagulation,
    liver,
    cardiovascular,
    cns,
    renal,
    sofa_score,
    (sofa_score >= 2 AND suspected_infection = 1) AS sepsis3,
    ROW_NUMBER() OVER (
      PARTITION BY soi.stay_id 
      ORDER BY suspected_infection_time NULLS FIRST, 
               antibiotic_time NULLS FIRST, 
               culture_time NULLS FIRST, 
               endtime NULLS FIRST
    ) AS rn_sus
  FROM mimiciv_derived.suspicion_of_infection AS soi
  INNER JOIN sofa ON soi.stay_id = sofa.stay_id
    AND sofa.endtime >= soi.suspected_infection_time - INTERVAL '48 HOUR'
    AND sofa.endtime <= soi.suspected_infection_time + INTERVAL '24 HOUR'
  WHERE soi.stay_id IS NOT NULL
)
SELECT
  subject_id,
  stay_id,
  antibiotic_time,
  culture_time,
  suspected_infection_time,
  endtime AS sofa_time,
  sofa_score,
  respiration,
  coagulation,
  liver,
  cardiovascular,
  cns,
  renal,
  sepsis3
FROM s1
WHERE rn_sus = 1;

-- Create index for faster lookups
CREATE INDEX IF NOT EXISTS idx_sepsis3_stay_id ON mimiciv_derived.sepsis3(stay_id);
CREATE INDEX IF NOT EXISTS idx_sepsis3_subject_id ON mimiciv_derived.sepsis3(subject_id);

-- ============================================================================
-- STEP 3: Create GT-Sepsis-Paper cohort with proper exclusions
-- ============================================================================

-- Remove old GT-Sepsis-Paper cohort if exists (use subquery to avoid error if not exists)
DELETE FROM public.cohort WHERE cohort_id IN (
    SELECT cohort_id FROM public.d_cohorts WHERE cohort_name = 'GT-Sepsis-Paper'
);
DELETE FROM public.d_cohorts WHERE cohort_name = 'GT-Sepsis-Paper';

-- Insert new cohort definition
INSERT INTO public.d_cohorts (cohort_name, cohort_description)
VALUES (
    'GT-Sepsis-Paper',
    'G-Transformer paper cohort: Sepsis-3 (SOFA>=2 + suspicion of infection), excludes cardiac/vascular/trauma surgery, excludes missing pre-ICU fluids. Matches Melnychuk et al. criteria.'
);

-- Insert patients into cohort (optimized: join-based filtering on 41k rows, not full table scans)
INSERT INTO public.cohort (cohort_id, subject_id, stay_id, hadm_id)
WITH sepsis3_stays AS (
    -- Get all Sepsis-3 positive ICU stays with ICU intime (~41k rows)
    SELECT DISTINCT s.subject_id, s.stay_id, i.hadm_id, i.intime
    FROM mimiciv_derived.sepsis3 s
    JOIN mimiciv_icu.icustays i ON s.stay_id = i.stay_id
    WHERE s.sepsis3 = true
),
-- Pre-ICU fluid evidence via pre-admission/non-ICU charted fluids
preicu_fluids AS (
    SELECT ss.stay_id, COALESCE(SUM(ie.amount), 0) AS preicu_fluid_volume
    FROM sepsis3_stays ss
    LEFT JOIN mimiciv_icu.inputevents ie ON ie.stay_id = ss.stay_id
        AND ie.ordercategoryname = '16-Pre Admission/Non-ICU'
        AND ie.amount > 0
    GROUP BY ss.stay_id
)
SELECT 
    (SELECT cohort_id FROM public.d_cohorts WHERE cohort_name = 'GT-Sepsis-Paper'),
    ss.subject_id,
    ss.stay_id,
    ss.hadm_id
FROM sepsis3_stays ss
-- Join to admissions only for these 41k patients (fast index lookup)
JOIN mimiciv_hosp.admissions adm ON ss.hadm_id = adm.hadm_id
-- Require documented pre-ICU fluids
JOIN preicu_fluids pf ON ss.stay_id = pf.stay_id
WHERE 
    -- Exclude cardiac/vascular/trauma surgery
    LOWER(adm.admission_type) NOT LIKE '%cardiac%'
    AND LOWER(adm.admission_type) NOT LIKE '%vascular%'
    AND LOWER(adm.admission_type) NOT LIKE '%trauma%'
    -- Require evidence of ED stay to ensure data availability
    AND adm.edregtime IS NOT NULL
    -- Require explicit pre-ICU fluid volume
    AND pf.preicu_fluid_volume > 0;

-- ============================================================================
-- STEP 4: Report cohort statistics
-- ============================================================================
DO $$
DECLARE
    total_patients INTEGER;
    total_stays INTEGER;
    sepsis3_count INTEGER;
    excluded_surgery_count INTEGER;
    excluded_fluids_count INTEGER;
BEGIN
    -- Total Sepsis-3 patients
    SELECT COUNT(DISTINCT subject_id) INTO sepsis3_count
    FROM mimiciv_derived.sepsis3 WHERE sepsis3 = true;
    
    -- Final cohort stats
    SELECT COUNT(DISTINCT subject_id), COUNT(DISTINCT stay_id) 
    INTO total_patients, total_stays
    FROM public.cohort 
    WHERE cohort_id = (SELECT cohort_id FROM public.d_cohorts WHERE cohort_name = 'GT-Sepsis-Paper');
    
    RAISE NOTICE '';
    RAISE NOTICE '============================================================';
    RAISE NOTICE 'G-TRANSFORMER COHORT CREATION COMPLETE';
    RAISE NOTICE '============================================================';
    RAISE NOTICE 'Sepsis-3 patients (before exclusions): %', sepsis3_count;
    RAISE NOTICE 'Final cohort patients: %', total_patients;
    RAISE NOTICE 'Final cohort ICU stays: %', total_stays;
    RAISE NOTICE '';
    RAISE NOTICE 'Paper reference: 8,934 patients';
    RAISE NOTICE '============================================================';
END $$;

-- Show final stats
SELECT 
    'GT-Sepsis-Paper' as cohort,
    COUNT(DISTINCT subject_id) as patients,
    COUNT(DISTINCT stay_id) as stays
FROM public.cohort 
WHERE cohort_id = (SELECT cohort_id FROM public.d_cohorts WHERE cohort_name = 'GT-Sepsis-Paper');







