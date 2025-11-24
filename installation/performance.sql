-- Performance enhancements for hosted/full MIMIC-IV database
-- Adds selective indexes tuned for common MIMICWizard workflows.

CREATE INDEX CONCURRENTLY IF NOT EXISTS chartevents_subject_hadm_charttime_idx
  ON mimiciv_icu.chartevents (subject_id, hadm_id, charttime);

CREATE INDEX CONCURRENTLY IF NOT EXISTS labevents_subject_hadm_charttime_idx
  ON mimiciv_hosp.labevents (subject_id, hadm_id, charttime);

CREATE INDEX CONCURRENTLY IF NOT EXISTS inputevents_subject_hadm_start_idx
  ON mimiciv_icu.inputevents (subject_id, hadm_id, starttime);
