#!/bin/bash
# MIMIC-IV Database Setup Script
# This script sets up the PostgreSQL database for MIMICWizard

set -e

echo "=== MIMIC-IV Database Setup ==="
echo ""

# Configuration
DB_NAME="mimiciv"
DB_USER="postgres"
DB_HOST="localhost"
DB_PORT="5432"

echo "Database configuration:"
echo "  Database: $DB_NAME"
echo "  User: $DB_USER"
echo "  Host: $DB_HOST"
echo "  Port: $DB_PORT"
echo ""

# Check if PostgreSQL is running
echo "Checking PostgreSQL status..."
if ! pg_isready -h $DB_HOST -p $DB_PORT > /dev/null 2>&1; then
    echo "ERROR: PostgreSQL is not running on $DB_HOST:$DB_PORT"
    echo "Please start PostgreSQL and try again."
    exit 1
fi
echo "✓ PostgreSQL is running"
echo ""

# Check if database exists
echo "Checking if database '$DB_NAME' exists..."
if psql -h $DB_HOST -p $DB_PORT -U $DB_USER -lqt | cut -d \| -f 1 | grep -qw $DB_NAME; then
    echo "✓ Database '$DB_NAME' already exists"
else
    echo "Creating database '$DB_NAME'..."
    createdb -h $DB_HOST -p $DB_PORT -U $DB_USER $DB_NAME
    echo "✓ Database created"
fi
echo ""

# Note about MIMIC-IV data
echo "=== IMPORTANT: MIMIC-IV Data Loading ==="
echo ""
echo "This script assumes you have already loaded the MIMIC-IV data into PostgreSQL."
echo "If you haven't done this yet, please follow these steps:"
echo ""
echo "1. Get access to MIMIC-IV from PhysioNet: https://physionet.org/content/mimiciv/"
echo "2. Download the MIMIC-IV data files"
echo "3. Load the data using the official MIMIC-IV scripts:"
echo "   https://github.com/MIT-LCP/mimic-code/tree/main/mimic-iv/buildmimic/postgres"
echo ""
echo "The database should have the following schemas:"
echo "  - mimiciv_hosp (hospital data)"
echo "  - mimiciv_icu (ICU data)"
echo "  - mimiciv_ed (emergency department data - optional)"
echo "  - mimiciv_note (clinical notes - optional)"
echo ""

read -p "Have you already loaded MIMIC-IV data? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Please load MIMIC-IV data first, then run this script again."
    exit 1
fi

echo ""
echo "=== Creating MIMICWizard-specific tables and indexes ==="
echo ""

# Create public schema tables for MIMICWizard
echo "Creating MIMICWizard tables..."
psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME << 'EOF'

-- Create cohort table
CREATE TABLE IF NOT EXISTS public.d_cohorts (
    cohort_id SERIAL PRIMARY KEY,
    cohort_name TEXT NOT NULL,
    cohort_description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create cohort membership table
CREATE TABLE IF NOT EXISTS public.cohort (
    cohort_id INTEGER REFERENCES public.d_cohorts(cohort_id) ON DELETE CASCADE,
    subject_id INTEGER NOT NULL,
    hadm_id INTEGER,
    stay_id INTEGER,
    PRIMARY KEY (cohort_id, subject_id, hadm_id, stay_id)
);

-- Create UID list view for fast patient lookups
CREATE MATERIALIZED VIEW IF NOT EXISTS public.uid_list AS
SELECT DISTINCT subject_id::TEXT AS uid, 'subject_id' AS type FROM mimiciv_hosp.patients
UNION
SELECT DISTINCT hadm_id::TEXT AS uid, 'hadm_id' AS type FROM mimiciv_hosp.admissions
UNION
SELECT DISTINCT stay_id::TEXT AS uid, 'stay_id' AS type FROM mimiciv_icu.icustays;

CREATE INDEX IF NOT EXISTS idx_uid_list_uid ON public.uid_list(uid);

-- Create custom events table (for user-defined events)
CREATE TABLE IF NOT EXISTS public.d_customevents (
    itemid SERIAL PRIMARY KEY,
    label TEXT NOT NULL,
    category TEXT,
    unitname TEXT
);

-- Create prescriptions dictionary table
CREATE TABLE IF NOT EXISTS public.d_prescriptions (
    drug TEXT PRIMARY KEY,
    drug_type TEXT,
    formulary_drug_cd TEXT
);

EOF

echo "✓ MIMICWizard tables created"
echo ""

# Apply performance indexes
echo "Applying performance indexes (this may take several minutes)..."
psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -f "$(dirname "$0")/../installation/performance.sql"
echo "✓ Performance indexes created"
echo ""

echo "=== Database Setup Complete ==="
echo ""
echo "Next steps:"
echo "1. Run 03_app_setup.sh to configure the R application"
echo "2. Start the Shiny app with: R -e \"shiny::runApp(port = 3838)\""
echo ""
