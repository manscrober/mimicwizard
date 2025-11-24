#!/bin/bash
# MIMIC-IV Data Loading Script
# Loads MIMIC-IV data from downloaded CSV files into PostgreSQL

set -e

echo "=== MIMIC-IV Data Loading Script ==="
echo ""
echo "ASSUMPTIONS:"
echo "  1. PostgreSQL is installed and running"
echo "  2. MIMIC-IV data files are already downloaded from PhysioNet"
echo "  3. Data files are in .csv.gz format in a local directory"
echo ""

# Configuration
DB_NAME="mimiciv"
DB_USER="postgres"
DB_HOST="localhost"
DB_PORT="5432"

# Prompt for data directory
read -p "Enter path to MIMIC-IV data directory (e.g., ~/mimic-iv-data): " DATA_DIR
DATA_DIR="${DATA_DIR/#\~/$HOME}"

if [ ! -d "$DATA_DIR" ]; then
    echo "ERROR: Directory $DATA_DIR does not exist"
    exit 1
fi

echo ""
echo "Configuration:"
echo "  Data directory: $DATA_DIR"
echo "  Database: $DB_NAME"
echo "  User: $DB_USER"
echo "  Host: $DB_HOST"
echo "  Port: $DB_PORT"
echo ""

# Check PostgreSQL is running
echo "Checking PostgreSQL..."
if ! pg_isready -h $DB_HOST -p $DB_PORT > /dev/null 2>&1; then
    echo "ERROR: PostgreSQL is not running on $DB_HOST:$DB_PORT"
    exit 1
fi
echo "✓ PostgreSQL is running"
echo ""

# Create database if it doesn't exist
echo "Setting up database..."
if ! psql -h $DB_HOST -p $DB_PORT -U $DB_USER -lqt | cut -d \| -f 1 | grep -qw $DB_NAME; then
    createdb -h $DB_HOST -p $DB_PORT -U $DB_USER $DB_NAME
    echo "✓ Database created"
else
    echo "✓ Database exists"
fi

# Create schemas
psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME << 'EOF'
CREATE SCHEMA IF NOT EXISTS mimiciv_hosp;
CREATE SCHEMA IF NOT EXISTS mimiciv_icu;
CREATE SCHEMA IF NOT EXISTS mimiciv_ed;
CREATE SCHEMA IF NOT EXISTS mimiciv_note;
EOF
echo "✓ Schemas created"
echo ""

# Clone MIMIC-Code if not present
if [ ! -d "$HOME/mimic-code" ]; then
    echo "Cloning MIMIC-Code repository..."
    git clone https://github.com/MIT-LCP/mimic-code.git "$HOME/mimic-code"
    echo "✓ MIMIC-Code cloned"
else
    echo "✓ MIMIC-Code already present"
fi
echo ""

# Load data using MIMIC-Code scripts
echo "Loading MIMIC-IV data (this will take 2-6 hours)..."
cd "$HOME/mimic-code/mimic-iv/buildmimic/postgres"

export MIMIC_PASSWORD=""
export MIMIC_USER="$DB_USER"
export MIMIC_DB="$DB_NAME"
export MIMIC_HOST="$DB_HOST"
export MIMIC_PORT="$DB_PORT"

make mimic-gz datadir="$DATA_DIR"

echo ""
echo "=== Data Loading Complete ==="
echo ""
echo "Verifying data..."
psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME << 'EOF'
SELECT 'patients' as table_name, COUNT(*) as count FROM mimiciv_hosp.patients
UNION ALL
SELECT 'admissions', COUNT(*) FROM mimiciv_hosp.admissions
UNION ALL
SELECT 'icustays', COUNT(*) FROM mimiciv_icu.icustays;
EOF

echo ""
echo "Next step: Run 02_database_setup.sh"
echo ""
