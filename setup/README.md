# MIMICWizard Setup Guide

Complete setup documentation for MIMICWizard with MIMIC-IV database.

## Prerequisites

- **PostgreSQL** installed and running (v12+)
- **R** installed (v4.0+)
- **MIMIC-IV data files** downloaded from PhysioNet (get access at https://physionet.org/content/mimiciv/)
- **100 GB free disk space**

## Quick Setup

```bash
cd setup

# 1. Load MIMIC-IV data into PostgreSQL (2-6 hours)
./01_load_mimic_data.sh

# 2. Create MIMICWizard tables and indexes (5-10 min)
./02_database_setup.sh

# 3. Install R packages and configure (10-20 min)
./03_app_setup.sh

# 4. Start the app
cd ..
R -e "shiny::runApp(port = 3838, launch.browser = FALSE)"
```

Open browser to: **http://localhost:3838**

## Scripts

### 01_load_mimic_data.sh
Loads MIMIC-IV CSV files into PostgreSQL. Assumes data is downloaded, PostgreSQL is installed. Creates schemas, clones MIMIC-Code repo, runs official loading scripts. **Time: 2-6 hours**

### 02_database_setup.sh
Creates MIMICWizard tables (`d_cohorts`, `cohort`, `uid_list`, `d_customevents`, `d_prescriptions`) and applies performance indexes on chartevents, labevents, inputevents. **Time: 5-10 min**

### 03_app_setup.sh
Installs R packages (shiny, RPostgres, dplyr, plotly, DT, etc.), configures database connection in `global.R`, tests connectivity. **Time: 10-20 min**

## Configuration

Edit `global.R` to customize database connection:
```r
CONFIG <- list(
  INTERACTIVE = FALSE,
  HOSTED_DBNAME = "mimiciv",
  HOSTED_HOST = "localhost",
  HOSTED_PORT = 5432,
  HOSTED_USER = "postgres",
  HOSTED_PASSWORD = ""
)
```

## Troubleshooting

**PostgreSQL not running:** `pg_isready -h localhost -p 5432`  
**Connection fails:** Check credentials in `global.R`  
**R packages fail:** Install system deps: `libpq-dev libssl-dev libcurl4-openssl-dev` (Ubuntu) or `brew install postgresql openssl curl` (macOS)  
**App won't start:** Kill port: `lsof -ti:3838 | xargs kill`

## Features

- **Cohort Creation** - Define cohorts using clinical criteria
- **Cohort Explorer** - Analyze outcomes and parameters
- **Patient Timeline** - Chronological view with all event types, ICU filtering, time series plots
- **Patient Explorer** - Search individual patients

## Resources

- MIMIC-IV docs: https://mimic.mit.edu/docs/iv/
- MIMIC-Code repo: https://github.com/MIT-LCP/mimic-code
