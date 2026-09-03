# s::can
In this repo you will find tools to manage and calibrate s::can data. You can find the scripts for each site in the scripts folders. Site two letter codes correspond to each five sites from the QuEST project. Other folders like "googledrive" and "data" are temporary data storing folders.

- NM: New Mexico - Santa Fe River


## What this is
An R-based data-processing pipeline for s::can spectral sensor data: it downloads/merges raw scan Excel files, cleans and fixes problematic sheets, matches grab-sample chemistry to sensor timestamps, and builds PLSR calibration models (TSS, DOC, NO3) to produce calibrated time series for multiple sites in New Mexico (Santa Fe watershed). Intended users are researchers or data stewards who maintain sensor/chemistry data and run reproducible calibration workflows.

### Stack
- **Language(s):** R (primary)
- **Framework / runtime:** Plain R scripts / RStudio project (no single web framework; scripts run via R or Rscript)
- **Notable libraries:** googledrive (data access), dplyr/readr/readxl (data wrangling & I/O), pls (PLSR modeling), spectrolab (spectral data handling), ggplot2 (plots); openxlsx used for Excel manipulation.

## How it's organized
```text
R/                      helper scripts (fixes for problematic Excel files; e.g. cleanUSF41.R)
scripts_nm/             main numbered pipeline scripts (merge → clean → calibrate → predict), qmd/ref files
data/                   expected data folder (scripts reference subfolders like data/raw, data/merged_timestamps)
scan_figs/              figures / outputs (placeholder)
predicted/              (written by calibration scripts; contains predicted CSVs)
scan.Rproj              RStudio project file (top-level)
scripts_nm.Rproj        RStudio project for the scripts_nm/ subproject
README.md               short repo description / notes
.gitignore              repo ignores
```

How it fits together:
- The numbered scripts in scripts_nm/ implement a linear processing flow: 00_merge_timestamps.R downloads/reads Excel scan files (from Google Drive), merges files per site, and writes merged CSVs; subsequent scripts (01–04) merge parameters, align grab samples and scan timestamps, and perform cleaning. Calibration scripts (05_*/08_*/09_*) build PLSR models using grab-sample training data and the full spectral matrices, produce predicted time series (written to predicted/), and upload results back to Google Drive. R/cleanUSF41.R contains an ad-hoc Excel-fixing helper for a known problematic file.

## How to run it
Shortest path (assumes you have R and git installed and you will authenticate googledrive interactively):

1. Clone and open the project
   - git clone https://github.com/FOR-NM/rg2_scan.git
   - cd rg2_scan
   - open scan.Rproj in RStudio (or run R from the repo root)

2. Install required packages (example)
   - In R:
     install.packages(c("googledrive","readxl","openxlsx","dplyr","readr","xts","pls","spectrolab","ggplot2","plotly","data.table"))

3. Ensure data and working folders exist and credentials are available
   - Create folders referenced by scripts if missing (examples: data/raw, data/merged_timestamps, googledrive/, predicted/)
   - Scripts use Google Drive folder IDs embedded in code (e.g., as_id("https://drive.google.com/drive/folders/1UDrRJ10t04kXT2Op0W-GZz9vEe2B_C8H?...") or drive_folder_id variables). You must authenticate googledrive (googledrive::drive_auth()) with an account that has access to those folders.

4. Run the pipeline (non-interactive example)
   - From shell:
     Rscript scripts_nm/00_merge_timestamps.R
     Rscript scripts_nm/01_merge_params_and_abs.R
     Rscript scripts_nm/02_scan_RawReview.R
     Rscript scripts_nm/03_clean.R
     Rscript scripts_nm/04_merge_grabsamples_and_scan.R
     Rscript scripts_nm/05_calibrate_TSS.R   # and the other 05_/08_/09_ scripts as needed
   - Or source the numbered scripts in RStudio in numeric order. Expect interactive Google Drive auth the first time.

Notes / gotchas:
- Several scripts assume specific folder structure and write outputs to predicted/ and data/merged_timestamps; make sure those directories exist or edit paths in the scripts.
- Some Excel files are known to be problematic (there is a dedicated R/cleanUSF41.R to fix one such file).
- The repo contains large temporary files (e.g., .RDataTmp) and many .DS_Store placeholders — these are not necessary for the pipeline.

