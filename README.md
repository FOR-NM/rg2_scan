# s::can
In this repo you will find tools to manage and calibrate s::can data. You can find the scripts for each site in the scripts folders. Site two letter codes correspond to each five sites from the QuEST project. Other folders like "googledrive" and "data" are temporary data storing folders.

- NM: New Mexico - Santa Fe River

An R-based data-processing pipeline for s::can spectral sensor data: it downloads/merges raw scan Excel files, cleans and fixes problematic sheets, matches grab-sample chemistry to sensor timestamps, and builds PLSR calibration models (TSS, DOC, NO3) to produce calibrated time series for multiple sites in New Mexico (Santa Fe watershed). Intended users are researchers or data stewards who maintain sensor/chemistry data and run reproducible calibration workflows.

```text
R/                      helper scripts (fixes for problematic Excel files; e.g. cleanUSF41.R)
scripts_nm/             main numbered pipeline scripts (merge → clean → calibrate → predict), 
data/                   expected data folder (scripts reference subfolders like data/raw,   data/merged_timestamps)
scan_figs/              figures / outputs (placeholder)
predicted/              (written by calibration scripts; contains predicted CSVs)
scan.Rproj              RStudio project file (top-level)
scripts_nm.Rproj        RStudio project for the scripts_nm/ subproject
README.md               short repo description / notes
.gitignore              repo ignores
```
