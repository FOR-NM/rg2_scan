##==============================================================================
## Project: QuEST
## DOC Sensor Calibration: PLSR using absorbance spectra + global TSS
## Following Arial's s::can guide
##
## Absorbance columns : 19:118  (already turbidity-compensated)
## TSS global column  : col 14  (TSS_mg.l — used as hydrological state indicator)
## Grab sample column : NPOC..mg.C.L.
##
## Two models are fitted and compared for each site:
##   Model A (spectra only)     : DOC ~ Absorbance spectra
##   Model B (spectra + TSS)    : DOC ~ Absorbance spectra + global TSS
##
## Rationale for adding TSS:
##   Absorbances are already turbidity-compensated, so TSS here is not a
##   redundant turbidity correction. Instead it acts as a hydrological state
##   indicator — telling the PLSR when high-flow / high-DOC conditions are
##   occurring. This may improve predictions during storm peaks where grab
##   sample coverage is poor. We compare LOO-RMSE for both models so you
##   can decide which to use.
##==============================================================================

library(googledrive)
library(data.table)
library(xts)
library(dplyr)
library(pls)
library(spectrolab)
library(ggplot2)
library(plotly)

###################################
#### Clear folders we will use ####
###################################
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

######################################
#### STEP 1: Prep grab sample data ###
######################################
# This data was matched using previous scripts
# See scripts merge_params_and_abs and merge_grabsamples_and_scan

########################################
#### STEP 2: Upload scan data frame ####
########################################
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1qjM3Zze-I5ycFCHNcd997UG6gYXBUoX8")

merged <- googledrive::drive_ls(path = scan, type = "csv")

googledrive::drive_download(file = merged$id[merged$name == "USF12_chem_Buttercup.csv"],
                            path = "googledrive/USF12_chem_Buttercup.csv", overwrite = TRUE)
googledrive::drive_download(file = merged$id[merged$name == "USF20_chem_Blossom.csv"],
                            path = "googledrive/USF20_chem_Blossom.csv",  overwrite = TRUE)
googledrive::drive_download(file = merged$id[merged$name == "USF21_chem_Bubbles.csv"],
                            path = "googledrive/USF21_chem_Bubbles.csv",  overwrite = TRUE)

USF12 <- read.csv("googledrive/USF12_chem_Buttercup.csv", na = c("", "NaN", "Na", "NA"))
USF20 <- read.csv("googledrive/USF20_chem_Blossom.csv",   na = c("", "NaN", "Na", "NA"))
USF21 <- read.csv("googledrive/USF21_chem_Bubbles.csv",   na = c("", "NaN", "Na", "NA"))

# Fill midnight timestamps
for (df_name in c("USF12", "USF20", "USF21")) {
  df  <- get(df_name)
  idx <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$", df$DateTime)
  df$DateTime[idx] <- paste(df$DateTime[idx], "00:00:00")
  assign(df_name, df)
}

USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M:%S")

USF12 <- USF12 %>% filter(!is.na(DateTime))
USF20 <- USF20 %>% filter(!is.na(DateTime))
USF21 <- USF21 %>% filter(!is.na(DateTime))

# Remove leading "X" and trailing ".nm" from column names
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}
USF12 <- rename_columns(USF12)
USF20 <- rename_columns(USF20)
USF21 <- rename_columns(USF21)

# Verify column positions — run these and check the output matches expectations:
#   col 14  = TSS_mg.l  (global TSS)
#   col 19:118 = absorbance bands
cat("Column 14:", colnames(USF12)[14], "\n")
cat("Columns 19:118:", colnames(USF12)[19:118], "\n")

# Extract DOC and TSS as xts time series
scan_DOC_USF12 <- xts(USF12$DOC_mg.l, order.by = USF12$DateTime)
scan_DOC_USF20 <- xts(USF20$DOC_mg.l, order.by = USF20$DateTime)
scan_DOC_USF21 <- xts(USF21$DOC_mg.l, order.by = USF21$DateTime)

scan_TSS_USF12 <- xts(USF12$TSS_mg.l, order.by = USF12$DateTime)
scan_TSS_USF20 <- xts(USF20$TSS_mg.l, order.by = USF20$DateTime)
scan_TSS_USF21 <- xts(USF21$TSS_mg.l, order.by = USF21$DateTime)

###############################################
####  STEP 3: Flag and filter grab samples ####
###############################################
USF12 <- USF12 %>%
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_))
USF20 <- USF20 %>%
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_))
USF21 <- USF21 %>%
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_))

grab_USF12 <- USF12[USF12$Grab_sample == "Y" & !is.na(USF12$Grab_sample), ]
grab_USF20 <- USF20[USF20$Grab_sample == "Y" & !is.na(USF20$Grab_sample), ]
grab_USF21 <- USF21[USF21$Grab_sample == "Y" & !is.na(USF21$Grab_sample), ]

# Remove known bad samples
grab_USF12 <- grab_USF12 %>%
  mutate(NPOC..mg.C.L. = ifelse(DateTime == as.POSIXct("2025-01-02 12:15:00"), NA, NPOC..mg.C.L.))
grab_USF20 <- grab_USF20 %>%
  mutate(NPOC..mg.C.L. = ifelse(DateTime == as.POSIXct("2024-06-19 14:00:00"), NA, NPOC..mg.C.L.))

# Extract grab NPOC vectors (used in spectral data frames below)
grab.DOC12 <- grab_USF12$NPOC..mg.C.L.
grab.DOC20 <- grab_USF20$NPOC..mg.C.L.
grab.DOC21 <- grab_USF21$NPOC..mg.C.L.

# Extract grab TSS vectors
grab.TSS12 <- grab_USF12$TSS_mg.l   # col 14 — global TSS at grab sample times
grab.TSS20 <- grab_USF20$TSS_mg.l
grab.TSS21 <- grab_USF21$TSS_mg.l

# Quick check: how does TSS vary across your grab samples?
# If TSS range is very narrow (all low-flow), the TSS predictor may not add much
summary(grab.TSS12)
summary(grab.TSS20)
summary(grab.TSS21)

##########################################################################
#### STEP 4: Build spectral matrices from GRAB sample rows (training) ####
##########################################################################
# Absorbance columns: 19:118 (already turbidity-compensated)
# NOTE: if you need to change the column range, update 19:118 here and in Step 5

build_spectral_matrix <- function(df, spec_cols) {
  spec_dat <- df[, spec_cols]
  colnames(spec_dat) <- gsub("^X|\\.nm$", "", colnames(spec_dat))
  wl  <- as.numeric(colnames(spec_dat))
  num <- seq_len(nrow(spec_dat))
  sp  <- spectra(value = spec_dat, bands = wl, names = num)
  sp  <- as.matrix(sp)
  attr(sp, 'wave_unit')        <- 'wavelength'
  attr(sp, 'measurement_unit') <- 'absorbance'
  return(list(matrix = sp, wl = wl, num = num))
}

grab.spec12 <- build_spectral_matrix(grab_USF12, 19:118)$matrix
grab.spec20 <- build_spectral_matrix(grab_USF20, 19:118)$matrix
grab.spec21 <- build_spectral_matrix(grab_USF21, 19:118)$matrix

# Plot grab spectra — check for outliers or bad scans
plot(spectra(value = grab_USF12[, 19:118],
             bands = as.numeric(gsub("^X|\\.nm$","",colnames(grab_USF12)[19:118])),
             names = seq_len(nrow(grab_USF12))),
     main = "USF12 grab sample spectra")
plot(spectra(value = grab_USF20[, 19:118],
             bands = as.numeric(gsub("^X|\\.nm$","",colnames(grab_USF20)[19:118])),
             names = seq_len(nrow(grab_USF20))),
     main = "USF20 grab sample spectra")
plot(spectra(value = grab_USF21[, 19:118],
             bands = as.numeric(gsub("^X|\\.nm$","",colnames(grab_USF21)[19:118])),
             names = seq_len(nrow(grab_USF21))),
     main = "USF21 grab sample spectra")

##########################################################################
#### STEP 5: Build spectral matrices from FULL time series (predict) #####
##########################################################################
scan.spec12 <- build_spectral_matrix(USF12, 19:118)$matrix
scan.spec20 <- build_spectral_matrix(USF20, 19:118)$matrix
scan.spec21 <- build_spectral_matrix(USF21, 19:118)$matrix

# Plot full spectra — identify problem time periods (e.g. biofouling)
plot(spectra(value = USF12[, 19:118],
             bands = as.numeric(gsub("^X|\\.nm$","",colnames(USF12)[19:118])),
             names = seq_len(nrow(USF12))),
     main = "USF12 full time series spectra")
plot(spectra(value = USF20[, 19:118],
             bands = as.numeric(gsub("^X|\\.nm$","",colnames(USF20)[19:118])),
             names = seq_len(nrow(USF20))),
     main = "USF20 full time series spectra")
plot(spectra(value = USF21[, 19:118],
             bands = as.numeric(gsub("^X|\\.nm$","",colnames(USF21)[19:118])),
             names = seq_len(nrow(USF21))),
     main = "USF21 full time series spectra")

########################################
#### STEP 6: Build PLSR data frames ####
########################################
# Model A: spectra only (baseline, same as original approach)
# Model B: spectra + global TSS (new — TSS as hydrological state indicator)
#
# We use I() to protect the matrix columns from being expanded by the formula.
# TSS is added as a plain numeric column alongside the protected spectra matrix.

## USF12 ##
# Grab (training)
grabcal.df12.A <- data.frame(DOC = grab.DOC12,
                              Spectra = I(grab.spec12))
grabcal.df12.B <- data.frame(DOC = grab.DOC12,
                              Spectra = I(grab.spec12),
                              TSS = grab.TSS12)
# Full time series (prediction)
spectralcal.df12.A <- data.frame(DOC = scan_DOC_USF12,
                                  Spectra = I(scan.spec12))
spectralcal.df12.B <- data.frame(DOC = scan_DOC_USF12,
                                  Spectra = I(scan.spec12),
                                  TSS = as.numeric(scan_TSS_USF12))

## USF20 ##
grabcal.df20.A <- data.frame(DOC = grab.DOC20,
                              Spectra = I(grab.spec20))
grabcal.df20.B <- data.frame(DOC = grab.DOC20,
                              Spectra = I(grab.spec20),
                              TSS = grab.TSS20)
spectralcal.df20.A <- data.frame(DOC = scan_DOC_USF20,
                                  Spectra = I(scan.spec20))
spectralcal.df20.B <- data.frame(DOC = scan_DOC_USF20,
                                  Spectra = I(scan.spec20),
                                  TSS = as.numeric(scan_TSS_USF20))

## USF21 ##
grabcal.df21.A <- data.frame(DOC = grab.DOC21,
                              Spectra = I(grab.spec21))
grabcal.df21.B <- data.frame(DOC = grab.DOC21,
                              Spectra = I(grab.spec21),
                              TSS = grab.TSS21)
spectralcal.df21.A <- data.frame(DOC = scan_DOC_USF21,
                                  Spectra = I(scan.spec21))
spectralcal.df21.B <- data.frame(DOC = scan_DOC_USF21,
                                  Spectra = I(scan.spec21),
                                  TSS = as.numeric(scan_TSS_USF21))

##########################################################
#### STEP 7: Fit PLSR models — Model A (spectra only) ####
##########################################################
# ncomp should be no more than N grab samples - 1
# LOO = leave-one-out cross-validation
# Start with a generous ncomp and use the RMSEP plot to find the optimal number

## USF12 — Model A ##
Cmod12.A <- plsr(DOC ~ Spectra, ncomp = 24, data = grabcal.df12.A, validation = "LOO")
summary(Cmod12.A)
plot(RMSEP(Cmod12.A), legendpos = "topright", main = "USF12 Model A (spectra only): RMSEP")
# Pick ncomp where RMSEP levels off — update ncomp_12A below
ncomp_12A <- 5   # <- CHOOSE NUMBER OF COMPONENTS
plot(Cmod12.A, ncomp = ncomp_12A, asp = 1, line = TRUE,
     main = paste0("USF12 Model A: predicted vs measured (ncomp = ", ncomp_12A, ")"))

## USF20 — Model A ##
Cmod20.A <- plsr(DOC ~ Spectra, ncomp = 14, data = grabcal.df20.A, validation = "LOO")
summary(Cmod20.A)
plot(RMSEP(Cmod20.A), legendpos = "topright", main = "USF20 Model A (spectra only): RMSEP")
ncomp_20A <- 3   # <- CHOOSE NUMBER OF COMPONENTS
plot(Cmod20.A, ncomp = ncomp_20A, asp = 1, line = TRUE,
     main = paste0("USF20 Model A: predicted vs measured (ncomp = ", ncomp_20A, ")"))

## USF21 — Model A ##
Cmod21.A <- plsr(DOC ~ Spectra, ncomp = 9, data = grabcal.df21.A, validation = "LOO")
summary(Cmod21.A)
plot(RMSEP(Cmod21.A), legendpos = "topright", main = "USF21 Model A (spectra only): RMSEP")
ncomp_21A <- 6   # <- CHOOSE NUMBER OF COMPONENTS
plot(Cmod21.A, ncomp = ncomp_21A, asp = 1, line = TRUE,
     main = paste0("USF21 Model A: predicted vs measured (ncomp = ", ncomp_21A, ")"))

###########################################################
#### STEP 8: Fit PLSR models — Model B (spectra + TSS) ####
###########################################################
# Adding TSS as a plain numeric predictor alongside the spectra matrix.
# PLSR handles mixed predictor types fine — TSS will be scaled together
# with the spectral variables in the latent component decomposition.
# The optimal ncomp may differ from Model A — re-inspect the RMSEP plots.

## USF12 — Model B ##
Cmod12.B <- plsr(DOC ~ Spectra + TSS, ncomp = 24, data = grabcal.df12.B, validation = "LOO")
summary(Cmod12.B)
plot(RMSEP(Cmod12.B), legendpos = "topright", main = "USF12 Model B (spectra + TSS): RMSEP")
ncomp_12B <- 6   # <- UPDATE after inspecting the RMSEP plot
plot(Cmod12.B, ncomp = ncomp_12B, asp = 1, line = TRUE,
     main = paste0("USF12 Model B: predicted vs measured (ncomp = ", ncomp_12B, ")"))


## USF20 — Model B ##
Cmod20.B <- plsr(DOC ~ Spectra + TSS, ncomp = 15, data = grabcal.df20.B, validation = "LOO")
summary(Cmod20.B)
plot(RMSEP(Cmod20.B), legendpos = "topright", main = "USF20 Model B (spectra + TSS): RMSEP")
ncomp_20B <- 2   # <- UPDATE
plot(Cmod20.B, ncomp = ncomp_20B, asp = 1, line = TRUE,
     main = paste0("USF20 Model B: predicted vs measured (ncomp = ", ncomp_20B, ")"))

## USF21 — Model B ##
Cmod21.B <- plsr(DOC ~ Spectra + TSS, ncomp = 9, data = grabcal.df21.B, validation = "LOO")
summary(Cmod21.B)
plot(RMSEP(Cmod21.B), legendpos = "topright", main = "USF21 Model B (spectra + TSS): RMSEP")
ncomp_21B <- 6   # <- UPDATE
plot(Cmod21.B, ncomp = ncomp_21B, asp = 1, line = TRUE,
     main = paste0("USF21 Model B: predicted vs measured (ncomp = ", ncomp_21B, ")"))

############################################
#### STEP 9: Compare Model A vs Model B ####
############################################
# Extract LOO-RMSE and % variance explained at chosen ncomp for both models
# Lower LOO-RMSE = better generalisation to unseen grab samples
# If Model B LOO-RMSE is not meaningfully lower (~10%+), stick with Model A

extract_model_stats <- function(model, ncomp, model_name, site) {
  # LOO RMSE at chosen ncomp (from the validation results)
  loo_rmse <- RMSEP(model, estimate = "CV")$val[1, 1, ncomp + 1]
  # % variance in Y explained
  var_y    <- explvar(model)[ncomp]
  data.frame(Site = site, Model = model_name, ncomp = ncomp,
             LOO_RMSE = round(loo_rmse, 4),
             Var_Y_pct = round(cumsum(explvar(model))[ncomp], 2))
}

comparison <- rbind(
  extract_model_stats(Cmod12.A, ncomp_12A, "A: Spectra only",  "USF12"),
  extract_model_stats(Cmod12.B, ncomp_12B, "B: Spectra + TSS", "USF12"),
  extract_model_stats(Cmod20.A, ncomp_20A, "A: Spectra only",  "USF20"),
  extract_model_stats(Cmod20.B, ncomp_20B, "B: Spectra + TSS", "USF20"),
  extract_model_stats(Cmod21.A, ncomp_21A, "A: Spectra only",  "USF21"),
  extract_model_stats(Cmod21.B, ncomp_21B, "B: Spectra + TSS", "USF21")
)

cat("\n=== DOC PLSR Model Comparison: A (spectra only) vs B (spectra + TSS) ===\n")
print(comparison)
cat("\nInterpretation: prefer Model B if its LOO_RMSE is >10% lower than Model A.\n")
cat("If similar or higher, TSS is not adding useful information and Model A is preferred.\n")

###########################################################
#### STEP 10: Loadings plots — which wavelengths matter? ##
###########################################################
# Loadings show how each wavelength (and TSS in Model B) contributes to each
# PLSR component. Peaks at specific wavelengths = those bands drive the model.

## USF12 ##
plot(Cmod12.A, plottype = "loading", comps = 1:2, main = "USF12 Model A: Loadings")
plot(Cmod12.B, plottype = "loading", comps = 1:2, main = "USF12 Model B: Loadings")

## USF20 ##
plot(Cmod20.A, plottype = "loading", comps = 1:2, main = "USF20 Model A: Loadings")
plot(Cmod20.B, plottype = "loading", comps = 1:2, main = "USF20 Model B: Loadings")

## USF21 ##
plot(Cmod21.A, plottype = "loading", comps = 1:2, main = "USF21 Model A: Loadings")
plot(Cmod21.B, plottype = "loading", comps = 1:2, main = "USF21 Model B: Loadings")

#######################################
#### STEP 11: Generate predictions ####
#######################################
# After inspecting Step 9, choose Model A or B for each site.
# Update the model and ncomp references in the predict() calls below.
# Default: using Model B (spectra + TSS) — swap .B to .A and _12B to _12A to revert.

## USF12 ##
predictedDOC12 <- predict(Cmod12.A, ncomp = ncomp_12A, newdata = spectralcal.df12.A)

pred_df12 <- data.frame(
  DateTime  = USF12$DateTime,
  Predicted = as.numeric(predictedDOC12))

p12 <- ggplot(pred_df12, aes(x = DateTime, y = Predicted)) +
  geom_line(color = "steelblue") +
  labs(x = "DateTime", y = "Predicted DOC (mg/L)",
       title = "USF12: Predicted DOC — PLSR (spectra + TSS)") +
  theme_minimal()
ggplotly(p12)

## USF20 ##
predictedDOC20 <- predict(Cmod20.B, ncomp = ncomp_20B, newdata = spectralcal.df20.B)

pred_df20 <- data.frame(
  DateTime  = USF20$DateTime,
  Predicted = as.numeric(predictedDOC20))

p20 <- ggplot(pred_df20, aes(x = DateTime, y = Predicted)) +
  geom_line(color = "steelblue") +
  labs(x = "DateTime", y = "Predicted DOC (mg/L)",
       title = "USF20: Predicted DOC — PLSR (spectra + TSS)") +
  theme_minimal()
ggplotly(p20)

## USF21 ##
predictedDOC21 <- predict(Cmod21.B, ncomp = ncomp_21B, newdata = spectralcal.df21.B)

pred_df21 <- data.frame(
  DateTime  = USF21$DateTime,
  Predicted = as.numeric(predictedDOC21))

p21 <- ggplot(pred_df21, aes(x = DateTime, y = Predicted)) +
  geom_line(color = "steelblue") +
  labs(x = "DateTime", y = "Predicted DOC (mg/L)",
       title = "USF21: Predicted DOC — PLSR (spectra + TSS)") +
  theme_minimal()
ggplotly(p21)

##########################################################
#### STEP 12: Save outputs                             ####
##########################################################
# NOTE: If your s::can has significant drift (e.g. biofouling), consider a
# moving-window calibration (calibrate 1 month at a time) rather than a
# single model across the full record.

write.csv(pred_df12, file = "predicted/PredictedDOC_USF12_PLSR_TSS.csv", row.names = FALSE)
write.csv(pred_df20, file = "predicted/PredictedDOC_USF20_PLSR_TSS.csv", row.names = FALSE)
write.csv(pred_df21, file = "predicted/PredictedDOC_USF21_PLSR_TSS.csv", row.names = FALSE)

# Upload to Google Drive
drive_folder_id <- "1wa1ycqUYv56y3fTn1-VaN2K-NLU3rFeU"
googledrive::drive_upload(media = "predicted/PredictedDOC_USF12_PLSR_TSS.csv", path = googledrive::as_id(drive_folder_id))
googledrive::drive_upload(media = "predicted/PredictedDOC_USF20_PLSR_TSS.csv", path = googledrive::as_id(drive_folder_id))
googledrive::drive_upload(media = "predicted/PredictedDOC_USF21_PLSR_TSS.csv", path = googledrive::as_id(drive_folder_id))
