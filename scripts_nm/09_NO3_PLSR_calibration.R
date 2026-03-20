##==============================================================================
## Project: QuEST
## Nitrate Sensor Calibration: PLSR using absorbance spectra + global TSS
## Following Arial's s::can guide
##
## Absorbance columns : 19:118  (already turbidity-compensated)
## TSS global column  : col 14  (TSS_mg.l — used as hydrological state indicator)
## Grab sample column : NO3..mg.N.L.
##
## Two models are fitted and compared for each site:
##   Model A (spectra only)     : NO3 ~ Absorbance spectra
##   Model B (spectra + TSS)    : NO3 ~ Absorbance spectra + global TSS
##
## Rationale for adding TSS:
##   Nitrate has strong UV absorption that can be affected by turbidity even
##   after spectral compensation. TSS here acts as a hydrological state
##   indicator, potentially helping the model distinguish high-nitrate from
##   high-turbidity conditions during storm events. We compare LOO-RMSE for
##   both models so you can decide which to use per site.
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

# Extract NO3 and TSS as xts time series
scan_NO3_USF12 <- xts(USF12$NO3.N_mg.l, order.by = USF12$DateTime)
scan_NO3_USF20 <- xts(USF20$NO3.N_mg.l, order.by = USF20$DateTime)
scan_NO3_USF21 <- xts(USF21$NO3.N_mg.l, order.by = USF21$DateTime)

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

# NOTE: No problematic samples removed yet for NO3 — add any bad datetimes here:
# grab_USF12 <- grab_USF12 %>%
#   mutate(NO3..mg.N.L. = ifelse(DateTime == as.POSIXct("YYYY-MM-DD HH:MM:SS"), NA, NO3..mg.N.L.))

# Extract grab NO3 and TSS vectors
grab.NO312 <- grab_USF12$NO3..mg.N.L.
grab.NO320 <- grab_USF20$NO3..mg.N.L.
grab.NO321 <- grab_USF21$NO3..mg.N.L.

grab.TSS12 <- grab_USF12$TSS_mg.l
grab.TSS20 <- grab_USF20$TSS_mg.l
grab.TSS21 <- grab_USF21$TSS_mg.l

# Quick check: how does TSS vary across your grab samples?
summary(grab.TSS12)
summary(grab.TSS20)
summary(grab.TSS21)

##########################################################################
#### STEP 4: Build spectral matrices from GRAB sample rows (training) ####
##########################################################################
# Absorbance columns: 19:118 (already turbidity-compensated)

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

# Plot full spectra — identify problem time periods
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
# Model A: spectra only (baseline)
# Model B: spectra + global TSS (TSS as hydrological state indicator)

## USF12 ##
grabcal.df12.A <- data.frame(NO3 = grab.NO312,
                              Spectra = I(grab.spec12))
grabcal.df12.B <- data.frame(NO3 = grab.NO312,
                              Spectra = I(grab.spec12),
                              TSS = grab.TSS12)
spectralcal.df12.A <- data.frame(NO3 = scan_NO3_USF12,
                                  Spectra = I(scan.spec12))
spectralcal.df12.B <- data.frame(NO3 = scan_NO3_USF12,
                                  Spectra = I(scan.spec12),
                                  TSS = as.numeric(scan_TSS_USF12))

## USF20 ##
grabcal.df20.A <- data.frame(NO3 = grab.NO320,
                              Spectra = I(grab.spec20))
grabcal.df20.B <- data.frame(NO3 = grab.NO320,
                              Spectra = I(grab.spec20),
                              TSS = grab.TSS20)
spectralcal.df20.A <- data.frame(NO3 = scan_NO3_USF20,
                                  Spectra = I(scan.spec20))
spectralcal.df20.B <- data.frame(NO3 = scan_NO3_USF20,
                                  Spectra = I(scan.spec20),
                                  TSS = as.numeric(scan_TSS_USF20))

## USF21 ##
grabcal.df21.A <- data.frame(NO3 = grab.NO321,
                              Spectra = I(grab.spec21))
grabcal.df21.B <- data.frame(NO3 = grab.NO321,
                              Spectra = I(grab.spec21),
                              TSS = grab.TSS21)
spectralcal.df21.A <- data.frame(NO3 = scan_NO3_USF21,
                                  Spectra = I(scan.spec21))
spectralcal.df21.B <- data.frame(NO3 = scan_NO3_USF21,
                                  Spectra = I(scan.spec21),
                                  TSS = as.numeric(scan_TSS_USF21))

##########################################################
#### STEP 7: Fit PLSR models — Model A (spectra only) ####
##########################################################
## USF12 — Model A ##
Nmod12.A <- plsr(NO3 ~ Spectra, ncomp = 24, data = grabcal.df12.A, validation = "LOO")
summary(Nmod12.A)
plot(RMSEP(Nmod12.A), legendpos = "topright", main = "USF12 Model A (spectra only): RMSEP")
ncomp_12A <- 3   # <- UPDATE after inspecting the RMSEP plot
plot(Nmod12.A, ncomp = ncomp_12A, asp = 1, line = TRUE,
     main = paste0("USF12 Model A: predicted vs measured (ncomp = ", ncomp_12A, ")"))

## USF20 — Model A ##
Nmod20.A <- plsr(NO3 ~ Spectra, ncomp = 15, data = grabcal.df20.A, validation = "LOO")
summary(Nmod20.A)
plot(RMSEP(Nmod20.A), legendpos = "topright", main = "USF20 Model A (spectra only): RMSEP")
ncomp_20A <- 3   # <- UPDATE
plot(Nmod20.A, ncomp = ncomp_20A, asp = 1, line = TRUE,
     main = paste0("USF20 Model A: predicted vs measured (ncomp = ", ncomp_20A, ")"))

## USF21 — Model A ##
Nmod21.A <- plsr(NO3 ~ Spectra, ncomp = 9, data = grabcal.df21.A, validation = "LOO")
summary(Nmod21.A)
plot(RMSEP(Nmod21.A), legendpos = "topright", main = "USF21 Model A (spectra only): RMSEP")
ncomp_21A <- 2   # <- UPDATE
plot(Nmod21.A, ncomp = ncomp_21A, asp = 1, line = TRUE,
     main = paste0("USF21 Model A: predicted vs measured (ncomp = ", ncomp_21A, ")"))

###########################################################
#### STEP 8: Fit PLSR models — Model B (spectra + TSS) ####
###########################################################
## USF12 — Model B ##
Nmod12.B <- plsr(NO3 ~ Spectra + TSS, ncomp = 25, data = grabcal.df12.B, validation = "LOO")
summary(Nmod12.B)
plot(RMSEP(Nmod12.B), legendpos = "topright", main = "USF12 Model B (spectra + TSS): RMSEP")
ncomp_12B <- 2   # <- UPDATE
plot(Nmod12.B, ncomp = ncomp_12B, asp = 1, line = TRUE,
     main = paste0("USF12 Model B: predicted vs measured (ncomp = ", ncomp_12B, ")"))

## USF20 — Model B ##
Nmod20.B <- plsr(NO3 ~ Spectra + TSS, ncomp = 15, data = grabcal.df20.B, validation = "LOO")
summary(Nmod20.B)
plot(RMSEP(Nmod20.B), legendpos = "topright", main = "USF20 Model B (spectra + TSS): RMSEP")
ncomp_20B <- 2   # <- UPDATE
plot(Nmod20.B, ncomp = ncomp_20B, asp = 1, line = TRUE,
     main = paste0("USF20 Model B: predicted vs measured (ncomp = ", ncomp_20B, ")"))

## USF21 — Model B ##
Nmod21.B <- plsr(NO3 ~ Spectra + TSS, ncomp = 9, data = grabcal.df21.B, validation = "LOO")
summary(Nmod21.B)
plot(RMSEP(Nmod21.B), legendpos = "topright", main = "USF21 Model B (spectra + TSS): RMSEP")
ncomp_21B <- 4   # <- UPDATE
plot(Nmod21.B, ncomp = ncomp_21B, asp = 1, line = TRUE,
     main = paste0("USF21 Model B: predicted vs measured (ncomp = ", ncomp_21B, ")"))

############################################
#### STEP 9: Compare Model A vs Model B ####
############################################

extract_model_stats <- function(model, ncomp, model_name, site) {
  loo_rmse <- RMSEP(model, estimate = "CV")$val[1, 1, ncomp + 1]
  data.frame(Site = site, Model = model_name, ncomp = ncomp,
             LOO_RMSE = round(loo_rmse, 4),
             Var_Y_pct = round(cumsum(explvar(model))[ncomp], 2))
}

comparison <- rbind(
  extract_model_stats(Nmod12.A, ncomp_12A, "A: Spectra only",  "USF12"),
  extract_model_stats(Nmod12.B, ncomp_12B, "B: Spectra + TSS", "USF12"),
  extract_model_stats(Nmod20.A, ncomp_20A, "A: Spectra only",  "USF20"),
  extract_model_stats(Nmod20.B, ncomp_20B, "B: Spectra + TSS", "USF20"),
  extract_model_stats(Nmod21.A, ncomp_21A, "A: Spectra only",  "USF21"),
  extract_model_stats(Nmod21.B, ncomp_21B, "B: Spectra + TSS", "USF21")
)

cat("\n=== NO3 PLSR Model Comparison: A (spectra only) vs B (spectra + TSS) ===\n")
print(comparison)
cat("\nInterpretation: prefer Model B if its LOO_RMSE is >10% lower than Model A.\n")
cat("If similar or higher, TSS is not adding useful information and Model A is preferred.\n")

#################################
#### STEP 10: Loadings plots ####
#################################

## USF12 ##
plot(Nmod12.A, plottype = "loading", comps = 1:2, main = "USF12 Model A: Loadings")
plot(Nmod12.B, plottype = "loading", comps = 1:2, main = "USF12 Model B: Loadings")

## USF20 ##
plot(Nmod20.A, plottype = "loading", comps = 1:2, main = "USF20 Model A: Loadings")
plot(Nmod20.B, plottype = "loading", comps = 1:2, main = "USF20 Model B: Loadings")

## USF21 ##
plot(Nmod21.A, plottype = "loading", comps = 1:2, main = "USF21 Model A: Loadings")
plot(Nmod21.B, plottype = "loading", comps = 1:2, main = "USF21 Model B: Loadings")

#######################################
#### STEP 11: Generate predictions ####
#######################################
# Default: using Model B (spectra + TSS) — swap .B to .A and _12B to _12A to revert.

## USF12 ##
predictedNO312 <- predict(Nmod12.A, ncomp = ncomp_12A, newdata = spectralcal.df12.A)

pred_df12 <- data.frame(
  DateTime  = USF12$DateTime,
  Predicted = as.numeric(predictedNO312))

p12 <- ggplot(pred_df12, aes(x = DateTime, y = Predicted)) +
  geom_line(color = "steelblue") +
  labs(x = "DateTime", y = "Predicted NO3 (mg N/L)",
       title = "USF12: Predicted NO3 — PLSR (spectra)") +
  theme_minimal()
ggplotly(p12)

## USF20 ##
predictedNO320 <- predict(Nmod20.A, ncomp = ncomp_20A, newdata = spectralcal.df20.A)

pred_df20 <- data.frame(
  DateTime  = USF20$DateTime,
  Predicted = as.numeric(predictedNO320))

p20 <- ggplot(pred_df20, aes(x = DateTime, y = Predicted)) +
  geom_line(color = "steelblue") +
  labs(x = "DateTime", y = "Predicted NO3 (mg N/L)",
       title = "USF20: Predicted NO3 — PLSR (spectra)") +
  theme_minimal()
ggplotly(p20)

## USF21 ##
predictedNO321 <- predict(Nmod21.A, ncomp = ncomp_21A, newdata = spectralcal.df21.A)

pred_df21 <- data.frame(
  DateTime  = USF21$DateTime,
  Predicted = as.numeric(predictedNO321))

p21 <- ggplot(pred_df21, aes(x = DateTime, y = Predicted)) +
  geom_line(color = "steelblue") +
  labs(x = "DateTime", y = "Predicted NO3 (mg N/L)",
       title = "USF21: Predicted NO3 — PLSR (spectra)") +
  theme_minimal()
ggplotly(p21)

###############################
#### STEP 12: Save outputs ####
###############################
write.csv(pred_df12, file = "predicted/PredictedNO3_USF12_PLSR_TSS.csv", row.names = FALSE)
write.csv(pred_df20, file = "predicted/PredictedNO3_USF20_PLSR_TSS.csv", row.names = FALSE)
write.csv(pred_df21, file = "predicted/PredictedNO3_USF21_PLSR_TSS.csv", row.names = FALSE)

# Upload to Google Drive
drive_folder_id <- "1wa1ycqUYv56y3fTn1-VaN2K-NLU3rFeU"
googledrive::drive_upload(media = "predicted/PredictedNO3_USF12_PLSR_TSS.csv", path = googledrive::as_id(drive_folder_id))
googledrive::drive_upload(media = "predicted/PredictedNO3_USF20_PLSR_TSS.csv", path = googledrive::as_id(drive_folder_id))
googledrive::drive_upload(media = "predicted/PredictedNO3_USF21_PLSR_TSS.csv", path = googledrive::as_id(drive_folder_id))
