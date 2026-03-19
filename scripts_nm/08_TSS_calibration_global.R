##==============================================================================
## Project: QuEST
## TSS Sensor Calibration: s::can global calibration -> grab sample correction
## Following Arial's s::can guide
##
## Global sensor column : TSS_mg.l
## Grab sample column   : TSS_mg_L
##
## Models fitted per site:
##   (a) Linear          : TSS grab ~ a + b * TSS sensor
##   (b) Log-log (power) : log(TSS grab) ~ log(TSS sensor)  =>  TSS = a * sensor^b
##   (c) Polynomial      : TSS grab ~ sensor + sensor^2
##   (d) Weighted linear : Linear weighted by 1/sensor^2
##
## NOTE ON TSS: Unlike DOC and NO3, TSS can be zero or near-zero, which will
## cause errors in the log and weighted models (log(0) = -Inf; 1/0 = Inf).
## If you have zeros in your sensor or grab data, either add a small constant
## (e.g. + 0.01) before log-transforming, or drop those rows before fitting.
## A comment is placed at each affected step as a reminder.
##==============================================================================

library(googledrive)
library(xts)
library(dplyr)
library(tidyr)
library(ggplot2)

###################################
#### Clear folders we will use ####
###################################
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

######################################
#### STEP 1: Prep grab sample data ###
######################################
# What you need to do here is match the grab samples with the time stamp of the s::can
# This data was matched using previous scripts
# See scripts merge_params_and_abs and merge_grabsamples_and_scan

########################################
#### STEP 2: Upload scan data frame ####
########################################
# This is the "with chem" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1qjM3Zze-I5ycFCHNcd997UG6gYXBUoX8")

# List all csv files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

# Download each site file
googledrive::drive_download(file = merged$id[merged$name == "USF12_chem_Buttercup.csv"],
                            path = "googledrive/USF12_chem_Buttercup.csv", overwrite = TRUE)
googledrive::drive_download(file = merged$id[merged$name == "USF20_chem_Blossom.csv"],
                            path = "googledrive/USF20_chem_Blossom.csv",  overwrite = TRUE)
googledrive::drive_download(file = merged$id[merged$name == "USF21_chem_Bubbles.csv"],
                            path = "googledrive/USF21_chem_Bubbles.csv",  overwrite = TRUE)

# Load CSVs
USF12 <- read.csv("googledrive/USF12_chem_Buttercup.csv", na = c("", "NaN", "Na", "NA"))
USF20 <- read.csv("googledrive/USF20_chem_Blossom.csv",   na = c("", "NaN", "Na", "NA"))
USF21 <- read.csv("googledrive/USF21_chem_Bubbles.csv",   na = c("", "NaN", "Na", "NA"))

# DateTime at midnight is missing 00:00:00 time, so filling in using grep
for (df_name in c("USF12", "USF20", "USF21")) {
  df  <- get(df_name)
  idx <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$", df$DateTime)
  df$DateTime[idx] <- paste(df$DateTime[idx], "00:00:00")
  assign(df_name, df)
}

# Convert the DateTime column to POSIXct and remove NA rows
USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M:%S")

USF12 <- USF12 %>% filter(!is.na(DateTime))
USF20 <- USF20 %>% filter(!is.na(DateTime))
USF21 <- USF21 %>% filter(!is.na(DateTime))

# Rename columns by removing the leading "X" and trailing ".nm" added by R
# to numeric column names (e.g. X200.nm -> 200)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}
USF12 <- rename_columns(USF12)
USF20 <- rename_columns(USF20)
USF21 <- rename_columns(USF21)

# Extract TSS as xts time series objects
scan_TSS_USF12 <- xts(USF12$TSS_mg.l, order.by = USF12$DateTime)
scan_TSS_USF20 <- xts(USF20$TSS_mg.l, order.by = USF20$DateTime)
scan_TSS_USF21 <- xts(USF21$TSS_mg.l, order.by = USF21$DateTime)

#################################################
####  STEP 3: Compare grab and raw scan data ####
#################################################
# Flag rows that have a paired grab sample
USF12 <- USF12 %>%
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))
USF20 <- USF20 %>%
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))
USF21 <- USF21 %>%
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))

# Filter to grab-sample rows only
grab_USF12 <- USF12[USF12$Grab_sample == "Y" & !is.na(USF12$Grab_sample), ]
grab_USF20 <- USF20[USF20$Grab_sample == "Y" & !is.na(USF20$Grab_sample), ]
grab_USF21 <- USF21[USF21$Grab_sample == "Y" & !is.na(USF21$Grab_sample), ]

# Remove problematic samples 
grab_USF20 <- grab_USF20 %>%
  mutate(TSS_mg_L = ifelse(Date == as.POSIXct(c("2024-09-25")), NA, TSS_mg_L))

# Quick scatter: raw sensor TSS vs. grab TSS
ggplot(grab_USF12, aes(x = TSS_mg_L, y = TSS_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = format(DateTime, "%Y-%m-%d")), vjust = -0.5, size = 3) +
  labs(title = "USF12: Raw sensor vs. grab TSS",
       x = "Grab TSS (mg/L)", y = "s::can TSS (mg/L)") +
  theme_minimal()

ggplot(grab_USF20, aes(x = TSS_mg_L, y = TSS_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = format(DateTime, "%Y-%m-%d")), vjust = -0.5, size = 3) +
  labs(title = "USF20: Raw sensor vs. grab TSS",
       x = "Grab TSS (mg/L)", y = "s::can TSS (mg/L)") +
  theme_minimal()


ggplot(grab_USF21, aes(x = TSS_mg_L, y = TSS_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = format(DateTime, "%Y-%m-%d")), vjust = -0.5, size = 3) +
  labs(title = "USF21: Raw sensor vs. grab TSS",
       x = "Grab TSS (mg/L)", y = "s::can TSS (mg/L)") +
  theme_minimal()

############################
#### STEP 4: Run models ####
############################
# NOTE: in all models the RESPONSE is grab TSS (the "truth") and the
# PREDICTOR is the sensor TSS (what you have continuously).
# That way predict() can be applied directly to the full sensor record.

# Subset to rows where both variables are present before fitting
# NOTE for TSS: also filtering out zeros to avoid log(0) / division-by-zero
# errors in the log and weighted models. If you want to keep zeros, use only
# the linear and polynomial models for those rows.
grab_USF12_fit <- grab_USF12 %>% filter(!is.na(TSS_mg_L), !is.na(TSS_mg.l), TSS_mg.l > 0, TSS_mg_L > 0)
grab_USF20_fit <- grab_USF20 %>% filter(!is.na(TSS_mg_L), !is.na(TSS_mg.l), TSS_mg.l > 0, TSS_mg_L > 0)
grab_USF21_fit <- grab_USF21 %>% filter(!is.na(TSS_mg_L), !is.na(TSS_mg.l), TSS_mg.l > 0, TSS_mg_L > 0)

##########################
#### (a) Linear model ####
##########################
# TSS_grab = a + b * TSS_sensor

## USF12 ##
linear_model12 <- lm(TSS_mg_L ~ TSS_mg.l, data = grab_USF12_fit)
summary(linear_model12)

## USF20 ##
linear_model20 <- lm(TSS_mg_L ~ TSS_mg.l, data = grab_USF20_fit)
summary(linear_model20)

## USF21 ##
linear_model21 <- lm(TSS_mg_L ~ TSS_mg.l, data = grab_USF21_fit)
summary(linear_model21)

####################################
#### (b) Log-log (power) model  ####
####################################
# Fit: log(TSS_grab) = a + b * log(TSS_sensor)  =>  back-transformed: TSS = exp(a) * sensor^b
# TSS often follows a power-law relationship with turbidity-based proxies,
# so this model is worth checking carefully here.

## USF12 ##
log_model12 <- lm(log(TSS_mg_L) ~ log(TSS_mg.l), data = grab_USF12_fit)
summary(log_model12)
cat("USF12 power law: TSS =", exp(coef(log_model12)[1]), "* sensor ^", coef(log_model12)[2], "\n")

## USF20 ##
log_model20 <- lm(log(TSS_mg_L) ~ log(TSS_mg.l), data = grab_USF20_fit)
summary(log_model20)
cat("USF20 power law: TSS =", exp(coef(log_model20)[1]), "* sensor ^", coef(log_model20)[2], "\n")

## USF21 ##
log_model21 <- lm(log(TSS_mg_L) ~ log(TSS_mg.l), data = grab_USF21_fit)
summary(log_model21)
cat("USF21 power law: TSS =", exp(coef(log_model21)[1]), "* sensor ^", coef(log_model21)[2], "\n")

##############################
#### (c) Polynomial model ####
##############################
# TSS_grab = a + b*sensor + c*sensor^2
# Check the summary: if the sensor^2 term is not significant (p > 0.05),
# the relationship is probably linear enough and you should stick with (a).

## USF12 ##
poly_model12 <- lm(TSS_mg_L ~ TSS_mg.l + I(TSS_mg.l^2), data = grab_USF12_fit)
summary(poly_model12)

## USF20 ##
poly_model20 <- lm(TSS_mg_L ~ TSS_mg.l + I(TSS_mg.l^2), data = grab_USF20_fit)
summary(poly_model20)

## USF21 ##
poly_model21 <- lm(TSS_mg_L ~ TSS_mg.l + I(TSS_mg.l^2), data = grab_USF21_fit)
summary(poly_model21)

###################################
#### (d) Weighted linear model ####
###################################
# Same as (a) but weighted by 1/sensor^2, so high-concentration grab samples
# have less influence on the fit. Particularly useful for TSS which often has
# high variance at elevated concentrations.

## USF12 ##
weighted_model12 <- lm(TSS_mg_L ~ TSS_mg.l,
                       weights = 1 / TSS_mg.l^2,
                       data = grab_USF12_fit)
summary(weighted_model12)

## USF20 ##
weighted_model20 <- lm(TSS_mg_L ~ TSS_mg.l,
                       weights = 1 / TSS_mg.l^2,
                       data = grab_USF20_fit)
summary(weighted_model20)

## USF21 ##
weighted_model21 <- lm(TSS_mg_L ~ TSS_mg.l,
                       weights = 1 / TSS_mg.l^2,
                       data = grab_USF21_fit)
summary(weighted_model21)

################################
#### STEP 5: Compare models ####
################################
# R²       : proportion of variance explained (higher = better)
# RMSE     : average prediction error in mg/L (lower = better)
# AIC      : penalises model complexity; lower = better
# LOO-RMSE : most important with small datasets — how well does the model
#            predict a grab sample it has never seen?

compare_models <- function(model_list, model_names, grabs, log_models = NULL) {
  results <- data.frame()
  for (i in seq_along(model_list)) {
    m      <- model_list[[i]]
    is_log <- !is.null(log_models) && model_names[i] %in% log_models
    obs    <- grabs$TSS_mg_L
    fit_v  <- if (is_log) exp(fitted(m)) else fitted(m)
    rmse   <- sqrt(mean((obs - fit_v)^2, na.rm = TRUE))
    r2     <- cor(obs, fit_v, use = "complete.obs")^2

    loo_errs <- numeric(nrow(grabs))
    for (j in seq_len(nrow(grabs))) {
      m_j    <- update(m, data = grabs[-j, ])
      pred_j <- predict(m_j, newdata = grabs[j, ])
      obs_j  <- grabs$TSS_mg_L[j]
      if (is_log) { pred_j <- exp(pred_j) }
      loo_errs[j] <- obs_j - pred_j
    }

    results <- rbind(results, data.frame(
      Model    = model_names[i],
      R2       = round(r2, 3),
      RMSE     = round(rmse, 3),
      AIC      = round(AIC(m), 2),
      LOO_RMSE = round(sqrt(mean(loo_errs^2)), 3)
    ))
  }
  return(results)
}

cat("--- USF12 model comparison ---\n")
compare_models(
  model_list  = list(linear_model12, log_model12, poly_model12, weighted_model12),
  model_names = c("Linear", "Log-Power", "Polynomial", "Weighted"),
  grabs       = grab_USF12_fit,
  log_models  = "Log-Power"
)

cat("--- USF20 model comparison ---\n")
compare_models(
  model_list  = list(linear_model20, log_model20, poly_model20, weighted_model20),
  model_names = c("Linear", "Log-Power", "Polynomial", "Weighted"),
  grabs       = grab_USF20_fit,
  log_models  = "Log-Power"
)

cat("--- USF21 model comparison ---\n")
compare_models(
  model_list  = list(linear_model21, log_model21, poly_model21, weighted_model21),
  model_names = c("Linear", "Log-Power", "Polynomial", "Weighted"),
  grabs       = grab_USF21_fit,
  log_models  = "Log-Power"
)

######################################################
#### STEP 6: Visualise models on calibration data ####
######################################################
plot_calib_fits <- function(grabs, models, model_names, log_models = NULL, site) {
  x_seq <- seq(min(grabs$TSS_mg.l, na.rm = TRUE),
               max(grabs$TSS_mg.l, na.rm = TRUE), length.out = 200)
  nd    <- data.frame(TSS_mg.l = x_seq)

  ribbon_df <- do.call(rbind, lapply(seq_along(models), function(i) {
    is_log <- !is.null(log_models) && model_names[i] %in% log_models
    p      <- predict(models[[i]], newdata = nd, interval = "prediction", level = 0.95)
    if (is_log) p <- exp(p)
    data.frame(TSS_mg.l = x_seq, fit = p[,"fit"],
               lo = p[,"lwr"], hi = p[,"upr"], Model = model_names[i])
  }))

  ggplot() +
    geom_ribbon(data = ribbon_df,
                aes(x = TSS_mg.l, ymin = lo, ymax = hi, fill = Model), alpha = 0.15) +
    geom_line(data  = ribbon_df,
              aes(x = TSS_mg.l, y = fit, color = Model), linewidth = 0.9) +
    geom_point(data = grabs,
               aes(x = TSS_mg.l, y = TSS_mg_L), color = "black", size = 2.5) +
    geom_text(data  = grabs,
              aes(x = TSS_mg.l, y = TSS_mg_L,
                  label = format(DateTime, "%Y-%m-%d")),
              vjust = -0.6, size = 2.5, color = "grey30") +
    labs(title    = paste0(site, ": Calibration model fits"),
         subtitle = "Shaded bands = 95% prediction interval  |  Points = grab samples",
         x = "s::can global TSS (mg/L)", y = "Grab TSS (mg/L)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

## USF12 ##
plot_calib_fits(
  grabs       = grab_USF12_fit,
  models      = list(linear_model12, log_model12, poly_model12, weighted_model12),
  model_names = c("Linear", "Log-Power", "Polynomial", "Weighted"),
  log_models  = "Log-Power",
  site        = "USF12"
)

## USF20 ##
plot_calib_fits(
  grabs       = grab_USF20_fit,
  models      = list(linear_model20, log_model20, poly_model20, weighted_model20),
  model_names = c("Linear", "Log-Power", "Polynomial", "Weighted"),
  log_models  = "Log-Power",
  site        = "USF20"
)

## USF21 ##
plot_calib_fits(
  grabs       = grab_USF21_fit,
  models      = list(linear_model21, log_model21, poly_model21, weighted_model21),
  model_names = c("Linear", "Log-Power", "Polynomial", "Weighted"),
  log_models  = "Log-Power",
  site        = "USF21"
)

###################################
#### STEP 7: Apply predictions ####
###################################
# NOTE: predictions are applied to the full time series including rows where
# TSS_mg.l may be zero or NA. Rows with TSS_mg.l <= 0 will produce NaN for
# the log and weighted models — this is expected and those values will be NA
# in the output, which is the correct behaviour.

## USF12 ##
nd12 <- data.frame(TSS_mg.l = USF12$TSS_mg.l)

p <- predict(linear_model12,   newdata = nd12, interval = "prediction")
USF12$pred_linear      <- p[,"fit"]; USF12$pred_linear_lo   <- p[,"lwr"]; USF12$pred_linear_hi   <- p[,"upr"]

p <- predict(log_model12,      newdata = nd12, interval = "prediction")
USF12$pred_logpower    <- exp(p[,"fit"]); USF12$pred_logpower_lo <- exp(p[,"lwr"]); USF12$pred_logpower_hi <- exp(p[,"upr"])

p <- predict(poly_model12,     newdata = nd12, interval = "prediction")
USF12$pred_poly        <- p[,"fit"]; USF12$pred_poly_lo     <- p[,"lwr"]; USF12$pred_poly_hi     <- p[,"upr"]

p <- predict(weighted_model12, newdata = nd12, interval = "prediction")
USF12$pred_weighted    <- p[,"fit"]; USF12$pred_weighted_lo <- p[,"lwr"]; USF12$pred_weighted_hi <- p[,"upr"]

## USF20 ##
nd20 <- data.frame(TSS_mg.l = USF20$TSS_mg.l)

p <- predict(linear_model20,   newdata = nd20, interval = "prediction")
USF20$pred_linear      <- p[,"fit"]; USF20$pred_linear_lo   <- p[,"lwr"]; USF20$pred_linear_hi   <- p[,"upr"]

p <- predict(log_model20,      newdata = nd20, interval = "prediction")
USF20$pred_logpower    <- exp(p[,"fit"]); USF20$pred_logpower_lo <- exp(p[,"lwr"]); USF20$pred_logpower_hi <- exp(p[,"upr"])

p <- predict(poly_model20,     newdata = nd20, interval = "prediction")
USF20$pred_poly        <- p[,"fit"]; USF20$pred_poly_lo     <- p[,"lwr"]; USF20$pred_poly_hi     <- p[,"upr"]

p <- predict(weighted_model20, newdata = nd20, interval = "prediction")
USF20$pred_weighted    <- p[,"fit"]; USF20$pred_weighted_lo <- p[,"lwr"]; USF20$pred_weighted_hi <- p[,"upr"]

## USF21 ##
nd21 <- data.frame(TSS_mg.l = USF21$TSS_mg.l)

p <- predict(linear_model21,   newdata = nd21, interval = "prediction")
USF21$pred_linear      <- p[,"fit"]; USF21$pred_linear_lo   <- p[,"lwr"]; USF21$pred_linear_hi   <- p[,"upr"]

p <- predict(log_model21,      newdata = nd21, interval = "prediction")
USF21$pred_logpower    <- exp(p[,"fit"]); USF21$pred_logpower_lo <- exp(p[,"lwr"]); USF21$pred_logpower_hi <- exp(p[,"upr"])

p <- predict(poly_model21,     newdata = nd21, interval = "prediction")
USF21$pred_poly        <- p[,"fit"]; USF21$pred_poly_lo     <- p[,"lwr"]; USF21$pred_poly_hi     <- p[,"upr"]

p <- predict(weighted_model21, newdata = nd21, interval = "prediction")
USF21$pred_weighted    <- p[,"fit"]; USF21$pred_weighted_lo <- p[,"lwr"]; USF21$pred_weighted_hi <- p[,"upr"]

#####################################
#### STEP 8: Compare predictions ####
#####################################

plot_pred_vs_obs <- function(grabs, site_df, site) {
  df <- grabs %>%
    left_join(site_df %>% select(DateTime, pred_linear, pred_logpower,
                                 pred_poly, pred_weighted), by = "DateTime") %>%
    filter(!is.na(TSS_mg_L)) %>%
    pivot_longer(cols = c(pred_linear, pred_logpower, pred_poly, pred_weighted),
                 names_to = "Model", values_to = "Predicted")

  ggplot(df, aes(x = TSS_mg_L, y = Predicted, color = Model)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.5, alpha = 0.8) +
    facet_wrap(~Model, ncol = 2) +
    labs(title    = paste0(site, ": Predicted vs. Observed (grab samples)"),
         subtitle = "Points on dashed 1:1 line = perfect prediction",
         x = "Observed grab TSS (mg/L)", y = "Predicted TSS (mg/L)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
}

## USF12 ##
plot_pred_vs_obs(grab_USF12_fit, USF12, "USF12")

## USF20 ##
plot_pred_vs_obs(grab_USF20_fit, USF20, "USF20")

## USF21 ##
plot_pred_vs_obs(grab_USF21_fit, USF21, "USF21")

# Residuals vs. fitted
plot_residuals <- function(grabs, site_df, site) {
  df <- grabs %>%
    left_join(site_df %>% select(DateTime, pred_linear, pred_logpower,
                                 pred_poly, pred_weighted), by = "DateTime") %>%
    filter(!is.na(TSS_mg_L)) %>%
    pivot_longer(cols = c(pred_linear, pred_logpower, pred_poly, pred_weighted),
                 names_to = "Model", values_to = "Predicted") %>%
    mutate(Residual = TSS_mg_L - Predicted)

  ggplot(df, aes(x = Predicted, y = Residual, color = Model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_text(aes(label = format(DateTime, "%m-%d")), vjust = -0.6, size = 2.2) +
    facet_wrap(~Model, ncol = 2) +
    labs(title    = paste0(site, ": Residuals"),
         subtitle = "Random scatter around 0 = good  |  Curved pattern = try a different model",
         x = "Predicted TSS (mg/L)", y = "Residual (Observed - Predicted)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
}

## USF12 ##
plot_residuals(grab_USF12_fit, USF12, "USF12")

## USF20 ##
plot_residuals(grab_USF20_fit, USF20, "USF20")

## USF21 ##
plot_residuals(grab_USF21_fit, USF21, "USF21")

######################################
#### STEP 9: Plot full time series ####
######################################

## USF12 ##
df_long12 <- USF12 %>%
  select(DateTime, TSS_mg.l, pred_linear, pred_logpower, pred_poly, pred_weighted) %>%
  pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = recode(Variable,
                           "TSS_mg.l"      = "Raw sensor",
                           "pred_linear"   = "Linear",
                           "pred_logpower" = "Log-Power",
                           "pred_poly"     = "Polynomial",
                           "pred_weighted" = "Weighted"))

df_grabs12 <- grab_USF12 %>% filter(!is.na(TSS_mg_L))

ggplot() +
  geom_ribbon(data = USF12,
              aes(x = DateTime, ymin = pred_linear_lo, ymax = pred_linear_hi),
              fill = "steelblue", alpha = 0.15) +
  geom_line(data = df_long12,
            aes(x = DateTime, y = Value, color = Variable, linetype = Variable), linewidth = 0.7) +
  geom_point(data = df_grabs12,
             aes(x = DateTime, y = TSS_mg_L),
             color = "black", size = 2.5, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(values = c("Raw sensor" = "grey60", "Linear" = "#1f78b4",
                                "Log-Power"  = "#e31a1c", "Polynomial" = "#33a02c",
                                "Weighted"   = "#ff7f00")) +
  scale_linetype_manual(values = c("Raw sensor" = "dashed", "Linear" = "solid",
                                   "Log-Power"  = "solid",  "Polynomial" = "solid",
                                   "Weighted"   = "solid")) +
  labs(title    = "USF12: TSS time series — all models",
       subtitle = "Shaded band = 95% PI (linear)  |  Open circles = grab samples  |  Dashed = raw sensor",
       x = "Date", y = "TSS (mg/L)", color = NULL, linetype = NULL) +
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

## USF20 ##
df_long20 <- USF20 %>%
  select(DateTime, TSS_mg.l, pred_linear, pred_logpower, pred_poly, pred_weighted) %>%
  pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = recode(Variable,
                           "TSS_mg.l"      = "Raw sensor",
                           "pred_linear"   = "Linear",
                           "pred_logpower" = "Log-Power",
                           "pred_poly"     = "Polynomial",
                           "pred_weighted" = "Weighted"))

df_grabs20 <- grab_USF20 %>% filter(!is.na(TSS_mg_L))

ggplot() +
  geom_ribbon(data = USF20,
              aes(x = DateTime, ymin = pred_linear_lo, ymax = pred_linear_hi),
              fill = "steelblue", alpha = 0.15) +
  geom_line(data = df_long20,
            aes(x = DateTime, y = Value, color = Variable, linetype = Variable), linewidth = 0.7) +
  geom_point(data = df_grabs20,
             aes(x = DateTime, y = TSS_mg_L),
             color = "black", size = 2.5, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(values = c("Raw sensor" = "grey60", "Linear" = "#1f78b4",
                                "Log-Power"  = "#e31a1c", "Polynomial" = "#33a02c",
                                "Weighted"   = "#ff7f00")) +
  scale_linetype_manual(values = c("Raw sensor" = "dashed", "Linear" = "solid",
                                   "Log-Power"  = "solid",  "Polynomial" = "solid",
                                   "Weighted"   = "solid")) +
  labs(title    = "USF20: TSS time series — all models",
       subtitle = "Shaded band = 95% PI (linear)  |  Open circles = grab samples  |  Dashed = raw sensor",
       x = "Date", y = "TSS (mg/L)", color = NULL, linetype = NULL) +
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

## USF21 ##
df_long21 <- USF21 %>%
  select(DateTime, TSS_mg.l, pred_linear, pred_logpower, pred_poly, pred_weighted) %>%
  pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = recode(Variable,
                           "TSS_mg.l"      = "Raw sensor",
                           "pred_linear"   = "Linear",
                           "pred_logpower" = "Log-Power",
                           "pred_poly"     = "Polynomial",
                           "pred_weighted" = "Weighted"))

df_grabs21 <- grab_USF21 %>% filter(!is.na(TSS_mg_L))

ggplot() +
  geom_ribbon(data = USF21,
              aes(x = DateTime, ymin = pred_linear_lo, ymax = pred_linear_hi),
              fill = "steelblue", alpha = 0.15) +
  geom_line(data = df_long21,
            aes(x = DateTime, y = Value, color = Variable, linetype = Variable), linewidth = 0.7) +
  geom_point(data = df_grabs21,
             aes(x = DateTime, y = TSS_mg_L),
             color = "black", size = 2.5, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(values = c("Raw sensor" = "grey60", "Linear" = "#1f78b4",
                                "Log-Power"  = "#e31a1c", "Polynomial" = "#33a02c",
                                "Weighted"   = "#ff7f00")) +
  scale_linetype_manual(values = c("Raw sensor" = "dashed", "Linear" = "solid",
                                   "Log-Power"  = "solid",  "Polynomial" = "solid",
                                   "Weighted"   = "solid")) +
  labs(title    = "USF21: TSS time series — all models",
       subtitle = "Shaded band = 95% PI (linear)  |  Open circles = grab samples  |  Dashed = raw sensor",
       x = "Date", y = "TSS (mg/L)", color = NULL, linetype = NULL) +
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

######################################
#### STEP 10: Clean up datasets   ####
######################################
# Keep only: DateTime, global sensor TSS, grab sample TSS, grab identifier
# columns (Date, Site), and all model predictions + prediction intervals

USF12 <- USF12 %>%
  select(DateTime, TSS_mg.l, Date, Site,
         TSS_mg_L,
         pred_linear,    pred_linear_lo,    pred_linear_hi,
         pred_logpower,  pred_logpower_lo,  pred_logpower_hi,
         pred_poly,      pred_poly_lo,      pred_poly_hi,
         pred_weighted,  pred_weighted_lo,  pred_weighted_hi)

USF20 <- USF20 %>%
  select(DateTime, TSS_mg.l, Date, Site,
         TSS_mg_L,
         pred_linear,    pred_linear_lo,    pred_linear_hi,
         pred_logpower,  pred_logpower_lo,  pred_logpower_hi,
         pred_poly,      pred_poly_lo,      pred_poly_hi,
         pred_weighted,  pred_weighted_lo,  pred_weighted_hi)

USF21 <- USF21 %>%
  select(DateTime, TSS_mg.l, Date, Site,
         TSS_mg_L,
         pred_linear,    pred_linear_lo,    pred_linear_hi,
         pred_logpower,  pred_logpower_lo,  pred_logpower_hi,
         pred_poly,      pred_poly_lo,      pred_poly_hi,
         pred_weighted,  pred_weighted_lo,  pred_weighted_hi)

#######################
#### Save in Drive ####
#######################
write.csv(USF12, file = "predicted/PredictedTSS_USF12_global.csv")
write.csv(USF20, file = "predicted/PredictedTSS_USF20_global.csv")
write.csv(USF21, file = "predicted/PredictedTSS_USF21_global.csv")

# Define the target folder ID in Google Drive
# This is the "Outputs" folder in the global calibration
drive_folder_id <- "17nqwOTPwHLC4f2scQuP3qA1DmoX5gAtl"

# Upload the file to the specified Google Drive folder
drive_upload(media = "predicted/PredictedTSS_USF12_global.csv", path = as_id(drive_folder_id))
drive_upload(media = "predicted/PredictedTSS_USF20_global.csv", path = as_id(drive_folder_id))
drive_upload(media = "predicted/PredictedTSS_USF21_global.csv", path = as_id(drive_folder_id))
