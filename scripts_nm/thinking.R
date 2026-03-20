##==============================================================================
## Project: QuEST
## Falling limb analysis: grab samples vs. global sensor calibration
##
## Concept (from sketch):
##   - Pull discharge from USGS via dataRetrieval
##   - Merge with s::can data frames
##   - Classify each 15-min timestep as: Baseflow | Rising limb | Falling limb
##   - For grab sample rows, compare global sensor value vs. lab grab value
##   - Plot 1: Time series with hydrograph + sensor trace + grab samples
##             coloured by flow condition
##   - Plot 2: Box plots — sensor vs. grab AND residuals by flow condition
##   - Plot 3: Same residuals split by site
##   - Table: Mean residual, SD, RMSE by variable / site / flow condition
##
## Variables:
##   DOC  : sensor = DOC_mg.l        | grab = NPOC..mg.C.L.
##   NO3  : sensor = NO3.N_mg.l      | grab = NO3..mg.N.L.
##   TSS  : sensor = TSS_mg.l        | grab = TSS_mg_L
##   Flow : Q.l.s  (from USGS, converted from cfs to L/s)
##==============================================================================

library(dataRetrieval)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xts)
library(zoo)

##==============================================================================
## SECTION 1: Pull scan and USGS discharge data
##==============================================================================
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


# Parameter code 00060 = discharge in cubic feet per second (cfs)
# service = "uv" pulls 15-minute instantaneous values (matches your sensor frequency)

# Pull dates to cover the full range of your s::can data
# Adjust start/end dates to match your deployment period
usgs_raw <- dataRetrieval::readNWISuv(
  siteNumbers = "08315480",
  parameterCd = "00060",
  startDate   = "2024-05-08",   # <- adjust to your earliest sensor date
  endDate     = "2025-10-20"    # <- adjust to your latest sensor date
)

# Check column names — find the right discharge column and update usgs_q_col above
cat("USGS column names:\n")
print(colnames(usgs_raw))

# Rename and convert units
# USGS discharge is in cfs; convert to L/s (1 cfs = 28.3168 L/s)
usgs_q_col     <- "X_00060_00000"
usgs_q <- usgs_raw %>%
  rename(DateTime = dateTime) %>%
  mutate(Q.l.s = .data[[usgs_q_col]] * 28.3168) %>%
  select(DateTime, Q.l.s) %>%
  filter(!is.na(Q.l.s))

cat("\nUSGS discharge data pulled:", nrow(usgs_q), "rows\n")
cat("Date range:", format(min(usgs_q$DateTime)), "to", format(max(usgs_q$DateTime)), "\n")

##==============================================================================
## SECTION 2: Merge discharge with s::can data frames
##==============================================================================
# Uses your existing merge function — merges on DateTime using xts outer join
# so all s::can timestamps are preserved and Q is filled where available.
# Timestamps with no matching USGS reading will have NA for Q.l.s.

merge_usgs_with_scan <- function(scan_df, usgs_df) {
  scan_xts    <- xts(scan_df,  order.by = scan_df$DateTime)
  usgs_xts    <- xts(usgs_df,  order.by = usgs_df$DateTime)
  combined    <- merge(scan_xts, usgs_xts, join = "left")  # left = keep all scan rows
  combined_df <- data.frame(DateTime = index(combined), coredata(combined))
  return(combined_df)
}

USF12 <- merge_usgs_with_scan(USF12, usgs_q)
USF20 <- merge_usgs_with_scan(USF20, usgs_q)
USF21 <- merge_usgs_with_scan(USF21, usgs_q)

USF12$Q.l.s <- as.numeric(USF12$Q.l.s)
USF20$Q.l.s <- as.numeric(USF20$Q.l.s)
USF21$Q.l.s <- as.numeric(USF21$Q.l.s)

USF12$DOC_mg.l <- as.numeric(USF12$DOC_mg.l)
USF20$DOC_mg.l <- as.numeric(USF20$DOC_mg.l)
USF21$DOC_mg.l <- as.numeric(USF21$DOC_mg.l)

USF12$NPOC..mg.C.L. <- as.numeric(USF12$NPOC..mg.C.L.)
USF20$NPOC..mg.C.L. <- as.numeric(USF20$NPOC..mg.C.L.)
USF21$NPOC..mg.C.L. <- as.numeric(USF21$NPOC..mg.C.L.)

USF12$NO3.N_mg.l <- as.numeric(USF12$NO3.N_mg.l)
USF20$NO3.N_mg.l <- as.numeric(USF20$NO3.N_mg.l)
USF21$NO3.N_mg.l <- as.numeric(USF21$NO3.N_mg.l)

USF12$NO3..mg.N.L. <- as.numeric(USF12$NO3..mg.N.L.)
USF20$NO3..mg.N.L. <- as.numeric(USF20$NO3..mg.N.L.)
USF21$NO3..mg.N.L. <- as.numeric(USF21$NO3..mg.N.L.)

USF12$TSS_mg.l <- as.numeric(USF12$TSS_mg.l)
USF20$TSS_mg.l <- as.numeric(USF20$TSS_mg.l)
USF21$TSS_mg.l <- as.numeric(USF21$TSS_mg.l)

USF12$TSS_mg_L <- as.numeric(USF12$TSS_mg_L)
USF20$TSS_mg_L <- as.numeric(USF20$TSS_mg_L)
USF21$TSS_mg_L <- as.numeric(USF21$TSS_mg_L)

# Confirm Q merged correctly
cat("USF12 Q.l.s non-NA rows:", sum(!is.na(USF12$Q.l.s)), "/", nrow(USF12), "\n")
cat("USF20 Q.l.s non-NA rows:", sum(!is.na(USF20$Q.l.s)), "/", nrow(USF20), "\n")
cat("USF21 Q.l.s non-NA rows:", sum(!is.na(USF21$Q.l.s)), "/", nrow(USF21), "\n")

# Quick discharge plot — sanity check before going further
bind_rows(
  USF12 %>% transmute(DateTime, Q.l.s, Site = "USF12"),
  USF20 %>% transmute(DateTime, Q.l.s, Site = "USF20"),
  USF21 %>% transmute(DateTime, Q.l.s, Site = "USF21")
) %>%
  filter(!is.na(Q.l.s)) %>%
  ggplot(aes(x = DateTime, y = Q.l.s)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  facet_wrap(~Site, ncol = 1, scales = "free_y") +
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(title = "Discharge sanity check (all sites share same USGS station)",
       x = "Date", y = "Discharge (L/s)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##==============================================================================
## SECTION 3: Storm classification function
##==============================================================================
# Classifies each row as Baseflow, Rising, or Falling based on discharge.
#
# Method:
#   1. Smooth Q with a rolling median to reduce noise
#   2. Detect storm peaks where smoothed Q exceeds baseflow_multiplier * baseline Q
#   3. Within each storm window:
#        - Rising  = Q increasing toward peak
#        - Falling = Q decreasing from peak back toward baseline
#   4. Everything outside storm windows = Baseflow
#
# Parameters to tune if detection looks wrong in Section 4:
#   baseflow_multiplier : how many times above 10th-percentile Q = storm
#                         lower (e.g. 1.5) catches more events
#                         higher (e.g. 3)  catches only big events
#   peak_window         : timesteps each side of peak to search (default 96 = 24 hrs)
#   window_smoothing    : rolling median window (default 4 = 1 hr at 15-min data)

classify_flow <- function(df,
                          q_col               = "Q.l.s",
                          window_smoothing    = 4,
                          baseflow_multiplier = 2,
                          peak_window         = 96) {
  Q <- df[[q_col]]
  n <- length(Q)
  
  # Interpolate NAs for classification only (not stored back)
  Q_filled <- Q
  if (any(is.na(Q))) {
    Q_filled <- approx(seq_along(Q), Q, seq_along(Q), rule = 2)$y
  }
  
  # Smooth with rolling median
  Q_smooth <- zoo::rollmedian(Q_filled, k = window_smoothing, fill = "extend")
  
  # Baseline = 10th percentile of smoothed Q
  Q_base          <- quantile(Q_smooth, 0.10, na.rm = TRUE)
  storm_threshold <- Q_base * baseflow_multiplier
  
  # Find local peaks above threshold
  is_peak <- rep(FALSE, n)
  for (i in 2:(n - 1)) {
    if (!is.na(Q_smooth[i]) &&
        Q_smooth[i] > storm_threshold &&
        Q_smooth[i] >= Q_smooth[i - 1] &&
        Q_smooth[i] >= Q_smooth[i + 1]) {
      is_peak[i] <- TRUE
    }
  }
  
  condition     <- rep("Baseflow", n)
  peak_indices  <- which(is_peak)
  
  for (pk in peak_indices) {
    start_win <- max(1, pk - peak_window)
    end_win   <- min(n, pk + peak_window)
    
    for (i in start_win:pk) {
      if (!is.na(Q_smooth[i]) && Q_smooth[i] > storm_threshold)
        condition[i] <- "Rising"
    }
    for (i in (pk + 1):end_win) {
      if (!is.na(Q_smooth[i]) && Q_smooth[i] > storm_threshold)
        condition[i] <- "Falling"
    }
  }
  
  df$Flow_condition <- factor(condition,
                              levels = c("Baseflow", "Rising", "Falling"))
  return(df)
}

# Apply to all sites — tune baseflow_multiplier per site if needed
USF12 <- classify_flow(USF12, baseflow_multiplier = 2)
USF20 <- classify_flow(USF20, baseflow_multiplier = 2)
USF21 <- classify_flow(USF21, baseflow_multiplier = 2)

# Quick check: timestep counts per condition
cat("--- USF12 flow condition counts ---\n"); print(table(USF12$Flow_condition))
cat("--- USF20 flow condition counts ---\n"); print(table(USF20$Flow_condition))
cat("--- USF21 flow condition counts ---\n"); print(table(USF21$Flow_condition))

##==============================================================================
## SECTION 4: Diagnostic — check storm detection looks right
##==============================================================================
# Run this and visually confirm Rising/Falling/Baseflow look sensible.
# If storms are missed:  lower baseflow_multiplier in Section 3 (e.g. 1.5)
# If too many detections: raise it (e.g. 3)

plot_storm_detection <- function(df, site) {
  ggplot(df, aes(x = DateTime, y = Q.l.s, color = Flow_condition)) +
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = c("Baseflow" = "grey60",
                                  "Rising"   = "#e31a1c",
                                  "Falling"  = "#1f78b4")) +
    labs(title    = paste0(site, ": Storm detection check"),
         subtitle = "Check Rising/Falling are correctly assigned around storm peaks",
         x = "Date", y = "Discharge (L/s)", color = "Flow condition") +
    scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b %d") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

plot_storm_detection(USF12, "USF12")
plot_storm_detection(USF20, "USF20")
plot_storm_detection(USF21, "USF21")

##==============================================================================
## SECTION 5: Extract grab sample rows with flow condition
##==============================================================================

extract_grabs <- function(df, site_name) {
  df %>%
    filter(!is.na(Sample.Name) & Sample.Name == "Y") %>%
    filter(!is.na(Flow_condition)) %>%
    transmute(
      DateTime       = DateTime,
      Site           = site_name,
      Flow_condition = Flow_condition,
      Q              = Q.l.s,
      # DOC
      DOC_sensor     = DOC_mg.l,
      DOC_grab       = NPOC..mg.C.L.,
      DOC_residual   = NPOC..mg.C.L. - DOC_mg.l,
      # NO3
      NO3_sensor     = NO3.N_mg.l,
      NO3_grab       = NO3..mg.N.L.,
      NO3_residual   = NO3..mg.N.L. - NO3.N_mg.l,
      # TSS
      TSS_sensor     = TSS_mg.l,
      TSS_grab       = TSS_mg_L,
      TSS_residual   = TSS_mg_L - TSS_mg.l
    )
}

grabs12   <- extract_grabs(USF12, "USF12")
grabs20   <- extract_grabs(USF20, "USF20")
grabs21   <- extract_grabs(USF21, "USF21")
all_grabs <- bind_rows(grabs12, grabs20, grabs21)

# Check falling limb sample counts — interpret cautiously if very few
cat("\n--- Grab samples by site and flow condition ---\n")
print(table(all_grabs$Site, all_grabs$Flow_condition))

##==============================================================================
## SECTION 6: Time series — hydrograph + sensor trace + grab samples
##             coloured by flow condition
##==============================================================================

plot_timeseries_hydrograph <- function(df, grabs, sensor_col, grab_col,
                                       ylab, title_var, site) {
  # Scale Q to the sensor value range for dual-axis overlay
  sensor_range <- range(df[[sensor_col]], na.rm = TRUE)
  q_range      <- range(df$Q.l.s,         na.rm = TRUE)
  
  df$Q_scaled <- (df$Q.l.s - q_range[1]) /
    (q_range[2] - q_range[1]) *
    (sensor_range[2] - sensor_range[1]) + sensor_range[1]
  
  grabs_var <- grabs %>%
    select(DateTime, Flow_condition, grab = all_of(grab_col)) %>%
    filter(!is.na(grab))
  
  ggplot() +
    geom_line(data = df,
              aes(x = DateTime, y = Q_scaled),
              color = "grey85", linewidth = 0.5) +
    geom_line(data = df,
              aes(x = DateTime, y = .data[[sensor_col]],
                  color = Flow_condition),
              linewidth = 0.6) +
    geom_point(data = grabs_var,
               aes(x = DateTime, y = grab, fill = Flow_condition),
               shape = 21, color = "black", size = 3, stroke = 0.8) +
    scale_color_manual(values = c("Baseflow" = "grey50",
                                  "Rising"   = "#e31a1c",
                                  "Falling"  = "#1f78b4")) +
    scale_fill_manual(values  = c("Baseflow" = "grey50",
                                  "Rising"   = "#e31a1c",
                                  "Falling"  = "#1f78b4")) +
    labs(title    = paste0(site, ": ", title_var,
                           " — sensor trace + grab samples"),
         subtitle = "Grey = scaled discharge  |  Points = lab grab  |  Line = global sensor",
         x = "Date", y = ylab,
         color = "Flow condition", fill = "Flow condition") +
    scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b %d") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

# DOC
plot_timeseries_hydrograph(USF12, grabs12, "DOC_mg.l",   "DOC_grab", "DOC / NPOC (mg/L)", "DOC", "USF12")
plot_timeseries_hydrograph(USF20, grabs20, "DOC_mg.l",   "DOC_grab", "DOC / NPOC (mg/L)", "DOC", "USF20")
plot_timeseries_hydrograph(USF21, grabs21, "DOC_mg.l",   "DOC_grab", "DOC / NPOC (mg/L)", "DOC", "USF21")

# NO3
plot_timeseries_hydrograph(USF12, grabs12, "NO3.N_mg.l", "NO3_grab", "NO3 (mg N/L)",      "NO3", "USF12")
plot_timeseries_hydrograph(USF20, grabs20, "NO3.N_mg.l", "NO3_grab", "NO3 (mg N/L)",      "NO3", "USF20")
plot_timeseries_hydrograph(USF21, grabs21, "NO3.N_mg.l", "NO3_grab", "NO3 (mg N/L)",      "NO3", "USF21")

# TSS
plot_timeseries_hydrograph(USF12, grabs12, "TSS_mg.l",   "TSS_grab", "TSS (mg/L)",        "TSS", "USF12")
plot_timeseries_hydrograph(USF20, grabs20, "TSS_mg.l",   "TSS_grab", "TSS (mg/L)",        "TSS", "USF20")
plot_timeseries_hydrograph(USF21, grabs21, "TSS_mg.l",   "TSS_grab", "TSS (mg/L)",        "TSS", "USF21")

##==============================================================================
## SECTION 7: Box plots — sensor vs. grab AND residuals by flow condition
##==============================================================================

# ---- Plot A: Sensor vs. grab by flow condition (all sites combined) ----
box_paired <- all_grabs %>%
  pivot_longer(
    cols      = c(DOC_sensor, DOC_grab,
                  NO3_sensor, NO3_grab,
                  TSS_sensor, TSS_grab),
    names_to  = c("Variable", "Type"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value)) %>%
  mutate(Variable = factor(Variable, levels = c("DOC", "NO3", "TSS")),
         Type     = factor(Type, levels = c("grab", "sensor"),
                           labels = c("Lab grab", "Global sensor")))

ggplot(box_paired, aes(x = Flow_condition, y = Value, fill = Type)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5,
               position = position_dodge(0.8), alpha = 0.8) +
  geom_point(aes(color = Type),
             position = position_jitterdodge(jitter.width = 0.15,
                                             dodge.width  = 0.8),
             size = 1.5, alpha = 0.6) +
  facet_wrap(~Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values  = c("Lab grab"      = "#1f78b4",
                                "Global sensor" = "#ff7f00")) +
  scale_color_manual(values = c("Lab grab"      = "#1f78b4",
                                "Global sensor" = "#ff7f00")) +
  labs(title    = "All sites: Lab grab vs. global sensor by flow condition",
       subtitle = "Blue = lab grab  |  Orange = global sensor  |  Closer boxes = better sensor performance",
       x = "Flow condition", y = "Concentration",
       fill = NULL, color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

# ---- Plot B: Residuals (grab - sensor) by flow condition — all sites ----
# Positive = sensor underestimates | Negative = sensor overestimates
box_resid <- all_grabs %>%
  pivot_longer(
    cols      = c(DOC_residual, NO3_residual, TSS_residual),
    names_to  = "Variable",
    values_to = "Residual"
  ) %>%
  filter(!is.na(Residual)) %>%
  mutate(Variable = recode(Variable,
                           "DOC_residual" = "DOC",
                           "NO3_residual" = "NO3",
                           "TSS_residual" = "TSS"),
         Variable = factor(Variable, levels = c("DOC", "NO3", "TSS")))

ggplot(box_resid, aes(x = Flow_condition, y = Residual,
                      fill = Flow_condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
  geom_point(position = position_jitter(width = 0.15),
             size = 1.5, alpha = 0.5, color = "grey30") +
  facet_wrap(~Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("Baseflow" = "grey70",
                               "Rising"   = "#e31a1c",
                               "Falling"  = "#1f78b4")) +
  labs(title    = "All sites: Residual (grab − sensor) by flow condition",
       subtitle = "Above zero = sensor underestimates  |  Below zero = overestimates  |  Dashed = perfect",
       x = "Flow condition", y = "Residual (grab − sensor)",
       fill = "Flow condition") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

# ---- Plot C: Residuals split by site ----
ggplot(box_resid, aes(x = Flow_condition, y = Residual,
                      fill = Flow_condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
  geom_point(position = position_jitter(width = 0.15),
             size = 1.5, alpha = 0.5, color = "grey30") +
  facet_grid(Variable ~ Site, scales = "free_y") +
  scale_fill_manual(values = c("Baseflow" = "grey70",
                               "Rising"   = "#e31a1c",
                               "Falling"  = "#1f78b4")) +
  labs(title    = "By site: Residual (grab − sensor) by flow condition",
       subtitle = "Above zero = sensor underestimates  |  Dashed = perfect agreement",
       x = "Flow condition", y = "Residual (grab − sensor)",
       fill = "Flow condition") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

##==============================================================================
## SECTION 8: Summary statistics table
##==============================================================================
# Mean residual and RMSE by variable, site, and flow condition
# Positive mean residual = sensor systematically underestimates
# Negative mean residual = sensor systematically overestimates

summary_table <- box_resid %>%
  group_by(Site, Variable, Flow_condition) %>%
  summarise(
    N          = sum(!is.na(Residual)),
    Mean_resid = round(mean(Residual, na.rm = TRUE), 3),
    SD_resid   = round(sd(Residual,   na.rm = TRUE), 3),
    RMSE       = round(sqrt(mean(Residual^2, na.rm = TRUE)), 3),
    .groups    = "drop"
  ) %>%
  arrange(Variable, Site, Flow_condition)

cat("\n=== Residual summary by variable, site, and flow condition ===\n")
cat("Positive mean = sensor underestimates | Negative = sensor overestimates\n\n")
print(summary_table, n = Inf)