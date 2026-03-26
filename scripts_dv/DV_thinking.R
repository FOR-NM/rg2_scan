##==============================================================================
## Project: QuEST
## Grab sample coverage analysis
##
## How well do the grab samples cover the range of the full sensor record?
##
## Plots produced:
##   1. Density overlay    : full sensor distribution vs. grab sample distribution
##   2. Empirical CDF      : same comparison as cumulative curves
##   3. Time series coverage: sensor record with grab sample times marked,
##                            coloured by whether the grab value falls inside
##                            or outside the "typical" sensor range
##   4. Coverage summary table: % of sensor range covered by grabs,
##                              % of sensor readings outside grab range
##
## Variables: DOC (DOCeq..mg.l....Measured.value / NPOC..mg.C.L.)
##            NO3 (NO3.Neq..mg.l....Measured.value / NO3..mg.N.L.)
##            TSS (TSS_mg.l / TSS_mg_L)
##==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

##==============================================================================
## SECTION 1: Pull scan and USGS discharge data
##==============================================================================
scan <- googledrive::as_id("https://drive.google.com/drive/folders/12N6uUxXTttdnadDrn43mL6ilz-Ui2eQX")

merged <- googledrive::drive_ls(path = scan, type = "csv")

googledrive::drive_download(file = merged$id[merged$name == "DVO_chem.csv"],
                            path = "googledrive/DVO_chem.csv", overwrite = TRUE)
googledrive::drive_download(file = merged$id[merged$name == "DVMS1_chem.csv"],
                            path = "googledrive/DVMS1_chem.csv",  overwrite = TRUE)
googledrive::drive_download(file = merged$id[merged$name == "DVNWT5_chem.csv"],
                            path = "googledrive/DVNWT5_chem.csv",  overwrite = TRUE)

DVO <- read.csv("googledrive/DVO_chem.csv", na = c("", "NaN", "Na", "NA"))
DVMS1 <- read.csv("googledrive/DVMS1_chem.csv",   na = c("", "NaN", "Na", "NA"))
DVNWT5 <- read.csv("googledrive/DVNWT5_chem.csv",   na = c("", "NaN", "Na", "NA"))

# Fill midnight timestamps
for (df_name in c("DVO", "DVMS1", "DVNWT5")) {
  df  <- get(df_name)
  idx <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$", df$DateTime)
  df$DateTime[idx] <- paste(df$DateTime[idx], "00:00:00")
  assign(df_name, df)
}

DVO$DateTime <- as.POSIXct(DVO$DateTime, format = "%Y-%m-%d %H:%M:%S")
DVMS1$DateTime <- as.POSIXct(DVMS1$DateTime, format = "%Y-%m-%d %H:%M:%S")
DVNWT5$DateTime <- as.POSIXct(DVNWT5$DateTime, format = "%Y-%m-%d %H:%M:%S")

DVO <- DVO %>% filter(!is.na(DateTime))
DVMS1 <- DVMS1 %>% filter(!is.na(DateTime))
DVNWT5 <- DVNWT5 %>% filter(!is.na(DateTime))

##==============================================================================
## SECTION 1: Build tidy long-format data for each variable
##==============================================================================
# We need two things for each variable at each site:
#   (a) The full 15-min sensor record (sensor_val)
#   (b) The sensor value at grab sample times, paired with the lab value

# Helper: extract full sensor + grab rows for one variable at one site
build_coverage_df <- function(df, site_name,
                              sensor_col, grab_col,
                              var_label) {
  # Full sensor record
  full <- df %>%
    filter(!is.na(.data[[sensor_col]])) %>%
    transmute(DateTime,
              Site     = site_name,
              Variable = var_label,
              Type     = "Full sensor record",
              Value    = .data[[sensor_col]])
  
  # Grab sample rows — use the SENSOR value at that time (not the lab value)
  # This shows where in the sensor distribution the grabs were collected
  grabs_sensor <- df %>%
    filter(!is.na(Site) & Site == "Y",
           !is.na(.data[[grab_col]]),
           !is.na(.data[[sensor_col]])) %>%
    transmute(DateTime,
              Site     = site_name,
              Variable = var_label,
              Type     = "Sensor at grab times",
              Value    = .data[[sensor_col]])
  
  # Also keep the lab grab value separately for the time series plot
  grabs_lab <- df %>%
    filter(!is.na(Site) & Site == "Y",
           !is.na(.data[[grab_col]])) %>%
    transmute(DateTime,
              Site      = site_name,
              Variable  = var_label,
              Sensor_at_grab = .data[[sensor_col]],
              Lab_value      = .data[[grab_col]])
  
  list(full = full, grabs_sensor = grabs_sensor, grabs_lab = grabs_lab)
}

# Build for all sites and variables
vars <- list(
  list(sensor = "DOCeq..mg.l....Measured.value",   grab = "NPOC..mg.C.L.", label = "DOC (mg/L)"),
  list(sensor = "NO3.Neq..mg.l....Measured.value", grab = "NO3..mg.N.L.",  label = "NO3 (mg N/L)")
)

sites <- list(
  list(df = DVO, name = "DVO"),
  list(df = DVMS1, name = "DVMS1"),
  list(df = DVNWT5, name = "DVNWT5")
)

# Collect everything into combined data frames
all_dist   <- data.frame()   # for density + CDF plots
all_grabs  <- data.frame()   # for time series coverage plot

for (s in sites) {
  for (v in vars) {
    result <- build_coverage_df(s$df, s$name, v$sensor, v$grab, v$label)
    all_dist  <- bind_rows(all_dist,  result$full, result$grabs_sensor)
    all_grabs <- bind_rows(all_grabs, result$grabs_lab %>% mutate(Site = s$name))
  }
}

all_dist$Type <- factor(all_dist$Type,
                        levels = c("Full sensor record", "Sensor at grab times"))

##==============================================================================
## SECTION 2: Plot 1 — Density overlay
##==============================================================================
# Blue = full 15-min sensor distribution
# Orange = sensor values at grab sample times
#
# What to look for:
#   - Orange peak in same place as blue = grabs are representative
#   - Orange peak shifted left of blue = grabs missed high-concentration events
#   - Orange has no right tail where blue does = extrapolation risk at high values

ggplot(all_dist, aes(x = Value, fill = Type, color = Type)) +
  geom_density(alpha = 0.4, linewidth = 0.7) +
  facet_grid(Site ~ Variable, scales = "free") +
  scale_fill_manual(values  = c("Full sensor record"    = "#1f78b4",
                                "Sensor at grab times"  = "#ff7f00")) +
  scale_color_manual(values = c("Full sensor record"    = "#1f78b4",
                                "Sensor at grab times"  = "#ff7f00")) +
  labs(title    = "Grab sample coverage: sensor value distribution",
       subtitle = "Orange = sensor readings at grab times  |  Blue = full 15-min record\nOrange shifted left of blue = grab samples missed high-concentration events",
       x = "Sensor value", y = "Density",
       fill = NULL, color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

##==============================================================================
## SECTION 3: Plot 2 — Empirical CDF
##==============================================================================
# CDF shows cumulative probability — easier to read quantitatively than density.
# The gap between the two lines at the high end shows what % of sensor readings
# are above the maximum grab sample value (i.e. pure extrapolation territory).
#
# What to look for:
#   - Lines close together = good coverage
#   - Orange line reaches 1.0 (100%) well before blue = grabs don't cover the
#     upper tail of the sensor distribution = extrapolation at high values

ggplot(all_dist, aes(x = Value, color = Type)) +
  stat_ecdf(linewidth = 0.9) +
  facet_grid(Site ~ Variable, scales = "free_x") +
  scale_color_manual(values = c("Full sensor record"   = "#1f78b4",
                                "Sensor at grab times" = "#ff7f00")) +
  labs(title    = "Empirical CDF: grab sample vs. full sensor range",
       subtitle = "Orange reaching 1.0 before blue = grab samples do not cover high-concentration tail\nGap at right = % of sensor readings in extrapolation territory",
       x = "Sensor value", y = "Cumulative probability",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

##==============================================================================
## SECTION 4: Plot 3 — Time series coverage
##==============================================================================
# Shows the full sensor time series with grab sample points overlaid.
# Points are coloured by whether the grab lab value falls INSIDE or OUTSIDE
# the range of all other grab samples (i.e. is this grab sample adding new
# information or just resampling already-covered conditions?)
#
# More useful framing: colour by whether the SENSOR VALUE at grab time is
# above the 90th percentile of the full sensor record — these are the grabs
# that were actually collected during elevated conditions.

plot_timeseries_coverage <- function(full_df, grabs_df, sensor_col,
                                     grab_col, site, var_label, ylab) {
  
  # 90th percentile of full sensor record
  p90 <- quantile(full_df[[sensor_col]], 0.90, na.rm = TRUE)
  p95 <- quantile(full_df[[sensor_col]], 0.95, na.rm = TRUE)
  
  grabs_plot <- grabs_df %>%
    filter(!is.na(.data[[grab_col]]), !is.na(.data[[sensor_col]])) %>%
    mutate(Coverage = case_when(
      .data[[sensor_col]] >= p95 ~ "Top 5% of sensor range",
      .data[[sensor_col]] >= p90 ~ "Top 10% of sensor range",
      TRUE                       ~ "Below 90th percentile"
    ),
    Coverage = factor(Coverage,
                      levels = c("Below 90th percentile",
                                 "Top 10% of sensor range",
                                 "Top 5% of sensor range")))
  
  ggplot() +
    # Full sensor trace
    geom_line(data = full_df,
              aes(x = DateTime, y = .data[[sensor_col]]),
              color = "grey70", linewidth = 0.5) +
    # 90th and 95th percentile reference lines
    geom_hline(yintercept = p90, linetype = "dashed",
               color = "#ff7f00", linewidth = 0.6) +
    geom_hline(yintercept = p95, linetype = "dashed",
               color = "#e31a1c", linewidth = 0.6) +
    # Grab sample points
    geom_point(data = grabs_plot,
               aes(x = DateTime, y = .data[[grab_col]],
                   fill = Coverage, size = Coverage),
               shape = 21, color = "black", stroke = 0.7) +
    annotate("text", x = min(full_df$DateTime), y = p90 * 1.02,
             label = "90th pct", hjust = 0, size = 3, color = "#ff7f00") +
    annotate("text", x = min(full_df$DateTime), y = p95 * 1.02,
             label = "95th pct", hjust = 0, size = 3, color = "#e31a1c") +
    scale_fill_manual(values = c("Below 90th percentile"  = "grey50",
                                 "Top 10% of sensor range" = "#ff7f00",
                                 "Top 5% of sensor range"  = "#e31a1c")) +
    scale_size_manual(values = c("Below 90th percentile"  = 2.5,
                                 "Top 10% of sensor range" = 3.5,
                                 "Top 5% of sensor range"  = 4.5)) +
    labs(title    = paste0(site, ": ", var_label, " — grab sample timing"),
         subtitle = "Grey = full sensor record  |  Points = grab sample lab values\nOrange/red dashed = 90th/95th percentile of sensor range",
         x = "Date", y = ylab,
         fill = "Grab sample position", size = "Grab sample position") +
    scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b %d") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

# DOC
plot_timeseries_coverage(DVO, DVO, "DOCeq..mg.l....Measured.value", "NPOC..mg.C.L.", "DVO", "DOC", "DOC / NPOC (mg/L)")
plot_timeseries_coverage(DVMS1, DVMS1, "DOCeq..mg.l....Measured.value", "NPOC..mg.C.L.", "DVMS1", "DOC", "DOC / NPOC (mg/L)")
plot_timeseries_coverage(DVNWT5, DVNWT5, "DOCeq..mg.l....Measured.value", "NPOC..mg.C.L.", "DVNWT5", "DOC", "DOC / NPOC (mg/L)")

# NO3
plot_timeseries_coverage(DVO, DVO, "NO3.Neq..mg.l....Measured.value", "NO3..mg.N.L.", "DVO", "NO3", "NO3 (mg N/L)")
plot_timeseries_coverage(DVMS1, DVMS1, "NO3.Neq..mg.l....Measured.value", "NO3..mg.N.L.", "DVMS1", "NO3", "NO3 (mg N/L)")
plot_timeseries_coverage(DVNWT5, DVNWT5, "NO3.Neq..mg.l....Measured.value", "NO3..mg.N.L.", "DVNWT5", "NO3", "NO3 (mg N/L)")

##==============================================================================
## SECTION 5: Coverage summary table
##==============================================================================
# For each variable and site, reports:
#   Sensor_min / max     : full range of sensor readings
#   Grab_min / max       : range covered by grab samples
#   Pct_range_covered    : grab range / sensor range * 100
#   Pct_readings_outside : % of 15-min sensor readings above max grab sample
#                          value — this is your extrapolation % 
#   N_grabs              : number of usable grab samples

coverage_summary <- function(df, site_name, sensor_col, grab_col, var_label) {
  
  sensor_vals <- df[[sensor_col]][!is.na(df[[sensor_col]])]
  
  grab_rows <- df %>%
    filter(!is.na(Sample.Name) & Sample.Name == "Y",
           !is.na(.data[[grab_col]]),
           !is.na(.data[[sensor_col]]))
  
  grab_sensor_vals <- grab_rows[[sensor_col]]
  
  if (length(grab_sensor_vals) == 0) {
    return(data.frame(Site = site_name, Variable = var_label,
                      Sensor_min = NA, Sensor_max = NA,
                      Grab_min = NA, Grab_max = NA,
                      Pct_range_covered = NA,
                      Pct_readings_above_grab_max = NA,
                      Pct_readings_below_grab_min = NA,
                      N_grabs = 0))
  }
  
  sensor_min <- min(sensor_vals)
  sensor_max <- max(sensor_vals)
  grab_min   <- min(grab_sensor_vals)
  grab_max   <- max(grab_sensor_vals)
  
  pct_range   <- round((grab_max - grab_min) / (sensor_max - sensor_min) * 100, 1)
  pct_above   <- round(mean(sensor_vals > grab_max) * 100, 1)
  pct_below   <- round(mean(sensor_vals < grab_min) * 100, 1)
  
  data.frame(
    Site                        = site_name,
    Variable                    = var_label,
    Sensor_min                  = round(sensor_min, 3),
    Sensor_max                  = round(sensor_max, 3),
    Grab_min                    = round(grab_min,   3),
    Grab_max                    = round(grab_max,   3),
    Pct_range_covered           = pct_range,
    Pct_readings_above_grab_max = pct_above,
    Pct_readings_below_grab_min = pct_below,
    N_grabs                     = nrow(grab_rows)
  )
}

summary_table <- bind_rows(
  # DOC
  coverage_summary(DVO, "DVO", "DOCeq..mg.l....Measured.value",   "NPOC..mg.C.L.", "DOC"),
  coverage_summary(DVMS1, "DVMS1", "DOCeq..mg.l....Measured.value",   "NPOC..mg.C.L.", "DOC"),
  coverage_summary(DVNWT5, "DVNWT5", "DOCeq..mg.l....Measured.value",   "NPOC..mg.C.L.", "DOC"),
  # NO3
  coverage_summary(DVO, "DVO", "NO3.Neq..mg.l....Measured.value", "NO3..mg.N.L.",  "NO3"),
  coverage_summary(DVMS1, "DVMS1", "NO3.Neq..mg.l....Measured.value", "NO3..mg.N.L.",  "NO3"),
  coverage_summary(DVNWT5, "DVNWT5", "NO3.Neq..mg.l....Measured.value", "NO3..mg.N.L.",  "NO3"),
) %>% arrange(Variable, Site)

cat("\n=== Grab sample coverage summary ===\n")
cat("Pct_range_covered           : % of sensor min-max range spanned by grab samples\n")
cat("Pct_readings_above_grab_max : % of 15-min readings ABOVE max grab value (extrapolation)\n")
cat("Pct_readings_below_grab_min : % of 15-min readings BELOW min grab value (extrapolation)\n\n")
print(summary_table, row.names = FALSE)

##==============================================================================
## SECTION 6: Visual summary table (for slides)
##==============================================================================
# Heatmap-style tile plot of extrapolation risk — good for a single slide
# showing the team which variable/site combinations are most at risk

summary_table %>%
  select(Site, Variable, Pct_readings_above_grab_max) %>%
  mutate(Risk = case_when(
    Pct_readings_above_grab_max >= 20 ~ "High (>20%)",
    Pct_readings_above_grab_max >= 10 ~ "Medium (10-20%)",
    Pct_readings_above_grab_max >= 5  ~ "Low-medium (5-10%)",
    TRUE                               ~ "Low (<5%)"
  ),
  Risk = factor(Risk, levels = c("Low (<5%)", "Low-medium (5-10%)",
                                 "Medium (10-20%)", "High (>20%)"))) %>%
  ggplot(aes(x = Variable, y = Site, fill = Risk)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = paste0(Pct_readings_above_grab_max, "%")),
            size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Low (<5%)"          = "#33a02c",
                               "Low-medium (5-10%)" = "#b2df8a",
                               "Medium (10-20%)"    = "#fdbf6f",
                               "High (>20%)"        = "#e31a1c")) +
  labs(title    = "Extrapolation risk: % of sensor readings above max grab sample value",
       subtitle = "Red = model is extrapolating for >20% of the time series",
       x = NULL, y = NULL, fill = "Risk level") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text  = element_text(face = "bold"))
print(summary_table, n = Inf)
