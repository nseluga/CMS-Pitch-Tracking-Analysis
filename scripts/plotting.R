# plotting.R
# Basic visualization functions for Rapsodo data

library(ggplot2)

# Movement chart (IVB vs HB)
plot_movement <- function(data) {
  library(ggplot2)
  library(dplyr)
  
  plot_data <- data %>%
    filter(!is.na(HB), !is.na(IVB))
  
  pitch_means <- plot_data %>%
    group_by(PitchType) %>%
    summarise(
      mean_IVB = mean(IVB, na.rm = TRUE),
      mean_HB  = mean(HB, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  ggplot(plot_data, aes(x = HB, y = IVB, color = PitchType)) +
    stat_density_2d(
      data = plot_data %>% group_by(PitchType) %>% filter(n() >= 3),
      aes(fill = PitchType),
      geom = "polygon",
      alpha = 0.25,
      contour = TRUE,
      show.legend = FALSE
    ) +
    geom_point(alpha = 0.2, size = 1) +  # light individual pitches
    geom_point(
      data = pitch_means,
      aes(x = mean_HB, y = mean_IVB, color = PitchType),
      size = 5, shape = 16
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top") +
    labs(
      title = "Pitch Movement (IVB vs HB)",
      x = "Horizontal Break (inches)",
      y = "Induced Vertical Break (inches)"
    ) +
    expand_limits(x = c(-25, 25), y = c(-20, 40))
}


# Release points (Side vs Height)
plot_release <- function(df) {
  ggplot(df, aes(x = ReleaseSide, y = ReleaseHeight, color = PitchType)) +
    geom_point(size = 3, alpha = 0.7) +
    theme_minimal() +
    labs(title = "Release Points", x = "Release Side (ft)", y = "Release Height (ft)")
}

# Spin profile (Velocity vs Spin Rate)
plot_spin_profile <- function(df) {
  ggplot(df, aes(x = Velocity, y = TotalSpin, color = PitchType)) +
    geom_point(size = 3, alpha = 0.7) +
    theme_minimal() +
    labs(title = "Velocity vs Spin Rate", x = "Velocity (mph)", y = "Spin Rate (rpm)")
}

# Pitch usage
plot_pitch_usage <- function(data) {
  library(ggplot2)
  library(dplyr)
  
  usage <- data %>%
    group_by(PitchType) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(pct = count / sum(count) * 100)
  
  ggplot(usage, aes(x = reorder(PitchType, -pct), y = pct, fill = PitchType)) +
    geom_col() +
    geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none") +
    labs(
      title = "Pitch Usage",
      x = "Pitch Type",
      y = "Usage (%)"
    )
}

# Whiff % by location 4SFB
plot_fsfb_whiff_heatmap <- function(data) {
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  
  # Define bin edges
  x_bins <- seq(-2, 2, by = 0.5)
  y_bins <- seq(0, 5, by = 0.5)
  
  # Full grid
  grid <- expand.grid(
    SideMid = head(x_bins, -1) + diff(x_bins)/2,
    HeightMid = head(y_bins, -1) + diff(y_bins)/2
  )
  
  # Summarize pitch data
  fsfb <- data %>%
    filter(PitchType == "Fastball") %>%
    mutate(
      WhiffFlag = ifelse(PitchOutcome == "Whiff", 1, 0),
      SideBin = cut(`Strike Zone Side`, breaks = x_bins, include.lowest = TRUE),
      HeightBin = cut(`Strike Zone Height`, breaks = y_bins, include.lowest = TRUE)
    ) %>%
    group_by(SideBin, HeightBin) %>%
    summarise(
      pitches = n(),
      whiffs = sum(WhiffFlag, na.rm = TRUE),
      whiff_rate = whiffs / pitches,
      .groups = "drop"
    ) %>%
    mutate(
      SideMid = (as.numeric(sub("\\((.+),.*", "\\1", SideBin)) + 
                   as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", SideBin)))/2,
      HeightMid = (as.numeric(sub("\\((.+),.*", "\\1", HeightBin)) + 
                     as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", HeightBin)))/2
    ) %>%
    select(SideMid, HeightMid, whiff_rate, pitches)
  
  # Join grid + keep NA for empty bins
  fsfb_full <- grid %>%
    left_join(fsfb, by = c("SideMid", "HeightMid"))
  
  # Plot
  ggplot(fsfb_full, aes(x = SideMid, y = HeightMid)) +
    geom_tile(aes(fill = whiff_rate), color = "white") +
    geom_text(aes(label = ifelse(!is.na(whiff_rate),
                                 paste0(round(whiff_rate*100), "%"), "")),
              size = 3, color = "black") +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue",
      midpoint = 0.5, limits = c(0,1),
      labels = scales::percent,
      na.value = "grey90"
    ) +
    labs(
      title = "4-Seam Fastball Whiff Rate by Location",
      x = "Horizontal Location (ft.)",
      y = "Vertical Location (ft.)",
      fill = "Whiff %"
    ) +
    coord_fixed(ratio = 1, xlim = c(-2, 2), ylim = c(0, 5)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid = element_blank()) +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             color = "green", fill = NA, size = 1)
}
