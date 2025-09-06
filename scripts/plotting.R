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
  
  # filter for fastballs only
  fsfb <- data %>%
    filter(PitchType == "Fastball") %>%
    mutate(
      WhiffFlag = ifelse(PitchOutcome == "Whiff", 1, 0),
      SideBin = cut(`Strike Zone Side`, breaks = seq(-25, 25, by = 5)),
      HeightBin = cut(`Strike Zone Height`, breaks = seq(0, 60, by = 5))
    ) %>%
    group_by(SideBin, HeightBin) %>%
    summarise(
      pitches = n(),
      whiffs = sum(WhiffFlag, na.rm = TRUE),
      whiff_rate = whiffs / pitches,
      .groups = "drop"
    )
  
  print(fsfb)  # 👈 print table to debug what’s going into the plot
  
  ggplot(fsfb, aes(x = SideBin, y = HeightBin, fill = whiff_rate)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(labels = scales::percent, option = "C") +
    labs(
      title = "4-Seam Fastball Whiff Rate by Location",
      x = "Strike Zone Side",
      y = "Strike Zone Height",
      fill = "Whiff Rate"
    ) +
    theme_minimal(base_size = 14)
}

