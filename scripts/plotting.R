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
