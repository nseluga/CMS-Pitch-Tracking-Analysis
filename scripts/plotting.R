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
  
  # Create full grid of bin centers
  grid <- expand.grid(
    SideMid = head(x_bins, -1) + diff(x_bins)/2,
    HeightMid = head(y_bins, -1) + diff(y_bins)/2
  )
  
  # Filter and bin the fastball data
  fsfb <- data %>%
    filter(PitchType == "Fastball") %>%
    # Create whiff flag (1 for whiff, 0 for contact/other)
    mutate(WhiffFlag = ifelse(PitchOutcome == "Whiff", 1, 0)) %>%
    # Assign each pitch to a bin
    mutate(
      SideBin = cut(`Strike Zone Side`, 
                    breaks = x_bins, 
                    include.lowest = TRUE, 
                    labels = FALSE),
      HeightBin = cut(`Strike Zone Height`, 
                      breaks = y_bins, 
                      include.lowest = TRUE, 
                      labels = FALSE)
    ) %>%
    # Remove pitches that fall outside our bins
    filter(!is.na(SideBin) & !is.na(HeightBin)) %>%
    # Calculate bin centers
    mutate(
      SideMid = x_bins[SideBin] + (x_bins[2] - x_bins[1])/2,
      HeightMid = y_bins[HeightBin] + (y_bins[2] - y_bins[1])/2
    ) %>%
    # Group by bin centers and calculate whiff rate
    group_by(SideMid, HeightMid) %>%
    summarise(
      pitches = n(),
      whiffs = sum(WhiffFlag, na.rm = TRUE),
      whiff_rate = whiffs / pitches,
      .groups = "drop"
    )
  
  # Join with full grid to ensure all bins are represented
  fsfb_full <- grid %>%
    left_join(fsfb, by = c("SideMid", "HeightMid"))
  
  # Create the heatmap
  p <- ggplot(fsfb_full, aes(x = SideMid, y = HeightMid)) +
    geom_tile(aes(fill = whiff_rate), 
              color = "white", 
              size = 0.5,
              width = 0.48,  # Slightly smaller than bin width to show grid
              height = 0.48) +
    # Add percentage labels for bins with data
    geom_text(aes(label = ifelse(!is.na(whiff_rate) & pitches >= 3,  # Only show if 3+ pitches
                                 paste0(round(whiff_rate*100), "%"), "")),
              size = 3, 
              color = "black",
              fontface = "bold") +
    # Add pitch count labels (smaller, below percentage)
    geom_text(aes(label = ifelse(!is.na(pitches) & pitches >= 3,
                                 paste0("(n=", pitches, ")"), "")),
              size = 2, 
              color = "darkgray",
              nudge_y = -0.1) +
    # Color scale
    scale_fill_gradient2(
      low = "red", 
      mid = "white", 
      high = "blue",
      midpoint = 0.5, 
      limits = c(0, 1),
      labels = scales::percent,
      na.value = "grey90",
      name = "Whiff %"
    ) +
    # Labels and title
    labs(
      title = "4-Seam Fastball Whiff Rate by Location",
      subtitle = "Only bins with 3+ pitches are labeled",
      x = "Horizontal Location (ft.)",
      y = "Vertical Location (ft.)"
    ) +
    # Fixed aspect ratio and limits
    coord_fixed(ratio = 1, xlim = c(-2, 2), ylim = c(0, 5)) +
    # Theme
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "gray60"),
      legend.position = "right"
    ) +
    # Strike zone overlay
    annotate("rect", 
             xmin = -0.83, xmax = 0.83, 
             ymin = 1.5, ymax = 3.5,
             color = "green", 
             fill = NA, 
             size = 1)
  
  return(p)
}

# Example usage and debugging function
debug_whiff_data <- function(data) {
  # Check your data structure and values
  cat("Data columns:", names(data), "\n\n")
  
  # Check fastball data
  fsfb_check <- data %>%
    filter(PitchType == "Fastball")
  
  cat("Total fastballs:", nrow(fsfb_check), "\n")
  cat("Unique PitchOutcomes:", levels(data$PitchOutcome), "\n")
  cat("Whiff count:", sum(fsfb_check$PitchOutcome == "Whiff", na.rm = TRUE), "\n")
  
  # Check location data ranges
  cat("Strike Zone Side range:", range(data$`Strike Zone Side`, na.rm = TRUE), "\n")
  cat("Strike Zone Height range:", range(data$`Strike Zone Height`, na.rm = TRUE), "\n")
  
  # Sample of data
  cat("\nSample of fastball data:\n")
  print(head(fsfb_check %>% select(PitchType, PitchOutcome, `Strike Zone Side`, `Strike Zone Height`)))
}
