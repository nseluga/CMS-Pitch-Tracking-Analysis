# cleaning.R
# Basic cleaning function for Rapsodo data

library(readr)
library(dplyr)

clean_rapsodo <- function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(
      Velocity = as.numeric(Velocity),
      TotalSpin = as.numeric(`Total Spin`),
      TrueSpin = as.numeric(`True Spin`),
      SpinEfficiency = as.numeric(`Spin Efficiency`),
      IVB = as.numeric(IVB),
      HB = as.numeric(HB),
      ReleaseHeight = as.numeric(`Release Height`),
      ReleaseSide = as.numeric(`Release Side`),
      ReleaseExtension = readr::parse_number(na_if(`Release Extension`, "-")),
      Gyro = as.numeric(Gyro),
      PitchType = as.factor(`Pitch Type`),
      SwingResult = as.factor(`Swing Result`),
      PitchOutcome = as.factor(`Pitch Outcome`),
      HitterHandedness = as.factor(`Hitter Handedness`)
    )
}

