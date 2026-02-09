library(dplyr)

## ----  get_mean_tibia ---- ##
# Function for finding the mean tibia length (mm) in a specific age/gender group.
# Input
# - data: data frame that contain the columns tibia_mm, age_group, gender
# - age: age of tibia measurement [int] 
# - sex: gender of subject [chr]
# Output
# - mean tibia length

get_mean_tibia <- function(data, age, sex) {
  # Input checks
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.")
  }
  if (!("tibia_mm" %in% names(data))) {
    stop("`tibia_mm` column not found in data.")
  }
  if (!("age_group" %in% names(data))) {
    stop("`age_group` column not found in data.")
  }
  if (!("gender" %in% names(data))) {
    stop("`gender` column not found in data.")
  }
  if (!is.numeric(data$tibia_mm)) {
    stop("`tibia_mm` is not numeric. Convert it before calling this function.")
  }
  
  # Filter, pull, compute mean
  vec <- data %>%
    filter(age_group == age, gender == sex) %>%
    pull(tibia_mm)
  
  if (length(vec) == 0) {
    warning("No rows match the specified age_group and gender; returning NA.")
    return(NA_real_)
  }
  
  mean(vec, na.rm = TRUE)
}
