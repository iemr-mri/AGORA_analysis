library(DBI)
library(RSQLite)
library(tidyverse)
source("Data handling/functions.r")

con <- dbConnect(RSQLite::SQLite(), "C:\\Users\\heskalde\\Databases\\AGORA.db")

# ---- LA_df -----
# animal_id | age_months | cohort | week | max_volume_ml | min_volume_ml | stroke_volume_ml | ejection_fraction | max_length_mm | model
aging_view <- "la_mri_aging"
LA_df <- dbGetQuery(con, sprintf("SELECT * from %s;", DBI::dbQuoteIdentifier(con, aging_view))) %>%
  as_tibble()

# grouping of age/gender with combined group label
LA_df <- LA_df %>%
  mutate(gender_age = paste0(gender, " ", as.character(age_months), "M")) # e.g. "Male 9M", "Female 16M"

# ---- subjects_df ----
# animal_id | gender | cohort | model | tibia_mm
subjects_view <- "subjects"
subjects_df <- dbGetQuery(con, sprintf("SELECT * from %s;", DBI::dbQuoteIdentifier(con, subjects_view))) %>%
  as_tibble()

subjects_df <- subjects_df %>%
  mutate(
    age_group = str_extract(cohort, "(?<=-)[0-9]+") %>%
      str_replace("^0+", "") %>%
      na_if("") %>%
      as.integer()
)

# ---- tibia_df ----
# first find each mean tibia length per age-gender group
tib9f <- get_mean_tibia(subjects_df, 9, "Female")

tib9m <- get_mean_tibia(subjects_df, 9, "Male")

tib16f <- get_mean_tibia(subjects_df, 16, "Female")

tib16m <- get_mean_tibia(subjects_df, 16, "Male")


# age_months | gender | tibia_mean
tibia_df <- tibble(
  age_months = c(9, 9, 16, 16),
  gender     = c("Female", "Male", "Female", "Male"),
  tibia_mean = c(tib9f, tib9m, tib16f, tib16m)
)

# ---- LA_tib ----
# LA data where volumetric parameters have been normalized to corresponding mean tibia length per age/gender
LA_tib <- LA_df %>%
  left_join(tibia_df, by = c("age_months", "gender")) %>%
  mutate(across(all_of(c("max_volume_ml", "min_volume_ml", "stroke_volume_ml", "max_length_mm")), ~ .x / tibia_mean)) %>%
  select(-tibia_mean)   # remove tibia mean column afterwards

# volumetric parameters are now different unit, keeping it simple by removing suffix
LA_tib <- LA_tib %>%
  rename(max_volume    = max_volume_ml,
         min_volume    = min_volume_ml,
         stroke_volume = stroke_volume_ml,
         max_length    = max_length_mm
  )

# ---- save data ----
save(LA_tib, LA_df, file = "Data handling/LA_data.Rdata")
save.image()

rm(subjects_df, tibia_df, aging_view, subjects_view, tib9m, tib9f, tib16m, tib16f)

dbDisconnect(con)