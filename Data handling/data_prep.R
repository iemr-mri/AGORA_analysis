# Load packages
library(readxl)
library(tidyverse)


## Load Excel files
# Data - Henrik Elias
LA_simpson_HE <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw")
LA_biplane_HE <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw_biplane")

#Data - Haelin
LA_simpson_Hae <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Haelin.xlsx", sheet = "raw")
Echo_data <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size_ECHO_Haelin.xlsx", sheet = "max")

# Create data frames for each group, method and observer
## Simpson - HE ----
simpson_HE <- tibble(
  ID       = LA_simpson_HE$`Animal ID`,
  Cohort   = LA_simpson_HE$Cohort,
  Gender   = LA_simpson_HE$Gender,
  Max_V    = LA_simpson_HE$`Max volume [ml]`,
  Min_V    = LA_simpson_HE$`Min volume [ml]`,
  SV       = LA_simpson_HE$`SV [ml]`,
  EF       = LA_simpson_HE$EF,
  Method   = "Simpson",
  Operator = "A",
  Age      = LA_simpson_HE$`Age [months]`,
  MI_week  = LA_simpson_HE$MI_Week
)

# Drop rows containing no max volume
simpson_HE <- simpson_HE %>%
  drop_na(Max_V)

# Convert F/M into Female/Male for aesthetics
simpson_HE$Gender <- factor(simpson_HE$Gender, 
                               levels = c("F", "M"),
                               labels = c("Female", "Male"))

# Extract the cohort prefix for grouping aging vs MI rats
simpson_HE$Group <- factor(sub("^(AG|MI).*", "\\1", simpson_HE$Cohort),
                           levels = c("AG", "MI"),
                           labels = c("Aging", "MI"))

## Simpson - Hae ----
simpson_Hae <- tibble(
  ID       = LA_simpson_Hae$`Animal ID`,
  Cohort   = LA_simpson_Hae$Cohort,
  Gender   = LA_simpson_Hae$Gender,
  Max_V    = LA_simpson_Hae$`Max Simpson's method [ml]`/1000, # divide by 1000 for correct unit
  Min_V    = LA_simpson_Hae$`Min Simpson's method [ml]`/1000, # divide by 1000 for correct unit
  SV       = LA_simpson_Hae$`SV [ml]`/1000, # divide by 1000 for correct unit
  EF       = LA_simpson_Hae$EF,
  Method   = "Simpson",
  Operator = "B",
  Age      = LA_simpson_Hae$`Age [months]`,
  MI_week  = LA_simpson_Hae$MI_week
)

# Drop rows containing no max volume
simpson_Hae <- simpson_Hae %>%
  drop_na(Max_V)

# Convert F/M into Female/Male for aesthetics
simpson_Hae$Gender <- factor(simpson_Hae$Gender, 
                            levels = c("F", "M"),
                            labels = c("Female", "Male"))

# Extract the cohort prefix for grouping aging vs MI rats
simpson_Hae$Group <- factor(sub("^(AG|MI).*", "\\1", simpson_Hae$Cohort),
                           levels = c("AG", "MI"),
                           labels = c("Aging", "MI"))
## Biplane - HE ----
biplane_HE <- tibble(
  ID        = LA_biplane_HE$`Animal ID`,
  Cohort    = LA_biplane_HE$Cohort,
  Gender    = LA_biplane_HE$Gender,
  Max_V     = LA_biplane_HE$`Max LA volume [mL] (avg L)`,
  Min_V     = LA_biplane_HE$`Min LA volume [mL] (avg L)`,
  SV        = LA_biplane_HE$`Volume difference`,
  EF        = LA_biplane_HE$EF,
  Method    = "Biplane",
  Operator  = "A",
  Age       = LA_biplane_HE$`Age [months]`,
  MI_week   = LA_biplane_HE$MI_week,
  Max_d     = LA_biplane_HE$`Max 4CH-Length [mm]`,
  Min_d     = LA_biplane_HE$`Min 4CH-Length [mm]`
)

# If diameter is measured, but not area, the volume parameters can be 0 instead of NA. So we need another step before omitting NA values
biplane_HE <- biplane_HE %>%
  mutate(across(c('Max_V','Min_V','SV','EF'), ~na_if(.,0)))

# Drop rows containing no max volume
biplane_HE <- biplane_HE %>%
  drop_na(Max_V)


# Convert F/M into Female/Male for aesthetics
biplane_HE$Gender <- factor(biplane_HE$Gender, 
                               levels = c("F", "M"),
                               labels = c("Female", "Male"))

# Extract the cohort prefix for grouping aging vs MI rats
biplane_HE$Group <- factor(sub("^(AG|MI).*", "\\1", biplane_HE$Cohort),
                           levels = c("AG", "MI"),
                           labels = c("Aging", "MI"))


## COMBINED FRAMES ----
join_list = c('ID', "Age", "Gender", "Cohort", "Group", "MI_week")
# A tibble to compare data with different methods (Simpson and biplane)
method_wide <- inner_join(simpson_HE, biplane_HE, by = c(join_list, "Operator"), suffix = c("_simpson", "_biplane"))
method_long <- bind_rows(simpson_HE, biplane_HE)


# Wide and long tibbles combining data from two observers
observer_wide <- inner_join(simpson_HE, simpson_Hae, by = c(join_list, "Method"), suffix = c("_HE", "_Hae"))
observer_long <- bind_rows(simpson_HE, simpson_Hae)


## LA DIAMETER FRAME ----
Echo_frame <- tibble(
  ID = Echo_data$`Animal ID`,
  Max_d = Echo_data$`LAD avg mm`,
  Gender = Echo_data$Gender,
  Age = Echo_data$Age,
  Cohort = Echo_data$Cohort
)

# Extract the cohort prefix for grouping aging and MI rats
Echo_frame$Group <- factor(sub("^(AG|MI).*", "\\1", Echo_frame$Cohort),
                           levels = c("AG", "MI"),
                           labels = c("Aging", "MI"))

# Convert F/M into Female/Male for aesthetics
Echo_frame$Gender <- factor(Echo_frame$Gender, 
                            levels = c("F", "M"),
                            labels = c("Female", "Male"))

# Remove rows without data
Echo_frame <- Echo_frame %>%
  drop_na(Max_d)

LA_dm <-  inner_join(Echo_frame, biplane_HE, by = c('ID', "Age", "Gender", "Cohort", "Group"), suffix = c("_MR", "_Echo"))


## SAVE DATA ----
save(simpson_HE, biplane_HE, method_wide, method_long, observer_wide, observer_long, LA_dm, file = "Data handling/LA_data.Rdata")
save.image()
