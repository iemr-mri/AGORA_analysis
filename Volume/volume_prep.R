# Load packages
library(readxl)
library(tidyverse)


## Load Excel files
# Data - Henrik Elias
LA_simpson_HE <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw")
LA_biplane_HE <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw_biplane")

#Data - Haelin
LA_simpson_Hae <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Haelin.xlsx", sheet = "raw")

# Create data frames for each group, method and observer
## Simpson - HE ----
simpson_HE <- tibble(
  ID       = LA_simpson_HE$`Animal ID`,
  Cohort   = LA_simpson_HE$Cohort,
  Gender   = LA_simpson_HE$Gender,
  Max_V    = LA_simpson_HE$`Max volume [ml]`,
  Min_V    = LA_simpson_HE$`Min volume [ml]`,
  SV       = LA_simpson_HE$`SV [ml]`,
  Method   = "Simpson",
  Operator = "A",
  Age      = LA_simpson_HE$`Age [months]`
)
simpson_HE <- na.omit(simpson_HE)

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
  Method   = "Simpson",
  Operator = "B",
  Age      = LA_simpson_Hae$`Age [months]`
)
simpson_Hae <- na.omit(simpson_Hae)

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
  Method    = "Biplane",
  Operator  = "A",
  Age       = LA_biplane_HE$`Age [months]`
)

# Omit NA and 0 values
biplane_HE <- na.omit(biplane_HE)
biplane_HE <- biplane_HE %>%
  filter(if_all(everything(),~ . != 0))

# Convert F/M into Female/Male for aesthetics
biplane_HE$Gender <- factor(biplane_HE$Gender, 
                               levels = c("F", "M"),
                               labels = c("Female", "Male"))

# Extract the cohort prefix for grouping aging vs MI rats
biplane_HE$Group <- factor(sub("^(AG|MI).*", "\\1", biplane_HE$Cohort),
                           levels = c("AG", "MI"),
                           labels = c("Aging", "MI"))



## COMBINED FRAMES ----

method_frame <- bind_rows(simpson_HE, biplane_HE)

observer_wide <- inner_join(simpson_HE, simpson_Hae, by = c('ID', "Age", "Gender", "Cohort", "Group", "Method"), suffix = c("_HE", "_Hae"))
observer_long <- bind_rows(simpson_HE, simpson_Hae)

## SAVE DATA ----
save.image(file = "Volume/volume_data.RData")

