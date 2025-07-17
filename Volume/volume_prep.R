# Load packages
library(readxl)
library(tidyr)
library(dplyr)


## Load Excel files
# Data - Henrik Elias
LA_simpson_HE <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw")
LA_biplane_HE <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw_biplane")
#Data - Haelin
LA_simpson_Hae <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Haelin.xlsx", sheet = "raw")

## Create data frames for each group, method and observer ----
# Simpson - HE
simpson_HE <- data.frame(
  ID       = LA_simpson_HE$`Animal ID`,
  Gender   = LA_simpson_HE$Gender,
  Max_V    = LA_simpson_HE$`Max volume [ml]`,
  Min_V    = LA_simpson_HE$`Min volume [ml]`,
  SV       = LA_simpson_HE$`SV [ml]`,
  Method   = "Simpson",
  Age      = LA_simpson_HE$`Age [months]`
)
simpson_HE <- na.omit(simpson_HE)
simpson_HE$Gender <- factor(simpson_HE$Gender, 
                               levels = c("F", "M"),
                               labels = c("Female", "Male"))

# Simpson - Hae
simpson_Hae <- data.frame(
  ID      = LA_simpson_Hae$`Animal ID`,
  Gender  = LA_simpson_Hae$Gender,
  Max_V   = LA_simpson_Hae$`Max Simpson's method [ml]`,
  Min_V   = LA_simpson_Hae$`Min Simpson's method [ml]`,
  SV      = LA_simpson_Hae$`SV [ml]`,
  Method  = "Simpson",
  Age     = LA_simpson_Hae$`Age [months]`
)
simpson_Hae <- na.omit(simpson_Hae)
simpson_Hae$Gender <- factor(simpson_Hae$Gender, 
                            levels = c("F", "M"),
                            labels = c("Female", "Male"))
# Biplane - HE
biplane_HE <- data.frame(
  ID        = LA_biplane_HE$`Animal ID`,
  Gender    = LA_biplane_HE$Gender,
  Max_V     = LA_biplane_HE$`Max LA volume [mL] (avg L)`,
  Min_V     = LA_biplane_HE$`Min LA volume [mL] (avg L)`,
  SV        = LA_biplane_HE$`Volume difference`,
  Method    = "Biplane",
  Age       = LA_biplane_HE$`Age [months]`
)
biplane_HE <- na.omit(biplane_HE)
biplane_HE$Gender <- factor(biplane_HE$Gender, 
                               levels = c("F", "M"),
                               labels = c("Female", "Male"))




method_frame <- rbind(simpson_HE, biplane_HE)
method_frame <- method_frame %>%
  mutate(Group = paste(Method, Gender, sep = " - "))


observer_simpson <- merge(simpson_HE, simpson_Hae, by = 'ID', suffixes = c("_HE", "_Hae"))
observer_simpson <- observer_simpson %>%
  mutate(Gender = Gender_HE) %>%
  select(-Gender_Hae)

save.image(file = "Volume/volume_data.RData")

