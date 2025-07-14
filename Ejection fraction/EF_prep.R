# Load packages
library(readxl)
library(tidyr)
library(dplyr)


## Load Excel files
LA_simpson <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw")
LA_biplane <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw_biplane")

## Create data frames for each group and method
simpson_frame <- data.frame(
  ID = LA_simpson$`Animal ID`,
  Gender = LA_simpson$Gender,
  EF = LA_simpson$EF,
  Method = "Simpson",
  Age = LA_simpson$`Age [months]`
)
simpson_frame <- na.omit(simpson_frame)
simpson_frame$Gender <- factor(simpson_frame$Gender, 
                              levels = c("F", "M"),
                              labels = c("Female", "Male"))

biplane_frame <- data.frame(
  ID = LA_biplane$`Animal ID`,
  Gender = LA_biplane$Gender,
  EF = LA_biplane$EF,
  Method = "Biplane",
  Age = LA_biplane$`Age [months]`
)
biplane_frame <- na.omit(biplane_frame)
biplane_frame$Gender <- factor(biplane_frame$Gender, 
                              levels = c("F", "M"),
                              labels = c("Female", "Male"))

group_frame <- rbind(simpson_frame, biplane_frame)
group_frame <- group_frame %>%
  mutate(Group = paste(Method, Gender, sep = " - "))


corr_frame <- merge(simpson_frame, biplane_frame, by = 'ID', suffixes = c("_simpson", "_biplane"))
corr_frame <- corr_frame %>%
  select(ID, Gender = Gender_simpson, EF_simpson, EF_biplane, Age = Age_simpson)

save.image(file = "EF_data.RData")
