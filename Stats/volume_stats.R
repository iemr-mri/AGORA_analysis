# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)

## MAX VOLUME ## ----

# Maximum left atrium volume from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE %>% filter(Group == "Aging"), 
          x = "Age", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Maximum left atrium volume from 9 to 16 months old - Simpson/3D method",
          fill  = "Gender")


## Min VOLUME ## ----

# Minimum left atrium volume from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE %>% filter(Group == "Aging"), 
          x = "Age", y = "Min_V",
          ylab  = "Minimum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Minimum left atrium volume from 9 to 16 months old - Simpson/3D method",
          fill  = "Gender")


## STROKE VOLUME ## ----

# LA stroke volume from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE %>% filter(Group == "Aging"), 
          x = "Age", y = "SV",
          ylab  = "Stroke volume (ml)",
          xlab  = "Age (months)",
          title = "Left atrium stroke volume from 9 to 16 months old - Simpson/3D method",
          fill  = "Gender")


## EJECTION FRACTION ## ----

# Ejection fraction (9 months old) - Simpson's method
ggboxplot(simpson_HE %>% filter(Age == 9, Group == "Aging"), x = "Gender", y = "EF", fill = "Gender",
          xlab = "Gender", ylab = "EF (%)") + 
  theme(legend.position = "none")

# Ejection fraction (9 to 16 months old) - Simpson's method
ggboxplot(simpson_HE %>% filter(Group == "Aging"), 
          x = "Age", y = "EF", fill = "Gender",
          xlab = "Age (months)", ylab = "EF (%)")

