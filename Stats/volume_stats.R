# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)
library(tidyverse)

simpson_HE$Age <- factor(simpson_HE$Age)

## MAX VOLUME ## ----

# Maximum left atrium volume from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE %>% filter(Group == "Aging"), 
          x = "Age", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Maximum left atrium volume from 9 to 16 months old - Simpson/3D method",
          fill  = "Gender",
          facet.by = "Gender") +
  stat_compare_means(comparisons = list(c("9", "16")), method = "t.test")
  
# Paired maximum left atrium volume 9-16 months old
ggpaired(simpson_HE %>% 
           filter(Group == "Aging") %>% 
           group_by(ID) %>%
           filter(n() == 2) %>%
           ungroup(), 
          x = "Age", y = "Max_V",
          id = "ID",
          ylab  = "Maximum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Paired maximum left atrium volume from 9 to 16 months old - Simpson/3D method",
          fill  = "Gender",
          facet.by = "Gender") + 
          stat_compare_means(comparisons = list(c("9", "16")), method = "t.test")



## Min VOLUME ## ----

# Minimum left atrium volume from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE %>% filter(Group == "Aging"), 
          x = "Age", y = "Min_V",
          ylab  = "Minimum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Minimum left atrium volume from 9 to 16 months old - Simpson/3D method",
          fill  = "Gender",
          facet.by = "Gender") +
  stat_compare_means(comparisons = list(c("9", "16")), method = "t.test")


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
          xlab = "Age (months)", ylab = "EF (%)",
          facet.by = "Gender") +
  stat_compare_means(comparisons = list(c("9", "16")), method = "t.test")

# Paired EF in males
ggpaired(simpson_HE %>% 
           filter(Group == "Aging", Gender == "Male") %>%
           group_by(ID) %>%
           filter(n() == 2) %>%
           ungroup(), 
         x = "Age", y = "EF", fill = "lightblue",
         id = "ID",
         xlab = "Age (months)", ylab = "EF (%)") +
  stat_compare_means(comparisons = list(c("9", "16")), method = "t.test")

