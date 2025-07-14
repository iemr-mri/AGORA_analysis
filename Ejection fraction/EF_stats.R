# RUN DATA PREPARATION
source("Ejection fraction/EF_prep.r")
# or load saved data
load("Ejection fraction/EF_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)

## EF BOX PLOTS ----

# Ejection fraction (9 months old) - Simpson's method
ggboxplot(simpson_frame %>% filter(Age == 9), x = "Gender", y = "EF", fill = "Gender",
          xlab = "Gender", ylab = "EF (%)") + 
          theme(legend.position = "none")

# Ejection fraction (9 to 16 months old) - Simpson's method
ggboxplot(simpson_frame, x = "Age", y = "EF", fill = "Gender",
          xlab = "Age (months)", ylab = "EF (%)")

# Ejection fraction (9 months old) - Biplane method
ggboxplot(biplane_frame %>% filter(Age == 9), x = "Gender", y = "EF", fill = "Gender",
          xlab = "Gender", ylab = "EF (%)") + 
          theme(legend.position = "none")

# Ejection fraction (9 months old) - both methods
ggboxplot(group_frame %>% filter(Age == 9), x = "Method", y = "EF", fill = "Gender",
          xlab = "", ylab = "EF (%)")


## CORRELATION ----
# Correlation between ejection fraction with two methods for female, 9 month old animals
ggscatter(corr_frame %>% filter(Gender == "Female", Age == 9), x = "EF_simpson", y = "EF_biplane",
          xlab = "EF (%) - Simpson's", ylab = "EF (%) - Biplane",
          add = "reg.line", conf.int = TRUE)

# Correlation between ejection fraction with two methods for male, 9 month old animals
ggscatter(corr_frame %>% filter(Gender == "Male", Age == 9), x = "EF_simpson", y = "EF_biplane",
          xlab = "EF (%) - Simpson's", ylab = "EF (%) - Biplane",
          add = "reg.line", conf.int = TRUE)


## SUMMARY ----
summary(subset(simpson_frame, Gender == "F"))
summary(subset(simpson_frame, Gender == "M"))
summary(subset(simpson_frame))

summary(subset(biplane_frame, Gender == "F"))
summary(subset(biplane_frame, Gender == "M"))
summary(subset(biplane_frame))


