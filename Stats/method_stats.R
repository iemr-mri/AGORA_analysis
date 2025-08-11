# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)

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
