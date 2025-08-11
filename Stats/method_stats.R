# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)

## BOX PLOT COMPARISONS ## ----

# Ejection fraction (9 months old) - both methods
ggboxplot(method_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Method", y = "EF", fill = "Gender",
          xlab = "", ylab = "EF (%)")

# Maximum left atrium volume (9 months old) - both methods
ggboxplot(method_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Method", y = "Max_V",
          ylab  ="Maximum left atrium volume (ml)",
          xlab  = "",
          title = "Maximum left atrium volume (9 months old) - both methods",
          fill  = "Gender")

# Minimum left atrium volume (9 months old) - both methods
ggboxplot(method_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Method", y = "Min_V",
          ylab  ="Minimum left atrium volume (ml)",
          xlab  = "",
          title = "Minimum left atrium volume (9 months old) - both methods",
          fill  = "Gender")

# LA stroke volume (9 months old) - both methods
ggboxplot(method_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Method", y = "SV",
          ylab  ="Stroke volume (ml)",
          xlab  = "",
          title = "Left atrium stroke volume (9 months old) - both methods",
          fill  = "Gender")


## CORRELATION ----
# Correlation between ejection fraction with two methods for female, 9 month old animals
ggscatter(method_wide %>% filter(Gender == "Female", Age == 9, Group == "Aging"), 
          x = "EF_simpson", y = "EF_biplane",
          xlab = "EF (%) - Simpson's", ylab = "EF (%) - Biplane",
          add = "reg.line", conf.int = TRUE)

# Correlation between ejection fraction with two methods for male, 9 month old animals
ggscatter(method_wide %>% filter(Gender == "Male", Age == 9, Group == "Aging"), 
          x = "EF_simpson", y = "EF_biplane",
          xlab = "EF (%) - Simpson's", ylab = "EF (%) - Biplane",
          add = "reg.line", conf.int = TRUE)

