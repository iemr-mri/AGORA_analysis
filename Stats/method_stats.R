# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)
library(tidyverse)

## BOX PLOT COMPARISONS ## ----

# Ejection fraction (9 months old) - both methods
ggboxplot(master_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Method", y = "EF", fill = "Gender",
          title = "Ejection fraction - 9 month old",
          xlab = "", ylab = "EF (%)", 
          facet.by = "Operator")

# Maximum left atrium volume (9 months old)
ggboxplot(master_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Method", y = "Max_V",
          ylab  ="Maximum left atrium volume (ml)",
          xlab  = "",
          title = "Maximum left atrium volume (9 months old)",
          fill  = "Gender", facet.by = "Operator")

# LA stroke volume (9 months old) - both methods
ggboxplot(method_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Method", y = "SV",
          ylab  ="Stroke volume (ml)",
          xlab  = "",
          title = "Left atrium stroke volume (9 months old) - both methods",
          fill  = "Gender")


## CORRELATION ----
# Correlation between ejection fraction with two methods for female, 9 month old animals
ggscatter(opA_wide %>% 
            filter(Group == "Aging"), 
          x = "EF_simpson", y = "EF_biplane",
          xlab = "EF (%) - Simpson's", ylab = "EF (%) - Biplane",
          add = "reg.line", conf.int = TRUE,
          title = "Ejection fraction - intra-method operator A variability"
          )

ggscatter(opA_wide %>% 
            filter(Group == "Aging"), 
          x = "Max_V_simpson", y = "Max_V_biplane",
          xlab = "Maximum left atrium (ml) - Simpson's", ylab = "Maximum left atrium (ml) - Biplane",
          add = "reg.line", conf.int = TRUE,
          title = "Maximum left atrium - intra-method operator A variability"
)

ggscatter(opB_wide %>%
            filter(Group == "Aging"),
          x = "EF_simpson", y = "EF_biplane",
          xlab = "EF (%) - Simpson's", ylab = "EF (%) - Biplane",
          add = "reg.line", conf.int = TRUE,
          title = "Ejection fraction - intra-method operator B variability",
          facet.by = "Age"
            )

## BLAND-ALTMAN ----

method_BA <- tibble(
  MaxV_diff = method_wide$Max_V_simpson - method_wide$Max_V_biplane,
  MaxV_avg  = rowMeans(method_wide[, c("Max_V_simpson","Max_V_biplane")]),
  
  MinV_diff = method_wide$Min_V_simpson - method_wide$Min_V_biplane,
  MinV_avg  = rowMeans(method_wide[, c("Min_V_simpson","Min_V_biplane")]),
  
  EF_diff = method_wide$EF_simpson - method_wide$EF_biplane,
  EF_avg  = rowMeans(method_wide[, c("EF_simpson","EF_biplane")]),
)

method_BA <- na.omit(method_BA)

# BA of max volume
MaxV_meanDiff <- mean(method_BA$MaxV_diff)
MaxV_sdDiff   <- sd(method_BA$MaxV_diff)

ggplot(method_BA, aes(x = MaxV_avg, y = MaxV_diff)) +
  geom_point() +
  geom_hline(yintercept = MaxV_meanDiff, color = "steelblue") +
  geom_hline(yintercept = MaxV_meanDiff + 1.96*MaxV_sdDiff, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = MaxV_meanDiff - 1.96*MaxV_sdDiff, linetype = "dashed", color = "red") +
  labs(title = "Maximum left atrium volume", x = "Average maximum volume between methods", y = "Difference between methods")

# BA of EF
EF_meanDiff <- mean(method_BA$EF_diff)
EF_sdDiff   <- sd(method_BA$EF_diff)

ggplot(method_BA, aes(x = EF_avg, y = EF_diff)) +
  geom_point() +
  geom_hline(yintercept = EF_meanDiff, color = "steelblue") +
  geom_hline(yintercept = EF_meanDiff + 1.96*EF_sdDiff, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = EF_meanDiff - 1.96*EF_sdDiff, linetype = "dashed", color = "red") +
  labs(title = "Ejection fraction", x = "Average ejection fraction between methods", y = "Difference between methods")

