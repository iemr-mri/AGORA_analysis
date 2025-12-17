# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)
library(tidyverse)

## OBSERVER BOX/SCATTER COMPARISON ----

# Maximum left atrium volume boxplot
ggboxplot(master_long %>% filter(Group == "Aging", Method == "Simpson"), 
          x = "Operator", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          xlab  = "Observer",
          title = "Maximum left atrium volume (9-16 months old) - observer comparison",
          fill = "Gender", facet.by = "Age")

# Maximum left atrium volume scatterplot
ggscatter(simpson_wide %>% filter(Group == "Aging"), 
          x = "Max_V_HE" , y = "Max_V_Hae",
          add   = "reg.line", conf.int = TRUE,
          ylab  = "Maximum left atrium volume (ml) - Observer B",
          xlab  = "Maximum left atrium volume (ml) - Observer A",
          title = "Maximum left atrium volume - observer comparison",
          color = "Gender", facet.by = "Age")

# Left atrium ejection fraction boxplot
ggboxplot(observer_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Operator", y = "SV",
          ylab  = "Left atrium stroke volume (ml)",
          xlab  = "Observer",
          title = "Left atrium stroke volume (9 months old) - observer comparison",
          fill = "Gender")

# Left atrium ejection fraction scatterplot
ggscatter(observer_wide %>% filter(Age == 9, Group == "Aging"), 
          x = "EF_HE" , y = "EF_Hae",
          add   = "reg.line", conf.int = TRUE,
          ylab  = "Left atrium ejection fraction (%) - Observer B",
          xlab  = "Left atrium ejection fraction (%) - Observer A",
          title = "Left atrium ejection fraction (9 months old) - observer comparison",
          color = "Gender")


## SIMPSON BLAND-ALTMAN COMPARISON ----
# ggpubr does not have easy functionality for BA plots so using basic ggplot2

simpson_BA <- tibble(
  MaxV_diff = simpson_wide$Max_V_HE - simpson_wide$Max_V_Hae,
  MaxV_avg  = rowMeans(simpson_wide[, c("Max_V_HE","Max_V_Hae")]),
  
  MinV_diff = simpson_wide$Min_V_HE - simpson_wide$Min_V_Hae,
  MinV_avg  = rowMeans(simpson_wide[, c("Min_V_HE","Min_V_Hae")]),
  
  EF_diff   = simpson_wide$EF_HE    - simpson_wide$EF_Hae,
  EF_avg    = rowMeans(simpson_wide[, c("EF_HE","EF_Hae")])
)

# BA of max volume
simp_MaxV_meanDiff <- mean(simpson_BA$MaxV_diff)
simp_MaxV_sdDiff   <- sd(simpson_BA$MaxV_diff)

ggplot(simpson_BA, aes(x = MaxV_avg, y = MaxV_diff)) +
  geom_point() +
  geom_hline(yintercept = simp_MaxV_meanDiff, color = "steelblue") +
  geom_hline(yintercept = simp_MaxV_meanDiff + 1.96*simp_MaxV_sdDiff, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = simp_MaxV_meanDiff - 1.96*simp_MaxV_sdDiff, linetype = "dashed", color = "red") +
  labs(title = "Maximum left atrium volume - Simpson's method", x = "Average maximum volume between observers [ml]", y = "Difference between observers [ml]")

# BA of EF
simp_EF_meanDiff <- mean(simpson_BA$EF_diff)
simp_EF_sdDiff   <- sd(simpson_BA$EF_diff)

ggplot(simpson_BA, aes(x = EF_avg, y = EF_diff)) +
  geom_point() +
  geom_hline(yintercept = simp_EF_meanDiff, color = "steelblue") +
  geom_hline(yintercept = simp_EF_meanDiff + 1.96*simp_EF_sdDiff, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = simp_EF_meanDiff - 1.96*simp_EF_sdDiff, linetype = "dashed", color = "red") +
  labs(title = "Ejection fraction - Simpson's method", x = "Average ejection fraction between observers [ml]", y = "Difference between observers [ml]")


## BIPLANE BLAND-ALTMAN COMPARISON ----
biplane_BA <- tibble(
  MaxV_diff = biplane_wide$Max_V_HE - biplane_wide$Max_V_Hae,
  MaxV_avg  = rowMeans(biplane_wide[, c("Max_V_HE","Max_V_Hae")]),
  
  EF_diff   = biplane_wide$EF_HE    - biplane_wide$EF_Hae,
  EF_avg    = rowMeans(biplane_wide[, c("EF_HE","EF_Hae")])
)

# BA of max volume
bi_MaxV_meanDiff <- mean(biplane_BA$MaxV_diff, na.rm = TRUE)
bi_MaxV_sdDiff   <- sd(biplane_BA$MaxV_diff, na.rm = TRUE)

ggplot(biplane_BA, aes(x = MaxV_avg, y = MaxV_diff)) +
  geom_point() +
  geom_hline(yintercept = bi_MaxV_meanDiff, color = "steelblue") +
  geom_hline(yintercept = bi_MaxV_meanDiff + 1.96*bi_MaxV_sdDiff, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = bi_MaxV_meanDiff - 1.96*bi_MaxV_sdDiff, linetype = "dashed", color = "red") +
  labs(title = "Maximum left atrium volume- Biplane method", x = "Average maximum volume between observers [ml]", y = "Difference between observers [ml]")

# BA of EF
bi_EF_meanDiff <- mean(biplane_BA$EF_diff, na.rm = TRUE)
bi_EF_sdDiff   <- sd(biplane_BA$EF_diff, na.rm = TRUE)

ggplot(biplane_BA, aes(x = EF_avg, y = EF_diff)) +
  geom_point() +
  geom_hline(yintercept = bi_EF_meanDiff, color = "steelblue") +
  geom_hline(yintercept = bi_EF_meanDiff + 1.96*bi_EF_sdDiff, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = bi_EF_meanDiff - 1.96*bi_EF_sdDiff, linetype = "dashed", color = "red") +
  labs(title = "Ejection fraction - Biplane method", x = "Average ejection fraction between observers [ml]", y = "Difference between observers [ml]")

## DIAMETER BLAND-ALTMAN COMPARISON ----
diameter_BA <- tibble(
  MaxD_diff = biplane_wide$Max_d_HE - biplane_wide$Max_d_Hae,
  MaxD_avg  = rowMeans(biplane_wide[, c("Max_d_HE","Max_d_Hae")]),
)

# BA of max volume
dim_MaxD_meanDiff <- mean(diameter_BA$MaxD_diff, na.rm = TRUE)
dim_MaxD_sdDiff   <- sd(diameter_BA$MaxD_diff, na.rm = TRUE)

ggplot(diameter_BA, aes(x = MaxD_avg, y = MaxD_diff)) +
  geom_point() +
  geom_hline(yintercept = dim_MaxD_meanDiff, color = "steelblue") +
  geom_hline(yintercept = dim_MaxD_meanDiff + 1.96*dim_MaxD_sdDiff, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = dim_MaxD_meanDiff - 1.96*dim_MaxD_sdDiff, linetype = "dashed", color = "red") +
  labs(title = "Maximum left atrium diameter (4 chamber view)", x = "Average diameter length [mm]", y = "Difference between observers [mm]")
