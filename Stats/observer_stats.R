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
ggboxplot(observer_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Operator", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          xlab  = "Observer",
          title = "Maximum left atrium volume (9 months old) - observer comparison",
          fill = "Gender")

# Maximum left atrium volume boxplot
ggboxplot(observer_long %>% filter(Age == 16, Group == "Aging"), 
          x = "Operator", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          xlab  = "Observer",
          title = "Maximum left atrium volume (16 months old) - observer comparison",
          fill = "Gender")

# Maximum left atrium volume scatterplot
ggscatter(observer_wide %>% filter(Age == 9, Group == "Aging"), 
          x = "Max_V_HE" , y = "Max_V_Hae",
          add   = "reg.line", conf.int = TRUE,
          ylab  = "Maximum left atrium volume (ml) - Observer B",
          xlab  = "Maximum left atrium volume (ml) - Observer A",
          title = "Maximum left atrium volume (9 months old) - observer comparison",
          color = "Gender")

# Minimum left atrium volume boxplot
ggboxplot(observer_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Operator", y = "Min_V",
          ylab  = "Minimum left atrium volume (ml)",
          xlab  = "Observer",
          title = "Minimum left atrium volume (9 months old) - observer comparison",
          fill = "Gender")

# Minimum left atrium volume scatterplot
ggscatter(observer_wide %>% filter(Age == 9, Group == "Aging"), 
          x = "Min_V_HE" , y = "Min_V_Hae",
          add   = "reg.line", conf.int = TRUE,
          ylab  = "Minimum left atrium volume (ml) - Observer B",
          xlab  = "Minimum left atrium volume (ml) - Observer A",
          title = "Minimum left atrium volume (9 months old) - observer comparison",
          color = "Gender")

# Left atrium stroke volume boxplot
ggboxplot(observer_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Operator", y = "SV",
          ylab  = "Left atrium stroke volume (ml)",
          xlab  = "Observer",
          title = "Left atrium stroke volume (9 months old) - observer comparison",
          fill = "Gender")

# Left atrium stroke volume scatterplot
ggscatter(observer_wide %>% filter(Age == 9, Group == "Aging"), 
          x = "EF_HE" , y = "EF_Hae",
          add   = "reg.line", conf.int = TRUE,
          ylab  = "Left atrium ejection fraction (%) - Observer B",
          xlab  = "Left atrium ejection fraction (%) - Observer A",
          title = "Left atrium ejection fraction (9 months old) - observer comparison",
          color = "Gender")


## OBSERVER BLAND-ALTMAN COMPARISON ----
# ggpubr does not have easy functionality for BA plots so using basic ggplot2

observer_BA <- tibble(
  MaxV_diff = observer_wide$Max_V_HE - observer_wide$Max_V_Hae,
  MaxV_avg  = rowMeans(observer_wide[, c("Max_V_HE","Max_V_Hae")]),
  
  MinV_diff = observer_wide$Min_V_HE - observer_wide$Min_V_Hae,
  MinV_avg  = rowMeans(observer_wide[, c("Min_V_HE","Min_V_Hae")]),
  
  EF_diff   = observer_wide$EF_HE    - observer_wide$EF_Hae,
  EF_avg    = rowMeans(observer_wide[, c("EF_HE","EF_Hae")])
)

# BA of max volume
MaxV_meanDiff <- mean(observer_BA$MaxV_diff)
MaxV_sdDiff   <- sd(observer_BA$MaxV_diff)

ggplot(observer_BA, aes(x = MaxV_avg, y = MaxV_diff)) +
  geom_point() +
  geom_hline(yintercept = MaxV_meanDiff, color = "steelblue") +
  geom_hline(yintercept = MaxV_meanDiff + 1.96*MaxV_sdDiff, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = MaxV_meanDiff - 1.96*MaxV_sdDiff, linetype = "dashed", color = "red") +
  labs(title = "Maximum left atrium volume", x = "Average maximum volume between observers", y = "Difference between observers")

# BA of min volume
MinV_meanDiff <- mean(observer_BA$MinV_diff)
MinV_sdDiff   <- sd(observer_BA$MinV_diff)

ggplot(observer_BA, aes(x = MinV_avg, y = MinV_diff)) +
  geom_point() +
  geom_hline(yintercept = MinV_meanDiff, color = "steelblue") +
  geom_hline(yintercept = MinV_meanDiff + 1.96*MinV_sdDiff, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = MinV_meanDiff - 1.96*MinV_sdDiff, linetype = "dashed", color = "red") +
  labs(title = "Minimum left atrium volume", x = "Average minimum volume between observers", y = "Difference between observers")

# BA of EF
EF_meanDiff <- mean(observer_BA$EF_diff)
EF_sdDiff   <- sd(observer_BA$EF_diff)

ggplot(observer_BA, aes(x = EF_avg, y = EF_diff)) +
  geom_point() +
  geom_hline(yintercept = EF_meanDiff, color = "steelblue") +
  geom_hline(yintercept = EF_meanDiff + 1.96*EF_sdDiff, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = EF_meanDiff - 1.96*EF_sdDiff, linetype = "dashed", color = "red") +
  labs(title = "Ejection fraction", x = "Average ejection fraction between observers", y = "Difference between observers")

