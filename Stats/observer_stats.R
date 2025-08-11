# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)

## OBSERVER COMPARISON ----

# Maximum left atrium volume boxplot
ggboxplot(observer_long %>% filter(Age == 9, Group == "Aging"), 
          x = "Operator", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          xlab  = "Observer",
          title = "Maximum left atrium volume (9 months old) - observer comparison",
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
          x = "SV_HE" , y = "SV_Hae",
          add   = "reg.line", conf.int = TRUE,
          ylab  = "Left atrium stroke volume (ml) - Observer B",
          xlab  = "Left atrium stroke volume (ml) - Observer A",
          title = "Left atrium stroke volume (9 months old) - observer comparison",
          color = "Gender")
