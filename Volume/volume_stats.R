# RUN DATA PREPARATION
source("Volume/volume_prep.r")
# or load saved data
load("Volume/volume_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)

## MAX VOLUME ## ----

# Maximum left atrium volume from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE, x = "Age", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Maximum left atrium volume from 9 to 16 months old - Simpson/3D method",
          fill  = "Gender")

# Maximum left atrium volume (9 months old) - both methods
ggboxplot(method_frame[method_frame$Age == 9, ], x = "Method", y = "Max_V",
          ylab  ="Maximum left atrium volume (ml)",
          xlab  = "",
          title = "Maximum left atrium volume (9 months old) - both methods",
          fill  = "Gender")


## Min VOLUME ## ----

# Minimum left atrium volume from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE, x = "Age", y = "Min_V",
          ylab  = "Minimum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Minimum left atrium volume from 9 to 16 months old - Simpson/3D method",
          fill  = "Gender")

# Minimum left atrium volume (9 months old) - both methods
ggboxplot(method_frame %>% filter(Age == 9), x = "Method", y = "Min_V",
          ylab  ="Minimum left atrium volume (ml)",
          xlab  = "",
          title = "Minimum left atrium volume (9 months old) - both methods",
          fill  = "Gender")

## STROKE VOLUME ## ----

# LA stroke volume from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE, x = "Age", y = "SV",
          ylab  = "Stroke volume (ml)",
          xlab  = "Age (months)",
          title = "Left atrium stroke volume from 9 to 16 months old - Simpson/3D method",
          fill  = "Gender")

# LA stroke volume (9 months old) - both methods
ggboxplot(method_frame[method_frame$Age == 9, ], x = "Method", y = "SV",
          ylab  ="Stroke volume (ml)",
          xlab  = "",
          title = "Left atrium stroke volume (9 months old) - both methods",
          fill  = "Gender")

## OBSERVER COMPARISON ----

# Maximum left atrium volume boxplot
ggboxplot(observer_long %>% filter(Age == 9), x = "Operator", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          xlab  = "Observer",
          title = "Maximum left atrium volume (9 months old) - observer comparison",
          fill = "Gender")

# Maximum left atrium volume scatterplot
ggscatter(observer_wide %>% filter(Age == 9), x = "Max_V_HE" , y = "Max_V_Hae",
          add   = "reg.line", conf.int = TRUE,
          ylab  = "Maximum left atrium volume (ml) - Observer B",
          xlab  = "Maximum left atrium volume (ml) - Observer A",
          title = "Maximum left atrium volume (9 months old) - observer comparison",
          color = "Gender")

# Minimum left atrium volume boxplot
ggboxplot(observer_long %>% filter(Age == 9), x = "Operator", y = "Min_V",
          ylab  = "Minimum left atrium volume (ml)",
          xlab  = "Observer",
          title = "Minimum left atrium volume (9 months old) - observer comparison",
          fill = "Gender")

# Minimum left atrium volume scatterplot
ggscatter(observer_wide %>% filter(Age == 9), x = "Min_V_HE" , y = "Min_V_Hae",
          add   = "reg.line", conf.int = TRUE,
          ylab  = "Minimum left atrium volume (ml) - Observer B",
          xlab  = "Minimum left atrium volume (ml) - Observer A",
          title = "Minimum left atrium volume (9 months old) - observer comparison",
          color = "Gender")

# Left atrium stroke volume boxplot
ggboxplot(observer_long %>% filter(Age == 9), x = "Operator", y = "SV",
          ylab  = "Left atrium stroke volume (ml)",
          xlab  = "Observer",
          title = "Left atrium stroke volume (9 months old) - observer comparison",
          fill = "Gender")

# Left atrium stroke volume scatterplot
ggscatter(observer_wide %>% filter(Age == 9), x = "SV_HE" , y = "SV_Hae",
          add   = "reg.line", conf.int = TRUE,
          ylab  = "Left atrium stroke volume (ml) - Observer B",
          xlab  = "Left atrium stroke volume (ml) - Observer A",
          title = "Left atrium stroke volume (9 months old) - observer comparison",
          color = "Gender")
