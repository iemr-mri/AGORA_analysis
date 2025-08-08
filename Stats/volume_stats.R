# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

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

