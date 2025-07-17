# RUN DATA PREPARATION
source("Volume/volume_prep.r")
# or load saved data
load("Volume/volume_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)

## MAX VOLUME ## ----

# Maximum left atrium volume (9 months old) - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Age == 9, ], x= "Gender", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          title = "Maximum left atrium volume (9 months old) - Simpson/3D method",
          fill  = "Gender") + 
  theme(legend.position = "none")

# Maximum left atrium volume (16 months old) - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Age == 16, ], x= "Gender", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          title = "Maximum left atrium volume (16 months old) - Simpson/3D method",
          fill  = "Gender") + 
  theme(legend.position = "none")

# Maximum left atrium volume in females from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Gender == "Female", ], x = "Age", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Maximum left atrium volume in females from 9 to 16 months old - Simpson/3D method",
          fill  = "Age") + 
  theme(legend.position = "none")

# Maximum left atrium volume in males from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Gender == "Male", ], x = "Age", y = "Max_V",
          ylab  = "Maximum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Maximum left atrium volume in males from 9 to 16 months old - Simpson/3D method",
          fill  = "Age") + 
  theme(legend.position = "none")

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

# Minimum left atrium volume (9 months old) - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Age == 9, ], x= "Gender", y = "Min_V",
          ylab  = "Minimum left atrium volume (ml)",
          title = "Minimum left atrium volume (9 months old) - Simpson/3D method",
          fill  = "Gender") + 
  theme(legend.position = "none")

# Minimum left atrium volume (16 months old) - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Age == 16, ], x= "Gender", y = "Min_V",
          ylab  = "Minimum left atrium volume (ml)",
          title = "Minimum left atrium volume (16 months old) - Simpson/3D method",
          fill  = "Gender") + 
  theme(legend.position = "none")

# Minimum left atrium volume in females from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Gender == "Female", ], x = "Age", y = "Min_V",
          ylab  = "Minimum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Minimum left atrium volume in females from 9 to 16 months old - Simpson/3D method",
          fill  = "Age") + 
  theme(legend.position = "none")

# Minimum left atrium volume in males from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Gender == "Male", ], x = "Age", y = "Min_V",
          ylab  = "Minimum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Minimum left atrium volume in males from 9 to 16 months old - Simpson/3D method",
          fill  = "Age") + 
  theme(legend.position = "none")

# Minimum left atrium volume from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE, x = "Age", y = "Min_V",
          ylab  = "Minimum left atrium volume (ml)",
          xlab  = "Age (months)",
          title = "Minimum left atrium volume from 9 to 16 months old - Simpson/3D method",
          fill  = "Gender")

# Minimum left atrium volume (9 months old) - both methods
ggboxplot(method_frame[method_frame$Age == 9, ], x = "Method", y = "Min_V",
          ylab  ="Minimum left atrium volume (ml)",
          xlab  = "",
          title = "Minimum left atrium volume (9 months old) - both methods",
          fill  = "Gender")

## STROKE VOLUME ## ----

# LA stroke volume (9 months old) - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Age == 9, ], x= "Gender", y = "SV",
          ylab  = "Stroke volume (ml)",
          title = "Left atrium stroke volume (9 months old) - Simpson/3D method",
          fill  = "Gender") + 
  theme(legend.position = "none")

# LA stroke volume (16 months old) - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Age == 16, ], x= "Gender", y = "SV",
          ylab  = "Stroke volume (ml)",
          title = "Left atrium stroke volume (16 months old) - Simpson/3D method",
          fill  = "Gender") + 
  theme(legend.position = "none")

# LA stroke volume in females from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Gender == "Female", ], x = "Age", y = "SV",
          ylab  = "Stroke volume (ml)",
          xlab  = "Age (months)",
          title = "Left atrium stroke volume in females from 9 to 16 months old - Simpson/3D method",
          fill  = "Age") + 
  theme(legend.position = "none")

# LA stroke volume in males from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Gender == "Male", ], x = "Age", y = "SV",
          ylab  = "Stroke volume (ml)",
          xlab  = "Age (months)",
          title = "LA stroke volume in males from 9 to 16 months old - Simpson/3D method",
          fill  = "Age") + 
  theme(legend.position = "none")

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
