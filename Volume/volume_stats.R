# RUN DATA PREPARATION
source("Volume/volume_prep.r")
# or load saved data
load("Volume/volume_data.RData")

# ACTIVATE PACKAGES
library(ggpubr)

## MAX VOLUME ## ----

# Maximum left atrium volume (9 months old) - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Age == 9, ], x= "Gender", y = "Max_V",
          ylab = "Maximum left atrium volume (ml)",
          fill = "Gender") + 
  theme(legend.position = "none")

# Maximum left atrium volume (16 months old) - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Age == 16, ], x= "Gender", y = "Max_V",
          ylab = "Maximum left atrium volume (ml)",
          fill = "Gender") + 
  theme(legend.position = "none")

# Maximum left atrium volume in females from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Gender == "Female", ], x = "Age", y = "Max_V",
          ylab = "Maximum left atrium volume (ml)",
          xlab = "Age (months)",
          fill = "Age") + 
  theme(legend.position = "none")

# Maximum left atrium volume in males from 9 to 16 months old - Simpson/3D method
ggboxplot(simpson_HE[simpson_HE$Gender == "Male", ], x = "Age", y = "Max_V",
          ylab = "Maximum left atrium volume (ml)",
          xlab = "Age (months)",
          fill = "Age") + 
  theme(legend.position = "none")

# Maximum left atrium volume (9 months old) - both methods
ggboxplot(method_frame[method_frame$Age == 9, ], x = "Method", y = "Max_V",
          ylab ="Maximum left atrium volume (ml)",
          xlab = "",
          fill = "Gender")




