# RUN DATA PREPARATION
source("Volume/volume_prep.r")
# or load saved data
load("Volume/volume_data.RData")

# ACTIVATE PACKAGES
library(ggpubr)

# Max volume
ggboxplot(simpson_HE[simpson_HE$Scan_nr == 1, ], x= "Gender", y = "Max_V",
          ylab = "Maximum left atrium volume (ml)",
          fill = "Gender", palette = "RdBu")

ggboxplot(simpson_HE[simpson_HE$Scan_nr == 2, ], x= "Gender", y = "Max_V",
          ylab = "Maximum left atrium volume (ml)",
          fill = "Gender", palette = "RdBu")

ggboxplot(simpson_HE[simpson_HE$Gender == "Female", ], x = "Scan_nr", y = "Max_V",
          ylab = "Maximum left atrium volume (ml)",
          fill = "Scan_nr", palette = "RdBu")

ggboxplot(simpson_HE[simpson_HE$Gender == "Male", ], x = "Scan_nr", y = "Max_V",
          ylab = "Maximum left atrium volume (ml)",
          fill = "Scan_nr", palette = "RdBu")
