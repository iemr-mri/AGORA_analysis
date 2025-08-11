# Load packages
library(readxl)
library(tidyverse)
library(ggpubr)

# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

# LA diameter - MRI vs Echo
ggscatter(LA_dm %>% filter(Age == '9', Group == "Aging"), 
          x = "Max_d_MR", y = "Max_d_Echo",
          add = "reg.line", conf.int = TRUE,
          xlab = "MRI [mm]", ylab = "Echo [mm]")

# LA diameter - MRI vs Echo - males
ggscatter(LA_dm %>% filter(Age == '9', Gender == 'Male', Group == "Aging"), 
          x = "Max_d_MR", y = "Max_d_Echo",
          add = "reg.line", conf.int = TRUE,
          xlab = "MRI [mm]", ylab = "Echo [mm]")

# LA diameter - MRI vs Echo - females
ggscatter(LA_dm %>% filter(Age == '9', Gender == 'Female', Group == "Aging"), 
          x = "Max_d_MR", y = "Max_d_Echo",
          add = "reg.line", conf.int = TRUE,
          xlab = "MRI [mm]", ylab = "Echo [mm]")

# LA diameter (MRI) vs LA size
ggscatter(LA_dm %>% filter(Age == '9', Group == "Aging"),
          x = "Max_d_MR", y = "Max_V",
          add = "reg.line", conf.int = TRUE,
          xlab = "LA max diameter [mm]", ylab = "LA max volume [ml]"
          )

# LA diameter (Echo) vs LA size
ggscatter(LA_dm %>% filter(Age == '9', Group == "Aging"),
          x = "Max_d_Echo", y = "Max_V",
          add = "reg.line", conf.int = TRUE,
          xlab = "LA max diameter - echo [mm]", ylab = "LA max volume [ml]"
)
