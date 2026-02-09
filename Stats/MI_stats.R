# RUN DATA PREPARATION
source("Data handling/data_prep_excel.r")
# or load saved data
load("Data handling/LA_data_old.RData")

# ----------------- not yet updated to SQL standard ------------
# runs on old excel format

LGE_extent <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "MI size")
simpson_LGE <- inner_join(simpson_HE, LGE_extent, by = c("ID","Cohort"))
simpson_LGE$`LGE extent` <- 
  factor(simpson_LGE$`LGE extent`, levels = c("0-5%","5-10%","10-15%","15-20%","20-25%","25-30%"))
simpson_LGE$`LGE extent` <- 
  factor(simpson_LGE$`LGE extent`, 
         levels = c("0-5%","5-10%","10-15%","15-20%","20-25%","25-30%"),
         labels = c("0-15%","0-15%","0-15%","15-30%","15-30%","15-30%"))

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)
library(tidyverse)

## COHORT HISTORY line diagrams ----

# Line plot of maximum LA volume per week - gender grouped
ggline(simpson_HE %>% filter(Age == 9, Group == 'MI'),
       x = 'MI_week', y = 'Max_V',
       xlab = 'MI week number', ylab = 'Maximum LA volume [ml]',
       color = 'Gender', add = "mean_se") +
  theme_grey(base_size = 15)

# Line plot of maximum LA diameter per week - gender grouped
ggline(simpson_HE %>% filter(Age == 9, Group == 'MI'),
       x = 'MI_week', y = 'Max_D',
       xlab = 'MI week number', ylab = 'Maximum LA diameter [mm]',
       color = 'Gender', add = "mean_se") +
  theme_grey(base_size = 15)

# Line plot of LA ejection fraction per week
ggline(simpson_HE %>% filter(Age == 9, Group == 'MI'),
       x = 'MI_week', y = 'EF',
       xlab = 'MI week number', ylab = 'LA ejection fraction [%]',
       add = c("mean_se")) +
  theme_grey(base_size = 15)

## LGE EXTENT ----
ggline(simpson_LGE %>% filter(Age == 9, Group == "MI", MI_week %in% c("0","1","6")),
       x = "MI_week", y = "EF", color = "LGE extent", palette = "jco",
       xlab = "MI week number", ylab = "LA ejection fraction [%]",
       add = c("mean_se")) +
  theme_grey(base_size = 15)

ggline(simpson_LGE %>% filter(Age == 9, Group == "MI", MI_week %in% c("0","1","6")),
       x = "MI_week", y = "Max_D", color = "LGE extent", palette = "jco",
       xlab = "MI week number", ylab = "LA maximum diameter [mm]",
       facet.by = "Gender",
       add = c("mean_se")) +
  theme_grey(base_size = 15)

ggline(simpson_LGE %>% filter(Age == 9, Group == "MI", MI_week %in% c("0","1","6")),
       x = "MI_week", y = "SV", color = "LGE extent", palette = "jco",
       xlab = "MI week number", ylab = "LA stroke volume",
       facet.by = "Gender",
       add = c("mean_se")) +
  theme_grey(base_size = 15)





## EXTRA MIN SIZE PLOTS ----
# Line plot of maximum LA volume per week - gender grouped
ggline(simpson_HE %>% filter(Age == 9, Group == 'MI'),
       x = 'MI_week', y = 'Min_V',
       xlab = 'MI week number', ylab = 'Minimum LA volume [ml]',
       color = 'Gender', add = "mean_se") +
  theme_grey(base_size = 15)

# Line plot of maximum LA volume per week - gender grouped
ggline(simpson_HE %>% filter(Age == 9, Group == 'MI'),
       x = 'MI_week', y = 'Min_D',
       xlab = 'MI week number', ylab = 'Minimum LA diameter [mm]',
       color = 'Gender', add = "mean_se") +
  theme_grey(base_size = 15)
