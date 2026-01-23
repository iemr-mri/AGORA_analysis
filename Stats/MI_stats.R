# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

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

# Line plot of maximum LA volume per week - gender grouped
ggline(simpson_HE %>% filter(Age == 9, Group == 'MI'),
       x = 'MI_week', y = 'Max_D',
       xlab = 'MI week number', ylab = 'Maximum LA diameter [mm]',
       color = 'Gender', add = "mean_se") +
  theme_grey(base_size = 15)

# Line plot of LA ejection fraction per week
ggline(simpson_HE %>% filter(Age == 9, Group == 'MI'),
       x = 'MI_week', y = 'EF',
       xlab = 'MI week number', ylab = 'Ejection fraction [%]',
       color = 'Gender',
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
