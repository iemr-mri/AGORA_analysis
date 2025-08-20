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
       color = 'Gender', add = "mean_se")

# Line plot of LA ejection fraction per week
ggline(simpson_HE %>% filter(Age == 9, Group == 'MI'),
       x = 'MI_week', y = 'EF',
       xlab = 'MI week number', ylab = 'Ejection fraction [%]',
       add = c("mean_se"))

# line plot of LA diameter per week
ggline(biplane_HE %>% filter(Age == 9, Group == 'MI'),
       x = 'MI_week', y = 'Max_d',
       xlab = 'MI week number', ylab = 'Maximum LA diameter [mm]',
       color = 'Gender', add = "mean_se")

## COHORT HISTORY box diagrams ----

# Box plot of maximum LA volume per week
ggboxplot(simpson_HE %>% filter(Age == 9, Group == 'MI'),
          x = 'MI_week', y = 'Max_V',
          xlab = 'MI week number', ylab = 'Maximum LA volume [ml]',
          fill = 'Gender',
          facet.by = "Gender")

ggboxplot(simpson_HE %>% filter(Age == 9, Group == 'MI'),
       x = 'MI_week', y = 'EF',
       xlab = 'MI week number', ylab = 'Ejection fraction [%]')

ggboxplot(biplane_HE %>% filter(Age == 9, Group == 'MI'),
          x = 'MI_week', y = 'Max_d',
          xlab = 'MI week number', ylab = 'Maximum LA diameter [mm]',
          fill = 'Gender',
          facet.by = "Gender")
