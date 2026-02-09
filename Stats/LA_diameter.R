# Load packages
library(readxl)
library(tidyverse)
library(ggpubr)
library(ggplot2)

# ----------------- not yet updated to SQL standard ------------
# runs on old excel format

# RUN DATA PREPARATION
source("Data handling/data_prep_excel.r")
# or load saved data
load("Data handling/LA_data_old.RData")

## Max LA diameter (MRI) over time ----
# Maximum left atrium diameter from 9 to 16 months old
ggplot(simpson_HE %>% filter(Group == "Aging"), 
       aes(x = factor(Age), y = Max_D)) +
  geom_boxplot(aes(fill=Gender)) +
  xlab("Age (Months)") +
  ylab("Maximum LA diameter (mm)") +
  theme_grey(base_size = 15) +
  facet_grid(~ Gender) +
  stat_compare_means(comparisons = list(c("9","16")), method = "t.test", label = "p.signif")

# Maximum left atrium diameter from 9 to 16 months old - faceted
ggplot(simpson_HE %>% filter(Group == "Aging"), 
       aes(x = factor(Age), y = Max_D)) +
  geom_boxplot(aes(fill=Gender)) +
  xlab("Age (Months)") +
  ylab("Maximum LA diameter (mm)") +
  theme_grey(base_size = 15) +
  facet_grid(~ Gender) +
  stat_compare_means(comparisons = list(c("9","16")), method = "t.test", label = "p.signif")

# Maximum left atrium diameter from 9 to 16 months old (tibia corrected)
ggplot(simpson_tib %>% filter(Group == "Aging"), 
       aes(fill = Gender, x = factor(Age), y = Max_D)) +
  geom_boxplot() +
  xlab("Age (Months)") +
  ylab("Maximum LA diameter (mm) / tibia length (mm)") +
  theme_grey(base_size = 15) +
  #facet_grid(. ~ Gender) +
  stat_compare_means(aes(group = Gender), method = "t.test", label = "p.signif")

## Min LA diameter (MRI) over time ----
# Minimum left atrium diameter from 9 to 16 months old
ggplot(simpson_HE %>% filter(Group == "Aging"), 
       aes(fill = Gender, x = factor(Age), y = Min_D)) +
  geom_boxplot() +
  xlab("Age (Months)") +
  ylab("Minimum LA diameter (mm)") +
  theme_grey(base_size = 15) +
  #facet_grid(. ~ Gender) +
  stat_compare_means(aes(group = Gender), method = "t.test", label = "p.signif")

# Minimum left atrium diameter from 9 to 16 months old (tibia corrected)
ggplot(simpson_tib %>% filter(Group == "Aging"), 
       aes(fill = Gender, x = factor(Age), y = Min_D)) +
  geom_boxplot() +
  xlab("Age (Months)") +
  ylab("Minimum LA diameter (mm) / tibia length (mm)") +
  theme_grey(base_size = 15) +
  #facet_grid(. ~ Gender) +
  stat_compare_means(aes(group = Gender), method = "t.test", label = "p.signif")

## LA diameter - MRI vs Echo ----



## LA diameter (MRI) vs LA size ----


## LA diameter (Echo) vs LA size ----


## Bland-Altman ----
dm_BA <- tibble(
  dm_diff = LA_dm$Max_d_MR - LA_dm$Max_d_Echo,
  dm_avg  = rowMeans(LA_dm[, c("Max_d_MR", "Max_d_Echo")])
)

dm_BA <- na.omit(dm_BA)

dm_meanDiff = mean(dm_BA$dm_diff)
dm_sdDiff   = sd(dm_BA$dm_avg)

ggplot(dm_BA, aes(x = dm_avg, y = dm_diff)) +
  geom_point() + 
  geom_hline(yintercept = dm_meanDiff, color = "steelblue") +
  geom_hline(yintercept = dm_meanDiff + 1.96*dm_sdDiff, linetype = "dashed", color = "red") +
  geom_hline(yintercept = dm_meanDiff - 1.96*dm_sdDiff, linetype = "dashed", color = "red") +
  labs(x = "Average between modalities [mm]", y = "Difference between modalities [mm]")
