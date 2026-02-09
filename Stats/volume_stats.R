# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)
library(tidyverse)

# -------- MAX VOLUME --------
# Maximum left atrium volume - faceted by gender
ggplot(LA_tib, aes(x = factor(age_months), y = max_volume)) +
  geom_boxplot(aes(fill=gender), width = .5) +
  xlab("Age (Months)") +
  ylab("Maximum LA volume (mL)") +
  theme_grey(base_size = 15) +
  guides(fill = FALSE) +
  facet_grid(~ gender) +
  stat_compare_means(comparisons = list(c("9","16"),c("16","24"),c("9","24")), method = "t.test", label = "p.signif")

ggplot(LA_tib, aes(fill = gender, x = factor(gender_age), y = max_volume)) +
  geom_boxplot() +
  xlab("Age (Months)") +
  ylab("Maximum LA volume / tibia length (mL/mm)") +
  theme_grey(base_size = 15) +
  guides(fill = FALSE) +
  scale_x_discrete(limits = c("Female 9M", "Female 16M", "Male 9M", "Male 16M")) + 
  stat_compare_means(comparisons = list(c("Female 9M", "Female 16M"),
                                        c("Male 9M", "Male 16M"),
                                        c("Male 9M","Female 9M"),
                                        c("Male 16M", "Female 16M")), 
                     method = "t.test", label = "p.signif")


# -------- ejection fraction --------
# Left atrium ejection fraction - faceted by gender
ggplot(LA_tib %>% filter(model == "Aging"), aes(x = factor(age_months), y = ejection_fraction)) +
  geom_boxplot(aes(fill=gender)) +
  xlab("Age (Months)") +
  ylab("LA ejection fraction (%)") +
  theme_grey(base_size = 15) +
  guides(fill = FALSE) +
  facet_grid(~ gender) +
  stat_compare_means(comparisons = list(c("9","16"),c("16","24"),c("9","24")), method = "t.test", label = "p.signif")

ggplot(LA_tib, aes(x = factor(age_months), y = ejection_fraction)) +
  geom_boxplot(aes(fill=gender)) +
  xlab("Age (Months)") +
  ylab("LA ejection fraction (%)") +
  theme_grey(base_size = 15) +
  guides(fill = FALSE) +
  facet_grid(~ gender) +
  stat_compare_means(comparisons = list(c("9","16"),c("16","24"),c("9","24")), method = "t.test", label = "p.signif")

# Left atrium ejection fraction - grouped by gender and age
ggplot(LA_tib, 
       aes(x = factor(gender_age), y = ejection_fraction)) +
  geom_boxplot(aes(fill=gender)) +
  xlab("") +
  ylab("LA ejection fraction (%)") +
  theme_grey(base_size = 15) +
  guides(fill = FALSE) +
  scale_x_discrete(limits = c("Female 9M", "Female 16M", "Male 9M", "Male 16M")) + 
  stat_compare_means(comparisons = list(c("Female 9M", "Female 16M"),
                                        c("Male 9M", "Male 16M"),
                                        c("Male 9M","Female 9M"),
                                        c("Male 16M", "Female 16M")), 
                     method = "t.test", label = "p.signif")


# -------- stroke volume --------
# Left atrium stroke volume
ggplot(LA_tib, 
       aes(x = factor(age_months), y = stroke_volume)) +
  geom_boxplot(aes(fill=gender)) +
  xlab("Age (Months)") +
  ylab("LA stroke volume (ml) / tibia length (mm)") +
  theme_grey(base_size = 15) +
  facet_grid(~ gender) +
  scale_x_discrete(limits = c("9","16")) + # tibia lengths for 24m not available yet
  stat_compare_means(comparisons = list(c("9","16")), method = "t.test", label = "p.signif")

ggplot(LA_tib, 
       aes(x = factor(gender_age), y = stroke_volume)) +
  geom_boxplot(aes(fill=gender)) +
  xlab("") +
  ylab("LA stroke volume (mL) / tibia length (mm)") +
  theme_grey(base_size = 15) +
  guides(fill = FALSE) +
  scale_x_discrete(limits = c("Female 9M", "Female 16M", "Male 9M", "Male 16M")) + 
  stat_compare_means(comparisons = list(c("Female 9M", "Female 16M"),
                                        c("Male 9M", "Male 16M"),
                                        c("Male 9M","Female 9M"),
                                        c("Male 16M", "Female 16M")), 
                     method = "t.test", label = "p.signif")
