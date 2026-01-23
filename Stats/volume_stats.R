# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)
library(tidyverse)

## MAX VOLUME ## ----

# Maximum left atrium volume from 9 to 16 months old
ggplot(simpson_HE %>% filter(Group == "Aging"), 
       aes(fill = Gender, x = factor(Age), y = Max_V)) +
  geom_boxplot() +
  xlab("Age (Months)") +
  ylab("Maximum LA volume (mL)") +
  theme_grey(base_size = 15) +
  #facet_grid(. ~ Gender) +
  stat_compare_means(aes(group = Gender), method = "t.test", label = "p.signif")

# Maximum left atrium volume from 9 to 16 months old - faceted by gender
ggplot(simpson_HE %>% filter(Group == "Aging"), 
       aes(x = factor(Age), y = Max_V)) +
  geom_boxplot(aes(fill=Gender)) +
  xlab("Age (Months)") +
  ylab("Maximum LA volume (mL)") +
  theme_grey(base_size = 15) +
  facet_grid(~ Gender) +
  stat_compare_means(comparisons = list(c("9","16")), method = "t.test", label = "p.signif")

# Tibia corrected max volume
ggplot(simpson_tib %>% filter(Group == "Aging"), 
       aes(fill = Gender, x = factor(Age), y = Max_V)) +
  geom_boxplot() +
  xlab("Age (Months)") +
  ylab("Maximum LA volume / tibia length (mL/mm)") +
  theme_grey(base_size = 15) +
  #facet_grid(. ~ Gender) +
  stat_compare_means(aes(group = Gender), method = "t.test", label = "p.signif")

  
# Paired maximum left atrium volume 9-16 months old
ggplot(simpson_HE %>% filter(Group == "Aging")%>% 
         group_by(ID) %>%
         filter(n() == 2) %>%
         ungroup(), 
       aes(group = Age, x = factor(Age), y = Max_V)) +
  geom_boxplot(aes(fill = Gender)) +
  xlab("Age (Months)") +
  ylab("Maximum left atrium volume (mL)") +
  facet_grid(. ~ Gender) +
  stat_compare_means(comparisons = list(c("9", "16")), method = "t.test", paired = TRUE, label = "p.format")


## MIN VOLUME ----
# Minimum left atrium volume from 9 to 16 months old - Simpson/3D method
ggplot(simpson_HE %>% filter(Group == "Aging"), 
       aes(fill = Gender, x = factor(Age), y = Min_V)) +
  geom_boxplot() +
  xlab("Age (Months)") +
  ylab("Minimum LA volume (mL)") +
  theme_grey(base_size = 15) +
  #facet_grid(. ~ Gender) +
  stat_compare_means(aes(group = Gender), method = "t.test", label = "p.signif")

# Tibia corrected max volume
ggplot(simpson_tib %>% filter(Group == "Aging"), 
       aes(fill = Gender, x = factor(Age), y = Min_V)) +
  geom_boxplot() +
  xlab("Age (Months)") +
  ylab("Minimum LA volume / tibia length (mL/mm)") +
  theme_grey(base_size = 15) +
  #facet_grid(. ~ Gender) +
  stat_compare_means(aes(group = Gender), method = "t.test", label = "p.signif")

# Paired minimum left atrium volume 9-16 months old
ggplot(simpson_HE %>% filter(Group == "Aging")%>% 
         group_by(ID) %>%
         filter(n() == 2) %>%
         ungroup(), 
       aes(group = Age, x = factor(Age), y = Min_V)) +
  geom_boxplot(aes(fill = Gender)) +
  xlab("Age (Months)") +
  ylab("Minimum left atrium volume (mL)") +
  facet_grid(. ~ Gender) +
  stat_compare_means(comparisons = list(c("9", "16")), method = "t.test", paired = TRUE, label = "p.format")

## VOLUME CHANGE
ggplot(simpson_HE %>% filter(Group == "Aging")%>% 
         group_by(ID) %>%
         filter(n() == 2) %>%
         ungroup(), 
       aes(x = Gender, y = Max_V-Min_V)) +
  geom_boxplot(aes(fill = Gender)) +
  xlab("Gender") +
  ylab("Maximum volume change 9-16 months") +
  stat_compare_means(comparisons = list(c("Female", "Male")), method = "t.test", label = "p.format")

## EJECTION FRACTION ----

# Left atrium ejection fraction from 9 to 16 months old
ggplot(simpson_HE %>% filter(Group == "Aging"), 
       aes(x = factor(Age), y = EF)) +
  geom_boxplot(aes(fill=Gender)) +
  xlab("Age (Months)") +
  ylab("LA ejection fraction (%)") +
  theme_grey(base_size = 15) +
  facet_grid(~ Gender) +
  stat_compare_means(comparisons = list(c("9","16")), method = "t.test", label = "p.signif")
