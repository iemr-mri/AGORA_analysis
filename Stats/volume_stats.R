# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)
library(ggpubr)
library(tidyverse)

## GROUPING
# create combined group label
simpson_grouped <- simpson_HE %>%
  filter(Group == "Aging") %>%
  mutate(Age = as.character(Age),
         GenderAge = paste0(Gender, " ", Age, "M")) # e.g. "Male 9M", "Female 16M"

---- ## MAX VOLUME ## ----

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
  geom_boxplot(aes(fill=Gender),outlier.shape = NA) +
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
       aes(x = factor(Age), y = Min_V)) +
  geom_boxplot(aes(fill = Gender)) +
  xlab("Age (Months)") +
  ylab("Minimum LA volume (mL)") +
  theme_grey(base_size = 15) +
  facet_grid(~ Gender) +
  stat_compare_means(comparisons = list(c("9","16")), method = "t.test", label = "p.signif")

# Tibia corrected min volume
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

ggplot(simpson_grouped %>% filter(Group == "Aging"), 
       aes(x = factor(GenderAge), y = EF)) +
  geom_boxplot(aes(fill=Gender)) +
  xlab("") +
  ylab("LA ejection fraction (%)") +
  theme_grey(base_size = 15) +
  scale_x_discrete(limits = c("Female 9M", "Female 16M", "Male 9M", "Male 16M")) + 
  stat_compare_means(comparisons = list(c("Female 9M", "Female 16M"),
                                        c("Male 9M", "Male 16M"),
                                        c("Male 9M","Female 9M"),
                                        c("Male 16M", "Female 16M")), 
                     method = "t.test", label = "p.signif")

# Left atrium stroke volume from 9 to 16 months old
ggplot(simpson_HE %>% filter(Group == "Aging"), 
       aes(x = factor(Age), y = SV)) +
  geom_boxplot(aes(fill=Gender)) +
  xlab("Age (Months)") +
  ylab("LA stroke volume (ml)") +
  theme_grey(base_size = 15) +
  facet_grid(~ Gender) +
  stat_compare_means(comparisons = list(c("9","16")), method = "t.test", label = "p.signif")

ggplot(simpson_grouped %>% filter(Group == "Aging"), 
       aes(x = factor(GenderAge), y = SV)) +
  geom_boxplot(aes(fill=Gender)) +
  xlab("") +
  ylab("LA stroke volume (mL)") +
  theme_grey(base_size = 15) +
  scale_x_discrete(limits = c("Female 9M", "Female 16M", "Male 9M", "Male 16M")) + 
  stat_compare_means(comparisons = list(c("Female 9M", "Female 16M"),
                                        c("Male 9M", "Male 16M"),
                                        c("Male 9M","Female 9M"),
                                        c("Male 16M", "Female 16M")), 
                     method = "t.test", label = "p.signif")
