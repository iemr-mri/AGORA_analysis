# Load packages
library(readxl)
library(tidyverse)
library(ggpubr)
library(ggplot2)

# RUN DATA PREPARATION
source("Data handling/data_prep.r")
# or load saved data
load("Data handling/LA_data.RData")
load("Data handling/master_long.RData")

## LA diameter (MRI) over time ----
ggboxplot(master_long %>% filter(Group == "Aging"), 
          x = "Age", y = "Max_d",
          add = "jitter", shape = "Gender",
          xlab = "Age [months]", ylab = "LA diameter [mm]",
          fill = "Gender")

ggboxplot(master_long %>% filter(Group == "Aging"), 
          x = "Age", y = "Max_d",
          xlab = "Age [months]", ylab = "LA diameter [mm]",
          fill = "Gender")

ggline(master_long %>% filter(Group == "Aging"), 
       x = "Age", y = "Max_d",
       add = "mean_se",
       xlab = "Age [months]", ylab = "Maximum LA diameter [mm]",
       color = "Gender")

ggbarplot(master_long %>% filter(Group == "Aging"), 
       x = "Age", y = "Max_d",
       add = "mean_se",
       xlab = "Age [months]", ylab = "Maximum LA diameter [mm]",
       fill = "Gender",
       position = position_dodge(0.7))

## LA diameter - MRI vs Echo ----
ggscatter(LA_dm %>% filter(Age == '9', Group == "Aging"), 
          x = "Max_d_MR", y = "Max_d_Echo",
          add = "reg.line", conf.int = TRUE,
          xlab = "MRI [mm]", ylab = "Echo [mm]",
          color = "Gender")


## LA diameter (MRI) vs LA size ----
ggscatter(simpson_HE %>% filter(Group == "Aging"),
          x = "Max_d", y = "Max_V",
          add = "reg.line", conf.int = TRUE,
          xlab = "LA max diameter [mm]", ylab = "LA max volume [ml]",
          color = "Gender"
          )

## LA diameter (Echo) vs LA size ----
ggscatter(LA_dm %>% filter(Age == '9', Group == "Aging"),
          x = "Max_d_Echo", y = "Max_V",
          add = "reg.line", conf.int = TRUE,
          xlab = "LA max diameter - echo [mm]", ylab = "LA max volume [ml]",
          color = "Gender"
)

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
