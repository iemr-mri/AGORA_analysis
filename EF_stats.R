# RUN DATA PREPARATION
source("EF_data.r")
# or load saved data
load("EF_data.RData")

# ACTIVATE PACKAGES
library(ggplot2)

## EF BOX PLOTS ----
# EF box - Simpson's
simpson_frame %>%
  ggplot(aes(x = Gender, y = EF, fill = Gender)) +
    geom_boxplot(width = 0.5) +
    labs(title = "Ejection fraction - Simpson's method",
         x = "Gender",
         y = "EF (%)") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("tomato", "steelblue"))

# EF box - Biplane
biplane_frame %>%
  ggplot(aes(x = Gender, y = EF, fill = Gender)) +
    geom_boxplot(width = 0.5) +
    labs(title = "Ejection fraction - Biplane method",
         x = "Gender",
         y = "EF (%)") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("tomato", "steelblue"))

# EF box - both methods
group_frame %>%
  ggplot(aes(x = Method, y = EF, fill = Group)) +
    geom_boxplot(width = 0.75) +
    labs(title = "Ejection fraction by gender and method",
         y = "EF (%)",
         x = "Group") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.ticks.x = element_blank()) +
    scale_fill_manual(values = c("tomato", "steelblue","lightpink","lightblue"))


## CORRELATION ----
# FEMALES
RegFem <- lm(`Biplane Method` ~ `Simpson's Method`, data = corr_frame %>% filter(Gender == "F"))

corr_frame %>%
  filter(Gender == "Female") %>%
  ggplot(aes(EF_simpson, EF_biplane)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "black", fill="lightpink") +
    labs(x = "EF (%) - Simpson's", 
         y = "EF (%) - Biplane", 
         title = "Correlation between Ejection Fractions (Simpson's vs. Biplane) for Females") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(color = "none") +
    scale_color_manual(values = c("tomato"))

# MALES
RegMa <- lm(`Biplane Method` ~ `Simpson's Method`, data = corr_frame %>% filter(Gender == "M"))

corr_frame %>% 
  filter(Gender == "Male") %>%
  ggplot(aes(EF_simpson, EF_biplane)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "black", fill="lightblue") +
    labs(x = "EF (%) - Simpson's",
         y = "EF (%) - Biplane",
         title = "Correlation between Ejection Fractions (Simpson's vs. Biplane) for Males") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(color = "none") +
    scale_color_manual(values = c("steelblue"))


## SUMMARY ----
summary(subset(simpson_frame, Gender == "F"))
summary(subset(simpson_frame, Gender == "M"))
summary(subset(simpson_frame))

summary(subset(biplane_frame, Gender == "F"))
summary(subset(biplane_frame, Gender == "M"))
summary(subset(biplane_frame))


## EF paired t-test (old module) ----
# FEMALES
ggplot(long_EF[long_EF$Gender == "F", ], aes(x = Method, y = EF)) +
  geom_boxplot(aes(fill = Method), position = position_dodge(width = 0.75), alpha = 0.5) +
  geom_jitter(aes(color = Gender), position = position_jitterdodge(jitter.width = 0, dodge.width = 0), size = 2) +
  geom_line(aes(group = ID, color = Gender), position = position_dodge(width = 0), alpha = 0.5) +
  labs(x = "Method", 
       y = "EF (%)", 
       title = "Ejection fraction paired by method for females") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = "none") +
  scale_color_manual(values = c("tomato3")) +
  scale_fill_manual(values = c("lightpink", "tomato"))

# MALES
ggplot(long_EF[long_EF$Gender == "M", ], aes(x = Method, y = EF)) +
  geom_boxplot(aes(fill = Method), position = position_dodge(width = 0.75), alpha = 0.5) +
  geom_jitter(aes(color = Gender), position = position_jitterdodge(jitter.width = 0, dodge.width = 0), size = 2) +
  geom_line(aes(group = ID, color = Gender), position = position_dodge(width = 0), alpha = 0.5) +
  labs(x = "Method", y = "EF (%)", title = "Ejection Fraction paired by method for males") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = "none") +
  scale_color_manual(values = c("navy")) +
  scale_fill_manual(values = c("lightblue", "steelblue"))



