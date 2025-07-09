# Load packages
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

MR_data <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw_biplane")
Echo_data <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw_echo")

MR_frame <- data.frame(
  ID = MR_data$`Animal ID`,
  L = MR_data$`Max 4CH-Length [mm]`,
  Gender = MR_data$Gender
)

Echo_frame <- data.frame(
  ID = Echo_data$`Animal ID`,
  L = Echo_data$`LAD avg mm`
)


LA_dm <- merge(MR_frame, Echo_frame, by = 'ID', suffixes = c("_MR", "_Echo"))

ggplot(LA_dm, aes(x = L_MR, y = L_Echo)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "azure4") +
  labs(x = "MRI [mm]",
       y = "Echo [mm]",
       title = "LA diameter measurements") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(LA_dm %>% filter(Gender == 'M'), aes(x = L_MR, y = L_Echo)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "lightblue") +
  labs(x = "MRI [mm]",
       y = "Echo [mm]",
       title = "LA diameter measurements - Males") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(LA_dm %>% filter(Gender == 'F'), aes(x = L_MR, y = L_Echo)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "lightpink") +
  labs(x = "MRI [mm]",
       y = "Echo [mm]",
       title = "LA diameter measurements - Females") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
