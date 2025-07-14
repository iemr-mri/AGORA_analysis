# Load packages
library(readxl)
library(tidyr)
library(dplyr)
library(ggpubr)

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

# LA diameter - MRI vs Echo
ggscatter(LA_dm, x = "L_MR", y = "L_Echo",
          add = "reg.line", conf.int = TRUE,
          xlab = "MRI [mm]", ylab = "Echo [mm]")

# LA diameter - MRI vs Echo (males)
ggscatter(LA_dm %>% filter(Gender == 'M'), x = "L_MR", y = "L_Echo",
          add = "reg.line", conf.int = TRUE,
          xlab = "MRI [mm]", ylab = "Echo [mm]")

# LA diameter - MRI vs Echo (females)
ggscatter(LA_dm %>% filter(Gender == 'F'), x = "L_MR", y = "L_Echo",
          add = "reg.line", conf.int = TRUE,
          xlab = "MRI [mm]", ylab = "Echo [mm]")