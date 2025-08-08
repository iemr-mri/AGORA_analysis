# Load packages
library(readxl)
library(tidyverse)
library(ggpubr)

MR_data <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw_biplane")
Echo_data <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size_ECHO_Haelin.xlsx", sheet = "max")

MR_frame <- tibble(
  ID = MR_data$`Animal ID`,
  L = MR_data$`Max 4CH-Length [mm]`,
  Gender = MR_data$Gender,
  Age = MR_data$`Age [months]`
)

Echo_frame <- tibble(
  ID = Echo_data$`Animal ID`,
  L = Echo_data$`LAD avg mm`,
  Gender = Echo_data$Gender,
  Age = Echo_data$Age
)

LA_dm <- inner_join(MR_frame, Echo_frame, by = c("ID", "Age", "Gender"), suffix = c("_MR", "_Echo"))

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
