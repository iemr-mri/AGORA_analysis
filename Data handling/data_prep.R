# Load packages
library(readxl)
library(tidyverse)


## LOAD AND READ EXCEL FILES ----
# Data - Henrik Elias
LA_simpson_HE <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw")
LA_biplane_HE <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Henrik.xlsx", sheet = "raw_biplane")

#Data - Haelin
LA_simpson_Hae <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Haelin.xlsx", sheet = "raw")
LA_biplane_Hae <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size measurements_Haelin.xlsx", sheet = "raw_biplane")
Echo_data <- read_excel("R:\\Projects\\AGORA\\LA measurements\\LA size_ECHO_Haelin.xlsx", sheet = "max")

# Convert F/M into Female/Male for aesthetics
# Gender factoring function
change_gender <- function(df) {
  df %>%
    mutate(Gender = recode(Gender,
                           `F` = "Female",
                           `M` = "Male")) %>%
    mutate(Gender = factor(Gender, levels = c("Female", "Male")))
}

LA_simpson_HE <- change_gender(LA_simpson_HE)

LA_biplane_HE <- change_gender(LA_biplane_HE)

LA_simpson_Hae <- change_gender(LA_simpson_Hae)

LA_biplane_Hae <- change_gender(LA_biplane_Hae)

Echo_data <- change_gender(Echo_data)

# save excel sheets
save(LA_simpson_HE, LA_biplane_HE, LA_simpson_Hae, LA_biplane_Hae, Echo_data, file = "Data handling/excel_sheets.Rdata")
save.image()

## LOAD SAVED EXCEL SHEETS ----
load("Data handling/excel_sheets.Rdata")


## SIMPSON - HE ----
simpson_HE <- tibble(
  ID       = LA_simpson_HE$`Animal ID`,
  Cohort   = LA_simpson_HE$Cohort,
  Gender   = LA_simpson_HE$Gender,
  Max_V    = LA_simpson_HE$`Max volume [ml]`,
  Min_V    = LA_simpson_HE$`Min volume [ml]`,
  SV       = LA_simpson_HE$`SV [ml]`,
  EF       = LA_simpson_HE$EF,
  Method   = "Simpson",
  Operator = "Henrik",
  Age      = LA_simpson_HE$`Age [months]`,
  MI_week  = LA_simpson_HE$MI_Week,
  Max_D    = LA_simpson_HE$`Max 4CH-Length [mm]`,
  Min_D    = LA_simpson_HE$`Min 4CH-Length [mm]`
)

# Drop rows containing no max volume
simpson_HE <- simpson_HE %>%
  drop_na(Max_V)

# Extract the cohort prefix for grouping aging vs MI rats
simpson_HE$Group <- factor(sub("^(AG|MI).*", "\\1", simpson_HE$Cohort),
                           levels = c("AG", "MI"),
                           labels = c("Aging", "MI"))

## SIMPSON - HAE ----
simpson_Hae <- tibble(
  ID       = LA_simpson_Hae$`Animal ID`,
  Cohort   = LA_simpson_Hae$Cohort,
  Gender   = LA_simpson_Hae$Gender,
  Max_V    = LA_simpson_Hae$`Max Simpson's method [ml]`/1000, # divide by 1000 for correct unit
  Min_V    = LA_simpson_Hae$`Min Simpson's method [ml]`/1000, # divide by 1000 for correct unit
  SV       = LA_simpson_Hae$`SV [ml]`/1000, # divide by 1000 for correct unit
  EF       = LA_simpson_Hae$EF*100, # times by 100 for correct unit
  Method   = "Simpson",
  Operator = "Haelin",
  Age      = LA_simpson_Hae$`Age [months]`,
  MI_week  = LA_simpson_Hae$MI_week
)

# Drop rows containing no max volume
simpson_Hae <- simpson_Hae %>%
  drop_na(Max_V)

# Extract the cohort prefix for grouping aging vs MI rats
simpson_Hae$Group <- factor(sub("^(AG|MI).*", "\\1", simpson_Hae$Cohort),
                           levels = c("AG", "MI"),
                           labels = c("Aging", "MI"))
## BIPLANE - HE ----
biplane_HE <- tibble(
  ID        = LA_biplane_HE$`Animal ID`,
  Cohort    = LA_biplane_HE$Cohort,
  Gender    = LA_biplane_HE$Gender,
  Max_V     = LA_biplane_HE$`Max LA volume [mL] (shortest L)`,
  Min_V     = LA_biplane_HE$`Min LA volume [mL] (shortest L)`,
  EF        = LA_biplane_HE$EF,
  Method    = "Biplane",
  Operator  = "Henrik",
  Age       = LA_biplane_HE$`Age [months]`,
  MI_week   = LA_biplane_HE$MI_week,
  Max_d     = LA_biplane_HE$`Max 4CH-Length [mm]`,
  Min_d     = LA_biplane_HE$`Min 4CH-Length [mm]`
)

# If diameter is measured, but not area, the volume parameters can be 0 instead of NA
# We do not drop NA values however since some subjects may have diameter and not volume recordings and vice versa
biplane_HE <- biplane_HE %>%
  mutate(across(c('Max_V','Min_V','EF'), ~na_if(.,0)))


# Extract the cohort prefix for grouping aging vs MI rats
biplane_HE$Group <- factor(sub("^(AG|MI).*", "\\1", biplane_HE$Cohort),
                           levels = c("AG", "MI"),
                           labels = c("Aging", "MI"))

## BIPLANE - HAE ----
biplane_Hae <- tibble(
  ID        = LA_biplane_Hae$`Animal ID`,
  Cohort    = LA_biplane_Hae$Cohort,
  Gender    = LA_biplane_Hae$Gender,
  Max_V     = LA_biplane_Hae$`Max LA volume [mL] (shortest L)`,
  Min_V     = LA_biplane_Hae$`Min LA volume [mL] (shortest L)`,
  Method    = "Biplane",
  Operator  = "Haelin",
  Age       = LA_biplane_Hae$`Age [months]`,
  Max_d     = LA_biplane_Hae$`Max 4CH-Length [mm]`,
  Min_d     = LA_biplane_Hae$`Min 4CH-Length [mm]`,
  MI_week   = NA
)

# If diameter is measured, but not area, the volume parameters can be 0 instead of NA
# We do not drop NA values however since some subjects may have diameter and not volume recordings and vice versa
biplane_Hae <- biplane_Hae %>%
  mutate(across(c('Max_V','Min_V'), ~na_if(.,0)))

# Calculate the EF column
biplane_Hae$EF <- (LA_biplane_Hae$`Max LA volume [mL] (shortest L)`-LA_biplane_Hae$`Min LA volume [mL] (shortest L)`)/LA_biplane_Hae$`Max LA volume [mL] (shortest L)`*100


# Extract the cohort prefix for grouping aging vs MI rats
biplane_Hae$Group <- factor(sub("^(AG|MI).*", "\\1", biplane_Hae$Cohort),
                           levels = c("AG", "MI"),
                           labels = c("Aging", "MI"))

## COMBINED FRAMES ----
join_list = c('ID', "Age", "Gender", "Cohort", "Group", "MI_week")

# Wide tibbles to compare data with different methods (Simpson and biplane)
opA_wide <- inner_join(simpson_HE, biplane_HE, by = c(join_list, "Operator"), suffix = c("_simpson", "_biplane"))
opB_wide <- inner_join(simpson_Hae, biplane_Hae, by = c(join_list, "Operator"), suffix = c("_simpson", "_biplane"))
method_wide <- bind_rows(opA_wide,opB_wide)

# Wide tibbles combining data from two observers
simpson_wide <- inner_join(simpson_HE, simpson_Hae, by = c(join_list, "Method"), suffix = c("_HE", "_Hae"))
biplane_wide <- inner_join(biplane_HE, biplane_Hae, by = c(join_list, "Method"), suffix = c("_HE", "_Hae"))
operator_wide <- bind_rows(simpson_wide, biplane_wide)

## LA DIAMETER FRAME ----
Echo_frame <- tibble(
  ID     = Echo_data$`Animal ID`,
  Max_D  = Echo_data$`LAD avg mm`,
  Gender = Echo_data$Gender,
  Age    = Echo_data$Age,
  Cohort = Echo_data$Cohort
)

# Extract the cohort prefix for grouping aging and MI rats
Echo_frame$Group <- factor(sub("^(AG|MI).*", "\\1", Echo_frame$Cohort),
                           levels = c("AG", "MI"),
                           labels = c("Aging", "MI"))

# Remove rows without data
Echo_frame <- Echo_frame %>%
  drop_na(Max_D)

LA_dm <-  inner_join(Echo_frame, simpson_HE, by = c('ID', "Age", "Gender", "Cohort", "Group"), suffix = c("_MR", "_Echo"))

## REMOVE UNUSED TIBBLES
rm(LA_biplane_HE, LA_biplane_Hae, LA_simpson_HE, LA_simpson_Hae, Echo_frame, Echo_data, opA_wide, opB_wide, simpson_wide, biplane_wide)

## SAVE DATA ----
save(simpson_HE, biplane_HE, operator_wide, method_wide, LA_dm, file = "Data handling/LA_data.Rdata")
save.image()
