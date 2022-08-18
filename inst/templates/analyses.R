library(tidyverse)
library(psythelper)
library(benelib)

theme_set(theme_bene())
use_personal_theme()




# Data Import -------------------------------------------------------------
clean_data()
therapy_data <- read_rds("02 Data/therapie-verlauf.rds")




# Analyses ----------------------------------------------------------------
evaluate_instruments(therapy_data)
