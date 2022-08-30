library(tidyverse)
library(psythelper)
library(benelib)

theme_set(theme_bene())
use_personal_theme()




# Data Import -------------------------------------------------------------
clean_data()




# Analyses ----------------------------------------------------------------
evaluated_instruments <- evaluate_instruments()
create_table(evaluated_instruments)
evaluated_instruments
