## --------------------------------------------------------------------------------
## description: calculate climate benefits from kigali using purohit data
## --------------------------------------------------------------------------------

rm(list=ls())

list.of.packages <- c("dplyr", "tidyverse", "readxl", "data.table", "stringr", "ggplot2", 
                        "magrittr", "showtext", "FreqProf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

## --------------------------------------------------------------------------------
## explore data
## --------------------------------------------------------------------------------

# MimiIWG scghgs
scghgs_iwg <- fread("../MimiIWG/output/scghg_annual.csv", sep = ",")

# MimiGIVE scghgs
scghgs_give <- fread("../MimiGIVE/output/scghg_annual.csv", sep = ",")

# combine
scghgs <- rbind(scghgs_iwg, scghgs_give)

# calculate min and max % difference for 2023 schfcs
diff <- merge(scghgs_give %>% filter(emissions.year == 2023), 
                scghgs_iwg %>% filter(emissions.year == 2023), by = c("gas", "emissions.year")) %>%
                mutate(difference = (scghg.x - scghg.y)/scghg.x * 100)
max(diff$difference)
min(diff$difference)
