## prepares PAGE discontinuities for merge with PAGE schfcs

##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse','stringi','arrow')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
#################### parts
##########################

## function to prepare PAGE discontinuities
prepare_page_discontinuities <- function(x) {
  filename = basename(x)
  scenario = stri_split(filename, fixed = ' ')[[1]][1]
  read_csv(x, col_types = cols()) %>%
    mutate(scenario      = case_when(scenario=="USG1" ~ "IMAGE",
                                     scenario=="USG2" ~ "MERGE Optimistic",
                                     scenario=="USG3" ~ "MESSAGE",
                                     scenario=="USG4" ~ "MiniCAM Base",
                                     scenario=="USG5" ~ "5th Scenario"),
           discount.rate = paste0(as.numeric(str_remove(stri_split(filename, fixed=' ')[[1]][2], '.csv'))*100, '%'),
           trial = seq(n())) %>% 
    pivot_longer(cols = 1:7, names_to = 'emissions.year', values_to = 'discontinuity') %>% 
    mutate(emissions.year = as.numeric(emissions.year),
           model          = 'PAGE 2009')
  
}

## List of gases
gas_list <- c('HFC23','HFC32','HFC125','HFC134a','HFC143a','HFC152a','HFC227ea','HFC236fa','HFC245fa','HFC43_10','HFC365mfc','CO2')

##########################
#################  process
##########################

## specify output directory
output.path = 'output/page_discontinuities'

## create a new sub directory if the path doesn't exist 
if (!file.exists(output.path)){
  dir.create(output.path)
}

for (gas in gas_list) {
  
  ## path to directory
  path = paste0(list.dirs(paste0('output/scghgs/', gas, '/page/'), recursive = F), '/discontinuity_mismatch') 
  
  ## read discontinuities for each hfc
  data =
    list.files(path, pattern = "USG*", full.names = T) %>%
    map_df(~prepare_page_discontinuities(.))
  
  ## export
  data %>% 
    write_parquet(paste0(output.path, '/', gas, '.parquet'))
  
}

## end of script, have a great day.