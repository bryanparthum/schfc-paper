##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse','data.table','arrow',
                      'stringi')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
###################  parts
##########################

## function to compress dice
comp_dice_temp <- function(x) {
  write_parquet(fread(x, fill = T)[between(time, 2000, 2300)] %>% 
                  rename(temp = TATM), 
                file.path(path, 'climatedynamics_TATM.parquet'))
}

## function to compress fund
comp_fund_temp <- function(x) {
  write_parquet(fread(x, fill = T)[between(time, 2000, 2300)], 
                file.path(path, 'climatedynamics_temp.parquet'))
}

## function to compress page
comp_page_temp <- function(x) {
  write_parquet(fread(x, fill = T)[between(time, 2000, 2300)] %>% 
                  rename(temp = rt_g_globaltemperature), 
                file.path(path, 'climatetemperature_global.parquet'))
}

##########################
################## process
##########################

## paths to subdirectories
paths = grep('/model', list.dirs('output/save_list'), value = T)

for (path in paths){
  
  files = list.files(path, pattern = '.csv', full.names = T)
  
  for (file in files){
    
    if (basename(file) == 'climatedynamics_TATM.csv') {
      comp_dice_temp(file)
    }
    
    if (basename(file) == 'climatedynamics_temp.csv') {
      comp_fund_temp(file)
    }
    
    if (basename(file) == 'ClimateTemperature_rt_g_globaltemperature.csv') {
      comp_page_temp(file)
    }
  }
}

## end of script, have a great day. 