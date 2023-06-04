##########################
#################  library
##########################

## clear work space
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

## function to compress total forcing
comp_total_forcing <- function(x) {
  write_parquet(fread(x)[time >= 2000], file.path(path, 'total_forcing_F.parquet'))
}

## function to compress total forcing scale
comp_total_forcing_scale <- function(x) {
  write_parquet(fread(x), file.path(path, 'total_forcing_scale.parquet'))
}

## function to compress temperatures
comp_temp <- function(x) {
  write_parquet(fread(x)[time >= 1900], file.path(path, 'global_temperature_norm.parquet'))
}

##########################
################## process
##########################

## paths to subdirectories
paths = grep('model', list.dirs('output/save_list'), value = T)

for (path in paths){
  
  files = list.files(path, pattern = '.csv', full.names = T)
  
  for (file in files){

    if (basename(file) == 'total_forcing_F_other_ghg.csv') {
      comp_total_forcing(file)
    }
    
    if (basename(file) == 'total_forcing_scale_other_ghg.csv') {
      comp_total_forcing_scale(file)
    }
    
    if (basename(file) == 'TempNorm_1850to1900_global_temperature_norm.csv') {
      comp_temp(file)
    }
  }
}

## end of script, have a great day. 