##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'arrow',
                      'stringi')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
#################### parts
##########################

## function to clean give data
clean_give <- function(x) {
  filename = basename(x)
  read_parquet(x, show_col_types = F) %>%
    filter(sector %in% 'total') %>% 
    mutate(gas            = stri_split(filename, fixed='-')[[1]][2],
           emissions.year = as.numeric(str_remove_all(stri_split(filename, fixed='-')[[1]][4],'.parquet')))
}

##########################
##################### give
##########################

## path to directory
path.give = paste0('output/scghgs/')

## read in give and h&s data
data = 
  list.files(path.give, pattern = "*.parquet", full.names = T) %>%
  map_df(~clean_give(.))

## replace hfc43_10 with full name
data %<>%
  mutate(gas = case_when(gas == 'HFC43_10' ~ 'HFC4310mee', 
                         T ~ gas))

##########################
################## process
##########################

## linearly interpolate to annual values and export
data %>%
  select(-sector) %>% 
  rename(discount.rate = discount_rate) %>%
  group_by(gas, discount.rate) %>% 
  complete(emissions.year = seq(first(emissions.year), last(emissions.year))) %>%
  mutate(scghg = round(zoo::na.approx(scghg)),
         damage.function = 'MimiGIVE') %>% 
  ungroup %>% 
  relocate(damage.function,
           gas, 
           discount.rate,
           emissions.year) %>% 
  write_csv('output/scghg_annual.csv')

## end of script, have a great day.