##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'readxl')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
##################### data
##########################

data = 
  bind_rows(
    read_csv('../MimiGIVE/output/scghg_annual.csv', show_col_types = F),
    read_csv('../MimiIWG/output/scghg_annual.csv', show_col_types = F)
  ) %>% 
  filter(gas != 'CO2') %>% 
  mutate(scghg         = scghg/1e3,
         discount.rate = case_when(!(grepl("Ramsey", discount.rate)) ~ paste0(discount.rate, ' Constant Discount Rate'),
                                   T ~ discount.rate)) %>% 
  rename(model = damage.function,
         year  = emissions.year)

##########################
##################  tables
##########################

## decadal
data %>% 
  filter(year %in% seq(2020, 2100, 10)) %>% 
  pivot_wider(names_from  = gas, 
              values_from = scghg) %>% 
  mutate_if(is.numeric, round) %>% 
  relocate(model, year, HFC23, HFC32, HFC125, HFC134a, HFC143a, HFC152a, HFC227ea, HFC236fa, HFC245fa, HFC365mfc, HFC4310mee) %>% 
  write_csv('output/tables/schfcs_decadal.csv')

## annual
data %>% 
  pivot_wider(names_from  = gas, 
              values_from = scghg) %>% 
  mutate_if(is.numeric, round) %>% 
  relocate(model, year, HFC23, HFC32, HFC125, HFC134a, HFC143a, HFC152a, HFC227ea, HFC236fa, HFC245fa, HFC365mfc, HFC4310mee) %>%  
  write_csv('output/tables/schfcs_annual.csv')

## end of script. have a great day!