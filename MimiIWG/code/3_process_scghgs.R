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
###################  parts
##########################

## function to prepare dice scghgs
prepare_dice <- function(x) {
  filename = basename(x)
  scenario = stri_split(filename, fixed = ' ')[[1]][1]
  read_csv(x, col_types = cols()) %>%
    mutate(scenario      = case_when(scenario == "USG1" ~ "IMAGE",
                                     scenario == "USG2" ~ "MERGE Optimistic",
                                     scenario == "USG3" ~ "MESSAGE",
                                     scenario == "USG4" ~ "MiniCAM Base",
                                     scenario == "USG5" ~ "5th Scenario"),
           discount.rate = paste0(as.numeric(str_remove(stri_split(filename, fixed=' ')[[1]][2], '.csv'))*100, '%'),
           trial = seq(n())) %>% 
    pivot_longer(cols = 1:9, names_to = 'emissions.year', values_to = 'scghg') %>% 
    mutate(emissions.year = as.numeric(emissions.year),
           model          = 'DICE 2010')
}

## function to prepare fund scghgs
prepare_fund <- function(x) {
  filename = basename(x)
  scenario = stri_split(filename, fixed = ' ')[[1]][1]
  read_csv(x, col_types = cols()) %>%
    mutate(scenario      = case_when(scenario == "USG1" ~ "IMAGE",
                                     scenario == "USG2" ~ "MERGE Optimistic",
                                     scenario == "USG3" ~ "MESSAGE",
                                     scenario == "USG4" ~ "MiniCAM Base",
                                     scenario == "USG5" ~ "5th Scenario"),
           discount.rate = paste0(as.numeric(str_remove(stri_split(filename, fixed=' ')[[1]][2], '.csv'))*100, '%'),
           trial = seq(n())) %>% 
    pivot_longer(cols = 1:9, names_to = 'emissions.year', values_to = 'scghg') %>% 
    mutate(emissions.year = as.numeric(emissions.year),
           model          = 'FUND 3.8')
}

## function to prepare page scghgs
prepare_page <- function(x) {
  filename = basename(x)
  scenario = stri_split(filename, fixed = ' ')[[1]][1]
  read_csv(x, col_types = cols()) %>%
    mutate(scenario      = case_when(scenario == "USG1" ~ "IMAGE",
                                     scenario == "USG2" ~ "MERGE Optimistic",
                                     scenario == "USG3" ~ "MESSAGE",
                                     scenario == "USG4" ~ "MiniCAM Base",
                                     scenario == "USG5" ~ "5th Scenario"),
           discount.rate = paste0(as.numeric(str_remove(stri_split(filename, fixed=' ')[[1]][2], '.csv'))*100, '%'),
           trial = seq(n())) %>% 
    pivot_longer(cols = 1:7, names_to = 'emissions.year', values_to = 'scghg') %>% 
    mutate(emissions.year = as.numeric(emissions.year),
           model          = 'PAGE 2009') 
}

## scghgs are in 2007 USD from MimiIWG, this deflator brings them to 2020 USD. accessed sept 28th 2022: https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=3&isuri=1&select_all_years=0&nipa_table_list=13&series=a&first_year=2005&last_year=2020&scale=-99&categories=survey&thetable=
pricelevel_2007_to_2020 =  113.648/92.642

## list of gases
gas_list <- c('HFC23', 'HFC32', 'HFC125', 'HFC134a', 'HFC143a', 'HFC152a', 'HFC227ea', 'HFC236fa', 'HFC245fa', 'HFC4310mee', 'HFC365mfc', 'CO2')

##########################
#################  process
##########################

## specify output directory
output.path = 'output/scghgs/full_distributions'

## create a new sub directory if the path doesn't exist 
if (!file.exists(output.path)){
  dir.create(output.path)
}

## set up data
data = tibble()

for (gas in gas_list) {
  
  print(gas)
  
  ##########################
  ####################  dice
  ##########################
  
  print('dice')
  
  ## path to directory
  path.dice = paste0(list.dirs(paste0('output/scghgs/', gas, '/dice/'), recursive = F), '/SC-', gas)
  
  ## read data
  dice = 
    list.files(path.dice, pattern = "USG*", full.names = T) %>%
    map_df(~prepare_dice(.))
  
  ## export full distribution
  dice %>%
    select(-model) %>% 
    mutate(scghg = scghg * pricelevel_2007_to_2020) %>% 
    write_parquet(paste0('output/scghgs/full_distributions/sc-', gas, '_dice.parquet'))
  
  ## summarize for mean values
  dice %<>% 
    group_by(emissions.year, model, discount.rate) %>% 
    summarise(scghg = mean(scghg, na.rm = T) * pricelevel_2007_to_2020, .groups = 'drop') %>% 
    ungroup
  
  ##########################
  ####################  fund
  ##########################
  
  print('fund')
  
  ## path to directory
  path.fund = paste0(list.dirs(paste0('output/scghgs/', gas, '/fund/'), recursive = F), '/SC-', gas)
  
  ## read data
  fund = 
    list.files(path.fund, pattern = "USG*", full.names = T) %>%
    map_df(~prepare_fund(.)) 
  
  ## export full distribution
  fund %>%
    select(-model) %>% 
    mutate(scghg = scghg * pricelevel_2007_to_2020) %>% 
    write_parquet(paste0('output/scghgs/full_distributions/sc-', gas, '_fund.parquet'))
  
  ## summarize for mean values
  fund %<>% 
    group_by(emissions.year, model, discount.rate) %>% 
    summarise(scghg = mean(scghg, na.rm = T) * pricelevel_2007_to_2020, .groups = 'drop') %>% 
    ungroup
  
  ##########################
  ####################  page
  ##########################
  
  print('page')
  
  ## path to directory
  path.page = paste0(list.dirs(paste0('output/scghgs/', gas, '/page/'), recursive = F), '/SC-', gas)
  
  ## read page discontinuities
  page.discontinuities = read_parquet(paste0('output/page_discontinuities/', gas, '.parquet')) %>% 
    filter(discontinuity == T) %>% 
    select(trial, emissions.year) %>% 
    distinct
  
  ## read data
  page = 
    list.files(path.page, pattern = "USG*", full.names = T) %>%
    map_df(~prepare_page(.)) %>%
    filter(!(trial %in% page.discontinuities$trial & emissions.year %in% page.discontinuities$emissions.year))
  
  ## export full distribution
  page %>%
    select(-model) %>% 
    group_by(scenario, trial, discount.rate) %>% 
    complete(emissions.year = seq(2020, 2100, 10)) %>% 
    mutate(scghg = round(zoo::na.approx(scghg, rule = 2)), ## extrapolate to 2100 with the 2080 value
           scghg = scghg * pricelevel_2007_to_2020) %>% 
    ungroup %>% 
    as_tibble %>% 
    write_parquet(paste0('output/scghgs/full_distributions/sc-', gas, '_page.parquet'))
  
  ## summarize for mean values
  page %<>% 
    group_by(emissions.year, model, discount.rate) %>% 
    summarise(scghg = mean(scghg, na.rm = T) * pricelevel_2007_to_2020, .groups = 'drop') %>% 
    ungroup %>% 
    group_by(model, discount.rate) %>% 
    complete(emissions.year = seq(2020, 2100, 10)) %>%
    mutate(scghg = zoo::na.approx(scghg, rule = 2)) %>% ## extrapolate to 2100 with the 2080 value
    ungroup
  
  ##########################
  ################## combine
  ##########################
  
  ## combine
  data = 
    bind_rows(data, 
              bind_rows(dice, fund, page) %>%
                group_by(emissions.year, discount.rate) %>% 
                summarise(scghg = mean(scghg, na.rm = T), .groups = 'drop') %>% 
                mutate(gas = paste0(gas)))
  
  ## clean house
  rm(dice, fund, page, page.discontinuities)
  gc()
  
}

## linearly interpolate to annual values and export
data %>%
  group_by(gas, discount.rate) %>% 
  complete(emissions.year = seq(first(emissions.year), last(emissions.year))) %>%
  mutate(scghg = round(zoo::na.approx(scghg)),
         damage.function = 'MimiIWG') %>% 
  ungroup %>% 
  relocate(damage.function,
           gas, 
           discount.rate,
           emissions.year) %>% 
  write_csv('output/scghg_annual.csv')

## end of script, have a great day.
