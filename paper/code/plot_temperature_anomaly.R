##########################
#################  library
##########################

## Clear workspace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'stringi',
                      'arrow',
                      'ggplot2','ggrepel','ggpubr',
                      'showtext')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
#################### parts
##########################

## add fonts
font_add_google("Quattrocento Sans", "sans-serif")
showtext_auto()

## colorblind friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
colors = c("#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442", "#000000", "#0072B2", "#D55E00", "#999999") 

## function to read and join temperature data
read_temps <- function(x) {
  read_parquet(x) %>%
    filter(time > 2019) %>%
    mutate(gas   = str_split(str_split(x, pattern = '/')[[1]][5], pattern = '-')[[1]][1],
           model = str_split(x, pattern = '/')[[1]][7]) %>%
    rename(year  = time,
           value = 2,
           trial = trialnum) %>%
    pivot_wider(names_from  = model,
                values_from = value)
}

## function to read and join temperature data
read_temps_iwg <- function(x) {
  read_parquet(x) %>%
    filter(time > 2019) %>%
    mutate(gas   = str_split(str_split(x, pattern = '/')[[1]][5], pattern = '-')[[1]][1],
           scen  = substr(str_split(x, pattern = '/')[[1]][9], 1, 4),
           model = str_split(x, pattern = '/')[[1]][10],
           iam   = str_split(x, pattern = '/')[[1]][6]) %>%
    rename(year  = time,
           value = temp,
           trial = trialnum) %>%
    pivot_wider(names_from  = model,
                values_from = value)
}

## function to summarize temperature data
summarize_temps <- function(x) {
  x %>%
    group_by(year, gas) %>%
    summarise(mean = mean(value),
              med  = median(value),
              min  = min(value),
              max  = max(value),
              q01  = quantile(value, .01),
              q025 = quantile(value, .025),
              q05  = quantile(value, .05),
              q25  = quantile(value, .25),
              q75  = quantile(value, .75),
              q95  = quantile(value, .95),
              q975 = quantile(value, .975),
              q99  = quantile(value, .99),
              .groups = 'drop')
}

## function to summarize temperature data
summarize_temps_iwg <- function(x) {
  x %>%
    group_by(iam, year, gas) %>%
    summarise(mean = mean(value),
              med  = median(value),
              min  = min(value),
              max  = max(value),
              q01  = quantile(value, .01),
              q025 = quantile(value, .025),
              q05  = quantile(value, .05),
              q25  = quantile(value, .25),
              q75  = quantile(value, .75),
              q95  = quantile(value, .95),
              q975 = quantile(value, .975),
              q99  = quantile(value, .99),
              .groups = 'drop')
}

## get page discontinuities to drop from temperature anomalies the same as we do the scghgs
page.discontinuities = 
  list.files('../MimiIWG/output/page_discontinuities/', pattern = '.parquet', full.names = T) %>% 
  map_df(
    function(x){
      read_parquet(x) %>% 
        mutate(gas = stri_split(basename(x), fixed = '.')[[1]][1]) %>% 
        filter(discontinuity == T,
               emissions.year == 2030) %>% 
        select(gas, trial, emissions.year) %>% 
        distinct
    }
  )

##########################
####################  data
##########################

## these lines are commented out after they are ran for the first time to save time. 
## the result is saved below and read back in 
## paths to MimiGIVE subdirectories
paths_1 = grep(pattern = 'model_1', list.dirs('../MimiGIVE/output/save_list'), value = T)
files_1 = list.files(paths_1, pattern = 'global_temperature_norm.parquet', full.names = T)
paths_2 = grep(pattern = 'model_2', list.dirs('../MimiGIVE/output/save_list'), value = T)
files_2 = list.files(paths_2, pattern = 'global_temperature_norm.parquet', full.names = T)

## set canvas
temp_give = tibble()

## loop through MimiGIVE
for (i in 1:length(files_1)){
  temp_give =
    bind_rows(
      temp_give,
      left_join(
        read_temps(files_1[i]),
        read_temps(files_2[i]),
        by = c('year', 'trial', 'gas')
      ) %>%
        mutate(value = model_2 - model_1,
               gas = case_when(gas == 'HFC43_10' ~ 'HFC4310mee',
                               T ~ gas)) %>%
        summarize_temps
    )
}

## set canvas
temp_iwg = tibble()

## need to do this piecewise because the 5 USG scenarios make a large object
for (mod in c('*fund', '*dice', '*page')) {
  
  ## paths to MimiIWG subdirectories
  paths_iwg_1 = grep(pattern = glob2rx(paste0(mod, '*model_1')), list.dirs('../MimiIWG/output/save_list'), value = T)
  files_iwg_1 = list.files(paths_iwg_1, pattern = '.parquet', full.names = T)
  paths_iwg_2 = grep(pattern = glob2rx(paste0(mod, '*model_2')), list.dirs('../MimiIWG/output/save_list'), value = T)
  files_iwg_2 = list.files(paths_iwg_2, pattern = '.parquet', full.names = T)
  
  ## set canvas
  temp = tibble()
  
  ## make dictionary of chemistry for co2 in each model
  dice_co2_pulse_size = 1e3          ## in MtC, converted from a pulse of 1GtC 
  fund_co2_pulse_size = 10 * 12/44   ## in MtC, converted from a pulse of 10 MtCO2
  page_co2_pulse_size = 1e5 * 12/44  ## in MtC, converted from a pulse of 1e5 MtCO2

  ## loop through MimiIWG    
  for (i in 1:length(files_iwg_1)){
    temp =
      bind_rows(
        temp,
        left_join(
          read_temps_iwg(files_iwg_1[i]),
          read_temps_iwg(files_iwg_2[i]),
          by = c('iam', 'year', 'trial', 'gas', 'scen')
        ) %>%
          mutate(value = case_when(iam == 'dice' & gas == 'CO2' ~ (model_2 - model_1)/dice_co2_pulse_size,
                                   iam == 'fund' & gas == 'CO2' ~ (model_2 - model_1)/fund_co2_pulse_size, 
                                   iam == 'page' & gas == 'CO2' ~ (model_2 - model_1)/page_co2_pulse_size, 
                                   iam == 'dice' & gas != 'CO2' ~ (model_2 - model_1)/(1e6), ## (1 MtHFC -> 1 tHFC)
                                   iam == 'fund' & gas != 'CO2' ~ (model_2 - model_1)/(1e7), ## (10 MtHFC -> 1 tHFC)
                                   iam == 'page' & gas != 'CO2' ~ (model_2 - model_1)/1e6)) ## (1 MtHFC -> 1 tHFC)
      )
  }
  
  if (mod == '*page')
    temp %>%
    filter(!(gas %in% page.discontinuities$gas & trial %in% page.discontinuities$trial))
  
  ## reduce
  temp_iwg =
    bind_rows(
      temp_iwg,
      temp %>%
        summarize_temps_iwg %>%
        arrange(iam, gas, year)
    )
  
  ## clean
  rm(temp)
  gc()
  
}

## combine
data =
  bind_rows(
    temp_give %>%
      mutate(iam = 'MimiGIVE'),
    temp_iwg %>%
      mutate(iam = paste0('MimiIWG-', toupper(iam)))
  )

## add 2030 value for dice and page
data =
  bind_rows(
    data,
    tibble(year = rep(2029, 24),
           gas  = rep(c('HFC125', 'HFC134a', 'HFC143a', 'HFC152a', 'HFC227ea', 'HFC23', 'HFC236fa', 'HFC245fa', 'HFC32', 'HFC365mfc', 'HFC4310mee', 'CO2'), 2),
           mean = 0, med = 0,  min = 0,  max = 0,  q01 = 0,  q025 = 0, q05 = 0,  q25 = 0,  q75 = 0,  q95 = 0,  q975 = 0, q99 = 0,
           iam  = c(rep('MimiIWG-DICE', 12),
                    rep('MimiIWG-PAGE', 12)))
  )

## export to skip above
data %>%
  write_parquet('data/temperature_anomalies.parquet')

## import from lines above
data = 
  read_parquet('data/temperature_anomalies.parquet')

## arrange order
data %<>% 
  mutate(iam = fct_relevel(iam, 'MimiGIVE', 'MimiIWG-PAGE', 'MimiIWG-DICE', 'MimiIWG-FUND'),
         gas = fct_relevel(gas, 'HFC23', 'HFC32', 'HFC125', 'HFC134a', 'HFC143a', 'HFC152a', 'HFC227ea', 'HFC236fa', 'HFC245fa', 'HFC365mfc', 'HFC4310mee', 'CO2'))

## get maths in label for CO2
data$gas <- factor(data$gas,
                   labels = c('HFC23', 'HFC32', 'HFC125', 'HFC134a', 'HFC143a', 'HFC152a', 'HFC227ea', 'HFC236fa', 'HFC245fa', 'HFC365mfc', 'HFC4310mee', 'CO[2]'))

##########################
####################  plot
##########################

data %>% 
  ggplot() +
  facet_wrap(~gas, scales = 'free', ncol = 3, labeller = label_parsed) +
  geom_line(aes(x = year, y = mean, color = iam, linetype = iam), size = 0.6) +
  geom_ribbon(aes(x = year, ymin = q05, ymax = q95, fill = iam), color = NA, linetype = 'dotted', alpha = 0.1, show.legend = F) +
  annotation_custom(
    grob = grid::rectGrob(gp = grid::gpar(col = NA, fill = "white")),
    xmin = 2300
  ) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = c(2020, 2050, 2100, 2150, 2200, 2250, 2300), 
                     limits = c(2020, 2300),
                     labels = c(2020, '', 2100, '', 2200, '', 2300)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  labs(x         = 'Year',
       y         = 'Temperature Anomaly from a 1 Megatonne Pulse in 2030',
       color     = '',
       linetype  = '',
       group     = '',
       fill      = '') +
  theme_minimal() + 
  theme(legend.position = 'bottom',
        legend.title     = element_text(size = 14, color = 'grey20'),
        legend.text      = element_text(size = 16, color = 'grey20'),
        legend.key.size  = unit(1, 'cm'),
        legend.margin    = margin(0, 0, 0, 0),
        axis.title       = element_text(size = 12),
        axis.text.x      = element_text(size = 10, angle = -45, vjust = -0.5, hjust = 0.65),
        axis.text.y      = element_text(size = 10),
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black", linewidth = 1),
        strip.text       = element_text(color = 'grey20', size = 13, face = "bold"), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color='grey70', linetype="dotted"),
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(size = 11, hjust = 0.5),
        plot.title       = element_text(size = 14, hjust = 0.5),
        text             = element_text(family = "sans-serif", color = 'grey20')) +
  guides(color = guide_legend(nrow = 1)) 

## export
ggsave('output/figures/temp_anomaly.svg', width = 9, height = 12)

## end of script, have a great day.
