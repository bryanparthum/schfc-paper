##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
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
colors = c("#000000", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

## function to read and join temperature data
read_forcing <- function(x) {
  read_parquet(x) %>%
    filter(time > 2019,
           other_ghg == str_split(str_split(x, pattern = '/')[[1]][5], pattern = '-')[[1]][1]) %>%
    rename(gas   = other_ghg,
           trial = trialnum,
           year  = time) %>%
    mutate(model = str_split(x, pattern = '/')[[1]][7]) %>%
    pivot_wider(names_from  = model,
                values_from = F_other_ghg)
}

## function to read and join temperature data
read_scale <- function(x) {
  read_parquet(x) %>%
    rename(gas   = other_ghg,
           trial = trialnum)
}

## function to summarize data
summarize_forcings <- function(x) {
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

##########################
####################  data
##########################

## these lines are commented out after they are initially ran. the result is saved and read in below
# ## paths to MimiGIVE subdirectories for random scale, one file includes all gases so we only need one of these
# scale.path = grep('HFC23-2030-n10000/results/model_1', list.dirs('../MimiGIVE/output/save_list'), value = T)
# scale.file = list.files(scale.path, pattern = 'total_forcing_scale.parquet', full.names = T)
# 
# ## paths to MimiGIVE subdirectories
# paths_1 = grep(pattern = glob2rx('*HFC*model_1*'), list.dirs('../MimiGIVE/output/save_list'), value = T) 
# files_1 = list.files(paths_1, pattern = 'total_forcing_F.parquet', full.names = T)
# paths_2 = grep(pattern = glob2rx('*HFC*model_2*'), list.dirs('../MimiGIVE/output/save_list'), value = T)
# files_2 = list.files(paths_2, pattern = 'total_forcing_F.parquet', full.names = T)
# 
# ## set canvas
# give = tibble()
# 
# ## loop through gas and MimiGIVE perturbations
# for (i in 1:length(files_1)){
#   give =
#     bind_rows(
#       give,
#       left_join(
#         left_join(
#           read_forcing(files_1[i]),
#           read_forcing(files_2[i]),
#           by = c('year', 'trial', 'gas')
#         ),
#         read_scale(scale.file),
#         by = c('gas', 'trial')
#       ) %>%
#         mutate(value = (model_2 - model_1) * scale_other_ghg,
#                gas   = case_when(gas == 'HFC43_10' ~ 'HFC4310mee',
#                                  T ~ gas)) %>%
#         summarize_forcings
#     )
# }
# 
# ## export to skip above
# give %>% 
#   write_parquet('../MimiGIVE/output/save_list/radiative_forcings_from_pulse.parquet')

## read from above
give = 
  read_parquet('../MimiGIVE/output/save_list/radiative_forcings_from_pulse.parquet') %>% 
  mutate(across(3:14, ~ .x * 1e6)) ## 1 metric ton pulse to 1 megaton to match MimiIWG


## get MimiIWG forcing
iwg = 
  bind_rows(tibble(year = c(rep(2020, 11), rep(2029, 11)),
                   gas  = rep(c('HFC23', 'HFC32', 'HFC125', 'HFC134a', 'HFC143a', 'HFC152a', 'HFC227ea', 'HFC236fa', 'HFC245fa', 'HFC365mfc', 'HFC4310mee'), 2),
                   mean = 0, med = 0,  min = 0,  max = 0,  q01 = 0,  q025 = 0, q05 = 0,  q25 = 0,  q75 = 0,  q95 = 0,  q975 = 0, q99 = 0),
            read_csv('../MimiIWG/data/ghg_radiative_forcing_perturbation.csv') %>% 
              rename(year = t,
                     gas  = ghg,
                     mean = rf) %>% 
              mutate(year = year + 2029) %>% 
              filter(year < 2301) %>% 
              select(-c)
  ) %>% 
  arrange(gas, year)

## combine
data =
  bind_rows(
    give %>%
      mutate(iam = 'MimiGIVE'),
    iwg %>%
      mutate(iam = 'MimiIWG')
  )

## arrange order
data %<>% 
  mutate(iam = fct_relevel(iam, 'MimiGIVE', 'MimiIWG'),
         gas = fct_relevel(gas,'HFC23', 'HFC32', 'HFC125', 'HFC134a', 'HFC143a', 'HFC152a', 'HFC227ea', 'HFC236fa', 'HFC245fa', 'HFC365mfc', 'HFC4310mee'))

##########################
####################  plot
##########################

data %>%
  filter(year > 2024) %>% 
  mutate(year = year - 2024) %>% 
  ggplot() +
  facet_wrap(~gas, scales = 'free', ncol = 3) +
  geom_line(aes(x        = year, 
                y        = mean, 
                color    = iam, 
                linetype = iam), 
            linewidth = 1) +
  geom_ribbon(aes(x    = year, 
                  ymin = q05, 
                  ymax = q95, 
                  fill = iam), 
              color       = NA, 
              linetype    = 'dotted', 
              alpha       = 0.2, 
              show.legend = F) +
  annotation_custom(
    grob = grid::rectGrob(gp = grid::gpar(col = NA, fill = "white")),
    xmin = 2300
  ) +
  scale_color_manual(values = c(colors[4], colors[3])) +
  scale_fill_manual(values = c(colors[4], colors[3])) +
  scale_x_continuous(trans  = "log10",
                     limits = c(1, 400),
                     expand = c(0, 0),
                     breaks = c(1, 10, 100, 300)) +
  labs(x         = 'Years Since Emissions',
       y         = 'Excess Radiative Forcing from a 1 Megatonne Pulse in 2030',
       color     = '',
       linetype  = '',
       group     = '',
       fill      = '') +
  theme_minimal() +
  theme(legend.position = c(0.93, 0.07),
        legend.justification = c(1, 0),
        legend.title     = element_text(size = 14, color='grey20'),
        legend.text      = element_text(size = 16, color='grey20'),
        legend.key.size  = unit(1, 'cm'),
        legend.margin    = margin(0, 0, 0, 0),
        axis.title       = element_text(size = 12),
        axis.text        = element_text(size = 12),
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black", size = 1),
        strip.text       = element_text(color = 'grey20', size = 13, face = "bold"), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey70', linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(size = 12, hjust = 0.5),
        plot.title       = element_text(size = 12, hjust = 0.5),
        plot.margin      = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), 'cm'),
        text             = element_text(family = "sans-serif", color = 'grey20'))

## export
ggsave('output/figures/Extended Data Figure 3.pdf', 
       width  = 180, 
       height = 215,
       units  = 'mm')

## end of script, have a great day.