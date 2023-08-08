##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'stringi',
                      'arrow','readxl',
                      'ggplot2','ggpubr','scales',
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

# ## mimigive results are in 2005 USD, this is the price deflator to bring the results to 2020 USD. accessed 09/13/2022. source: https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=3&isuri=1&select_all_years=0&nipa_table_list=13&series=a&first_year=2005&last_year=2020&scale=-99&categories=survey&thetable=
# pricelevel_2005_to_2020 = 113.648/87.504

# ## function to prepare mimigive scco2
# read_give_co2 <- function(x) {
#   filename = basename(x)
#   read_parquet(x) %>%
#     rename(discount.rate = discount_rate,
#            scco2         = scghg) %>%
#     filter(discount.rate == '2.0% Ramsey',
#            sector == 'total',
#            region == 'globe') %>%
#     select(scco2) %>% 
#     summarize_all(mean) %>% 
#     mutate(emissions.year = as.numeric(stri_split(filename, fixed = '-')[[1]][5]),
#            scco2 = scco2 * pricelevel_2005_to_2020) 
# }

##########################
##################### data
##########################

## read scghgs
data = 
  left_join(
    bind_rows(
      read_csv('../MimiIWG/output/scghg_annual.csv', show_col_types = FALSE) %>% 
        filter(gas           != 'CO2',
               discount.rate == '3%'),
      read_csv('../MimiGIVE/output/scghg_annual.csv', show_col_types = FALSE) %>% 
        filter(gas           != 'CO2',
               discount.rate == '2.0% Ramsey')
    ) %>% 
      rename(schfc = scghg) %>% 
      rename(model = damage.function),
    bind_rows(
      read_csv('../MimiIWG/output/scghg_annual.csv', show_col_types = FALSE) %>% 
        filter(gas           == 'CO2',
               discount.rate == '3%'),
      read_csv('../MimiGIVE/output/scghg_annual.csv', show_col_types = FALSE) %>% 
        filter(gas           == 'CO2',
               discount.rate == '2.0% Ramsey')
    ) %>% 
      rename(scco2 = scghg) %>% 
      select(-gas) %>% 
      rename(model = damage.function),
    by = c('model', 'discount.rate', 'emissions.year')
  )

## gwp data from AR4
gwp = 
  tibble(
    gas    = c('HFC23', 'HFC32', 'HFC125', 'HFC134a', 'HFC143a', 'HFC152a', 'HFC227ea', 'HFC236fa', 'HFC245fa', 'HFC365mfc', 'HFC4310mee'),
    gwp100 = c(14800, 675, 3500, 1430, 4470, 124, 3220, 9810, 1030, 794, 1640)
  )

## join with gwp and estimate gwp ratios
data %<>% 
  left_join(gwp, by = 'gas') %>% 
  mutate(schfc.gwp = gwp100 * scco2,
         gwp.ratio = schfc.gwp/schfc)

## arrange order
data %<>% mutate(gas = fct_relevel(gas,'HFC23', 'HFC32', 'HFC125', 'HFC134a', 'HFC143a', 'HFC152a', 'HFC227ea', 'HFC236fa', 'HFC245fa', 'HFC365mfc', 'HFC4310mee'))

##########################
####################  plot
##########################

data %>% 
  filter(emissions.year < 2080) %>% 
  ggplot() +
  facet_wrap(~gas, ncol = 3) +
  geom_line(aes(x        = emissions.year, 
                y        = gwp.ratio, 
                color    = model,
                linetype = model), 
            linewidth = 1) +
  geom_segment(aes(x    = 2020, 
                   xend = 2070, 
                   y    = 1, 
                   yend = 1), 
               color     = 'grey20', 
               linetype  = 'dotdash',
               linewidth = 0.5) +
  scale_x_continuous(breaks = seq(2020, 2070, 10),
                     limits = c(2020, 2070),
                     labels = c(2020, '', 2040, '', 2060, '')) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_color_manual(values = c(colors[4], colors[3])) +
  scale_fill_manual(values = c(colors[4], colors[3])) +
  labs(x        = "Emissions Year",
       y        = 'Ratio of GWP-based Appriximation of the SC-HFC to the Direct Etimate SC-HFC',
       color    = '',
       linetype = '') +
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
        text             = element_text(family = "sans-serif", color = 'grey20'))

## export
ggsave('output/figures/gwp_ratio.svg', width = 9, height = 11)

## end of script, have a great day! 
