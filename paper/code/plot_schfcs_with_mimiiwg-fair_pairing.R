##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'arrow','stringi',
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

## function to summarize scghgs
summarise_scghg <- function(x) {
  x %>%
    group_by(model, gas, discount.rate, emissions.year) %>%
    summarise(mean = mean(scghg),
              med  = median(scghg),
              min  = min(scghg),
              max  = max(scghg),
              q01  = quantile(scghg, .01),
              q025 = quantile(scghg, .025),
              q05  = quantile(scghg, .05),
              q25  = quantile(scghg, .25),
              q75  = quantile(scghg, .75),
              q95  = quantile(scghg, .95),
              q975 = quantile(scghg, .975),
              q99  = quantile(scghg, .99),
              .groups = 'drop')
}

## function to prepare mimiiwg scghg
read_iwg <- function(x) {
  filename = basename(x)
  read_parquet(x) %>%
    mutate(gas   = stri_split(stri_split(filename, fixed = '-')[[1]][2],  fixed = '_')[[1]][1],
           scghg = case_when(stri_split(stri_split(filename, fixed = '-')[[1]][2],  fixed = '_')[[1]][1] != 'CO2' ~ scghg/1e3,
                             T ~ scghg),
           model = 'MimiIWG')
}

## function to prepare mimigive schfcs
read_give <- function(x) {
  filename = basename(x)
  read_parquet(x) %>% 
    rename(discount.rate = discount_rate) %>%
    mutate(gas   = case_when(stri_split(filename, fixed = '-')[[1]][2] == 'HFC43_10' ~ 'HFC4310mee',
                             T ~ stri_split(filename, fixed = '-')[[1]][2]),
           emissions.year = as.numeric(stri_split(filename, fixed = '-')[[1]][4]),
           scghg = case_when(gas != 'CO2' ~ as.numeric(scghg)/1e3,
                             T ~ as.numeric(scghg)),
           model = 'MimiGIVE')
}

##########################
##################### data
##########################

## combine 
data = 
  bind_rows(
    list.files('../MimiIWG/output/scghgs/full_distributions', pattern = '.parquet', full.names = T) %>% 
      map_df(~read_iwg(.)) %>% 
      summarise_scghg, 
    list.files('../MimiGIVE/output/scghgs/full_distributions', pattern = ".parquet", full.names = T) %>%
      map_df(~read_give(.)) %>% 
      summarise_scghg 
  ) 

## arrange order
data %<>% 
  mutate(gas = fct_relevel(gas,'HFC23', 'HFC32', 'HFC125', 'HFC134a', 'HFC143a', 'HFC152a', 'HFC227ea', 'HFC236fa', 'HFC245fa', 'HFC365mfc', 'HFC4310mee', 'CO2'))

## label CO2 as not in thousands
## get maths in label for CO2
facet_names = c(`1` = 'HFC23', 
                `2` = 'HFC32', 
                `3` = 'HFC125', 
                `4` = 'HFC134a', 
                `5` = 'HFC143a', 
                `6` = 'HFC152a', 
                `7` = 'HFC227ea', 
                `8` = 'HFC236fa', 
                `9` = 'HFC245fa', 
                `10` = 'HFC365mfc', 
                `11` = 'HFC4310mee', 
                `12` = expression(CO[2], ' (2020USD)'))

data %<>% 
  mutate_at(.vars = 'gas', 
            .funs = factor, 
            labels = facet_names)

data$gas <- factor(data$gas,
                   labels = c('HFC23', 'HFC32', 'HFC125', 'HFC134a', 'HFC143a', 'HFC152a', 'HFC227ea', 'HFC236fa', 'HFC245fa', 'HFC365mfc', 'HFC4310mee', expression(paste(CO[2], '  (2020USD)'))))

##########################
####################  plot
##########################

data %>% 
  ggplot() +
  facet_wrap(~gas, scales = 'free', ncol = 3, labeller = label_parsed) +
  geom_line(aes(x        = emissions.year, 
                y        = mean, 
                color    = model,
                linetype = model)) +
  geom_ribbon(aes(x    = emissions.year,
                  ymin = q25,
                  ymax = q75,
                  fill = model),
              color = NA,
              linetype = 'dotted',
              alpha = 0.1,
              show.legend = F) +
  scale_x_continuous(breaks = seq(2020, 2100, 10),
                     limits = c(2020, 2100),
                     labels = c(2020, '', '', '', 2060, '', '', '', 2100)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_color_manual(values = c(colors[4], colors[3])) +
  scale_fill_manual(values = c(colors[4], colors[3])) +
  labs(x        = "Emissions Year",
       y        = 'The Social Cost of Hydrofluorocarbons \n(SC-HFCs are in thousands, 2020USD per tonne)',
       color    = '',
       linetype = '') +
  theme_minimal() +
  theme(
    # legend.position = c(0.93, 0.07),
    legend.position = 'bottom',
    # legend.justification = c(1, 0),
    legend.title     = element_text(size = 14, color = 'grey20'),
    legend.text      = element_text(size = 16, color = 'grey20'),
    legend.key.size  = unit(1, 'cm'),
    legend.margin    = margin(0, 0, 0, 0),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 12),
    axis.line.x      = element_line(color = "black"),
    axis.ticks.x     = element_line(color = "black", size = 1),
    strip.text.x     = element_text(size = 13, face = "bold", family = "sans-serif", color = 'grey20'), 
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = 'grey70', linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 12, hjust = 0.5),
    plot.title       = element_text(size = 12, hjust = 0.5),
    text             = element_text(family = "sans-serif", color = 'grey20'))

## export
ggsave('output/figures/schfcs.svg', width = 9, height = 11)
