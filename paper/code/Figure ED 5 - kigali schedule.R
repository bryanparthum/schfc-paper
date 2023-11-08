##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'ggplot2','ggrepel','ggpubr','RColorBrewer',
                      'showtext', 'data.table')
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
colors = c("#56B4E9", "#0072B2", "#E69F00", "#D55E00" , "#F0E442", "#009E73", "#CC79A7", "#999999")

##########################
####################  data
##########################

nona5_group1 <- data.table(year = c(2019, 2024, 2024, 2029, 2029, 2034, 2034, 2036, 2036, 2050),
                           level = c(90, 90, 60, 60, 30, 30, 20, 20, 15, 15),
                           group = "Non-A5 Group 1")
nona5_group2 <- data.table(year = c(2020, 2025, 2025, 2029, 2029, 2034, 2034, 2036, 2036, 2050),
                           level = c(95, 95, 65, 65, 30, 30, 20, 20, 15, 15),
                           group = "Non-A5 Group 2")
a5_group1 <- data.table(year = c(2024, 2029, 2029, 2035, 2035, 2040, 2040, 2045, 2045, 2050),
                        level = c(100, 100, 90, 90, 70, 70, 50, 50, 20, 20),
                        group = "A5 Group 1")
a5_group2 <- data.table(year = c(2028, 2032, 2032, 2037, 2037, 2042, 2042, 2047, 2047, 2050),
                        level = c(100, 100, 90, 90, 80, 80, 70, 70, 15, 15),
                        group = "A5 Group 2")
phasedown_sched <- rbind(nona5_group1, nona5_group2, a5_group1, a5_group2)

##########################
####################  plot
##########################

phasedown_sched %>% 
  ggplot() +
  geom_line(aes(x        = year, 
                y        = level/100, 
                color    = group,
                linetype = group),
            linewidth = 1) +
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = c('dashed', 'solid', 'dashed', 'solid')) +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(labels = scales::percent_format(),
                     n.breaks = 10) +
  labs(x        = "Year",
       y        = "Percentage of Baseline",
       color    = "Article 5 Grouping",
       linetype = "Article 5 Grouping") +
  theme_minimal() +
  theme(legend.position = c(.16, 0.2),
        legend.title     = element_text(size = 12, color='grey20'),
        legend.text      = element_text(size = 12, color='grey20'),
        legend.key.width = unit(1.5, 'cm'),
        legend.margin    = margin(0, 0, 0, 0),
        legend.box.background = element_rect(fill = 'white', color = NA),
        axis.title       = element_text(size = 16),
        axis.text        = element_text(size = 16),
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black", size = 1),
        strip.text.x     = element_text(size = 13), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey70', linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(size = 12, hjust = 0.5),
        plot.title       = element_text(size = 12, hjust = 0.5),
        text             = element_text(family = "sans-serif", color = 'grey20'))

## export
ggsave('output/figures/Extended Data Figure 5.pdf', 
       width  = 180, 
       height = 120,
       units  = 'mm')

## end of script, have a great day.