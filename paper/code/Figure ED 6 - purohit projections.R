##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c("magrittr", "tidyverse", "data.table",
                      "readxl", 
                      "stringr", 
                      "ggplot2", "showtext",
                      "FreqProf")
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

##########################
####################  data
##########################

purohit_emissions_data_path <- "data/purohit_2020_gains_data.xlsx"
purohit_emissions_data <- read_excel(purohit_emissions_data_path)

# we are interested in SSP3 TEES
ssp3_tees_raw <- data.frame(read_excel(purohit_emissions_data_path, sheet = "SSP3_Baseline_TEES", range = cell_rows(6:108)))

# extract AR5 GWPs for conversion from CO2eq
gwps_ar5 <- ssp3_tees_raw[1,1:33] %>%
  select(matches("HFC")) %>%
  select(-matches("sum.HFC.HCFC"))
colnames(gwps_ar5) <- str_replace(colnames(gwps_ar5), "\\.\\.\\.[0-9]*", "") # clean column names
colnames(gwps_ar5) <- str_replace(colnames(gwps_ar5), "_", "") # clean column names
gwps_ar5 <- data.frame(t(gwps_ar5)) %>%
  rownames_to_column("gas") %>%
  rename("gwp" = "X1")
gwps_ar5$gwp <- as.numeric(gwps_ar5$gwp)

# drop extraneous header rows, change data classes, rename first two columns
ssp3_tees_clean <- ssp3_tees_raw[c(-1,-2),]
ssp3_tees_clean[-1] <- lapply(ssp3_tees_clean[-1], as.numeric)
ssp3_tees_clean[1] <- lapply(ssp3_tees_clean[1], as.factor)
ssp3_tees_clean %<>% rename("KA_group" = "...1", "year" = "...2")

# first 33 columns contain baseline projections (first two rows have KA group and year), cols 34-65 contain KA reductions under SSP3 TEES
ssp3_baseline_all_gases <- ssp3_tees_clean[,1:33]
ssp3_KA_reductions_all_gases <- cbind(ssp3_tees_clean[,1:2], ssp3_tees_clean[,34:65])
ssp3_mtfr_all_gases <- cbind(ssp3_tees_clean[,1:2], ssp3_tees_clean[,88:119])

subset_and_clean <- function(all_gases, gwp = gwps_ar5, a5_grouping = "Global") {
  
  # clean
  hfcs_only <- all_gases %>%
    select(matches("HFC|KA_group|year")) %>%
    select(-matches("sum.HFC.HCFC"))
  hfcs_only$KA_group <- factor(hfcs_only$KA_group, levels = c("Global", "Article5_Group1", "Article5_Group2", "Non-Article5_Group1", "Non-Article5_Group2"),
                               labels = c("Global", "Article 5 Group 1", "Article 5 Group 2", "Non-Article 5 Group 1", "Non-Article 5 Group 2"))
  colnames(hfcs_only) <- str_replace(colnames(hfcs_only), "\\.\\.\\.[0-9]*", "") # clean column names
  colnames(hfcs_only)[c(-1,-2)] <- str_replace(colnames(hfcs_only)[c(-1,-2)], "_", "") # clean column names
  
  # interpolate to get annual values
  hfcs_interp <- as.data.frame(matrix(,0,ncol(hfcs_only)))
  colnames(hfcs_interp) <- colnames(hfcs_only)
  for (group in unique(hfcs_only$KA_group)) {
    KA_group <- hfcs_only %>% 
      filter(KA_group == group)
    interp <- KA_group %>%
      select(matches("HFC|year")) %>% 
      approxm(n = 96)
    interp_final <- interp %>%
      mutate(KA_group = group, .before = 1)
    hfcs_interp <- rbind(hfcs_interp, interp_final)
  }
  
  hfcs_interp %<>% 
    as.data.table() %>%
    melt(id=1:2, variable.name = "gas", value.name = "value") %>%
    merge(gwp) %>%
    mutate(value_co2eq = value * gwp) %>%
    filter(KA_group == a5_grouping) %>%
    group_by(year) %>%
    summarize(total = sum(value_co2eq))
  
  return(hfcs_interp)
  
}

ssp3_baseline <- subset_and_clean(ssp3_baseline_all_gases)
ssp3_KA_reductions <- subset_and_clean(ssp3_KA_reductions_all_gases)
ssp3_mtfr_reductions <- subset_and_clean(ssp3_mtfr_all_gases)

# calculate total reductions
total_reduction_ka <- -sum(ssp3_KA_reductions$total)
total_reduction_mtfr <- -sum(ssp3_mtfr_reductions$total)
abs(total_reduction_ka - total_reduction_mtfr)/total_reduction_ka

# create projections
projections <- ssp3_baseline %>% 
  rename(Baseline = "total") %>%
  mutate(KA = (ssp3_baseline$total + ssp3_KA_reductions$total)/1e6,
         MTFR = (ssp3_baseline$total + ssp3_mtfr_reductions$total)/1e6,
         Baseline = Baseline/1e6) %>%
  melt(id = "year", variable.name = "projection", value.name = "emissions")

## rename some stuff
projections %<>% 
  mutate(projection = case_when(projection == 'Baseline' ~ 'Baseline',
                                projection == 'KA' ~ 'Kigali Amendment',
                                projection == 'MTFR' ~ 'Maximum Technologically \nFeasible Reduction'))

##########################
####################  data
##########################

projections %>% 
  ggplot() +
  geom_line(aes(x = year, 
                y = emissions, 
                color = projection,
                linetype = projection),
            size = 1) +
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = c('solid', 'dotdash', 'longdash')) +
  labs(y = expression(paste('HFC Emissions (Gt ', CO[2],'e)')),
       x = 'Year',
       color = '',
       linetype = '') +
  theme_minimal() +
  theme(legend.position = c(0.75, 0.53),
        legend.title     = element_text(size = 16, color='grey20'),
        legend.text      = element_text(size = 13, color='grey20'),
        legend.key.width  = unit(2.5, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.margin    = margin(0, 0, 0, 0),
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
        text             = element_text(family = 'sans-serif', color = 'grey20'))

## export
ggsave('output/figures/Extended Data Figure 6.pdf', 
       width  = 180, 
       height = 120,
       units  = 'mm')

## end of script, have a great day.