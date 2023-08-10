##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c("magrittr", "tidyverse",
                      'data.table','arrow',
                      'stringi',
                      'readxl',
                      'FreqProf',
                      'reshape2',
                      "ggplot2", "showtext")
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
              q05  = quantile(scghg, .05),
              q95  = quantile(scghg, .95),
              .groups = 'drop')
}

## function to prepare mimiiwg scghg
read_iwg <- function(x) {
  filename = basename(x)
  read_parquet(x) %>%
    # filter(discount.rate %in% c('3%')) %>% 
    mutate(gas   = stri_split(stri_split(filename, fixed = '-')[[1]][2],  fixed = '_')[[1]][1],
           model = 'MimiIWG') %>% 
    group_by(discount.rate, trial, emissions.year, gas, model) %>% 
    summarise(scghg = mean(scghg), 
              .groups = 'drop')
}

## function to prepare mimigive schfcs
read_give <- function(x) {
  filename = basename(x)
  read_parquet(x) %>%
    rename(discount.rate = discount_rate) %>%
    # filter(discount.rate %in% c('2.0% Ramsey')) %>%  
    mutate(gas   = case_when(stri_split(filename, fixed = '-')[[1]][2] == 'HFC43_10' ~ 'HFC4310mee',
                             T ~ stri_split(filename, fixed = '-')[[1]][2]),
           emissions.year = as.numeric(stri_split(filename, fixed = '-')[[1]][4]),
           model = 'MimiGIVE') %>% 
    select(-sector)
}

##########################
##################### data
##########################

## combine 
data = 
  bind_rows(
    list.files('../MimiIWG/output/scghgs/full_distributions', pattern = '.parquet', full.names = T) %>% 
      map_df(~read_iwg(.)),
    list.files('../MimiGIVE/output/scghgs/full_distributions', pattern = ".parquet", full.names = T) %>%
      map_df(~read_give(.))
  ) %>% 
  filter(gas != 'CO2') 

## blank canvas 
data2 = tibble()

## bootstrap to get confidence intervals
set.seed(42)
for (i in 1:50) {
  print(paste0('boot ', i))
  data2 = 
    bind_rows(
      data2,
      data %>% 
        group_by(discount.rate, emissions.year, gas, model) %>% 
        slice_sample(n = 1000) %>% 
        summarise(scghg = mean(scghg), 
                  .groups = 'drop') %>% 
        mutate(trial = i)
    )
}

## get intervals
data =
  summarise_scghg(data2)
  
## linearly interpolate between emissions years
data %<>% 
  group_by(model, gas, discount.rate) %>% 
  complete(emissions.year = seq(first(emissions.year), last(emissions.year))) %>%
  mutate(mean = round(zoo::na.approx(mean)),
         q05  = round(zoo::na.approx(q05)),
         q95  = round(zoo::na.approx(q95)))

##########################
#### purohit et al. (2020)
##########################

## path
purohit_emissions_data_path <- "data/purohit_2020_gains_data.xlsx"

## we are interested in SSP3 TEES
ssp3_tees_raw = data.frame(read_excel(purohit_emissions_data_path, sheet = "SSP3_Baseline_TEES", range = cell_rows(6:108)))

## drop extraenous header rows, change data classes, rename first two columns
ssp3_tees_clean <- ssp3_tees_raw[c(-1,-2),]
ssp3_tees_clean[-1] <- lapply(ssp3_tees_clean[-1], as.numeric)
ssp3_tees_clean[1] <- lapply(ssp3_tees_clean[1], as.factor)
ssp3_tees_clean %<>% rename("KA_group" = "...1", "year" = "...2")

# first 33 columns contain baseline projections (first two rows have KA group and year), cols 34-65 contain KA reductions under SSP3 TEES
ssp3_baseline_all_gases <- ssp3_tees_clean[,1:33]
ssp3_KA_reductions_all_gases <- cbind(ssp3_tees_clean[,1:2], ssp3_tees_clean[,34:65])
ssp3_mtfr_all_gases <- cbind(ssp3_tees_clean[,1:2], ssp3_tees_clean[,88:119])

# just get global hfc projections and interpolate to get annual reductions
subset_and_clean <- function(all_gases, gwp = gwps_ar5) {
  
  # clean
  hfcs_only <- all_gases %>%
    select(matches("HFC|KA_group|year")) %>%
    select(-matches("sum.HFC.HCFC")) %>%
    filter(KA_group == "Global")
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
    melt(id=1:2, variable.name = "gas", value.name = "value")
  
  return(hfcs_interp)
  
}

ssp3_baseline_hfcs_only      <- subset_and_clean(ssp3_baseline_all_gases)
ssp3_KA_reductions_hfcs_only <- subset_and_clean(ssp3_KA_reductions_all_gases)
ssp3_mtfr_hfcs_only          <- subset_and_clean(ssp3_mtfr_all_gases)

##########################
######### climate benefits
##########################

## function to get annual reductions
get_benefits_summary <- function(df, ka_group = "Global") {
  df %>%
    filter(KA_group == ka_group) %>%
    merge(data, by.x = c("gas", "year"), by.y = c("gas", "emissions.year")) %>%
    group_by(discount.rate) %>%
    mutate(mean.benefit = -value * mean / 10^9,
           q05.benefit  = -value * q05 / 10^9,
           q95.benefit  = -value * q95 / 10^9) %>% # convert from kt to t, dollars to trillions
    ungroup
}

## function to calculate total discounted climate benefits
get_discounted_benefits <- function(df, base_year, group = "Global") {
  df %>%
    filter(KA_group == group, year >= base_year) %>%
    merge(data, by.x = c("gas", "year"), by.y = c("gas", "emissions.year")) %>%
    mutate(discount_rate = 0.01*as.numeric(str_extract(discount.rate, "((\\b100)|(\\b[0-9]{1})\\.?[0-9]?)(?=%| *percent)")),
           discount_factor = 1/((1+discount_rate)^(year-base_year)), 
           discounted_benefit_mean = (-value * 1000 * mean) / 10^12 * discount_factor,
           discounted_benefit_q05  = (-value * 1000 * q05) / 10^12 * discount_factor,
           discounted_benefit_q95  = (-value * 1000 * q95) / 10^12 * discount_factor) %>% # convert from t to kt, dollars to trillions
    group_by(discount.rate, model) %>%
    summarize(total_discounted_benefit_mean = round(sum(discounted_benefit_mean), 2),
              total_discounted_benefit_q05  = round(sum(discounted_benefit_q05), 2),
              total_discounted_benefit_q95  = round(sum(discounted_benefit_q95), 2))
}

# comparison of annual benefits, MimiIWG 3% v.s. MimiGIVE 2%, KA
global_summary <- get_benefits_summary(ssp3_KA_reductions_hfcs_only)

# total discounted benefits from KA
total_benefits_KA <- get_discounted_benefits(ssp3_KA_reductions_hfcs_only, base_year = 2023)

# global benefits from MTFR
total_benefits_mtfr <- get_discounted_benefits(ssp3_mtfr_hfcs_only, base_year = 2023)

# compare
total_benefits <- rbind(total_benefits_KA %>% 
                          mutate(schedule = "A) Kigali Amendment"),
                        total_benefits_mtfr %>% 
                          mutate(schedule = "B) Maximum Technologically \nFeasible Reduction"))

## write total benefits for easy reference
total_benefits %>% 
  write_csv('output/tables/total_benefits.csv')

##########################
##################### plot
##########################

total_benefits %>% 
  filter(discount.rate %in% c('2.0% Ramsey', '3%')) %>% 
  ggplot() + 
  facet_wrap(~schedule, scales = "free_x") + 
  geom_bar(aes(x    = factor(model, levels = c("MimiGIVE", "MimiIWG")), 
               y    = total_discounted_benefit_mean, 
               fill = schedule),
           alpha = 0.8,
           stat     = "identity", 
           position = "dodge") +
  geom_errorbar(aes(x    = factor(model, levels = c("MimiGIVE", "MimiIWG")),
                    ymin      = total_discounted_benefit_q05,
                    ymax      = total_discounted_benefit_q95),
                color    = 'grey40',
                linetype = 'dashed',
                width    = 0.2,
                alpha    = 0.8) +
  geom_text(aes(x      = factor(model, levels = c("MimiGIVE", "MimiIWG")), 
                y      = total_discounted_benefit_mean, 
                label  = paste0('$', round(total_discounted_benefit_mean, 2), ' T')),
            family = "sans-serif", 
            color = 'grey20',
            vjust = -2) +
  scale_color_manual(values = c(colors[4], colors[3])) +
  scale_fill_manual(values = c(colors[4], colors[3])) +
  labs(x = '',
       y = 'Net Present Value of Climate Benefits \nfrom Global HFC Phasedown (trillions 2020 USD)') +
  theme_bw() +
  theme(
    legend.title     = element_blank(),
    legend.position  = 'none',
    legend.text      = element_text(size = 14),
    legend.key.size = unit(3.5, 'lines'),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 12),
    axis.line.x      = element_line(color = "black"),
    axis.ticks.x     = element_blank(),
    strip.text.x     = element_text(size = 13), 
    strip.background = element_rect(color="black",
                                    fill="white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = 'grey70', linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 14, hjust = 0.5),
    plot.title       = element_text(size = 14, hjust = 0.5),
    text             = element_text(family = "sans-serif", color = 'grey20'))

## export
ggsave("output/figures/total_discounted_benefits_with_uncertainty.svg", width = 9, height = 6)
