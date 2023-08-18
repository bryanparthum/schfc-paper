##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c("magrittr", "tidyverse", "reshape2",
                      "ggplot2", "showtext", "readxl", "FreqProf", "data.table")
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
##################### data
##########################

## read in scghg data
scghgs =
  rbind(read_csv("../MimiGIVE/output/scghg_annual.csv", show_col_types = F), 
        read_csv("../MimiIWG/output/scghg_annual.csv", show_col_types = F)) %>% 
  filter(gas != 'CO2')

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

ssp3_baseline_hfcs_only <- subset_and_clean(ssp3_baseline_all_gases)
ssp3_KA_reductions_hfcs_only <- subset_and_clean(ssp3_KA_reductions_all_gases)
ssp3_mtfr_hfcs_only <- subset_and_clean(ssp3_mtfr_all_gases)

##########################
######### climate benefits
##########################

## function to get annual reductions
get_benefits_summary <- function(data, ka_group = "Global") {
  data %>%
    filter(KA_group == ka_group) %>%
    merge(scghgs, by.x = c("gas", "year"), by.y = c("gas", "emissions.year")) %>%
    group_by(discount.rate) %>%
    mutate(benefit = -value * scghg / 10^9) %>% # convert from kt to t, dollars to trillions
    ungroup
}

## function to calculate total discounted climate benefits
get_discounted_benefits <- function(data, base_year, group = "Global") {
  data %>%
    filter(KA_group == group, year >= base_year) %>%
    merge(scghgs, by.x = c("gas", "year"), by.y = c("gas", "emissions.year")) %>%
    mutate(discount_rate = 0.01*as.numeric(str_extract(discount.rate, "((\\b100)|(\\b[0-9]{1})\\.?[0-9]?)(?=%| *percent)")),
           discount_factor = 1/((1+discount_rate)^(year-base_year)), 
           discounted_benefit = (-value * 1000 * scghg) / 10^12 * discount_factor) %>% # convert from kt to t, dollars to trillions
    group_by(discount.rate, damage.function) %>%
    summarize(total_discounted_benefit = sum(discounted_benefit))
}

# comparison of annual benefits, MimiIWG 3% v.s. MimiGIVE 2%, KA
global_summary <- get_benefits_summary(ssp3_KA_reductions_hfcs_only)

# total discounted benefits from KA
total_benefits_KA <- get_discounted_benefits(ssp3_KA_reductions_hfcs_only, base_year = 2023)

# global benefits from MTFR
total_benefits_mtfr <- get_discounted_benefits(ssp3_mtfr_hfcs_only, base_year = 2023)

# compare
total_benefits <- rbind(total_benefits_KA %>% 
                          filter(discount.rate == "2.0% Ramsey" | discount.rate == "3%") %>%
                          mutate(schedule = "A) Kigali Amendment"),
                        total_benefits_mtfr %>% 
                          filter(discount.rate == "2.0% Ramsey" | discount.rate == "3%") %>%
                          mutate(schedule = "B) Maximum Technologically \nFeasible Reduction"))

##########################
##################### plot
##########################

total_benefits %>% 
  ggplot() + 
  facet_wrap(~schedule, scales = "free_x") + 
  geom_bar(aes(x    = factor(damage.function, levels = c("MimiGIVE", "MimiIWG")), 
               y    = total_discounted_benefit, 
               fill = schedule),
           alpha = 0.8,
           stat     = "identity", 
           position = "dodge") +
  geom_text(aes(x      = factor(damage.function, levels = c("MimiGIVE", "MimiIWG")), 
                y      = total_discounted_benefit, 
                label  = paste0('$', round(total_discounted_benefit, 2), ' T')),
            family = "sans-serif", 
            color = 'grey20',
            vjust = -0.2) +
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
ggsave("output/figures/total_discounted_benefits.svg", width = 9, height = 6)
