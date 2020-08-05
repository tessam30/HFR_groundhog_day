# Purpose; HFR verrting
# Author: Tim Essam, SI
# Date: 1993-02-03
# Notes:


# LIBRARIES ---------------------------------------------------------------

  library(tidyverse)
  library(vroom)
  library(glitr)
  library(glamr)
  library(here)
library(extrafont)


# GLOBALS -----------------------------------------------------------------

  data_in  <- "../../HFR"
  hfr_10 <-  "HFR_2020.10_Tableau_20200728.csv"
  hfr_9  <-  "HFR_2020.09_Tableau_20200716.csv"
  hfr_7 <-  "HFR_2020.07_Tableau_20200520.csv"
  hfr_4 <-  "HFR_2020.04_Tableau_20200311.csv"


# MUNGS -------------------------------------------------------------------
 
  hfr <- vroom(here(data_in, hfr_10)) %>% 
    filter(countryname == "Zambia",
           indicator == "VMMC_CIRC")
  
  hfr7 <- vroom(here(data_in, hfr_7)) %>% 
    filter(countryname == "Zambia",
           indicator == "VMMC_CIRC",
           val != "\\N") %>%
    mutate(val = as.numeric(val))
  
  hfr4 <- vroom(here(data_in, hfr_4)) %>% 
    filter(countryname == "Zambia",
           indicator == "VMMC_CIRC",
           val != "\\N") %>%
    mutate(val = as.numeric(val)) %>% 
    select(-mer_targets, -mer_results)
  
  
  hfr <- bind_rows(hfr, hfr7, hfr4)
  

  hfr %>%  
    group_by(hfr_pd, mech_code, mech_name) %>% 
    summarise(vmmc = sum(val, na.rm = TRUE)) %>% 
    filter(vmmc != 0) %>% 
    ggplot(aes(hfr_pd, vmmc)) +
    geom_col() +
    facet_wrap(~mech_name) +
    si_style_ygrid() +
    scale_x_continuous(breaks = seq(2, 10, 1))
  