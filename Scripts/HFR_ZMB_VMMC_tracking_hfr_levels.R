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
  library(ICPIutilities)
  library(scales)
  library(Wavelength)
  library(patchwork)
  library(COVIDutilities)


# GLOBALS -----------------------------------------------------------------

  data_in  <- "../../HFR"
  images <- "Images"
  
  hfr_10 <-  "HFR_2020.10_Tableau_20200728.csv"
  hfr_7 <-  "HFR_2020.07_Tableau_20200520.csv"
  hfr_4 <-  "HFR_2020.04_Tableau_20200311.csv"
  hfr_3 <-  "HFR_2020.03_Tableau_20200210.csv"

  filter_hfr <- function(df) {
    df %>% 
      filter(countryname == "Zambia",
             indicator == "VMMC_CIRC",
             val != "\\N") %>%
      mutate(val = as.numeric(val))
  }

  

# COVID INFO --------------------------------------------------------------

  # COVID Cases
  covid_cases <- pull_jhu_covid() %>% 
    filter(countryname %in% c("Zambia")) %>% 
    mutate(zmb_color = if_else(countryname == "Zambia", wapo_dorange, grey20k))
  
  zmb_first <- as.Date("2020-03-18")

  df_gm <- extract_excel_data(hdx_govmes_url, hdx_govmes_linkid, 2, 'xlsx') %>% 
    filter(iso == "ZMB")
  
# MUNGING -------------------------------------------------------------------
 
  hfr10 <- vroom(here(data_in, hfr_10)) %>% 
    filter(countryname == "Zambia",
           indicator == "VMMC_CIRC")
  
  hfr7 <- vroom(here(data_in, hfr_7)) %>% filter_hfr()
  
  hfr4 <- vroom(here(data_in, hfr_4)) %>% filter_hfr() %>% 
    select(-mer_targets, -mer_results)

  hfr3 <- vroom(here(data_in, hfr_3)) %>% filter_hfr() %>% 
    select(-mer_targets, -mer_results)

  purrr::map(list(hfr, hfr7, hfr4, hfr3), .f = ~.x %>% count(hfr_pd))
    
  
  hfr_all <- bind_rows(hfr10, hfr7, hfr4, hfr3)
  
  #align dates with hfr_pds
  df_pds <- hfr_identify_pds(2020) %>% 
    group_by(hfr_pd) %>% 
    summarise(hfr_pd_date_min = min(date),
              hfr_pd_date_max = max(date)) %>% 
    ungroup() 
  
  
  hfr_all <- hfr_all %>% 
    left_join(df_pds, by = "hfr_pd")
  

 hfr_overall <-  hfr_all %>%  
    group_by(hfr_pd, hfr_pd_date_max) %>% 
    summarise(vmmc = sum(val, na.rm = TRUE)) %>% 
    filter(vmmc != 0) %>% 
    ggplot(aes(hfr_pd_date_max, vmmc)) +
    geom_vline(xintercept = zmb_first, size = 5, colour = grey20k, alpha = 0.80) +
    geom_col(fill = grey60k) +
    #facet_wrap(~str_c(mech_name, "\n")) +
    si_style_ygrid()+
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(labels = comma) +
    theme(axis.title = element_blank()) +
   labs(title = "USAID ZAMBIA VMMC_CIRC TOTAL FOR HFR REPORTING NOV - JUL",
        subtitle = "")

  
 hfr_mech <-  hfr_all %>%  
    mutate(mech_name = if_else(mech_name == "USAID/District Coverage of Health Services (DISCOVER-H)", "DISCOVER", mech_name)) %>% 
    group_by(hfr_pd, mech_code, mech_name, hfr_pd_date_max) %>% 
    summarise(vmmc = sum(val, na.rm = TRUE)) %>% 
    filter(vmmc != 0) %>% 
    ggplot(aes(hfr_pd_date_max, vmmc)) +
   geom_vline(xintercept = zmb_first, size = 3, colour = grey20k, alpha = 0.80) +
    geom_col(fill = grey60k) +
    facet_wrap(~str_c(mech_name, "\n")) +
    si_style_ygrid()+
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(labels = comma) +
   theme(axis.title = element_blank()) +
    labs(title = "USAID ZAMBIA VMMC_CIRC TOTAL BY MECHANISM FOR HFR REPORTING NOV - JUL",
         subtitle = "")

  gph <- hfr_overall / hfr_mech
  ggsave("ZMB_HFR_VMMC_CIRC_SUMMARY.pdf", plot = gph, path = images,
         scale = 1, 
         width = 10, height = 5.625, dpi = 320)


# MER DATA ----------------------------------------------------------------

  genie <- 
    vroom("../../DATIM_Data/MER_Structured_Datasets_Site_IM_FY18-20_20200626_v2_1_Zambia.txt") %>% filter(indicator == "VMMC_CIRC", disaggregate == "Total Numerator", operatingunit == "Zambia") %>% 
    reshape_msd()

  
  gen_lon %>% filter(fundingagency == "USAID",
                     str_detect(period, "2020q")) %>%  
    group_by(mech_code, mech_name, period) %>% 
    summarise(vmmc = sum(val, na.rm = T)) %>% 
    group_by(period) %>% 
    summarise(VMMC = sum(vmmc))
  
  %>% 
    ggplot(aes(period, vmmc)) +
    geom_col() +
    facet_wrap(~str_c(mech_name, "\n")) +
    si_style_ygrid() +
    scale_y_continuous(labels = comma)

    
    