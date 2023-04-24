# Innlesning av originale data fra Sikt ####



## OBS! Det er noen nummeriske med filter-verdier
library(tidyverse)
library(haven)
library(labelled)

filbane <- "C:/Users/torbskar/OneDrive - Universitetet i Oslo/Dokumenter/Undervisning/SOS4020_forkurs/data2023/rOBH3y10/"

faste <- read_stata( paste0(filbane, "NorLAG-lengde-faste.dta"), encoding = "utf-8")
lang <- read_stata( paste0(filbane, "NorLAG-lengde-intervju.dta"), encoding = "utf-8")

norlag <- merge(faste, lang, by = "ref_nr") %>% 
  filter(iodeltakelse == 1 |  
           iodeltakelse == 2 & round %in% c(1, 3) | 
           iodeltakelse == 3 & round %in% c(2, 3) |
           iodeltakelse == 4 & round %in% c(1) |
           iodeltakelse == 5 & round %in% c(1, 2) |
           iodeltakelse == 6 & round %in% c(2)
  ) %>% 
  # generelle missing
  mutate(across( where(is.labelled) ,  ~replace(., 
                                                . %in% c(997, 998, 999, 99999, 999999), 
                                                NA))) %>%
  # noen spesielle missing - som kanskje gjelder overalt?
  mutate(across(where(is.labelled) ,  ~replace(., 
                                               . %in% c(994, 995, 996, 9996), 
                                               NA))) %>%
  # noen spesielle missing - som kanskje gjelder overalt?
  mutate(across(where(is.labelled) ,  ~replace(., 
                                               . %in% c(9993, 9994, 9995, 9996, 9997, 9998, 999), 
                                               NA))) %>%
  # inpartbankinnsk og fremover
  mutate(across(where(is.labelled) ,  ~replace(., 
                                               . %in% c(99999996,
                                                        999999995,
                                                        999999996,
                                                        999999999), 
                                               NA))) %>%
  mutate(across(where(is.labelled), ~replace(. ,
                                             str_sub(., 1, 7) == "filter:", NA))) %>% 
  #mutate(across(where(is.labelled), ~drop_unused_value_labels(.))) %>% 
  # Noen spesielle der drop_unused_value_labels() ikke funker (f.eks. hc135). Gjør til factor og rydder etterpå. 
  mutate(across(where(is.labelled), ~as_factor(.))) %>% 
  mutate(across(where(is.factor), ~fct_drop(.)))


## Omkoder hovedaktivitet ####
fs <- lvls_union( list(norlag$wr001, norlag$wr002, norlag$wr003c)) %>% tolower() %>% unique()
norlag <- norlag %>% 
  mutate(across(wr001:wr003c, ~factor(tolower(.), levels=fs))) %>% 
  mutate(hovedaktivitet = case_when(round == 1 ~ wr001, 
                                    round == 2 ~ wr002, 
                                    round == 3 ~ wr003c) ) %>% 
  mutate(hovedaktivitet2 = case_when( str_sub(hovedaktivitet, 1, 5) == "yrkes" ~ "Yrkesaktiv", 
                                      str_detect(hovedaktivitet, "arbeidsledig") ~ "Trygdet/arbeidsledig/stud/annet", 
                                      str_detect(hovedaktivitet, "student") ~ "Trygdet/arbeidsledig/stud/annet", 
                                      str_detect(hovedaktivitet, "trygd") ~ "Trygdet/arbeidsledig/stud/annet", 
                                      str_detect(hovedaktivitet, "annet") ~ "Trygdet/arbeidsledig/stud/annet", 
                                      str_sub(hovedaktivitet,1,6)   == "hjemme" ~ "hjemmeværende/husmor", 
                                      str_detect(hovedaktivitet, "pensjonist") ~ "pensjonist", 
                                      is.na(hovedaktivitet) ~ "Trygdet/arbeidsledig/stud/annet") %>% as_factor())


saveRDS(norlag, paste0(filbane, "../data_tilDeling/norlag.rds"))





