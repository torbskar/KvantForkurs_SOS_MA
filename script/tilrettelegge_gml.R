
# tidligere versjon: 24.04.2023

norlag <- read_stata("data/norlag_panel2022.dta", encoding = "utf-8") %>% 
  filter(iodeltagelse == 1 |  
           iodeltagelse == 2 & round %in% c(1, 3) | 
           iodeltagelse == 3 & round %in% c(2, 3) |
           iodeltagelse == 4 & round %in% c(1) |
           iodeltagelse == 5 & round %in% c(1, 2) |
           iodeltagelse == 6 & round %in% c(2)
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
  labelled::drop_unused_value_labels() %>% 
  labelled::unlabelled()