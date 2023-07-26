

polit <- read.csv2("data/politics.csv", colClasses = "character")

antall_koder <- polit %>%  
  pull(isco08) %>%   # trekker ut en vektor med kun en variabel 
  unique() %>%       # beholder kun unike verdier 
  length()           # lengden på gjenværende vektor 

antall_koder



isco <- readxl::read_excel(path = "data/3_codes_isco88_ordc.xlsx", skip = 4, .name_repair = "universal", col_types = "text") %>% 
  select(2,9) 
head(isco)

isco <- isco %>% 
  rename(isco08 = ISCO.88.code)

polit2 <- left_join(polit, isco, by = "isco08")

head(polit2)


devtools::install_git("https://code.europa.eu/digclass/digclass.git")


library(DIGCLASS)

table(polit$isco08)[1]

head(polit)

suppressMessages({
polit %>% 
  mutate(isco08 = repair_isco(.$isco08)) %>% 
  mutate(orcd = isco88_to_ordc(isco08, label = FALSE), 
         orcd_lab = isco88_to_ordc(isco08, label = TRUE)) %>%
  #mutate(ordc_label = isco88_to_ordc(isco88, label = TRUE)) %>% 
  head()
})


###################



library(DIGCLASS)


isco_xls <- readxl::read_excel(path = "data/CODES_isco88_ordc.xlsx", 
                           skip = 4, .name_repair = "universal", 
                           col_types = "text") %>% 
  select(2,8) %>% 
  rename_with(tolower) %>% 
  rename(isco88 = isco.88.code, 
         ordc_klass_xl = 2) %>% 
  filter(!is.na(ordc_klass_xl)) %>% 
  group_by(isco88) %>% 
  slice(1) %>% 
  ungroup()


test <- left_join(isco_xls, isco_csv, by = "isco88")

test %>% 
  filter(ordc_klass_xl != ordc_klasse_csv) 



isco_csv <- read.csv("data/ISCO_ORDC.do", header = F, skip = 49, nrows = 521) %>% 
  mutate(ordc_klasse_csv = str_extract_all(V1, "\\d+", simplify = TRUE)[,1],
         isco88 = str_extract_all(V1, "\\d+", simplify = TRUE)[,3]) %>% 
  select(-V1) %>% 
  filter(isco88 != "") %>% 
  group_by(isco88) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(isco88)


suppressMessages({
essx <- ess %>%
  select(isco88) %>% 
    mutate(orcd_pkg = isco88_to_ordc(isco88, label = FALSE), 
           orcd_lab = isco88_to_ordc(isco88, label = TRUE)) %>%
  left_join(isco_xls, by = "isco88") %>% 
  left_join(isco_csv, by = "isco88")
})

essx %>% 
  #filter(ordc_klass_xl != orcd_pkg) %>% 
  filter(ordc_klasse_csv != orcd_pkg) %>% 
  pull(isco88) %>% 
  unique()


lvls <- as.character(c(1:12, 13))

essx %>% 
  select(orcd_pkg, ordc_klasse_csv) %>% 
  mutate(orcd_pkg = factor(orcd_pkg), 
         ordc_klasse_csv = factor(ordc_klasse_csv)) %>% 
  mutate(orcd_pkg = factor(orcd_pkg, levels = lvls), 
         ordc_klasse_csv = factor(ordc_klasse_csv, levels = lvls)) %>% 
  gtsummary::tbl_summary(by = orcd_pkg,
                         statistic = everything() ~ "{n}",
                         label = list(orcd_pkg ~ "Omkoding fra DIGCLASS-pakke", 
                                      ordc_klass_xl = "Omkoding fra excelfil"))


essx %>% 
  select(ordc_klasse_csv, ordc_klass_xl) %>% 
  mutate(ordc_klasse_csv = factor(ordc_klasse_csv), 
         ordc_klass_xl = factor(ordc_klass_xl)) %>% 
  mutate(ordc_klasse_csv = factor(ordc_klasse_csv, levels = lvls), 
         ordc_klass_xl = factor(ordc_klass_xl, levels = lvls)) %>% 
  gtsummary::tbl_summary(by = ordc_klass_xl,
                         statistic = everything() ~ "{n}",
                         label = list(ordc_klass_xl ~ "Omkoding fra excelfil", 
                                      ordc_klasse_csv = "Omkoding fra ado-fil"))

# Sender til Vidal ####
###################

library(DIGCLASS)
library(tidyverse)
library(gtsummary)

glimpse(ess)

isco_csv <- read.csv("data/ordc.ado", header = F, skip = 50, nrows = 520) %>% 
  mutate(ordc_klasse_csv = str_extract_all(V1, "\\d+", simplify = TRUE)[,2],
         isco88 = str_extract_all(V1, "\\d+", simplify = TRUE)[,1]) %>% 
   select(-V1)  
  # filter(isco88 != "") %>% 
  # group_by(isco88) %>% 
  # slice(1) %>% 
  # ungroup() %>% 
  # arrange(isco88)

tail(isco_csv)

dim(isco_csv)
isco_csv %>% 
  group_by(isco88) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
  


# recode
lvls <- as.character(c(1:12, 13)) # factor levels for making tables orderly

essx <- ess %>%
    select(isco88) %>% 
    mutate(orcd_pkg = isco88_to_ordc(isco88, label = FALSE), 
           orcd_lab = isco88_to_ordc(isco88, label = TRUE)) %>%
    left_join(isco_csv, by = "isco88") %>% 
    mutate(orcd_pkg = factor(orcd_pkg), 
           ordc_klasse_csv = factor(ordc_klasse_csv)) %>% 
    mutate(orcd_pkg = factor(orcd_pkg, levels = lvls), 
           ordc_klasse_csv = factor(ordc_klasse_csv, levels = lvls))

# Make a table
essx %>% 
  select(orcd_pkg, ordc_klasse_csv) %>% 
  gtsummary::tbl_summary(by = orcd_pkg,
                         statistic = everything() ~ "{n}",
                         label = list(orcd_pkg ~ "From DIGCLASS-pakke", 
                                      ordc_klasse_csv = "From ado-file"))



# Which codes are classified differently
essx %>% 
  filter(ordc_klasse_csv != orcd_pkg) %>% 
  pull(isco88) %>% 
  unique()

# get the specific differences
essx %>% 
  select(isco88, ordc_klasse_csv, orcd_pkg) %>% 
  filter(ordc_klasse_csv != orcd_pkg) %>% 
  unique() %>% 
  data.frame()



