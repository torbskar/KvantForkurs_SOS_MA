
# Kilde: https://link.springer.com/article/10.1007/s11205-021-02687-7

library(gtsummary)


table(faste$iodeltakelse)



norlag <- readRDS("data/norlag.rds")
dim(norlag)




glimpse(norlag[,1:10])

table(norlag$iodeltakelse)

levels(norlag$iodeltakelse)[c(1,3)]

humval <- norlag %>% 
  filter(iodeltakelse %in% levels(iodeltakelse)[c(1,3)] ) 


humval %>% 
  filter(!is.na(wbswlssum5)) %>% 
  group_by(ref_nr) %>% 
  slice(1) %>% 
  ungroup() %>% 
  #group_by(iodeltakelse) %>% 
  summarise(n = n())


ggplot(humval , aes(x = as.numeric(ioalder), y = as.numeric(wbswlssum5), group = iokjonn, col = iokjonn) ) +
  geom_smooth(se = F)


table(norlag$wbswlssum5)



# data ####
faste <- read_stata( paste0(infilbane, "NorLAG-lengde-faste.dta"), encoding = "utf-8")

lang <- read_stata( paste0(infilbane, "NorLAG-lengde-intervju.dta"), encoding = "utf-8")

bred <- read_stata( paste0(infilbane, "NorLAG-bredde.dta"), encoding = "utf-8")
bred %>% 
  group_by(iodeltakelse) %>% 
  summarise(n = n())


look_for(bred, "scale")

bred %>% 
  select(wb004_1) %>% 
  filter(wb004_1 <= 10) %>% 
  filter(!is.na(wb004_1)) %>% 
  summary()


summary(bred$wb004_1)

humval %>% 
  select(ioalder, wb004, wb006) %>% 
  filter(!is.na(wb004)) %>% 
  filter(!is.na(wb006)) %>% 
  glimpse()

ggplot(humval , aes(x = ioalder, y = wbswlssum5)) +
  geom_smooth()

dat_dict%>% 
  filter(col_nm == "wbswlssum5") %>% 
  filter(str_sub(tolower(label), 1, 10) == "ikke svart") %>% 
  pull(label)


dat_dict_na %>% 
  filter(col_nm == "wbswlssum5")



dat_dict_na <- dat_dict %>% 
  filter(str_sub(tolower(label), 1, 7) == "filter:" |
           str_sub(tolower(label), 1, 10) == "ikke svart" |
           tolower(label) %in% c("vil ikke svare",
                                 "deltok ikke i runden",
                                 "mangler data")) 

dat_dict %>% 
  filter(!str_sub(tolower(label), 1, 7) == "filter:") %>% 
  filter(value > 99) %>% 
  arrange(label) %>% 
  pull(label) %>% 
  unique() 

unique(dat_dict_na$label)