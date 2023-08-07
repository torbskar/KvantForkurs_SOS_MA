
library(tidyverse)
library(haven)

infilbane <- "C:/Users/torbskar/OneDrive - Universitetet i Oslo/Dokumenter/Undervisning/SOS4020_forkurs/data2023/rOBH3y10/"

register <- read_stata( paste0(infilbane, "NorLAG-lengde-register.dta"), encoding = "utf-8") %>% 
  select(ref_nr, year, 
         inpgivinnt, inwoverfor, inftryg, inwyrkinnt, inwoverfor,    # pensj_ft overf_ft penal_ft penuf_ft penin_ft 
         inkode217, inkode218 ) %>% 
  mutate(inpgivinnt = ifelse(inpgivinnt == 999999999, NA, inpgivinnt),
         inwoverfor = ifelse(inwoverfor == 999999999, NA, inwoverfor),
         inftryg = ifelse(inftryg == 999999999, NA, inftryg)) %>% 
  drop_na()


glimpse(register)
dim(register)

table(as_factor(norlag$iodeltakelse))

glimpse(norlag[,1:10])

summary(as.numeric(norlag$ioalder))

# gen ss_con=va_bhvconsp if round==2  
# gen ss_ope=va_bhvotcp  if round==2
# gen ss_sha=va_bhvsenp  if round==2  
# gen ss_str=va_bhvstrp  if round==2  


look_for(norlag, "utdanning")
# physical health: hcPCS12
# self-enhancement: vahvssen
# self-transcendence: vahvsstr
# Openness to change: vahvsotc 
# Conservation: vahvscon 

library(labelled)

memisc::labels(norlag$utdanning)

table(norlag$iopafyll)

norlag %>% 
  summarise(n = n_distinct(ref_nr))

# Leser inn data. Bruker labelled for enklere oversettelse av stata-kode
norlag <- readRDS("data/norlag_labelled.rds") %>% 
  select(ref_nr, round, iodeltakelse, iointervjuaar,  # ref_nr round  io_intervjuyr  
         iofodselsaar, ioalder, iokjonn,              # io_ioalder  
         wb007, wr004,                                # wb007 wr004
         hcPCS12, starts_with("vahv")                 # hcPCS12 va_bhv*
         #vahvssen, vahvsstr, vahvsotc, vahvscon
         ) %>% 
  filter(round > 1, iodeltakelse %in% c(1,3)) %>% 
  mutate(alder = as.numeric(ioalder), 
         satis = as.numeric(wb007)) %>% 
  mutate(ald10 = alder/10, 
         ald2 = ald10^2,
         ald3 = ald10^3,
         year = iointervjuaar) %>% 
  filter(alder < 86, satis < 15, wr004 <= 5, !is.na(wr004), !is.na(satis))  %>% 
  arrange(ref_nr, round, year) %>% 
  filter(complete.cases(.))  


# Kobler på register, beholder bare treff på begge.
norlagreg <- merge(norlag, register, by = c("ref_nr", "year") ) %>% 
  mutate(hcPCS12 = hcPCS12/10,
         fyshel1 = hcPCS12) %>% 
  group_by(ref_nr) %>% 
  mutate(antobs = n()) %>%      # Filtrerer på de som har akkurat to observasjoner. Altså: deltar i begge runder. 
  filter(antobs == 2) %>% 
  select(-antobs) %>% 
  group_by(ref_nr) %>% 
  mutate(pensj_ft = mean(inftryg),    #"Pensjoner folketrygden"
         overf_ft = mean(inwoverfor), #"Overføringer i alt"
         penal_ft = mean(inkode217),  # "Alderspensjon"
         penuf_ft = mean(inkode218),  # "Uførepensjon"
         penin_ft = mean(inpgivinnt)  #"Pensjonsgivende inntekt"
  )
  

dim(norlagreg)

summary(norlagreg$alder)

# Utvalgsstørrelse og personer ####
norlagreg %>% 
  group_by(round) %>% 
  ungroup() %>% 
  summarise(personer = n_distinct(ref_nr), observasjoner = n())

table(is.na(norlagreg$satis))

# Fig 2 ####

glimpse(norlagreg)

library(fixest)
norlagreg %>% 
  filter(iokjonn == 2) %>% 
  head()

summary(norlagreg$satis)

# separate FE-models
feols_model_m <- feols(satis ~ ald10 + ald2 + ald3 | ref_nr , cluster = ~ref_nr,  
                       data = filter(norlagreg, iokjonn == 1))
feols_model_f <- feols(satis ~ ald10 + ald2 + ald3 | ref_nr , cluster = ~ref_nr,
                       data = filter(norlagreg, iokjonn == 2))

library(plm)
feols_model_m <- plm(satis ~ ald10 + ald2 + ald3, model = "within", index = "ref_nr",  
               data = filter(norlagreg, iokjonn == 1))
feols_model_f <- plm(satis ~ ald10 + ald2 + ald3, model = "within", index = "ref_nr",  
                     data = filter(norlagreg, iokjonn == 2))

glimpse(norlagreg)
pred <- norlagreg %>% 
  #filter(round == 1) %>% 
  mutate( pred = ifelse(iokjonn == 1, 
                        predict(feols_model_m, type = "response"),
                        predict(feols_model_f, type = "response")))

agg <- pred %>% 
  #filter(round == 3) %>% 
  mutate(iokjonn = as_factor(iokjonn)) %>% 
  group_by(alder, iokjonn) %>% 
  summarise(pred = mean(pred), observert = mean(satis)) 
  

ggplot(agg , aes(x = alder, y = observert, group = as_factor(iokjonn), col = as_factor(iokjonn))) +
  stat_smooth(method = "lm", se = F, formula = y ~ x + I(x^2) + I(x^3), size = 1) +
  geom_smooth(se = F)+
  xlim(40, 85)+
  ylim(7.5, 9.5)


lm1 <- lm(satis ~ ald10 + ald2 + ald3, data = norlagreg)

summary(lm1)


