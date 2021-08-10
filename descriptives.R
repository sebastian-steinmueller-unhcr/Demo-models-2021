############################################ START ###########################################################

############################################ descriptives.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, Sebastian, steinmul@unhcr.org
#### Project: Demographic models 2021 (after GT)
#### Description: Data preparation plus descriptive analysis and visualisation of ASR 2020 REF + VDA demographic data 


rm(list=ls()) # clear workspace


##### I. Read data, packages etc ##### 

### packages
library(tidyverse)
library(readxl)
library(stringi)
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggExtra)
library(electoral)

### options
options(scipen = 999)

### Other R files
source("unhcr_style.R")
source("functions_demomodels.R")

### data
load("data/demref2020.RData")




##### II. Descriptive analysis of 2020 demo data set for REF and VDA ##### 



t.typeOfDisaggregationBroad <- demref2020 %>% 
  group_by(typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))
  
t.typeOfDisaggregation <- demref2020 %>% 
  group_by(typeOfDisaggregation) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))



# show type of disaggregation by origin (show for safe pathway countries in figure)

t.typeOfDisaggregationBroad.ori <- demref2020 %>% 
  group_by(`origin_Sub-region Name`, origin, origin_iso3, origin_country,  typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))

#  View(demref2020 %>% filter(origin == "VEN") %>% 
#         select(origin_country, asylum_country, totalEndYear, typeOfDisaggregationBroad,
#                female_0_4:female_12_17, female_18_59, female_60, femaleAgeUnknown, female,
#                male_0_4:male_12_17, male_18_59, male_60, maleAgeUnknown, male))
# View(demref2020 %>% filter(origin == "VEN", asylum == "CHL"))
# View(t.typeOfDisaggregationBroad.ori %>% filter(origin == "VEN"))

p.typeOfDisaggregationBroad.ori.safepw <- ggplot(data = t.typeOfDisaggregationBroad.ori %>% filter(origin %in% c("VEN", "SYR", "AFG", "ERT", "SOM", "IRN", "IRQ")),
                                            aes(x = typeOfDisaggregationBroad, y = freq.totalEndYear, fill = typeOfDisaggregationBroad)) +
  geom_bar( width= 0.5, stat="identity", position=position_dodge(width=0.6)) +
  facet_wrap(~  `origin_country`, ncol = 4, scales = "free")


# type of disaggregation by origin and asylum region

t.typeOfDisaggregationBroad.ori.asysubreg <- demref2020 %>% 
  group_by(`origin_Sub-region Name`, origin, origin_iso3, origin_country, `asylum_Sub-region Name`,  typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))


t.typeOfDisaggregationBroad.ori.asyreg <- demref2020 %>% 
  group_by(`origin_Sub-region Name`, origin, origin_iso3, origin_country, `asylum_Region Name`,  typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))

p.typeOfDisaggregationBroad.ori.asyreg <- ggplot(data = t.typeOfDisaggregationBroad.ori.asyreg %>% 
                                                    filter(origin %in% c("VEN", "MYA", "SYR", "AFG", "ERT", "SOM", "IRN", "IRQ"), 
                                                    !is.na(`asylum_Region Name`) ),
                                            aes(x = fct_reorder(`asylum_Region Name`, desc(`asylum_Region Name`)), 
                                                y = freq.totalEndYear, 
                                                fill = typeOfDisaggregationBroad )) +
  geom_bar( position = "stack", stat="identity") +
  coord_flip() +
  facet_wrap(~  `origin_country`, ncol = 4, scales = "free")

t.totalEndYear.ori.asyreg <- demref2020 %>% 
  group_by(origin, origin_iso3, origin_country, `asylum_Region Name`) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum)) 

p.totalEndYear.ori.asyreg  <- ggplot(data = t.totalEndYear.ori.asyreg %>% filter(origin %in% c("VEN", "MYA", "SYR", "AFG", "ERT", "SOM", "IRN", "IRQ"), !is.na(`asylum_Region Name`)),
                                            aes(x = fct_reorder(`asylum_Region Name`, desc(`asylum_Region Name`)) , y = totalEndYear)) +
  geom_bar( position = "dodge", stat="identity") +
  coord_flip() +
  facet_wrap(~  `origin_country`, ncol = 4, scales = "free")





demref2020 <- demref2020 %>%
  group_by(asylum_main_office_short, `asylum_Region Name`, `asylum_Sub-region Name`, asylum, asylum_iso3, asylum_country, 
           origin, origin_iso3, origin_country) %>%
  summarise_at(vars(female_0_4:unhcrAssistedEndYear), ~sum(., na.rm = T)) %>%
  ungroup() %>%
  mutate(agecov_1859 = rowSums(select(., female_0_4, female_5_11, female_12_17, female_18_59, female_60,
                               male_0_4, male_5_11, male_12_17, male_18_59, male_60), na.rm = T ))
  

demasy2020 <- demasy2020 %>%
  group_by(asylum_main_office_short, `asylum_Region Name`, `asylum_Sub-region Name`, asylum, asylum_iso3, asylum_country, 
           origin, origin_iso3, origin_country) %>%
  summarise_at(vars(female_0_4:unhcrAssistedEndYear), ~sum(., na.rm = T)) %>%
  ungroup() %>%
  mutate(agecov_1859 = rowSums(select(., female_0_4, female_5_11, female_12_17, female_18_59, female_60,
                                      male_0_4, male_5_11, male_12_17, male_18_59, male_60), na.rm = T ))





### delete

# # data set for population pyramids
# 
# 
# t.demref2020pyr.asy.ori <-  demref2020 %>% 
#   group_by(asylum_iso3, asylum, asylum_country, origin_iso3, origin, origin_country) %>%
#   summarise_at(vars(female_0_4, female_5_11, female_12_17, female_18_59, female_60, 
#                     male_0_4, male_5_11, male_12_17, male_18_59, male_60), ~sum(., na.rm = T)) %>% 
#   pivot_longer(cols = female_0_4:male_60, names_to = "agesexcat", values_to = "population") %>% 
#   separate(agesexcat, into = c("sex", "age"), sep = "_") %>% 
#   mutate(age = case_when(
#     age == 0 ~ "0-4",
#     age == 5 ~ "5-11",
#     age == 12 ~ "12-17",
#     age == 18 ~ "18-59",
#       age == 60 ~ "60+"
#   )) %>% 
#   mutate(age = factor(age, levels = c("0-4", "5-11", "12-17","18-59", "60+")),
#          sex = str_to_title(sex)) %>%
#   ungroup() %>%
#   group_by(asylum_iso3, asylum, asylum_country, origin_iso3, origin, origin_country) %>%  
#   mutate(populationprop = population/sum(population)*100) %>%
#   mutate(
#     populationprop = case_when(
#       sex == "Male" ~ (populationprop),
#       sex == "Female" ~ -1*(populationprop)
#     ))



############################################ END ###########################################################