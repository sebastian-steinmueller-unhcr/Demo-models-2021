############################################ START ###########################################################

############################################ descriptives.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, Sebastian, steinmul@unhcr.org
#### Project: Demographic models 2021 (after GT)
#### Description: Descriptive analysis and visualisation of ASR 2020 REF + VDA demographic data 


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
library(bookdown)

### options
options(scipen = 999)

## colours (https://intranet.unhcr.org/content/dam/unhcr/intranet/staff%20support/strategic/documents/english/brand-book/UNHCR-Brand%20Book.pdf)
unhcrPaletteBlue <- c("#0072BC", "#338EC9", "#66AAD7", "#99C7E4", "#CCE3F2")
unhcrPaletteBlack <- c("#000000", "#333333", "#666666", "#999999", "#CCCCCC")
unhcrPaletteYellow <- c("#FAEB00", "#FBEF33", "#FCF366", "#FDF799", "#FEFBCC")
unhcrPaletteWhite <- c("#FFFFFF")
unhcrPaletteRed <- c("#E73451")
unhcrPaletteGreen <- c("#00AB92")

### Other R files
source("unhcr_style.R")
source("functions_demomodels.R")

### data
load("data/demref2020.RData")



##### II. Descriptive analysis of 2020 demo data set for REF and VDA ##### 

### data by disaggregation level

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


### missingness by country of asylum
t.typeOfDisaggregation.asy <- demref2020 %>% 
  group_by(`asylum_Sub-region Name`, asylum_country,  typeOfDisaggregation) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nOrigin = n_distinct(origin_country)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.origin = nOrigin / sum(nOrigin))


t.asy.typeOfDissaggregation <- demref2020 %>% 
  group_by(typeOfDisaggregationBroad,   asylum_country) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nOrigin = n_distinct(origin_country)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.origin = nOrigin / sum(nOrigin))


t.asyregion.typeOfDissaggregation <- demref2020 %>% 
  group_by(typeOfDisaggregationBroad,   `asylum_Region Name`) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nOrigin = n_distinct(origin_country)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.origin = nOrigin / sum(nOrigin))


p.asyregion.typeOfDissaggregationBroad <- ggplot(data = t.asyregion.typeOfDissaggregation %>% 
                                       filter(!is.na(`asylum_Region Name`)),
                                     aes(x = `asylum_Region Name`, 
                                         y = freq.totalEndYear*100,
                                         fill = `asylum_Region Name`)) +
  geom_bar( stat="identity") +
  facet_wrap(~  `typeOfDisaggregationBroad`, ncol = 3, scales = "fixed")



t.typeOfDissaggregation.asyregion <- demref2020 %>% 
  group_by(`asylum_Region Name`, typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nOrigin = n_distinct(origin_country)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.origin = nOrigin / sum(nOrigin))



p.typeOfDissaggregationBroad.asyregion <- ggplot(data = t.typeOfDissaggregation.asyregion %>% 
                                                   filter(!is.na(`asylum_Region Name`)),
                                                 aes(x = `typeOfDisaggregationBroad`, 
                                                     y = freq.totalEndYear*100,
                                                     fill = `typeOfDisaggregationBroad`)) +
  geom_bar( stat="identity") +
  facet_wrap(~  `asylum_Region Name`, ncol = 5, scales = "fixed")


t.typeOfDissaggregationBroad.asyregionhcr <- demref2020 %>% 
  group_by(asylum_hcr_region, typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nOrigin = n_distinct(origin_country)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.origin = nOrigin / sum(nOrigin))


p.typeOfDissaggregationBroad.asyregionhcr <- ggplot(data = t.typeOfDissaggregationBroad.asyregionhcr %>% 
                                                   filter(!is.na(asylum_hcr_region)),
                                                 aes(x = `typeOfDisaggregationBroad`, 
                                                     y = freq.totalEndYear*100,
                                                     fill = `typeOfDisaggregationBroad`)) +
  geom_bar( stat="identity") +
  facet_wrap(~  `asylum_hcr_region`, ncol = 5, scales = "fixed")

### missingness by country of origin

# by major world region

t.typeOfDisaggregation.ori <- demref2020 %>% 
  group_by(`origin_Sub-region Name`, origin_country,  typeOfDisaggregation) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum_country)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))


t.ori.typeOfDissaggregation <- demref2020 %>% 
  group_by(typeOfDisaggregationBroad,   origin_country) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum_country)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))


t.oriregion.typeOfDissaggregation <- demref2020 %>% 
  group_by(typeOfDisaggregationBroad,   `origin_Region Name`) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum_country)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))


p.oriregion.typeOfDissaggregationBroad <- ggplot(data = t.oriregion.typeOfDissaggregation %>% 
                                                   filter(!is.na(`origin_Region Name`)),
                                                 aes(x = `origin_Region Name`, 
                                                     y = freq.totalEndYear*100,
                                                     fill = `origin_Region Name`)) +
  geom_bar( stat="identity") +
  facet_wrap(~  `typeOfDisaggregationBroad`, ncol = 3, scales = "fixed")



t.typeOfDissaggregation.oriregion <- demref2020 %>% 
  group_by(`origin_Region Name`, typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum_country)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))



p.typeOfDissaggregationBroad.oriregion <- ggplot(data = t.typeOfDissaggregation.oriregion %>% 
                                                   filter(!is.na(`origin_Region Name`)),
                                                 aes(x = `typeOfDisaggregationBroad`, 
                                                     y = freq.totalEndYear*100,
                                                     fill = `typeOfDisaggregationBroad`)) +
  geom_bar( stat="identity") +
  facet_wrap(~  `origin_Region Name`, ncol = 5, scales = "fixed")



# by UNHCR region

t.oriregionhcr.typeOfDissaggregation <- demref2020 %>% 
  group_by(typeOfDisaggregationBroad,   `origin_hcr_region`) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum_country)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))


p.oriregionhcr.typeOfDissaggregationBroad <- ggplot(data = t.oriregionhcr.typeOfDissaggregation %>% 
                                                   filter(!is.na(`origin_hcr_region`)),
                                                 aes(x = `origin_hcr_region`, 
                                                     y = freq.totalEndYear*100,
                                                     fill = `origin_hcr_region`)) +
  geom_bar( stat="identity") +
  facet_wrap(~  `typeOfDisaggregationBroad`, ncol = 3, scales = "fixed")


p.oriregionhcr.typeOfDissaggregationBroad2 <- ggplot(data = t.oriregionhcr.typeOfDissaggregation %>% 
                                                      filter(!is.na(`origin_hcr_region`), typeOfDisaggregationBroad!="Sex"),
                                                    aes(x = `origin_hcr_region`, 
                                                        y = freq.totalEndYear*100,
                                                        fill = `origin_hcr_region`)) +
  geom_bar( stat="identity") +
  facet_wrap(~  `typeOfDisaggregationBroad`, ncol = 3, scales = "fixed")


t.typeOfDissaggregation.oriregionhcr <- demref2020 %>% 
  group_by(`origin_hcr_region`, typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum_country)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))



p.typeOfDissaggregationBroad.oriregionhcr <- ggplot(data = t.typeOfDissaggregation.oriregionhcr %>% 
                                                   filter(!is.na(`origin_hcr_region`)),
                                                 aes(x = `typeOfDisaggregationBroad`, 
                                                     y = freq.totalEndYear*100,
                                                     fill = `typeOfDisaggregationBroad`)) +
  geom_bar( stat="identity") +
  facet_wrap(~  `origin_hcr_region`, ncol = 7, scales = "fixed")




### demographics with available data only ### 

## sex within age

t.obsDemographicsBroad.age <- demref2020 %>% 
  filter(typeOfDisaggregationBroad == "Sex/Age") %>%
  summarise_at(vars(female_0_4, female_5_11, female_12_17, female_18_59, female_60, 
                    male_0_4, male_5_11, male_12_17, male_18_59, male_60), ~sum(., na.rm = T)) %>% 
  pivot_longer(cols = female_0_4:male_60, names_to = "agesexcat", values_to = "population") %>% 
  separate(agesexcat, into = c("sex", "age"), sep = "_") %>% 
  mutate(age = case_when(
    age == 0 ~ "0-4",
    age == 5 ~ "5-11",
    age == 12 ~ "12-17",
    age == 18~ "18-59",
    age == 60 ~ "60+"
  )) %>% 
  mutate(age = factor(age, levels = c("0-4", "5-11", "12-17","18-59", "60+")),
         sex = str_to_title(sex)) %>%
  group_by(age) %>%
  mutate(populationprop = population/sum(population)*100)


p.obsDemographicsBroad.age <- ggplot(data = t.obsDemographicsBroad.age,
                                 aes(x = age, 
                                     y = populationprop,
                                     fill = sex)) +
  geom_bar( stat="identity", position = "stack") +
  geom_hline(yintercept = 50, linetype="dashed")



t.obsDemographicsBroad.age.asyregionhcr <- demref2020 %>% 
  filter(typeOfDisaggregationBroad == "Sex/Age") %>%
  group_by(asylum_hcr_region) %>%
  summarise_at(vars(female_0_4, female_5_11, female_12_17, female_18_59, female_60, 
                    male_0_4, male_5_11, male_12_17, male_18_59, male_60), ~sum(., na.rm = T)) %>% 
  pivot_longer(cols = female_0_4:male_60, names_to = "agesexcat", values_to = "population") %>% 
  separate(agesexcat, into = c("sex", "age"), sep = "_") %>% 
  mutate(age = case_when(
    age == 0 ~ "0-4",
    age == 5 ~ "5-11",
    age == 12 ~ "12-17",
    age == 18~ "18-59",
    age == 60 ~ "60+"
  )) %>% 
  mutate(age = factor(age, levels = c("0-4", "5-11", "12-17","18-59", "60+")),
         sex = str_to_title(sex)) %>%
  group_by(asylum_hcr_region, age) %>%
  mutate(populationprop = population/sum(population)*100)


p.obsDemographicsBroad.asyregionhcr <- ggplot(data = t.obsDemographicsBroad.age.asyregionhcr,
                                 aes(x = age, 
                                     y = populationprop,
                                     fill = sex)) +
  geom_bar( stat="identity", position = "stack") +
  facet_wrap(~ `asylum_hcr_region`, ncol = 5, scales = "fixed")


## age sex 

t.obsDemographicsBroad.short <- demref2020 %>% 
  filter(typeOfDisaggregationBroad == "Sex/Age") %>%
  summarise_at(vars(female_children, female_adults, male_children, male_adults), ~sum(., na.rm = T)) %>% 
  pivot_longer(cols = female_children:male_adults, names_to = "agesexcat", values_to = "population") %>% 
  separate(agesexcat, into = c("sex", "age"), sep = "_") %>% 
  # mutate(age = case_when(
  #   age == 0 ~ "0-4",
  #   age == 5 ~ "5-11",
  #   age == 12 ~ "12-17",
  #   age == 18~ "18-59",
  #   age == 60 ~ "60+"
  # )) %>% 
  mutate(populationprop = population/sum(population)*100)  %>%
  mutate(
    populationprop = case_when(
      sex == "male" ~ (populationprop),
      sex == "female" ~ -1*(populationprop)
    ))  %>%
  mutate(age = factor(age, levels = c("children", "adults")),
         sex = factor(sex, levels = c("female", "male"))
  )


p.obsDemographicsBroad.short <- ggplot(data = t.obsDemographicsBroad.short %>% mutate(
                                              y_min = -30, 
                                              y_max = 30
                                              ), 
                                        aes(x = age, y = populationprop, fill = sex)) + 
  geom_bar(data = t.obsDemographicsBroad.short %>% filter(sex == "female"), stat = "identity") + 
  geom_bar(data = t.obsDemographicsBroad.short %>% filter(sex == "male"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-30, 30, 10), 
                     labels = paste0(c(seq(from = 30, to = 0, by=-10), seq(from = 10, to = 30, by=10) ), "%")) +
  theme_minimal() +
  theme(axis.title.x=element_text(size=10, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_flip() + 
  scale_fill_manual(values=c("#006AB4", "#00AB92")) + 
  theme(legend.title = element_blank(), legend.position="bottom") +
  theme(text = element_text(size=10),
        legend.text=element_text(size=10),
        axis.text.y = element_text(size=10, angle = 0),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=10, angle = 0))  +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  theme(legend.position = "bottom") +
  theme(plot.margin = unit(c(0.2,2,0.2,2), "cm"))



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



# 
# 
# demref2020 <- demref2020 %>%
#   group_by(asylum_main_office_short, `asylum_Region Name`, `asylum_Sub-region Name`, asylum, asylum_iso3, asylum_country, 
#            origin, origin_iso3, origin_country) %>%
#   summarise_at(vars(female_0_4:unhcrAssistedEndYear), ~sum(., na.rm = T)) %>%
#   ungroup() %>%
#   mutate(agecov_1859 = rowSums(select(., female_0_4, female_5_11, female_12_17, female_18_59, female_60,
#                                male_0_4, male_5_11, male_12_17, male_18_59, male_60), na.rm = T ))
#   


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