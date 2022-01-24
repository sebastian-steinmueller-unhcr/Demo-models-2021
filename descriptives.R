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
library(knitr)
library(kableExtra)
library(gridExtra)
library(cepiigeodist)

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
load("data/wpp_age_group.RData")
load('data/neighbor.Rdata') 
load('data/distance.Rdata') 
load('data/WB_classifications_2020.Rdata')
### helper function
addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6,1), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9,1), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}


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

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

t.typeOfDisaggregationBroad.ori.asyreghcr <- demref2020 %>% mutate(asylum_hcr_region = recode_factor(asylum_hcr_region, `Middle East and North Africa` = 'MENA', `Asia and the Pacific` = 'A&P')) %>%
  group_by(`origin_Sub-region Name`, origin, origin_iso3, origin_country, asylum_hcr_region,  typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))

p.totalEndYear.ori.asyreghcr <- ggplot(data = t.typeOfDisaggregationBroad.ori.asyreghcr %>% 
                                                   filter(origin %in% c("VEN", "MYA", "SYR", "AFG", "ERT", "SOM", "IRN", "IRQ"), 
                                                          !is.na(asylum_hcr_region) ),
                                                 aes(x = fct_reorder(asylum_hcr_region, desc(asylum_hcr_region)), 
                                                     y = totalEndYear, 
                                                     fill = typeOfDisaggregationBroad )) +
  scale_y_continuous(n.breaks = 3, labels = function(x) format(x, scientific = TRUE))+
  geom_bar( position = "stack", stat="identity") +
  coord_flip() +
  facet_wrap(~  `origin_country`, ncol = 4, scales = "free")+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme(axis.text.y = element_text(angle = 90))

t.typeOfDisaggregationBroad.asy.orireghcr <- demref2020 %>% mutate(origin_hcr_region = recode_factor(origin_hcr_region, `Middle East and North Africa` = 'MENA', `Asia and the Pacific` = 'A&P')) %>%
  group_by(`asylum_Sub-region Name`, asylum, asylum_iso3, asylum_country, origin_hcr_region,  typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nOrigin = n_distinct(origin)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.origin = nOrigin / sum(nOrigin))

p.totalEndYear.asy.orireghcr <- ggplot(data = t.typeOfDisaggregationBroad.asy.orireghcr %>% 
                                         filter(asylum %in% c("TUR","COL","PAK","UGA","SUD","GFR","LEB","BGD")) %>% # Top 8 asylum country
                                         mutate(origin_hcr_region = ifelse(is.na(as.character(origin_hcr_region)),'NA',as.character(origin_hcr_region))),
                                       aes(x = fct_reorder(origin_hcr_region, desc(origin_hcr_region)), 
                                           y = totalEndYear, 
                                           fill = typeOfDisaggregationBroad )) +
  scale_y_continuous(n.breaks = 3, labels = function(x) format(x, scientific = TRUE))+
  geom_bar( position = "stack", stat="identity") +
  coord_flip() +
  facet_wrap(~  `asylum_country`, ncol = 4, scales = "free")+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme(axis.text.y = element_text(angle = 90))

### Compare refugee population's age distribution with origin and asylum country's age distribution

t.ageDistribution.asy <- demref2020 %>% filter(typeOfDisaggregationBroad == 'Sex/Age') %>% mutate_at(vars(14:40),~replace_na(.,0)) %>% group_by(asylum_iso3) %>%
  summarise(total_0_4 = sum(male_0_4+female_0_4), total_5_11 = sum(male_5_11+female_5_11), total_12_17 = sum(male_12_17+female_12_17), total_18_59 = sum(male_18_59+female_18_59), total_60 = sum(male_60+female_60)) %>%
  gather(key = 'age_group', value = 'population_asy',2:6) %>% group_by(asylum_iso3) %>% mutate(pct_asy = population_asy/sum(population_asy)) %>% select(-population_asy)

t.ageDistribution.ori <- demref2020 %>% filter(typeOfDisaggregationBroad == 'Sex/Age') %>% mutate_at(vars(14:40),~replace_na(.,0)) %>% group_by(origin_iso3) %>%
  summarise(total_0_4 = sum(male_0_4+female_0_4), total_5_11 = sum(male_5_11+female_5_11), total_12_17 = sum(male_12_17+female_12_17), total_18_59 = sum(male_18_59+female_18_59), total_60 = sum(male_60+female_60)) %>%
  gather(key = 'age_group', value = 'population_ori',2:6) %>% group_by(origin_iso3) %>% mutate(pct_ori = population_ori/sum(population_ori)) %>% select(-population_ori)

t.ageDistribution.pop <- wpp.age.group.pct %>% filter(year == 2020, area.id<900) %>% select(area, iso3, total_0_4 = pop0to4.pct, total_5_11 = pop5to11.pct, total_12_17 = pop12to17.pct, total_18plus = pop18plus.pct, total_60 = pop60plus.pct) %>%
  mutate(total_18_59 = total_18plus-total_60) %>% select(-total_18plus) %>% mutate_at(vars(3:7),~./100) %>%
  gather(key = 'age_group', value = 'population',3:7) %>% group_by(iso3) %>% mutate(pct_pop = population/sum(population)) %>% select(-population)

t.ageDistribution <- inner_join(t.ageDistribution.pop, t.ageDistribution.ori, by = c('iso3' = 'origin_iso3', 'age_group')) %>% inner_join(t.ageDistribution.asy, by = c('iso3' = 'asylum_iso3','age_group')) %>% 
  mutate(age_group = factor(age_group, levels = c('total_0_4', 'total_5_11', 'total_12_17','total_18_59','total_60'))) %>% gather(key = 'type', value = 'pct',4:6)

p.ageDistribution <- ggplot(data = t.ageDistribution %>%
                              filter(iso3 %in% c('SYR','VEN','AFG', 'TUR', 'COL','DEU')),
                            aes(x = age_group,
                                y = pct,
                                color = type,
                                group = type)) +
  geom_line(linetype = "dashed")+
  geom_point()+
  facet_wrap(~  `area`, ncol = 3, scales = "free")+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme(axis.text.x = element_text(angle = 45))

### the distribution of data coverage, origin and destination

t.coverageDistribution.asy <- demref2020 %>% group_by(asylum_iso3,asylum_country) %>%
  summarise(coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), ref.pop = sum(totalEndYear, na.rm = T))

p.coverageDistribution.asy <- ggplot(data = t.coverageDistribution.asy,
                                     aes(x = coverage)) +
  geom_histogram(binwidth = 1)

t.coverageDistribution.ori <- demref2020 %>% group_by(origin_iso3,origin_country) %>% 
  summarise(coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), ref.pop = sum(totalEndYear, na.rm = T))

p.coverageDistribution.ori <- ggplot(data = t.coverageDistribution.ori,
                                     aes(x = coverage)) +
  geom_histogram(binwidth = 1)

### compare the origin between 0 coverage and 100 coverage asylum country
t.compareOriAsylumCountry <- demref2020 %>% mutate(origin_hcr_region = recode_factor(origin_hcr_region, `Middle East and North Africa` = 'MENA', `Asia and the Pacific` = 'A&P')) %>%
  filter(asylum_iso3 %in% t.coverageDistribution.asy$asylum_iso3[t.coverageDistribution.asy$coverage == 100]) %>%
  group_by(origin_hcr_region,typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nOrigin = n_distinct(origin)) %>% 
  mutate(type = '100% coverage') %>% bind_rows(
    demref2020 %>% mutate(origin_hcr_region = recode_factor(origin_hcr_region, `Middle East and North Africa` = 'MENA', `Asia and the Pacific` = 'A&P')) %>%
      filter(asylum_iso3 %in% t.coverageDistribution.asy$asylum_iso3[t.coverageDistribution.asy$coverage == 0]) %>%
      group_by(origin_hcr_region,typeOfDisaggregationBroad) %>% 
      summarise(totalEndYear = sum(totalEndYear, na.rm = T),
                nOrigin = n_distinct(origin)) %>% 
      mutate(type = '0% coverage')
  )
              
p.compareOriAsylumCountry <- ggplot(data = t.compareOriAsylumCountry %>%  # Top 8 asylum country
                                         mutate(origin_hcr_region = ifelse(is.na(as.character(origin_hcr_region)),'NA',as.character(origin_hcr_region))),
                                       aes(x = fct_reorder(origin_hcr_region, desc(origin_hcr_region)), 
                                           y = totalEndYear, 
                                           fill = typeOfDisaggregationBroad )) +
  scale_y_continuous(n.breaks = 5, labels = function(x) format(x, scientific = TRUE))+
  geom_bar( position = "stack", stat="identity") +
  coord_flip() +
  facet_wrap(~  `type`, ncol =2)+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme(axis.text.y = element_text(angle = 90))            

t.compareAsyOriginCountry <- demref2020 %>% mutate(asylum_hcr_region = recode_factor(asylum_hcr_region, `Middle East and North Africa` = 'MENA', `Asia and the Pacific` = 'A&P')) %>%
  filter(origin_iso3 %in% t.coverageDistribution.ori$origin_iso3[t.coverageDistribution.ori$coverage >=50]) %>%
  group_by(asylum_hcr_region,typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nOrigin = n_distinct(origin)) %>% 
  mutate(type = '>=50% coverage') %>% bind_rows(
    demref2020 %>% mutate(asylum_hcr_region = recode_factor(asylum_hcr_region, `Middle East and North Africa` = 'MENA', `Asia and the Pacific` = 'A&P')) %>%
      filter(origin_iso3 %in% t.coverageDistribution.ori$origin_iso3[t.coverageDistribution.ori$coverage <50]) %>%
      group_by(asylum_hcr_region,typeOfDisaggregationBroad) %>% 
      summarise(totalEndYear = sum(totalEndYear, na.rm = T),
                nOrigin = n_distinct(origin)) %>% 
      mutate(type = '<50% coverage') 
  )

p.compareAsyOriginCountry <- ggplot(data = t.compareAsyOriginCountry %>%  
                                      mutate(asylum_hcr_region = ifelse(is.na(as.character(asylum_hcr_region)),'NA',as.character(asylum_hcr_region))),
                                    aes(x = fct_reorder(asylum_hcr_region, desc(asylum_hcr_region)), 
                                        y = totalEndYear, 
                                        fill = typeOfDisaggregationBroad )) +
  scale_y_continuous(n.breaks = 5, labels = function(x) format(x, scientific = TRUE))+
  geom_bar( position = "stack", stat="identity") +
  coord_flip() +
  facet_wrap(~  `type`, ncol =2)+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme(axis.text.y = element_text(angle = 90)) 


### Q2.Is origin country a predictor of the demographic composition of refugee populations, even if they live in different countries of asylum?

t.demCompSameOri <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>% group_by(origin_iso3, origin_country, asylum_iso3) %>%
  summarise(total_0_4 = sum(male_0_4+female_0_4), total_5_11 = sum(male_5_11+female_5_11), total_12_17 = sum(male_12_17+female_12_17), total_18_59 = sum(male_18_59+female_18_59), total_60 = sum(male_60+female_60),
            coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
  filter(coverage >= 50, poptotal > 1000) %>% top_n(6, poptotal) %>%
  gather(key = 'age_group', value = 'population',4:8)  %>% arrange(origin_iso3,asylum_iso3) %>% group_by(origin_iso3,asylum_iso3) %>%
  mutate(pct = population/sum(population),age_group = factor(age_group, levels = c('total_0_4', 'total_5_11', 'total_12_17','total_18_59','total_60')), asylum_iso3 = paste0(asylum_iso3,', ', addUnits(poptotal),', cov: ', coverage))

t.demCompSameOri.top8 <- t.demCompSameOri %>% filter(origin_iso3 %in% c('SYR','AFG','SSD', 'MMR', 'COD','SOM','SDN','CAF')) %>% mutate(origin_country = as.character(origin_country)) %>% split(f = .$origin_country)

po1 <- ggplot(data = t.demCompSameOri.top8$`Syrian Arab Republic`,
                            aes(x = age_group,
                                y = pct,
                                color = asylum_iso3,
                                group = asylum_iso3)) +
  geom_line(linetype = "dashed")+
  geom_point()+
  facet_wrap(~origin_country, ncol=1)+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30))

po8 <- po1 %+% t.demCompSameOri.top8$`Central African Republic`
po5 <- po1 %+% t.demCompSameOri.top8$`Democratic Republic of the Congo`
po4 <- po1 %+% t.demCompSameOri.top8$Myanmar
po6 <- po1 %+% t.demCompSameOri.top8$Somalia
po3 <- po1 %+% t.demCompSameOri.top8$`South Sudan`
po7 <- po1 %+% t.demCompSameOri.top8$Sudan
po2 <- po1 %+% t.demCompSameOri.top8$Afghanistan

p.demCompSameOri.top8 <- arrangeGrob(po1,po2,po3,po4,po5,po6,po7,po8, ncol = 2)

## age and sex
t.ageSexCompSameOri <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>% group_by(origin_iso3, origin_country, asylum_iso3) %>%
  summarise(male.0to4 = sum(male_0_4), male.5to11 = sum(male_5_11), male.12to17 = sum(male_12_17), male.18to59 = sum(male_18_59), male.60plus = sum(male_60),
            female.0to4 = sum(female_0_4),female.5to11 = sum(female_5_11), female.12to17 = sum(female_12_17), female.18to59 = sum(female_18_59), female.60plus = sum(female_60),
            coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
  filter(coverage >= 50, poptotal > 1000) %>% top_n(6, poptotal) %>%
  gather(key = 'age_group', value = 'population',4:13) %>% separate(age_group,c('Sex','Age'))  %>% arrange(origin_iso3,asylum_iso3) %>% group_by(origin_iso3,asylum_iso3) %>%
  mutate(pct = population/sum(population),Age = factor(Age, levels = c('0to4', '5to11', '12to17','18to59','60plus')), asylum_iso3 = paste0(asylum_iso3,', ', addUnits(poptotal),', cov: ', coverage))

t.ageSexCompSameOri.top8 <- t.ageSexCompSameOri %>% filter(origin_iso3 %in% c('SYR','AFG','SSD', 'MMR', 'COD','SOM','SDN','CAF')) %>% mutate(origin_country = as.character(origin_country)) %>% split(f = .$origin_country)


pASo1 <- ggplot(data = t.ageSexCompSameOri.top8$`Syrian Arab Republic`,
                aes(x = Age,
                    y = pct,
                    color = asylum_iso3,
                    group = asylum_iso3)) +
  geom_line(data = . %>% filter(Sex == 'male'), linetype = "dashed")+
  geom_line(data = . %>% filter(Sex == 'female'), aes(y = pct * -1), linetype = "dashed")+
  geom_point(data = . %>% filter(Sex == 'male'))+
  geom_point(data = . %>% filter(Sex == 'female'), aes(y = pct * -1),)+
  annotate("text", x = '0to4', y = 0.25, label = "Male")+
  annotate("text", x =  '0to4', y = -0.25, label = "Female")+
  coord_flip()+
  facet_wrap(~origin_country, ncol=1)+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme_minimal()

pASo2 <- pASo1 %+% t.ageSexCompSameOri.top8$Afghanistan
pASo3 <- pASo1 %+% t.ageSexCompSameOri.top8$`South Sudan`
pASo4 <- pASo1 %+% t.ageSexCompSameOri.top8$Myanmar
pASo5 <- pASo1 %+% t.ageSexCompSameOri.top8$`Democratic Republic of the Congo`
pASo6 <- pASo1 %+% t.ageSexCompSameOri.top8$Somalia

p.ageSexCompSameOri.top6 <- arrangeGrob(pASo1,pASo2,pASo3,pASo4,pASo5,pASo6 ,ncol = 2)

### Q3.Is country of asylum a predictor of the demographic composition of refugee populations, even if they come from different countries of origin?

t.demCompSameAsy <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>% group_by(asylum_iso3, asylum_country, origin_iso3) %>%
  summarise(total_0_4 = sum(male_0_4+female_0_4), total_5_11 = sum(male_5_11+female_5_11), total_12_17 = sum(male_12_17+female_12_17), total_18_59 = sum(male_18_59+female_18_59), total_60 = sum(male_60+female_60),
            coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
  filter(coverage >= 50, poptotal > 1000) %>% top_n(6, poptotal) %>%
  gather(key = 'age_group', value = 'population',4:8)  %>% arrange(asylum_iso3,origin_iso3) %>% group_by(asylum_iso3,origin_iso3) %>%
  mutate(pct = population/sum(population),age_group = factor(age_group, levels = c('total_0_4', 'total_5_11', 'total_12_17','total_18_59','total_60')), origin_iso3 = paste0(origin_iso3,', ', addUnits(poptotal),', cov: ', coverage))

t.demCompSameAsy.top8 <- t.demCompSameAsy %>% filter(asylum_iso3 %in% c('TUR','PAK','UGA', 'DEU', 'SDN','LBN','BGD','ETH')) %>% mutate(asylum_country = as.character(asylum_country)) %>% split(f = .$asylum_country)

pa1 <- ggplot(data = t.demCompSameAsy.top8$Turkey,
              aes(x = age_group,
                  y = pct,
                  color = origin_iso3,
                  group = origin_iso3)) +
  geom_line(linetype = "dashed")+
  geom_point()+
  facet_wrap(~asylum_country, ncol=1)+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30))

pa2 <- pa1 %+% t.demCompSameAsy.top8$Pakistan
pa3 <- pa1 %+% t.demCompSameAsy.top8$Uganda
pa4 <- pa1 %+% t.demCompSameAsy.top8$Germany
pa5 <- pa1 %+% t.demCompSameAsy.top8$Sudan
pa6 <- pa1 %+% t.demCompSameAsy.top8$Lebanon
pa7 <- pa1 %+% t.demCompSameAsy.top8$Bangladesh
pa8 <- pa1 %+% t.demCompSameAsy.top8$Ethiopia

p.demCompSameAsy.top8 <- arrangeGrob(pa1,pa2,pa3,pa4,pa5,pa6,pa7,pa8, ncol = 2)

## Age and sex

t.ageSexCompSameAsy <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>% group_by(asylum_iso3, asylum_country, origin_iso3) %>%
  summarise(male.0to4 = sum(male_0_4), male.5to11 = sum(male_5_11), male.12to17 = sum(male_12_17), male.18to59 = sum(male_18_59), male.60plus = sum(male_60),
            female.0to4 = sum(female_0_4),female.5to11 = sum(female_5_11), female.12to17 = sum(female_12_17), female.18to59 = sum(female_18_59), female.60plus = sum(female_60),
            coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
  filter(coverage >= 50, poptotal > 1000) %>% top_n(6, poptotal) %>%
  gather(key = 'age_group', value = 'population',4:13) %>% separate(age_group,c('Sex','Age'))  %>% arrange(asylum_iso3,origin_iso3) %>% group_by(asylum_iso3,origin_iso3) %>%
  mutate(pct = population/sum(population),Age = factor(Age, levels = c('0to4', '5to11', '12to17','18to59','60plus')), origin_iso3 = paste0(origin_iso3,', ', addUnits(poptotal),', cov: ', coverage))

t.ageSexCompSameAsy.top8 <- t.ageSexCompSameAsy %>% filter(asylum_iso3 %in% c('TUR','PAK','UGA', 'DEU', 'SDN','LBN','BGD','ETH')) %>% mutate(asylum_country = as.character(asylum_country)) %>% split(f = .$asylum_country)


pASa1 <- ggplot(data = t.ageSexCompSameAsy.top8$Turkey,
                aes(x = Age,
                    y = pct,
                    color = origin_iso3,
                    group = origin_iso3)) +
  geom_line(data = . %>% filter(Sex == 'male'), linetype = "dashed")+
  geom_line(data = . %>% filter(Sex == 'female'), aes(y = pct * -1), linetype = "dashed")+
  geom_point(data = . %>% filter(Sex == 'male'))+
  geom_point(data = . %>% filter(Sex == 'female'), aes(y = pct * -1),)+
  annotate("text", x = '0to4', y = 0.25, label = "Male")+
  annotate("text", x =  '0to4', y = -0.25, label = "Female")+
  coord_flip()+
  facet_wrap(~asylum_country, ncol=1)+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme_minimal()

pASa2 <- pASa1 %+% t.ageSexCompSameAsy.top8$Pakistan
pASa3 <- pASa1 %+% t.ageSexCompSameAsy.top8$Uganda
pASa4 <- pASa1 %+% t.ageSexCompSameAsy.top8$Germany
pASa5 <- pASa1 %+% t.ageSexCompSameAsy.top8$Sudan
pASa6 <- pASa1 %+% t.ageSexCompSameAsy.top8$Lebanon

p.ageSexCompSameAsy.top6 <- arrangeGrob(pASa1,pASa2,pASa3,pASa4,pASa5,pASa6 ,ncol = 2)

### Q4. Neighbors
# find_cty <- function(i){
#   all.neighbors %>% filter(country_iso3 == i) %>% pull(neighbor_iso3)
# }
# 
# t.demCompSameOri.nei <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>% group_by(origin_iso3, origin_country, asylum_iso3) %>%
#   summarise(total_0_4 = sum(male_0_4+female_0_4), total_5_11 = sum(male_5_11+female_5_11), total_12_17 = sum(male_12_17+female_12_17), total_18_59 = sum(male_18_59+female_18_59), total_60 = sum(male_60+female_60),
#             coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
#   mutate(origin_iso3 = as.character(origin_iso3), asylum_iso3 = as.character(asylum_iso3)) %>%
#   mutate(is.neighbor = ifelse(asylum_iso3 %in% find_cty(origin_iso3),1,0)) %>% filter(is.neighbor == 1, coverage > 1) %>% top_n(6, poptotal) %>%
#   gather(key = 'age_group', value = 'population',4:8)  %>% arrange(origin_iso3,asylum_iso3) %>% group_by(origin_iso3,asylum_iso3) %>%
#   mutate(pct = population/sum(population),age_group = factor(age_group, levels = c('total_0_4', 'total_5_11', 'total_12_17','total_18_59','total_60')), asylum_iso3 = paste0(asylum_iso3,', ', addUnits(poptotal),', cov: ', coverage))
# 
# t.demCompSameOri.top8.nei <- t.demCompSameOri.nei %>% filter(origin_iso3 %in% c('SYR','AFG','SSD', 'MMR', 'COD','SOM','SDN','CAF')) %>% mutate(origin_country = as.character(origin_country)) %>% split(f = .$origin_country)
# 
# pon1 <- ggplot(data = t.demCompSameOri.top8.nei$`Syrian Arab Republic`,
#               aes(x = age_group,
#                   y = pct,
#                   color = asylum_iso3,
#                   group = asylum_iso3)) +
#   geom_line(linetype = "dashed")+
#   geom_point()+
#   facet_wrap(~origin_country, ncol=1)+
#   labs(y = NULL, x= NULL, fill = NULL)+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 30))
# 
# pon8 <- pon1 %+% t.demCompSameOri.top8.nei$`Central African Republic`
# pon5 <- pon1 %+% t.demCompSameOri.top8.nei$`Democratic Republic of the Congo`
# pon4 <- pon1 %+% t.demCompSameOri.top8.nei$Myanmar
# pon6 <- pon1 %+% t.demCompSameOri.top8.nei$Somalia
# pon3 <- pon1 %+% t.demCompSameOri.top8.nei$`South Sudan`
# pon7 <- pon1 %+% t.demCompSameOri.top8.nei$Sudan
# pon2 <- pon1 %+% t.demCompSameOri.top8.nei$Afghanistan
# 
# p.demCompSameOri.top8.nei <- arrangeGrob(pon1,pon2,pon3,pon4,pon5,pon6,pon7,pon8, ncol = 2)


# t.demCompSameAsy.nei <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>% group_by(asylum_iso3, asylum_country, origin_iso3) %>%
#   summarise(total_0_4 = sum(male_0_4+female_0_4), total_5_11 = sum(male_5_11+female_5_11), total_12_17 = sum(male_12_17+female_12_17), total_18_59 = sum(male_18_59+female_18_59), total_60 = sum(male_60+female_60),
#             coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
#   mutate(origin_iso3 = as.character(origin_iso3), asylum_iso3 = as.character(asylum_iso3)) %>%
#   mutate(is.neighbor = ifelse(asylum_iso3 %in% find_cty(origin_iso3),1,0)) %>% filter(is.neighbor == 1, coverage > 1) %>% top_n(6, poptotal) %>%
#   gather(key = 'age_group', value = 'population',4:8)  %>% arrange(asylum_iso3,origin_iso3) %>% group_by(asylum_iso3,origin_iso3) %>%
#   mutate(pct = population/sum(population),age_group = factor(age_group, levels = c('total_0_4', 'total_5_11', 'total_12_17','total_18_59','total_60')), origin_iso3 = paste0(origin_iso3,', ', addUnits(poptotal),', cov: ', coverage))
# 
# t.demCompSameAsy.top8.nei <- t.demCompSameAsy.nei %>% filter(asylum_iso3 %in% c('TUR','PAK','UGA', 'DEU', 'SDN','LBN','BGD','ETH')) %>% mutate(asylum_country = as.character(asylum_country)) %>% split(f = .$asylum_country)
# 
# pan1 <- ggplot(data = t.demCompSameAsy.top8.nei$Turkey,
#               aes(x = age_group,
#                   y = pct,
#                   color = origin_iso3,
#                   group = origin_iso3)) +
#   geom_line(linetype = "dashed")+
#   geom_point()+
#   facet_wrap(~asylum_country, ncol=1)+
#   labs(y = NULL, x= NULL, fill = NULL)+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 30))
# 
# pan2 <- pan1 %+% t.demCompSameAsy.top8.nei$Pakistan
# pan3 <- pan1 %+% t.demCompSameAsy.top8.nei$Uganda
# pan4 <- pan1 %+% t.demCompSameAsy.top8.nei$Germany
# pan5 <- pan1 %+% t.demCompSameAsy.top8.nei$Sudan
# pan6 <- pan1 %+% t.demCompSameAsy.top8.nei$Lebanon
# pan7 <- pan1 %+% t.demCompSameAsy.top8.nei$Bangladesh
# pan8 <- pan1 %+% t.demCompSameAsy.top8.nei$Ethiopia
# 
# p.demCompSameAsy.top8.nei <- arrangeGrob(pan1,pan2,pan3,pan4,pan5,pan6,pan7,pan8, ncol = 2)



######### distance ##########

t.dist <- demref2020 %>% group_by(origin_iso3,asylum_iso3) %>%
  summarise(coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>% ungroup() %>%
  mutate(origin_iso3 = as.character(origin_iso3), asylum_iso3 = as.character(asylum_iso3)) %>% 
  left_join(distance_long, by = c('origin_iso3' = 'col', 'asylum_iso3' = 'row')) %>% filter(!is.na(dist))

t.dist.bracket <- t.dist %>% mutate(bracket = cut(dist, breaks=seq(0, 20000000,1000000), right = FALSE)) %>% group_by(bracket) %>% summarise(coverage = weighted.mean(coverage,poptotal),poptotal = sum(poptotal))

p.dist.point <- ggplot(data = t.dist, aes(x = dist, y = poptotal,alpha =coverage/100)) + geom_point() + scale_alpha_continuous(name = 'dem. data cov.', range = c(0.2,1))


t.dist.bracket.big <- t.dist %>% mutate(bracket = case_when(dist < 1000000 ~ '<1000Km',
                                                            dist >= 1000000  & dist < 5000000 ~ '1000-5000Km',
                                                            dist >= 5000000 ~ '>5000Km')) %>% group_by(bracket) %>% summarise(coverage = weighted.mean(coverage,poptotal),poptotal = sum(poptotal)) %>% mutate(bracket = factor(bracket,levels = c('<1000Km','1000-5000Km','>5000Km')))

p.dist.hist <- ggplot(data = t.dist.bracket.big, aes(x = bracket, y = poptotal,alpha =coverage/100)) + geom_bar(stat = 'identity') + scale_alpha_continuous(name = 'dem. data cov.', range = c(0.5,1))+ theme(axis.text.x = element_text(angle =45))


t.dist.bracket.big.world <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>%
  left_join(distance_long, by = c('origin_iso3' = 'col', 'asylum_iso3' = 'row')) %>% filter(!is.na(dist)) %>%
  mutate(bracket = case_when(dist < 1000000 ~ '<1000Km',
                             dist >= 1000000  & dist < 5000000 ~ '1000-5000Km',
                             dist >= 5000000 ~ '>5000Km')) %>%
  group_by(bracket) %>%
  summarise(male.0to4 = sum(male_0_4), male.5to11 = sum(male_5_11), male.12to17 = sum(male_12_17), male.18to59 = sum(male_18_59), male.60plus = sum(male_60),
            female.0to4 = sum(female_0_4),female.5to11 = sum(female_5_11), female.12to17 = sum(female_12_17), female.18to59 = sum(female_18_59), female.60plus = sum(female_60),
            coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
  gather(key = 'age_group', value = 'population',2:11) %>% separate(age_group,c('Sex','Age'))   %>% group_by(bracket) %>%
  mutate(pct = population/sum(population),Age = factor(Age, levels = c('0to4', '5to11', '12to17','18to59','60plus')),bracket = factor(bracket,levels = c('<1000Km','1000-5000Km','>5000Km')), bracket = paste0(bracket,', ', addUnits(poptotal),', cov: ', coverage))



p.dist.bracket.big.world <- ggplot(data = t.dist.bracket.big.world,
                aes(x = Age,
                    y = pct,
                    color = bracket,
                    group = bracket)) +
  geom_line(data = . %>% filter(Sex == 'male'), linetype = "dashed")+
  geom_line(data = . %>% filter(Sex == 'female'), aes(y = pct * -1), linetype = "dashed")+
  geom_point(data = . %>% filter(Sex == 'male'))+
  geom_point(data = . %>% filter(Sex == 'female'), aes(y = pct * -1),)+
  annotate("text", x = '0to4', y = 0.25, label = "Male")+
  annotate("text", x =  '0to4', y = -0.25, label = "Female")+
  coord_flip()+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme_minimal()

t.dist.bracket.big.ctry <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>%
  left_join(distance_long, by = c('origin_iso3' = 'col', 'asylum_iso3' = 'row')) %>% filter(!is.na(dist)) %>%
  mutate(bracket = case_when(dist < 1000000 ~ '<1000Km',
                             dist >= 1000000  & dist < 5000000 ~ '1000-5000Km',
                             dist >= 5000000 ~ '>5000Km')) %>%
  group_by(origin_iso3,origin_country, bracket) %>%
  summarise(male.0to4 = sum(male_0_4), male.5to11 = sum(male_5_11), male.12to17 = sum(male_12_17), male.18to59 = sum(male_18_59), male.60plus = sum(male_60),
            female.0to4 = sum(female_0_4),female.5to11 = sum(female_5_11), female.12to17 = sum(female_12_17), female.18to59 = sum(female_18_59), female.60plus = sum(female_60),
            coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
  gather(key = 'age_group', value = 'population',4:13) %>% separate(age_group,c('Sex','Age'))   %>% group_by(origin_iso3, origin_country,bracket) %>%
  mutate(pct = population/sum(population),Age = factor(Age, levels = c('0to4', '5to11', '12to17','18to59','60plus')), bracket = paste0(bracket,', ', addUnits(poptotal),', cov: ', coverage))


t.dist.bracket.big.ctry.top8 <- t.dist.bracket.big.ctry %>% filter(origin_iso3 %in% c('SYR','AFG','SSD', 'MMR', 'COD','SOM','SDN','CAF')) %>% mutate(origin_country = as.character(origin_country)) %>% split(f = .$origin_country)


pdista1 <- ggplot(data = t.dist.bracket.big.ctry.top8$`Syrian Arab Republic`,
                aes(x = Age,
                    y = pct,
                    color = bracket,
                    group = bracket)) +
  geom_line(data = . %>% filter(Sex == 'male'), linetype = "dashed")+
  geom_line(data = . %>% filter(Sex == 'female'), aes(y = pct * -1), linetype = "dashed")+
  geom_point(data = . %>% filter(Sex == 'male'))+
  geom_point(data = . %>% filter(Sex == 'female'), aes(y = pct * -1),)+
  annotate("text", x = '0to4', y = 0.25, label = "Male")+
  annotate("text", x =  '0to4', y = -0.25, label = "Female")+
  coord_flip()+
  facet_wrap(~origin_country, ncol=1)+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme_minimal()

pdista2 <- pdista1 %+% t.dist.bracket.big.ctry.top8$Afghanistan
pdista3 <- pdista1 %+% t.dist.bracket.big.ctry.top8$`South Sudan`
pdista4 <- pdista1 %+% t.dist.bracket.big.ctry.top8$Myanmar
pdista5 <- pdista1 %+% t.dist.bracket.big.ctry.top8$`Democratic Republic of the Congo`
pdista6 <- pdista1 %+% t.dist.bracket.big.ctry.top8$Somalia

p.dist.bracket.big.ctry.top6 <- arrangeGrob(pdista1,pdista2,pdista3,pdista4,pdista5,pdista6 ,ncol = 2)

##################### Neighboring Country ############################

t.neighbor <- demref2020 %>% group_by(origin_iso3,asylum_iso3) %>%
  summarise(coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>% ungroup() %>%
  mutate(origin_iso3 = as.character(origin_iso3), asylum_iso3 = as.character(asylum_iso3)) %>% 
  left_join(all.neighbors %>% mutate(is.neighbor = 1), by = c('origin_iso3' = 'country_iso3', 'asylum_iso3' = 'neighbor_iso3')) %>% mutate(is.neighbor = ifelse(is.na(is.neighbor),'No','Yes'))

t.neighbor.big <- t.neighbor %>% group_by(is.neighbor) %>% summarise(coverage = weighted.mean(coverage,poptotal),poptotal = sum(poptotal))

p.neighbor.hist <- ggplot(data = t.neighbor.big, aes(x = is.neighbor, y = poptotal,alpha =coverage/100)) + geom_bar(stat = 'identity') + scale_alpha_continuous(name = 'dem. data cov.',range = c(0.5,1))+ theme(axis.text.x = element_text(angle =45))



t.neighbor.world <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>%
  left_join(all.neighbors %>% mutate(is.neighbor = 1), by = c('origin_iso3' = 'country_iso3', 'asylum_iso3' = 'neighbor_iso3')) %>% mutate(is.neighbor = ifelse(is.na(is.neighbor),'No','Yes')) %>%
  group_by(is.neighbor) %>%
  summarise(male.0to4 = sum(male_0_4), male.5to11 = sum(male_5_11), male.12to17 = sum(male_12_17), male.18to59 = sum(male_18_59), male.60plus = sum(male_60),
            female.0to4 = sum(female_0_4),female.5to11 = sum(female_5_11), female.12to17 = sum(female_12_17), female.18to59 = sum(female_18_59), female.60plus = sum(female_60),
            coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
  gather(key = 'age_group', value = 'population',2:11) %>% separate(age_group,c('Sex','Age'))   %>% group_by(is.neighbor) %>%
  mutate(pct = population/sum(population),Age = factor(Age, levels = c('0to4', '5to11', '12to17','18to59','60plus')), is.neighbor = paste0(is.neighbor,', ', addUnits(poptotal),', cov: ', coverage))



p.neighbor.world <- ggplot(data = t.neighbor.world,
                                   aes(x = Age,
                                       y = pct,
                                       color = is.neighbor,
                                       group = is.neighbor)) +
  geom_line(data = . %>% filter(Sex == 'male'), linetype = "dashed")+
  geom_line(data = . %>% filter(Sex == 'female'), aes(y = pct * -1), linetype = "dashed")+
  geom_point(data = . %>% filter(Sex == 'male'))+
  geom_point(data = . %>% filter(Sex == 'female'), aes(y = pct * -1),)+
  annotate("text", x = '0to4', y = 0.25, label = "Male")+
  annotate("text", x =  '0to4', y = -0.25, label = "Female")+
  coord_flip()+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme_minimal()

t.neighbor.ctry <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>%
  left_join(all.neighbors %>% mutate(is.neighbor = 1), by = c('origin_iso3' = 'country_iso3', 'asylum_iso3' = 'neighbor_iso3')) %>% mutate(is.neighbor = ifelse(is.na(is.neighbor),'No','Yes')) %>%
  group_by(origin_iso3,origin_country, is.neighbor) %>%
  summarise(male.0to4 = sum(male_0_4), male.5to11 = sum(male_5_11), male.12to17 = sum(male_12_17), male.18to59 = sum(male_18_59), male.60plus = sum(male_60),
            female.0to4 = sum(female_0_4),female.5to11 = sum(female_5_11), female.12to17 = sum(female_12_17), female.18to59 = sum(female_18_59), female.60plus = sum(female_60),
            coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
  gather(key = 'age_group', value = 'population',4:13) %>% separate(age_group,c('Sex','Age'))   %>% group_by(origin_iso3, origin_country, is.neighbor) %>%
  mutate(pct = population/sum(population),Age = factor(Age, levels = c('0to4', '5to11', '12to17','18to59','60plus')), is.neighbor = paste0(is.neighbor,', ', addUnits(poptotal),', cov: ', coverage))


t.neighbor.ctry.top8 <- t.neighbor.ctry %>% filter(origin_iso3 %in% c('SYR','AFG','SSD', 'MMR', 'COD','SOM','SDN','CAF')) %>% mutate(origin_country = as.character(origin_country)) %>% split(f = .$origin_country)


pnghbr1 <- ggplot(data = t.neighbor.ctry.top8$`Syrian Arab Republic`,
                  aes(x = Age,
                      y = pct,
                      color = is.neighbor,
                      group = is.neighbor)) +
  geom_line(data = . %>% filter(Sex == 'male'), linetype = "dashed")+
  geom_line(data = . %>% filter(Sex == 'female'), aes(y = pct * -1), linetype = "dashed")+
  geom_point(data = . %>% filter(Sex == 'male'))+
  geom_point(data = . %>% filter(Sex == 'female'), aes(y = pct * -1),)+
  annotate("text", x = '0to4', y = 0.25, label = "Male")+
  annotate("text", x =  '0to4', y = -0.25, label = "Female")+
  coord_flip()+
  facet_wrap(~origin_country, ncol=1)+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme_minimal()

pnghbr2 <- pnghbr1 %+% t.neighbor.ctry.top8$Afghanistan
pnghbr3 <- pnghbr1 %+% t.neighbor.ctry.top8$`South Sudan`
pnghbr4 <- pnghbr1 %+% t.neighbor.ctry.top8$Myanmar
pnghbr5 <- pnghbr1 %+% t.neighbor.ctry.top8$`Democratic Republic of the Congo`
pnghbr6 <- pnghbr1 %+% t.neighbor.ctry.top8$Somalia

p.neighbor.ctry.top6 <- arrangeGrob(pnghbr1,pnghbr2,pnghbr3,pnghbr4,pnghbr5,pnghbr6 ,ncol = 2)

##################### Economic status ############################

t.income <- demref2020 %>% group_by(origin_iso3,asylum_iso3) %>%
  summarise(coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>% ungroup() %>%
  mutate(origin_iso3 = as.character(origin_iso3), asylum_iso3 = as.character(asylum_iso3)) %>% 
  left_join(wb.income %>% select(iso3, wb.income.2020.orig = wb.income.2020), by = c('origin_iso3' = 'iso3')) %>%
  left_join(wb.income %>% select(iso3, wb.income.2020.asy = wb.income.2020), by = c('asylum_iso3' = 'iso3')) %>% 
  filter(!is.na( wb.income.2020.asy) & !is.na( wb.income.2020.orig)) %>%
  mutate(status.change = case_when(as.integer(wb.income.2020.orig) < as.integer(wb.income.2020.asy) ~ 'Higher income lvl',
                                   as.integer(wb.income.2020.orig) > as.integer(wb.income.2020.asy) ~ 'Lower income lvl',
                                   as.integer(wb.income.2020.orig) == as.integer(wb.income.2020.asy) ~ 'Same income lvl'))

t.income.big <- t.income %>% group_by(status.change) %>% summarise(coverage = weighted.mean(coverage,poptotal),poptotal = sum(poptotal)) %>% mutate(status.change = factor(status.change, levels = c('Higher income lvl', 'Same income lvl', 'Lower income lvl')))

p.income.hist <- ggplot(data = t.income.big, aes(x = status.change, y = poptotal,alpha =coverage/100)) + geom_bar(stat = 'identity')+scale_alpha_continuous(name ='dem. data cov.',  range = c(0.5,1)) + theme(axis.text.x = element_text(angle =45))



t.income.world <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>%
  left_join(wb.income %>% select(iso3, wb.income.2020.orig = wb.income.2020), by = c('origin_iso3' = 'iso3')) %>%
  left_join(wb.income %>% select(iso3, wb.income.2020.asy = wb.income.2020), by = c('asylum_iso3' = 'iso3')) %>% 
  filter(!is.na( wb.income.2020.asy) & !is.na( wb.income.2020.orig)) %>%
  mutate(status.change = case_when(as.integer(wb.income.2020.orig) < as.integer(wb.income.2020.asy) ~ 'Higher income lvl',
                                   as.integer(wb.income.2020.orig) > as.integer(wb.income.2020.asy) ~ 'Lower income lvl',
                                   as.integer(wb.income.2020.orig) == as.integer(wb.income.2020.asy) ~ 'Same income lvl')) %>%
  group_by(status.change) %>%
  summarise(male.0to4 = sum(male_0_4), male.5to11 = sum(male_5_11), male.12to17 = sum(male_12_17), male.18to59 = sum(male_18_59), male.60plus = sum(male_60),
            female.0to4 = sum(female_0_4),female.5to11 = sum(female_5_11), female.12to17 = sum(female_12_17), female.18to59 = sum(female_18_59), female.60plus = sum(female_60),
            coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
  gather(key = 'age_group', value = 'population',2:11) %>% separate(age_group,c('Sex','Age'))   %>% group_by(status.change) %>%
  mutate(pct = population/sum(population),Age = factor(Age, levels = c('0to4', '5to11', '12to17','18to59','60plus')), status.change = paste0(status.change,', ', addUnits(poptotal),', cov: ', coverage))



p.income.world <- ggplot(data = t.income.world,
                           aes(x = Age,
                               y = pct,
                               color = status.change,
                               group = status.change)) +
  geom_line(data = . %>% filter(Sex == 'male'), linetype = "dashed")+
  geom_line(data = . %>% filter(Sex == 'female'), aes(y = pct * -1), linetype = "dashed")+
  geom_point(data = . %>% filter(Sex == 'male'))+
  geom_point(data = . %>% filter(Sex == 'female'), aes(y = pct * -1),)+
  annotate("text", x = '0to4', y = 0.25, label = "Male")+
  annotate("text", x =  '0to4', y = -0.25, label = "Female")+
  coord_flip()+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme_minimal()

t.income.ctry <- demref2020 %>% mutate_at(vars(14:40),~replace_na(.,0)) %>%
  left_join(wb.income %>% select(iso3, wb.income.2020.orig = wb.income.2020), by = c('origin_iso3' = 'iso3')) %>%
  left_join(wb.income %>% select(iso3, wb.income.2020.asy = wb.income.2020), by = c('asylum_iso3' = 'iso3')) %>% 
  filter(!is.na( wb.income.2020.asy) & !is.na( wb.income.2020.orig)) %>%
  mutate(status.change = case_when(as.integer(wb.income.2020.orig) < as.integer(wb.income.2020.asy) ~ 'Higher income lvl',
                                   as.integer(wb.income.2020.orig) > as.integer(wb.income.2020.asy) ~ 'Lower income lvl',
                                   as.integer(wb.income.2020.orig) == as.integer(wb.income.2020.asy) ~ 'Same income lvl')) %>%
  group_by(origin_iso3,origin_country, status.change) %>%
  summarise(male.0to4 = sum(male_0_4), male.5to11 = sum(male_5_11), male.12to17 = sum(male_12_17), male.18to59 = sum(male_18_59), male.60plus = sum(male_60),
            female.0to4 = sum(female_0_4),female.5to11 = sum(female_5_11), female.12to17 = sum(female_12_17), female.18to59 = sum(female_18_59), female.60plus = sum(female_60),
            coverage = round(100*sum(totalEndYear[typeOfDisaggregationBroad == 'Sex/Age'], na.rm = T)/sum(totalEndYear, na.rm = T),1), poptotal = sum(totalEndYear, na.rm = T)) %>%
  gather(key = 'age_group', value = 'population',4:13) %>% separate(age_group,c('Sex','Age'))   %>% group_by(origin_iso3, origin_country, status.change) %>%
  mutate(pct = population/sum(population),Age = factor(Age, levels = c('0to4', '5to11', '12to17','18to59','60plus')), status.change = paste0(status.change,', ', addUnits(poptotal),', cov: ', coverage))


t.income.ctry.top8 <- t.income.ctry %>% filter(origin_iso3 %in% c('SYR','AFG','SSD', 'MMR', 'COD','SOM','SDN','CAF')) %>% mutate(origin_country = as.character(origin_country)) %>% split(f = .$origin_country)


pincome1 <- ggplot(data = t.income.ctry.top8$`Syrian Arab Republic`,
                  aes(x = Age,
                      y = pct,
                      color = status.change,
                      group = status.change)) +
  geom_line(data = . %>% filter(Sex == 'male'), linetype = "dashed")+
  geom_line(data = . %>% filter(Sex == 'female'), aes(y = pct * -1), linetype = "dashed")+
  geom_point(data = . %>% filter(Sex == 'male'))+
  geom_point(data = . %>% filter(Sex == 'female'), aes(y = pct * -1),)+
  annotate("text", x = '0to4', y = 0.25, label = "Male")+
  annotate("text", x =  '0to4', y = -0.25, label = "Female")+
  coord_flip()+
  facet_wrap(~origin_country, ncol=1)+
  labs(y = NULL, x= NULL, fill = NULL)+
  theme_minimal()

pincome2 <- pincome1 %+% t.income.ctry.top8$Afghanistan
pincome3 <- pincome1 %+% t.income.ctry.top8$`South Sudan`
pincome4 <- pincome1 %+% t.income.ctry.top8$Myanmar
pincome5 <- pincome1 %+% t.income.ctry.top8$`Democratic Republic of the Congo`
pincome6 <- pincome1 %+% t.income.ctry.top8$Somalia

p.income.ctry.top6 <- arrangeGrob(pincome1,pincome2,pincome3,pincome4,pincome5,pincome6 ,ncol = 2)



##################### Intra- vs inter-country of asylum variance of demographics ############################

### check whether there is significant variation on sub-national level in same origin population

## example Afghanistan origin with complete country of asylum data only


demref2020.oriAfg.compl <- demref2020 %>%
  filter(origin_iso3 == "AFG", typeOfDisaggregationBroad == "Sex/Age" ) %>%
  mutate(femaleProp = female / totalEndYear,
         childrenProp = children / totalEndYear)
  
# what are good measures of variance? a) % female, b) % children

# inter-country variation

t.oriAfg.asy.compl <- demref2020.oriAfg.compl %>%
  group_by(asylum_iso3, asylum_country) %>%
  summarise(totalEndYear = sum(totalEndYear),
            female = sum(female),
            children = sum(children)) %>%
  mutate(femaleProp = female / totalEndYear,
         childrenProp = children / totalEndYear)

sd(t.oriAfg.asy.compl$femaleProp) # 0.23
sd(t.oriAfg.asy.compl$childrenProp) # 0.20

# mean intra-country variation
t.oriAfg.asy.var <- demref2020.oriAfg.compl %>%
  group_by(asylum_iso3) %>%
  summarise(femalePropSD = sd(femaleProp),
            childrenPropSD = sd(childrenProp),
            totalEndYear = sum(totalEndYear))

mean(t.oriAfg.asy.var$femalePropSD, na.rm = T) # 0.20
mean(t.oriAfg.asy.var$childrenPropSD, na.rm = T) # 0.16
weighted.mean(t.oriAfg.asy.var$femalePropSD, w = t.oriAfg.asy.var$totalEndYear, na.rm = T) # 0.20
weighted.mean(t.oriAfg.asy.var$childrenPropSD, w = t.oriAfg.asy.var$totalEndYear, na.rm = T) # 0.16


 View(demref2020.oriAfg.compl %>% filter(asylum == "PAK") %>% 
       select(location, urbanRural, accommodationType, femaleProp, childrenProp, totalEndYear))


## example Syria origin with complete country of asylum data only


demref2020.oriSyr.compl <- demref2020 %>%
  filter(origin_iso3 == "SYR", typeOfDisaggregationBroad == "Sex/Age" ) %>%
  mutate(femaleProp = female / totalEndYear,
         childrenProp = children / totalEndYear)


# inter-country variation

t.oriSyr.asy.compl <- demref2020.oriSyr.compl %>%
  group_by(asylum_iso3, asylum_country) %>%
  summarise(totalEndYear = sum(totalEndYear),
            female = sum(female),
            children = sum(children)) %>%
  mutate(femaleProp = female / totalEndYear,
         childrenProp = children / totalEndYear)

sd(t.oriSyr.asy.compl$femaleProp) # 0.21
sd(t.oriSyr.asy.compl$childrenProp) # 0.19

# mean intra-country variation
t.oriSyr.asy.var <- demref2020.oriSyr.compl %>%
  group_by(asylum_iso3) %>%
  summarise(femalePropSD = sd(femaleProp),
            childrenPropSD = sd(childrenProp),
            totalEndYear = sum(totalEndYear))

mean(t.oriSyr.asy.var$femalePropSD, na.rm = T) # 0.17
mean(t.oriSyr.asy.var$childrenPropSD, na.rm = T) # 0.15

weighted.mean(t.oriSyr.asy.var$femalePropSD, w = t.oriSyr.asy.var$totalEndYear, na.rm = T) # 0.17
weighted.mean(t.oriSyr.asy.var$childrenPropSD,w = t.oriSyr.asy.var$totalEndYear, na.rm = T) # 0.15


# View(demref2020.oriSyr.compl %>% filter(asylum == "ARE") %>% 
#       select(location, urbanRural, accommodationType, femaleProp, childrenProp, totalEndYear))











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