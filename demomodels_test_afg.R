############################################ START ######################################################

############################################ demomodels_test_afg.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, Sebastian, steinmul@unhcr.org
#### Project: Demographic models 2021 (after GT)
#### Description: Test models with Afghan refugees

rm(list=ls()) # clear workspace


#### I. Read data, packages etc ##### 


### packages
library(tidyverse)
library(stringi)
library(magrittr)
library(brms)
#library(ggridges)
library(tidybayes)


### data

load("data/demref2020.RData")
rm(demref2020)


demref2020.ori.asy.age$agesex <- with(demref2020.ori.asy.age, cbind(female_0_4, female_5_11, female_12_17, female_18_59, female_60,
                                            male_0_4, male_5_11, male_12_17,male_18_59, male_60)) # needed for binomial models

demref2020.ori.asy.age <- demref2020.ori.asy.age %>% filter(origin_iso3 == "AFG") %>%
  select(origin, origin_iso3, origin_country, asylum, asylum_iso3, asylum_country,
         children, totalEndYear, typeOfDisaggregationAge)

View(demref2020.ori.asy.age)

####### II. Simple models without covariates #######


#### II.A Binomial models to estimate proportion of children and women only ##### 









#### II.B Multinomial models to estimate full demographic composition ##### 





####### III. Add structure and covariates #######
