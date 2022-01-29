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
  select(origin, origin_iso3, origin_country, asylum, asylum_iso3, c,
         children, totalEndYear, typeOfDisaggregationAge)

View(demref2020.ori.asy.age)

t.disaggregation <- demref2020.ori.asy.age %>% 
  group_by(origin_country, asylum_country, typeOfDisaggregationAge) %>%
  summarise(totalEndYear = sum(totalEndYear),
            children = sum(children)) %>%
  pivot_wider(id_cols = c(origin_country, asylum_country),
              names_from = typeOfDisaggregationAge,
              values_from = totalEndYear)
  



####### II. Simple models without covariates #######


#### II.A Binomial models to estimate proportion of children and women only ##### 

### children only

m.children.1 <- brm(data = demref2020.ori.asy.age %>% filter(typeOfDisaggregationAge  == "Age"), family = binomial,
      children | trials(totalEndYear) ~ 1 + (1|asylum_iso3),
      prior(normal(0, 10), class = Intercept),
      seed = 10)


saveRDS(m.children.1, file = "m.children.1.rds")




 #### II.B Multinomial models to estimate full demographic composition ##### 





####### III. Add structure and covariates #######
