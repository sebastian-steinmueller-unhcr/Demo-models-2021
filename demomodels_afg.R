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

### functions
source("functions_demomodels.R")



### data
load("data/demref2020.RData")
rm(demref2020)


# demref2020.ori.asy.age$agesex <- with(demref2020.ori.asy.age, cbind(female_0_4, female_5_11, female_12_17, female_18_59, female_60,
#                                             male_0_4, male_5_11, male_12_17,male_18_59, male_60)) # needed for multinomial models

demref2020.ori.asy.age <- demref2020.ori.asy.age %>% 
  filter(origin_iso3 == "AFG") %>%
  select(origin, origin_iso3, origin_country, asylum, asylum_iso3, asylum_country,
         children, totalEndYear, typeOfDisaggregationAge)

# View(demref2020.ori.asy.age %>% filter(typeOfDisaggregationAge  == "Age"))

t.disaggregation <- demref2020.ori.asy.age %>% 
  group_by(origin_country, asylum_country, typeOfDisaggregationAge) %>%
  summarise(totalEndYear = sum(totalEndYear),
            children = sum(children)) %>%
  pivot_wider(id_cols = c(origin_country, asylum_country),
              names_from = typeOfDisaggregationAge,
              values_from = totalEndYear) %>% 
  arrange(asylum_country, None)
  

demref2020.ori.asy.age.dat <- demref2020.ori.asy.age %>% filter(typeOfDisaggregationAge  == "Age")

demref2020.ori.asy.age.mis <- demref2020.ori.asy.age %>% filter(typeOfDisaggregationAge  == "None")

####### II. Simple models without covariates #######


#### II.A Binomial models to estimate proportion of children and women only ##### 


#### Model 0: binomial children with fixed intercept, no multilevel structure over countries of asylum

### prior predictive simulation

## normal(0, 1.5)

# simulate thetas 

priorint.child.0.norm.0.1_5 <- invlogit(rnorm(1000, mean = 0, sd = 1.5)) # simulate 1000 intercepts (= probability of child)
hist(priorint.child.0.norm.0.1_5) # nearly uniform but slightly less mass on extreme margins - generally sensible?

# for each country of asylum with available data, simulate 1 data points (= #of children) per theta for given number of refugees

# demref2020.ori.asy.age.dat$totalEndYear

priorpred.child.0 <- matrix(nrow = dim(demref2020.ori.asy.age.dat)[1], ncol = 10000) 



for(j in 1:length(priorint.child.0.norm.0.1_5)){
k = j*10-9
priorpred.child.0[,k:(k+9)] <- t(sapply(demref2020.ori.asy.age.dat$totalEndYear, 
                                        FUN = function(x){ rbinom(n=10, size= x, 
                                                            prob = priorint.child.0.norm.0.1_5[j])}))
  }


priorpred.child.0 <- cbind(demref2020.ori.asy.age.dat, priorpred.child.0)

hist(as.numeric(priorpred.child.0 %>% filter(asylum_iso3 == "PAK") %>% 
                  select(`1`:`10000`)), breaks = 100)



priorpred.child.0_check <- data.frame(theta = rep(priorint.child.0.norm.0.1_5, times = 1, each = 10),
                                      simulations = as.numeric(priorpred.child.0 %>% filter(asylum_iso3 == "PAK") %>% 
                                                                 select(`1`:`10000`))) %>% 
  arrange(simulations)



## beta(2, 3)

# simulate thetas 

priorint.child.0.beta.2.3 <- rbeta(1000, 2, 3) # simulate 1000 intercepts (= probability of child)
hist(priorint.child.0.beta.2.3) # nearly uniform but slightly less mass on extreme margins - generally sensible?

# for each country of asylum with available data, simulate 1 data points (= #of children) per theta for given number of refugees

# demref2020.ori.asy.age.dat$totalEndYear

priorpred.child.0.beta23 <- matrix(nrow = dim(demref2020.ori.asy.age.dat)[1], ncol = 10000) 



for(j in 1:length(priorint.child.0.beta.2.3)){
  k = j*10-9
  priorpred.child.0.beta23[,k:(k+9)] <- t(sapply(demref2020.ori.asy.age.dat$totalEndYear, 
                                          FUN = function(x){ rbinom(n=10, size= x, 
                                                                    prob = priorint.child.0.beta.2.3[j])}))
}


priorpred.child.0.beta23 <- cbind(demref2020.ori.asy.age.dat, priorpred.child.0.beta23)

hist(as.numeric(priorpred.child.0.beta23 %>% filter(asylum_iso3 == "PAK") %>% 
                  select(`1`:`10000`)), breaks = 100)



priorpred.child.0_check <- data.frame(theta = rep(priorint.child.0.norm.0.1_5, times = 1, each = 10),
                                      simulations = as.numeric(priorpred.child.0 %>% filter(asylum_iso3 == "PAK") %>% 
                                                                 select(`1`:`10000`))) %>% 
  arrange(simulations)



#### Model 1: binomial children with variable intercepts over countries of asylum

## prior predictive simulation

# simulate theta directly to start with

priortheta.child.1 <- invlogit(rnorm(1000, mean = 0, sd = 10))
hist(priortheta.child.1)

# simulate predictive priors

priorpred.child.1<- brm(children | trials(totalEndYear) ~ 1 + (1|asylum_iso3),
                    family = binomial(link = "logit"),
                    prior(normal(0, 10), class = Intercept),
                    sample_prior = "only",
                    data = demref2020.ori.asy.age %>% filter(typeOfDisaggregationAge  == "Age"))



## model

m.child.1 <- brm(data = demref2020.ori.asy.age %>% filter(typeOfDisaggregationAge  == "Age"), family = binomial,
                children | trials(totalEndYear) ~ 1 + (1|asylum_iso3),
                prior(normal(0, 10), class = Intercept),
                seed = 10)


saveRDS(m.child.1, file = "m.child.1.rds")




 #### II.B Multinomial models to estimate full demographic composition ##### 





####### III. Add structure and covariates #######
