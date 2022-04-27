############################################ START ######################################################

############################################ demomodels_afg.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, Sebastian, steinmul@unhcr.org
#### Project: Demographic models 2021 (after GT)
#### Description: Test models with Afghan refugees

rm(list=ls()) # clear workspace

# set seed
set.seed(1539)

#### I. Read data, packages etc ##### 


### packages
library(tidyverse)
library(stringi)
library(magrittr)
library(brms)
library(tidybayes)
library(ProbBayes)

### functions
source("functions_demomodels.R")



### data
load("data/demref2020.RData")
rm(demref2020)


# demref2020.ori.asy.age$agesex <- with(demref2020.ori.asy.age, cbind(female_0_4, female_5_11, female_12_17, female_18_59, female_60,
#                                             male_0_4, male_5_11, male_12_17,male_18_59, male_60)) # needed for multinomial models

demref2020.ori.asy.age <- demref2020.ori.asy.age %>% 
  filter(origin_iso3 == "AFG") %>%
  select(origin, origin_iso3, origin_country, asylum, asylum_iso3, asylum_country, `asylum_Region Name`,
         children, totalEndYear, typeOfDisaggregationAge) %>% # add-hoc adding land neighbours for Afghanistan as covariate
  mutate(neighbour = case_when(
    asylum_iso3 %in% c("PAK", "TJK", "CHN", "TKM", "UZB", "IRN") ~ 1,
    !(asylum_iso3 %in% c("PAK", "TJK", "CHN", "TKM", "UZB", "IRN")) ~ 0
    )
  ) %>% 
  rename(asylum_RegionName = `asylum_Region Name`)

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


##### II.A Binomial models to estimate proportion of children and women only ##### 


#### Model 0: binomial children with fixed intercept, multilevel structure over countries of asylum, no covariates

### find prior distribution on theta

## beta(wpp, 1.5) converted to normal distribution for logit link on theta

# simulate thetas 

# normal equivalent of beta with Afghan population 0.492% children and beta fixed to 1.5

# 1) find appropriate beta 
wppprop <- 0.492
hyp2 <- 1.5
hyp1 <- (hyp2*wppprop)/(1-wppprop)
priorint.child.0.beta <- rbeta(1000, hyp1, hyp2) # simulate 1000 intercepts (= probability of child)
hist(priorint.child.0.beta)

# 2 convert to theta as logit(p)
p_sim <- rbeta(10000, hyp1, hyp2)
theta_sim <- log(p_sim / (1 - p_sim))

# 3 find corresponding normal distribution
prior_mu <- mean(theta_sim)
prior_sd <- sd(theta_sim)
prior_sim <- rnorm(10000, prior_mu, prior_sd)
hist(prior_sim)



#### Model 1: binomial children with variable intercepts over countries of asylum


# simulate predictive priors and model


stanvars <- stanvar(prior_mu, name = "prior_mu") + stanvar(prior_sd, name = "prior_sd")

prior.child.1.int <- prior(
  normal(prior_mu, prior_sd),
  class = Intercept
)

m.child.1 <- brm(children | trials(totalEndYear) ~ 1 + (1|asylum_iso3),
                    family = binomial(link = "logit"),
                    prior = prior.child.1.int,
                    stanvars=stanvars,
                    sample_prior = "yes",
                    data = demref2020.ori.asy.age %>% filter(typeOfDisaggregationAge  == "Age"))

saveRDS(m.child.1, file = "output/mchild1-afg.rds")




 #### II.B Multinomial models to estimate full demographic composition ##### 





####### III. Add structure and covariates #######

#### Model 2: binomial children with variable intercepts over regions and countries of asylum and with neighbour covariate


# simulate predictive priors and model

m.child.2 <- brm(children | trials(totalEndYear) ~ 1 + neighbour + (1|asylum_RegionName/asylum_iso3),
                 family = binomial(link = "logit"),
                 prior = prior.child.1.int,
                 stanvars=stanvars,
                 sample_prior = "yes",
                 data = demref2020.ori.asy.age %>% filter(typeOfDisaggregationAge  == "Age"))

saveRDS(m.child.2, file = "output/mchild2-afg.rds")

