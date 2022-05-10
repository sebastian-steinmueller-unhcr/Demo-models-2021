############################################ START ######################################################

############################################ demomodels_afg_post.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, Sebastian, steinmul@unhcr.org
#### Project: Demographic models 2021 (after GT)
#### Description: Analyse posterior distribution of test models with Afghan refugees

rm(list=ls()) # clear workspace 


#### I. Read data, packages etc ##### 


### packages
library(tidyverse)
library(stringi)
library(magrittr)
library(brms)
library(bayesplot)
library(tidybayes)
library(ggplot2)
library(gridExtra)

### options
options(scipen = 999)

## colours (https://intranet.unhcr.org/content/dam/unhcr/intranet/staff%20support/strategic/documents/english/brand-book/UNHCR-Brand%20Book.pdf)
unhcrPaletteBlue <- c("#0072BC", "#338EC9", "#66AAD7", "#99C7E4", "#CCE3F2")
unhcrPaletteBlack <- c("#000000", "#333333", "#666666", "#999999", "#CCCCCC")
unhcrPaletteYellow <- c("#FAEB00", "#FBEF33", "#FCF366", "#FDF799", "#FEFBCC")
unhcrPaletteWhite <- c("#FFFFFF")
unhcrPaletteRed <- c("#E73451")
unhcrPaletteGreen <- c("#00AB92")


### functions
source("functions_demomodels.R")


### data
load("data/demref2020.RData")
m.child.1 <- readRDS("output/mchild1-afg.rds")
m.child.2 <- readRDS("output/mchild2-afg.rds")

##### II. Model checks #####


#### Model 1


### II.A General model summaries

summary(m.child.1)
prior_summary(m.child.1)
p.m.child.1.mcmc <- plot(m.child.1) # trace and density plots


### II.B Prior predictive checks: 
# Do our prior assumptions make sense?

m.child.1.lpprior <- prior_samples(m.child.1)
m.child.1.prior <- m.child.1.lpprior %>%
  mutate(across(.fns = inv_logit_scaled))

p.child.1.prior <- hist(m.child.1.prior$Intercept)

plot(density(m.child.1.lpprior$sd_asylum_iso3))

p.child.1.prior <- ggplot(m.child.1.prior, aes(x=Intercept)) + 
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  geom_histogram(color=unhcrPaletteBlue[1], fill="white") +
  geom_vline(aes(xintercept=0.492),
              color=unhcrPaletteRed[1], linetype="dashed", size=1)

### II.C Posterior predictive checks
# Could the data actually come from the posterior distribution, i.e. is the model reasonably specified?


m.child.1.postpred <- m.child.1$data %>% 
  add_predicted_draws(m.child.1)

p.child.1.postpred <- ppc_intervals_grouped(y = m.child.1$data$children, posterior_predict(m.child.1), prob = 0.95,
                       group = m.child.1$data$asylum_iso3)



### II.D Cross-validation
# Predictive accuracy of the models

m.child.1.cv <- loo(m.child.1, save_psis = TRUE)
m.child.1.cv
p.child.1.cv <- plot(m.child.1.cv) # Pareto k plot

m.child.1.cv.pred <-  m.child.1$data |>
  select(asylum_iso3 , totalEndYear, children) |>
  left_join(m.child.1.postpred %>% select(asylum_iso3, `.prediction`) %>% 
              group_by(asylum_iso3) %>% 
              summarise("Prediction" = mean(`.prediction`)), 
            by = "asylum_iso3") |>
  bind_cols("LOO predict" = loo_predict(m.child.1, sample_new_levels = "gaussian")) |>
  bind_cols(loo_predictive_interval(m.child.1, sample_new_levels = "gaussian")) |>
  mutate(
    children = children/totalEndYear,
    Prediction = Prediction/totalEndYear,
    `LOO predict` = `LOO predict`/totalEndYear,
    `5%` = `5%`/totalEndYear,
    `95%` = `95%`/totalEndYear
  ) |>
  left_join(asylum_countries %>% select(asylum_iso3, asylum_country), by = "asylum_iso3") 


p.child.1.cv.pred <- ggplot(m.child.1.cv.pred %>% arrange(children), aes(x=reorder(asylum_country, children), y=children)) + 
  geom_point() + 
  #  ylim(-10, 20) +
  geom_errorbar(stat="identity", aes(ymin = `5%`, ymax = `95%`), width=0) +
  coord_flip() 




#### Model 2


### II.A General model summaries

summary(m.child.2)
prior_summary(m.child.2)
p.m.child.2.mcmc <- plot(m.child.2) # trace and density plots


### II.B Prior predictive checks: 
# Do our prior assumptions make sense?

m.child.2.lpprior <- prior_samples(m.child.2)
m.child.2.prior <- m.child.2.lpprior %>%
  mutate(across(.fns = inv_logit_scaled))

p.child.2.prior <- hist(m.child.2.prior$Intercept)

plot(density(m.child.2.lpprior$sd_asylum_iso3))

p.child.2.prior <- ggplot(m.child.2.prior, aes(x=Intercept)) + 
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  geom_histogram(color=unhcrPaletteBlue[1], fill="white") +
  geom_vline(aes(xintercept=0.492),
             color=unhcrPaletteRed[1], linetype="dashed", size=1)

### II.C Posterior predictive checks
# Could the data actually come from the posterior distribution, i.e. is the model reasonably specified?


m.child.2.postpred <- m.child.2$data %>% 
  add_predicted_draws(m.child.2)

p.child.2.postpred <- ppc_intervals_grouped(y = m.child.2$data$children, posterior_predict(m.child.2), prob = 0.95,
                                            group = m.child.2$data$asylum_iso3)



### II.D Cross-validation
# Predictive accuracy of the models

m.child.2.cv <- loo(m.child.2, save_psis = TRUE)
m.child.2.cv
p.child.2.cv <- plot(m.child.2.cv) # Pareto k plot

m.child.2.cv.pred <-  m.child.2$data |>
  select(asylum_iso3 , totalEndYear, children) |>
  left_join(m.child.1.postpred %>% select(asylum_iso3, `.prediction`) %>% 
              group_by(asylum_iso3) %>% 
              summarise("Prediction" = mean(`.prediction`)), 
            by = "asylum_iso3") |>
  bind_cols("LOO predict" = loo_predict(m.child.2, sample_new_levels = "gaussian")) |>
  bind_cols(loo_predictive_interval(m.child.2, sample_new_levels = "gaussian")) |>
  mutate(
    children = children/totalEndYear,
    Prediction = Prediction/totalEndYear,
    `LOO predict` = `LOO predict`/totalEndYear,
    `5%` = `5%`/totalEndYear,
    `95%` = `95%`/totalEndYear
  ) |>
  left_join(asylum_countries %>% select(asylum_iso3, asylum_country), by = "asylum_iso3") 


p.child.2.cv.pred <- ggplot(m.child.2.cv.pred, aes(x=reorder(asylum_country, children), y=children)) + 
  geom_point() + 
#  ylim(-10, 20) +
  geom_errorbar(stat="identity", aes(ymin = `5%`, ymax = `95%`), width=0) +
  coord_flip() 

grid.arrange(p.child.1.cv.pred, p.child.2.cv.pred, nrow = 1)


##### save workspace

save.image( file = "output/post.mchild-afg.RData")

############################################ END ######################################################