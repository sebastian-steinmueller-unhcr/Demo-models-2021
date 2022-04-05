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
m.child.1 <- readRDS("output/mchild1-afg.rds")


##### II. Model checks #####


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
p.child.1.cv <- plot(m.child.1.cv)


save.image( file = "output/post.mchild1-afg.RData")

