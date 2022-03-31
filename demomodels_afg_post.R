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
#library(ggridges)
library(tidybayes)

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


### II.A Prior predictive checks: 
# Do our prior assumptions make sense?





### II.B Posterior predictive checks
# Could the data actually come from the posterior distribution, i.e. is the model reasonably specified?




### II.C Cross-validation
# Predictive accuracy of the models



