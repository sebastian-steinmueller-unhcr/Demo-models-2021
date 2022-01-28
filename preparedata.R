############################################ START ###########################################################

############################################ preparedata.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, Sebastian, steinmul@unhcr.org
#### Project: Demographic models 2021 (after GT)
#### Description: Data preparation of ASR 2020 REF + VDA + ASY demographic data 


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
load("data/asr_2019-2020_20210801.RData")
rm(hst, idp, oth, ret, roc, rsd, sta, uasc) # remove data sets not needed




##### II. Checks, new variables and data cleaning ##### 


## consistent variable naming, new disaggregation variable, index variable
dem <-  dem %>%
  rename(typeOfDisaggregation = typeOfAggregation) %>% 
  mutate(typeOfDisaggregation = recode(typeOfDisaggregation,
    `Detailed` = "Sex/Age fine",
    `M/F and 18-59` = "Sex/Age broad",
    `M/F` = "Sex",
    `Total` = "None"
    )
  ) %>%
  mutate(typeOfDisaggregationBroad = case_when(
    typeOfDisaggregation == "Sex/Age fine" | typeOfDisaggregation == "Sex/Age broad" ~ "Sex/Age",
    typeOfDisaggregation == "Sex" ~ "Sex",
    typeOfDisaggregation == "None" ~ "None"
    ),
  index = seq(1:n())
  )


## check: is there 18-59 age bracket data for "Sex/Age fine" aggregation type, and does it match the finer age brackets for this group?

dem.check1859 <- dem %>% 
  filter(typeOfDisaggregation == "Sex/Age fine") %>% 
  mutate(populationType = case_when(
    populationType == "ROC" ~ "REF",
    populationType != "ROC" ~ as.character(populationType)
    ),
  populationTypeName = case_when(
    populationType == "REF" ~ "Refugees",
    populationType != "REF" ~ as.character(populationTypeName)
    ),
  female_18_59Check = rowSums(select(., female_18_24, female_25_49, female_50_59), na.rm = T),
  male_18_59Check =  rowSums(select(., male_18_24, male_25_49, male_50_59), na.rm = T),
  female_18_59Diff = female_18_59- female_18_59Check,
  male_18_59Diff = male_18_59- male_18_59Check,
  female_18_59Flag = case_when(
    female_18_59Diff == 0 | (is.na(female_18_59) & female_18_59Check == 0) ~ "No discrepancy",
    female_18_59Diff != 0 & !(abs(female_18_59Diff) == female_18_59Check) ~ "Discrepancy",
    female_18_59Diff != 0 & (abs(female_18_59Diff) == female_18_59Check) ~ "18-59 group entered 0 instead NA",
    female_18_59Diff != 0 & is.na(female_18_59Diff) ~ "18-59 group NA"
  ),
  male_18_59Flag = case_when(
    male_18_59Diff == 0 | (is.na(male_18_59) & male_18_59Check == 0)  ~ "No discrepancy",
    male_18_59Diff != 0 & !(abs(male_18_59Diff) == male_18_59Check) ~ "Discrepancy",
    male_18_59Diff != 0 & (abs(male_18_59Diff) == male_18_59Check) ~ "18-59 group entered 0 instead NA",
    male_18_59Diff != 0 & is.na(male_18_59Diff) ~ "18-59 group NA"
  )
  ) 
  
t.dem.check1859.female <- dem.check1859 %>% 
  group_by(populationType, female_18_59Flag) %>%
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            n_asylum = n_distinct(asylum_iso3))

t.dem.check1859.male <- dem.check1859 %>% 
  group_by(populationType, male_18_59Flag) %>%
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            n_asylum = n_distinct(asylum_iso3))

# View(dem.check1859.discrepancy <- dem.check1859 %>% filter(male_18_59Flag != "No discrepancy" |  female_18_59Flag != "No discrepancy" | is.na(female_18_59Flag)| is.na(male_18_59Flag)) %>% 
#        select(populationTypeName, location, asylum, asylum_iso3, asylum_country, origin, origin_iso3, origin_country, 
#               female_18_59, female_18_59Check, female_18_59Diff, female_18_59Flag, female, 
#               male_18_59, male_18_59Check, male_18_59Diff, male_18_59Flag, male,
#               totalEndYear))
# write.xlsx(dem.check1859.discrepancy, "descriptive outputs/dem.check1859.discrepancy.xlsx") # this line only for sending discrepancy overview to ASR team

### check results: small and few discrepancies, orders of magnitude are all correct. 
### Re-populate the 18-59 bracket with the individual ones for "Sex/Age fine" rows to be on the safe side and have data for the few with NA in 18-59 groups

## check: why so many NAs in totalEndYear?
# View(dem %>% filter(year == 2020, is.na(totalEndYear))) # appear to be entries in demo table where population group table had end-year value of 0 (check with DAS unit)

## check: NAs in typeOfDisaggregation
# View(dem %>% filter(year == 2020, is.na(typeOfDisaggregation))) # none in 2020 data


dem <- dem %>% 
  filter(!is.na(totalEndYear) & totalEndYear != 0) %>%  # check with DAS unit why so many NA values here
#  mutate(typeOfDisaggregation = if_else(is.na(typeOfDisaggregation), "None", typeOfDisaggregation)) %>%
  mutate(female_18_59 = case_when(
    typeOfDisaggregation == "Sex/Age fine" ~ rowSums(select(., female_18_24, female_25_49, female_50_59), na.rm = T),
    typeOfDisaggregation != "Sex/Age fine" ~ female_18_59,
    ),
    male_18_59 = case_when(
    typeOfDisaggregation == "Sex/Age fine" ~ rowSums(select(., male_18_24, male_25_49, male_50_59), na.rm = T),
    typeOfDisaggregation != "Sex/Age fine" ~ male_18_59,
    )
  ) %>% 
  mutate(populationType = case_when(
    populationType == "ROC" ~ "REF",
    populationType != "ROC" ~ as.character(populationType)
    ),
    populationTypeName = case_when(
      populationType == "REF" ~ "Refugees",
      populationType != "REF" ~ as.character(populationTypeName)
    )
  )




# check coverage by population type
t.dem2020.reg.cov <- dem %>% 
  mutate(populationTypeName = factor(populationTypeName ),
         typeOfDisaggregationBroad = factor(typeOfDisaggregationBroad )) %>%
  filter(year == 2020) %>% 
  group_by(asylum_main_office_short, populationTypeName, typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T)) %>%
  mutate(freq.totalEndYear = round(totalEndYear/sum(totalEndYear)*100)) %>% 
  filter(typeOfDisaggregationBroad == "Sex/Age") %>% 
  pivot_wider(names_from = populationTypeName, values_from = freq.totalEndYear, id_cols = asylum_main_office_short) 
  
  
t.dem2020.cov <- dem %>% 
  mutate(populationTypeName = factor(populationTypeName ),
         typeOfDisaggregationBroad = factor(typeOfDisaggregationBroad )) %>%
  filter(year == 2020) %>% 
  group_by(populationTypeName, typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T)) %>%
  mutate(freq.totalEndYear = round(totalEndYear/sum(totalEndYear)*100), asylum_main_office_short = "WORLD") %>% 
  filter(typeOfDisaggregationBroad == "Sex/Age") %>% 
  pivot_wider(names_from = populationTypeName, values_from = freq.totalEndYear, id_cols = asylum_main_office_short) 

t.dem2020.reg.cov <- t.dem2020.reg.cov %>% 
  full_join(t.dem2020.cov)


# data set for models:
demref2020 <- dem %>% 
  filter(populationType %in% c("REF", "ROC", "VDA"), year == 2020) # %>% 
 # select(-female_18_24, -female_25_49, -female_50_59, -male_18_24, -male_25_49, -male_50_59)

demasy2020 <- dem %>% 
  filter(populationType %in% c("ASY"), year == 2020) # %>% 
  # select(-female_18_24, -female_25_49, -female_50_59, -male_18_24, -male_25_49, -male_50_59)





### following refugees only for now

##### number of refugees and countries of asylum by disaggregation type

t.typeOfDisaggregation <- demref2020 %>% 
  group_by(typeOfDisaggregation) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))

# clean demref2020 data for some asylum countries with disaggregated data but unknown ages:

#   View(demref2020  %>% filter(typeOfDisaggregationBroad == "Sex/Age" & (femaleAgeUnknown>0 | maleAgeUnknown > 0)) %>% select(asylum, asylum_country, origin, origin_country, 
#          femaleAgeUnknown,female, maleAgeUnknown , male, totalEndYear, typeOfDisaggregation) %>% arrange(asylum, desc(femaleAgeUnknown)))

table((demref2020$typeOfDisaggregationBroad))

t.checkunknowns.ref <- demref2020 %>% 
  filter(typeOfDisaggregationBroad == "Sex/Age") %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            femaleAgeUnknown = sum(femaleAgeUnknown, na.rm = T),
            maleAgeUnknown = sum(maleAgeUnknown, na.rm = T)) %>% 
  mutate(ageUnknown = rowSums(select(., femaleAgeUnknown, maleAgeUnknown)),
         freq.ageUnknown = ageUnknown/totalEndYear)

# (almost) all are unknown for the following asylum country entries, thus assuming age distribution not available. Canada: statistical disclosure control makes age data unusable
# Check the percentage, only Canada and UKR have large percentage, and UKR is because of coding error. PHI and NIC should not change
demref2020 %>% filter(asylum %in% c("ARM","CAN",'DOM','GFR', "UKR", "PHI", "NIC"),typeOfDisaggregationBroad == "Sex/Age") %>% group_by(asylum) %>% summarise(sum(femaleAgeUnknown,na.rm= T)/sum(female,na.rm= T), sum(maleAgeUnknown,na.rm= T)/sum(male,na.rm= T))


demref2020 <- demref2020 %>% 
  mutate(typeOfDisaggregationBroad = case_when(
    asylum %in% c("CAN", "UKR") & typeOfDisaggregationBroad == "Sex/Age" &  (femaleAgeUnknown>0 | maleAgeUnknown > 0 | is.na(femaleAgeUnknown) | is.na(maleAgeUnknown)) ~ "Sex",  
    !(asylum %in% c("CAN", "UKR") & typeOfDisaggregationBroad == "Sex/Age" &  (femaleAgeUnknown>0 | maleAgeUnknown > 0 | is.na(femaleAgeUnknown) | is.na(maleAgeUnknown)) ) ~ typeOfDisaggregationBroad
  ),
  typeOfDisaggregation = case_when(
    asylum %in% c("CAN", "UKR") & typeOfDisaggregation %in% c("Sex/Age fine", "Sex/Age broad") &  (femaleAgeUnknown>0 | maleAgeUnknown > 0 | is.na(femaleAgeUnknown) | is.na(maleAgeUnknown)) ~ "Sex",  
    !(asylum %in% c("CAN", "UKR") & typeOfDisaggregation %in% c("Sex/Age fine", "Sex/Age broad") &  (femaleAgeUnknown>0 | maleAgeUnknown > 0 | is.na(femaleAgeUnknown) | is.na(maleAgeUnknown)) ) ~ typeOfDisaggregation
    )
  )

table(demref2020$typeOfDisaggregationBroad)
table(demref2020$typeOfDisaggregation, demref2020$typeOfDisaggregationBroad)


#### redistribute unknowns for Armenia and Germany with d'hondt method to allocate age unknown to sex Nones:

# by row, for male and female separately, extract by index
# 1) named vector: extract Sex/Age fine or 18-59 age bracket counts plus age unknown and names (depending on Sex/Age fine or 18-59)
# 2) new named vector: unknown count allocated to age brackets 
# 3) new named vector: d'hondt allocated plus original counts
# 4) replace original counts with new counts

## female unknowns

dhondt_female <- demref2020  %>% 
  filter(typeOfDisaggregationBroad == "Sex/Age" & (femaleAgeUnknown>0))  %>% 
  select(index, female_0_4:femaleAgeUnknown, typeOfDisaggregation)

addDhondt_female <- dhondt_female[FALSE,] %>% select(-femaleAgeUnknown, - typeOfDisaggregation)

for(i in 1:nrow(dhondt_female)){
  index.i <- dhondt_female[i,"index"]
  if(dhondt_female[i,"typeOfDisaggregation"] == "Sex/Age fine")
          x <- select(dhondt_female[i,], female_0_4:female_50_59, female_60, femaleAgeUnknown) else
          x <- select(dhondt_female[i,], female_0_4:female_12_17, female_18_59, female_60, femaleAgeUnknown)
  x.dhondt <-seats_ha(parties = names(x)[1:(length(x)-1)], 
                                votes = as.numeric(x[1:(length(x)-1)]), 
                                n_seats = as.numeric(x[length(x)]), method = "dhondt")
  x.addDhondt <- unlist(c(index.i, x.dhondt))
  addDhondt_female <- addDhondt_female %>% full_join(bind_rows(x.addDhondt))
}

sumDhondt_female <- dhondt_female %>% 
  select(-femaleAgeUnknown, - typeOfDisaggregation) %>% 
  bind_rows(addDhondt_female) %>% 
  group_by(index) %>% 
  summarise_all(sum) %>%
  ungroup() %>%
  mutate(
  femaleAgeUnknown = 0,
  female_18_59 = case_when(
    is.na(female_18_59) ~ rowSums(select(., female_18_24, female_25_49, female_50_59), na.rm = T),
    !is.na(female_18_59) ~ female_18_59,
  )
  )


## male unknowns

dhondt_male <- demref2020  %>% 
  filter(typeOfDisaggregationBroad == "Sex/Age" & (maleAgeUnknown>0))  %>% 
  select(index, male_0_4:maleAgeUnknown, typeOfDisaggregation)

addDhondt_male <- dhondt_male[FALSE,] %>% select(-maleAgeUnknown, - typeOfDisaggregation)

for(i in 1:nrow(dhondt_male)){
  index.i <- dhondt_male[i,"index"]
  if(dhondt_male[i,"typeOfDisaggregation"] == "Sex/Age fine")
    x <- select(dhondt_male[i,], male_0_4:male_50_59, male_60, maleAgeUnknown) else
      x <- select(dhondt_male[i,], male_0_4:male_12_17, male_18_59, male_60, maleAgeUnknown)
    x.dhondt <-seats_ha(parties = names(x)[1:(length(x)-1)], 
                        votes = as.numeric(x[1:(length(x)-1)]), 
                        n_seats = as.numeric(x[length(x)]), method = "dhondt")
    x.addDhondt <- unlist(c(index.i, x.dhondt))
    addDhondt_male <- addDhondt_male %>% full_join(bind_rows(x.addDhondt))
}

sumDhondt_male <- dhondt_male %>% 
  select(-maleAgeUnknown, - typeOfDisaggregation) %>% 
  bind_rows(addDhondt_male) %>% 
  group_by(index) %>% 
  summarise_all(sum) %>%
  ungroup() %>%
  mutate(
    maleAgeUnknown = 0,
    male_18_59 = case_when(
      is.na(male_18_59) ~ rowSums(select(., male_18_24, male_25_49, male_50_59), na.rm = T),
      !is.na(male_18_59) ~ male_18_59,
    )
  )



# replace values in demref2020 with adjusted counts from sumDhondt female and male

dim(demref2020)

demref2020 <- demref2020 %>% 
  full_join(sumDhondt_female , by = "index") %>% 
  mutate(
    female_0_4 = ifelse(index %in% c(sumDhondt_female$index), female_0_4.y, female_0_4.x),
    female_5_11 = ifelse(index %in% c(sumDhondt_female$index), female_5_11.y, female_5_11.x),
    female_12_17 = ifelse(index %in% c(sumDhondt_female$index), female_12_17.y, female_12_17.x),
    female_18_24 = ifelse(index %in% c(sumDhondt_female$index), female_18_24.y, female_18_24.x),
    female_25_49 = ifelse(index %in% c(sumDhondt_female$index), female_25_49.y, female_25_49.x),
    female_50_59 = ifelse(index %in% c(sumDhondt_female$index), female_50_59.y, female_50_59.x),
    female_18_59 = ifelse(index %in% c(sumDhondt_female$index), female_18_59.y, female_18_59.x),
    female_60 = ifelse(index %in% c(sumDhondt_female$index), female_60.y, female_60.x),
    femaleAgeUnknown = ifelse(index %in% c(sumDhondt_female$index), femaleAgeUnknown.y, femaleAgeUnknown.x),
  ) %>% 
  full_join(sumDhondt_male , by = "index") %>% 
  mutate(
    male_0_4 = ifelse(index %in% c(sumDhondt_male$index), male_0_4.y, male_0_4.x),
    male_5_11 = ifelse(index %in% c(sumDhondt_male$index), male_5_11.y, male_5_11.x),
    male_12_17 = ifelse(index %in% c(sumDhondt_male$index), male_12_17.y, male_12_17.x),
    male_18_24 = ifelse(index %in% c(sumDhondt_male$index), male_18_24.y, male_18_24.x),
    male_25_49 = ifelse(index %in% c(sumDhondt_male$index), male_25_49.y, male_25_49.x),
    male_50_59 = ifelse(index %in% c(sumDhondt_male$index), male_50_59.y, male_50_59.x),
    male_18_59 = ifelse(index %in% c(sumDhondt_male$index), male_18_59.y, male_18_59.x),
    male_60 = ifelse(index %in% c(sumDhondt_male$index), male_60.y, male_60.x),
    maleAgeUnknown = ifelse(index %in% c(sumDhondt_male$index), maleAgeUnknown.y, maleAgeUnknown.x),
  ) %>% 
  mutate(
    children = rowSums(select(., female_0_4, female_5_11, female_12_17,
                              male_0_4, male_5_11, male_12_17 ), na.rm = T),
    adults = rowSums(select(., female_18_59, female_60, 
                            male_18_59, male_60), na.rm = T),
    female_children = rowSums(select(., female_0_4, female_5_11, female_12_17), na.rm = T),
    female_adults = rowSums(select(.,female_18_59, female_60), na.rm = T),
    male_children = rowSums(select(., male_0_4, male_5_11, male_12_17), na.rm = T),
    male_adults = rowSums(select(.,male_18_59, male_60), na.rm = T)
  ) %>%
  mutate(typeOfDisaggregationAge = case_when(
    typeOfDisaggregationBroad == "Sex/Age" ~ "Age",
    typeOfDisaggregationBroad == "Sex" ~ "None",
    typeOfDisaggregationBroad == "None" ~ "None"
    ),
    typeOfDisaggregationSex = case_when(
    typeOfDisaggregationBroad == "Sex/Age" ~ "Sex",
    typeOfDisaggregationBroad == "Sex" ~ "Sex",
    typeOfDisaggregationBroad == "None" ~ "None"
    )
  ) %>%
  mutate(
    typeOfDisaggregation = as.factor(typeOfDisaggregation),
    typeOfDisaggregationBroad = as.factor(typeOfDisaggregationBroad),
    typeOfDisaggregationAge = as.factor(typeOfDisaggregationAge),
    typeOfDisaggregationSex = as.factor(typeOfDisaggregationSex),
    urbanRural = as.factor(urbanRural),
    accommodationType = as.factor(accommodationType),
    populationType = as.factor(populationType),
    statelessStatus = as.factor(statelessStatus),
    populationTypeName = as.factor(populationTypeName),
  ) %>%
  select(index, year, asylum:populationPlanningGroup, populationTypeName, 
         female_0_4:femaleAgeUnknown, female_children, female_adults, female, 
         male_0_4:maleAgeUnknown, male_children, male_adults, male, children, adults,
         totalEndYear, unhcrAssistedEndYear,
         typeOfDisaggregation, typeOfDisaggregationBroad, typeOfDisaggregationAge, typeOfDisaggregationSex,
         asylum_iso3:`origin_Developed / Developing Countries`)
  
dim(demref2020)

## check NAs and 0s

t.checkna0 <- demref2020 %>% 
  mutate(
    femaleAgeUnknownNA = case_when(
      femaleAgeUnknown == 0 ~ "0",
      is.na(femaleAgeUnknown) ~ "NA",
      !is.na(femaleAgeUnknown) & femaleAgeUnknown != 0 ~ ">0"
    ), 
    maleAgeUnknownNA = case_when(
      maleAgeUnknown == 0 ~ "0",
      is.na(maleAgeUnknown) ~ "NA",
      !is.na(maleAgeUnknown) & maleAgeUnknown != 0 ~ ">0"
    )
  ) %>% 
  group_by(typeOfDisaggregation, femaleAgeUnknownNA) %>% 
  summarise(nrowsFemaleUnknown = n())


demref2020.ori.asy.age <- demref2020 %>%
  group_by(asylum_main_office_short, `asylum_Region Name`, `asylum_Sub-region Name`, asylum, asylum_iso3, asylum_country, 
           origin_main_office_short, `origin_Region Name`, `origin_Sub-region Name`, origin, origin_iso3, origin_country,
           typeOfDisaggregationAge) %>%
  summarise_at(vars(female_0_4:unhcrAssistedEndYear), ~sum(., na.rm = F)) %>%
  ungroup() %>%
  mutate(agecov_1859 = rowSums(select(., female_0_4, female_5_11, female_12_17, female_18_59, female_60,
                                      male_0_4, male_5_11, male_12_17, male_18_59, male_60), na.rm = T ))





##### III. write file for descriptive analysis and models ########## 


origin_countries <- countries %>%
  rename_all( ~ paste0("origin_", .))

asylum_countries <- countries %>%
  rename_all( ~ paste0("asylum_", .))

save(demref2020, demref2020.ori.asy.age,  origin_countries, asylum_countries, file = "data/demref2020.RData")
# save(demasy2020,  origin_countries, asylum_countries, file = "demasy2020.RData")

############################################ END ###########################################################