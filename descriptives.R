############################################ START ###########################################################

############################################ descriptives.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, Sebastian, steinmul@unhcr.org
#### Project: Demographic models 2021 (after GT)
#### Description: Data preparation plus descriptive analysis and visualisation of ASR 2020 REF + VDA demographic data 


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


##### II. Checks, new variables and data cleaning for modelling ##### 


## consistent variable naming, new disaggregation variable
dem <-  dem %>%
  rename(typeOfDisaggregation = typeOfAggregation) %>% 
  mutate(typeOfDisaggregationBroad = case_when(
    typeOfDisaggregation == "Detailed" | typeOfDisaggregation == "M/F and 18-59" ~ "Sex/Age",
    typeOfDisaggregation == "M/F" ~ "Sex",
    typeOfDisaggregation == "Total" ~ "None"
    )
  )


## check: is there 18-59 age bracket data for "detailed" aggregation type, and does it match the finer age brackets for this group?

dem.check1859 <- dem %>% 
  filter(typeOfDisaggregation == "Detailed") %>% 
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
### Re-populate the 18-59 bracket with the individual ones for "detailed" rows to be on the safe side and have data for the few with NA in 18-59 groups

## check: why so many NAs in totalEndYear?
# View(dem %>% filter(year == 2020, is.na(totalEndYear))) # appear to be entries in demo table where population group table had end-year value of 0 (check with DAS unit)

## check: NAs in typeOfDisaggregation
# View(dem %>% filter(year == 2020, is.na(typeOfDisaggregation))) # none in 2020 data


dem <- dem %>% 
  filter(!is.na(totalEndYear)) %>%  # check with DAS unit why so many NA values here
#  mutate(typeOfDisaggregation = if_else(is.na(typeOfDisaggregation), "Total", typeOfDisaggregation)) %>%
  mutate(typeOfDisaggregationBroad = case_when(
    typeOfDisaggregation == "Detailed" | typeOfDisaggregation == "M/F and 18-59" ~ "Sex/Age",
    typeOfDisaggregation == "M/F" ~ "Sex",
    typeOfDisaggregation == "Total" ~ "None"
  ),
  female_18_59 = case_when(
    typeOfDisaggregation == "Detailed" ~ rowSums(select(., female_18_24, female_25_49, female_50_59), na.rm = T),
    typeOfDisaggregation != "Detailed" ~ female_18_59,
    ),
  male_18_59 = case_when(
    typeOfDisaggregation == "Detailed" ~ rowSums(select(., male_18_24, male_25_49, male_50_59), na.rm = T),
    typeOfDisaggregation != "Detailed" ~ male_18_59,
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
demref2020 <- demref2020 %>% 
  mutate(typeOfDisaggregationBroad = case_when(
    asylum %in% c("CAN", "UKR", "PHI", "NIC") & typeOfDisaggregationBroad == "Sex/Age" &  (femaleAgeUnknown>0 | maleAgeUnknown > 0) ~ "Sex",  
    !(asylum %in% c("CAN", "UKR", "PHI", "NIC") & typeOfDisaggregationBroad == "Sex/Age" &  (femaleAgeUnknown>0 | maleAgeUnknown > 0) ) ~ typeOfDisaggregationBroad
  ),
  typeOfDisaggregation = case_when(
    asylum %in% c("CAN", "UKR", "PHI", "NIC") & typeOfDisaggregation %in% c("Detailed", "M/F and 18-59") &  (femaleAgeUnknown>0 | maleAgeUnknown > 0) ~ "M/F",  
    !(asylum %in% c("CAN", "UKR", "PHI", "NIC") & typeOfDisaggregation %in% c("Detailed", "M/F and 18-59") &  (femaleAgeUnknown>0 | maleAgeUnknown > 0) ) ~ typeOfDisaggregation
    )
  )

table(demref2020$typeOfDisaggregationBroad)
table(demref2020$typeOfDisaggregation, demref2020$typeOfDisaggregationBroad)


#### redistribute unknowns for Armenia and Germany with d'hondt method
# test d'hondt to allocate age unknown to sex totals:




test <- as.numeric((demref2020  %>% filter(typeOfDisaggregationBroad == "Sex/Age" & (femaleAgeUnknown>0 | maleAgeUnknown > 0)) %>% 
                      arrange(asylum, desc(femaleAgeUnknown)))[1,] %>% select(female_0_4:femaleAgeUnknown))
testnames <- names((demref2020  %>% filter(typeOfDisaggregationBroad == "Sex/Age" & (femaleAgeUnknown>0 | maleAgeUnknown > 0)) %>% 
                arrange(asylum, desc(femaleAgeUnknown)))[1,] %>% select(female_0_4:femaleAgeUnknown))
seats_ha(parties = testnames[c(1:3, 7:8)], votes = test[c(1:3, 7:8)], n_seats = test[9], method = "dhondt")





# data set for population pyramids


t.demref2020pyr.asy.ori <-  demref2020 %>% 
  group_by(asylum_iso3, asylum, asylum_country, origin_iso3, origin, origin_country) %>%
  summarise_at(vars(female_0_4, female_5_11, female_12_17, female_18_59, female_60, 
                    male_0_4, male_5_11, male_12_17, male_18_59, male_60), ~sum(., na.rm = T)) %>% 
  pivot_longer(cols = female_0_4:male_60, names_to = "agesexcat", values_to = "population") %>% 
  separate(agesexcat, into = c("sex", "age"), sep = "_") %>% 
  mutate(age = case_when(
    age == 0 ~ "0-4",
    age == 5 ~ "5-11",
    age == 12 ~ "12-17",
    age == 18 ~ "18-59",
      age == 60 ~ "60+"
  )) %>% 
  mutate(age = factor(age, levels = c("0-4", "5-11", "12-17","18-59", "60+")),
         sex = str_to_title(sex)) %>%
  ungroup() %>%
  group_by(asylum_iso3, asylum, asylum_country, origin_iso3, origin, origin_country) %>%  
  mutate(populationprop = population/sum(population)*100) %>%
  mutate(
    populationprop = case_when(
      sex == "Male" ~ (populationprop),
      sex == "Female" ~ -1*(populationprop)
    ))


##### III. Descriptive analysis of 2020 demo data set for REF and VDA ##### 

table(dem$populationType, useNA = "ifany")
table(dem$year, useNA = "ifany")



t.typeOfDisaggregationBroad <- demref2020 %>% 
  group_by(typeOfDisaggregationBroad) %>% 
  summarise(totalEndYear = sum(totalEndYear, na.rm = T),
            nAsylum = n_distinct(asylum)) %>% 
  mutate(freq.totalEndYear = totalEndYear/sum(totalEndYear),
         freq.asylum = nAsylum / sum(nAsylum))
  

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





demref2020 <- demref2020 %>%
  group_by(asylum_main_office_short, `asylum_Region Name`, `asylum_Sub-region Name`, asylum, asylum_iso3, asylum_country, 
           origin, origin_iso3, origin_country) %>%
  summarise_at(vars(female_0_4:unhcrAssistedEndYear), ~sum(., na.rm = T)) %>%
  ungroup() %>%
  mutate(agecov_1859 = rowSums(select(., female_0_4, female_5_11, female_12_17, female_18_59, female_60,
                               male_0_4, male_5_11, male_12_17, male_18_59, male_60), na.rm = T ))
  

demasy2020 <- demasy2020 %>%
  group_by(asylum_main_office_short, `asylum_Region Name`, `asylum_Sub-region Name`, asylum, asylum_iso3, asylum_country, 
           origin, origin_iso3, origin_country) %>%
  summarise_at(vars(female_0_4:unhcrAssistedEndYear), ~sum(., na.rm = T)) %>%
  ungroup() %>%
  mutate(agecov_1859 = rowSums(select(., female_0_4, female_5_11, female_12_17, female_18_59, female_60,
                                      male_0_4, male_5_11, male_12_17, male_18_59, male_60), na.rm = T ))



### write file for models

origin_countries <- countries %>%
  rename_all( ~ paste0("origin_", .))

asylum_countries <- countries %>%
  rename_all( ~ paste0("asylum_", .))

save(demref2020,  origin_countries, asylum_countries, file = "demref2020.RData")
save(demasy2020,  origin_countries, asylum_countries, file = "demasy2020.RData")


############################################ END ###########################################################