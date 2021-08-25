library(dplyr)
library(tidyr)
library(rgeos)
library(spdep)
library("sp")
library("RColorBrewer")
library("ggplot2")
library("rgdal")
library("scales")


# Read in UN shapefiles
# Read in the UN cartography polygon shapefile (no antarctica)
path.basic <- "."
map.dir <- file.path(path.basic,'data/unmap')
input.unicef <- file.path(path.basic, "data/UNICEF")
load(file.path(input.unicef, "location.Rdata"))
countries <- readOGR(map.dir, "un-world-2012-no-antartica")


# load UNHCR data
load("data/demref2020.RData")
unhcr.country.list <- demref2020

# Extract the ISO codes and map them to the numeric row names
country.names <- data_frame(id = row.names(countries@data),
                            country_iso3 = as.character(countries@data$ISO3_CODE),
                            neighbor_iso3 = country_iso3)

# Determine which countries are neighbors
# Adapted from http://stackoverflow.com/a/32318128/120898
#
# spdep::poly2nb/nb2mat method is faster and more accurate than rgeos::gTouches
#
# gTouches gives wrong results; doesn't see Russia-North Korea border; is suuuuuuuper slow
#   neighbor.matrix <- gTouches(countries, byid=TRUE)
#
neighbor.list <- poly2nb(countries)
neighbor.matrix <- nb2mat(neighbor.list, style="B", zero.policy=TRUE)
colnames(neighbor.matrix) <- rownames(neighbor.matrix)

# Clean up and transform the neighbor matrix
all.neighbors <- as.data.frame(neighbor.matrix) %>%
  mutate(country = row.names(.)) %>%  # Convert row names to actual column
  gather(neighbor, present, -country) %>%  # Convert to long
  filter(present == 1) %>%  # Only look at cells with a match
  # Add country names
  left_join(select(country.names, -neighbor_iso3), by=c("country" = "id")) %>%
  left_join(select(country.names, -country_iso3), by=c("neighbor" = "id")) %>%
  filter(country_iso3 != "XXX", neighbor_iso3 != "XXX") %>%  # Remove missing countries
  select(contains("iso3"))  # Just get the ISO columns
head(all.neighbors)
#
#   country_iso3 neighbor_iso3
# 1          CHN           AFG
# 2          IRN           AFG
# 3          PAK           AFG
# 4          TJK           AFG
# 5          TKM           AFG
# 6          UZB           AFG

neighbor.summary <- all.neighbors %>%
  group_by(country_iso3) %>%
  summarise(num = n()) %>%
  arrange(desc(num))
neighbor.summary
