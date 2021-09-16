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


#####calculate distance
# Read in UN shapefiles 
# Read in the UN cartography polygon shapefile (no antarctica)
world.un <- readOGR(map.dir, "un-world-2012-no-antartica-10pct") # (requires rgdal package)
# convert to Robinson projection (the projection preferred by UN Cartography)
proj4string(world.un) <- CRS("+proj=longlat +ellps=WGS84") # (requires sp package)
world.robin <- spTransform(world.un, CRS("+proj=robin")) # (requires rgdal package)

# Read in the Un Cartography shapefile with country/area boundaries
bnd.un <- readOGR(map.dir, "2012_UNGIWG_bnd_ln_01")
# convert to Robinson projection
proj4string(bnd.un) <- CRS("+proj=longlat +ellps=WGS84")
bnd <- spTransform(bnd.un, CRS("+proj=robin"))

# Read in the Un Cartography shapefile with coastlines
cst.un <- readOGR(map.dir, "2012_UNGIWG_cst_ln_01")
# convert to Robinson projection
proj4string(cst.un) <- CRS("+proj=longlat +ellps=WGS84")
cst <- spTransform(cst.un, CRS("+proj=robin"))
# remove Antarctica -- this is a bit clunky, but it works
cst.df <- fortify(cst)
cst.df <- cst.df[cst.df$lat>=-6285430,]

# Read in the Un Cartography shapefile with lakes
lks.un <- readOGR(map.dir, "2012_UNGIWG_lks_ply_01")
# convert to Robinson projection
proj4string(lks.un) <- CRS("+proj=longlat +ellps=WGS84")
lks <- spTransform(lks.un, CRS("+proj=robin"))
lks.df <- fortify(lks)

# Read in the Un Cartography shapefile with Antarctica
wld.un <- readOGR(map.dir, "un-world-2012-65pct")
ant.un <- wld.un[wld.un$TERR_NAME=="Antarctica",]
rm(wld.un)
# convert to Robinson projection
proj4string(ant.un) <- CRS("+proj=longlat +ellps=WGS84")
ant <- spTransform(ant.un, CRS("+proj=robin"))
ant.df <- fortify(ant)
ant.df$color.code <- NA
if (plot.coastlines==TRUE){ ant.df$color.code <- boundary.color }

world.robin@data$id <- as.integer(rownames(world.robin@data))+1

world.robin.df <- data.frame()
for(i in 1:length(world.robin)){
  new.df <- fortify(world.robin[world.robin$id == i,]) %>% mutate(id = i)
  world.robin.df <- rbind(world.robin.df, new.df)
}
world.robin.df <- left_join(world.robin.df, select(world.robin@data,id,ISO3_CODE),by = 'id')

bnd@data$id <- as.integer(rownames(bnd@data))+1

bnd.df <- data.frame()
for(i in 1:length(bnd)){
  new.df <- fortify(bnd[bnd$id == i,]) %>% mutate(id = i)
  bnd.df <- rbind(bnd.df, new.df)
}
bnd.df <- left_join(bnd.df, select(bnd@data,id,CARTOGRAPH),by = 'id') %>% 
  mutate(CARTOGRAPH = recode(CARTOGRAPH, 'International boundary line' = 'line', 
                             "Dashed boundary line" = 'dashed', "Undetermined international dashed boundary line" = 'dashed',
                             "Dotted boundary line" = 'dotted',"Dotted boundary line (Abyei)" ='dotted', .default = NA_character_)) %>%
  filter(!is.na(CARTOGRAPH ))



library(scatterpie)
#prepare data for pie charts
places <- data.frame(matrix(ncol = 5, nrow = length(world.robin)))
colnames(places) <- c("ISO3", "TERR_NAME", "long", "lat", "STATUS")

places$ISO3 <- as.character(world.robin$ISO3_CODE)
places$TERR_NAME <- world.robin$TERR_NAME
places$long <- coordinates(world.robin)[, 1]
places$lat <- coordinates(world.robin)[, 2]
places$STATUS <- world.robin$STATUS
places$ISO3[places$TERR_NAME == 'Abyei'] <- 'AB9'

distdf <- places %>% filter(ISO3 != '<NA>', ISO3 %in% union(demref2020 %>% distinct(asylum_iso3) %>% pull(), demref2020 %>% distinct(origin_iso3) %>% pull())) %>%
  filter(STATUS != 'PT Territory', !(TERR_NAME %in% c('Guernsey','Senkaku Islands','Gaza Strip','Kuril islands')) ) %>%
  
  group_by(ISO3) %>%
  sample_n(1)   
distmatrix <- distdf %>% ungroup() %>% select(long,lat) %>% as.matrix()
rownames(distmatrix) <- distdf %>% pull(ISO3)

distance_matrix <- dist(distmatrix,  method = "euclidean", diag = T) %>% as.matrix()
save(distance_matrix,file = 'data/distance.Rdata')






