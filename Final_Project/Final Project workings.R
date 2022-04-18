library(usmap)
library(ggplot2)
library(sf)
library(sp)
library(cartography)
library(lwgeom)
library(ggmap)
library(rstudioapi)
# this is my key, please don't use it. I switch between computers
register_google(key = "AIzaSyCv8INptsfW-5J2qv2WAQBkbpEbE0233z4")


# guide to the abreviations used in the dataset
# https://afdc.energy.gov/data_download/alt_fuel_stations_format

# skip over to actual code to see what I am working on, the other sections were
# things that I was using to figure out maps but I'll delete it when I am cleaning
# the rproj for final submission

# random map code ####
options(scipen=999)

plot_usmap(regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

plot_usmap(data = countypov, values = "pct_pov_2014", include = c("UT", "NV", "CO", "ID", "NM","AZ", "WY"), color = "blue") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 
  labs(title = "New England Region", subtitle = "Poverty Percentage Estimates for New England Counties in 2014") +
  theme(legend.position = "right")

# What was in the github ####
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-03-01')
tuesdata <- tidytuesdayR::tt_load(2022, week = 9)

stations <- tuesdata$stations

# Or read in the data manually

# cartography stuff that wasnt useful ####
library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
# plot municipalities (only borders are plotted)
plot(st_geometry(mtq), col = "grey80", border = "grey")
# plot population
propSymbolsLayer(
  x = mtq, 
  var = "POP", 
  inches = 0.25, 
  col = "brown4",
  legend.pos = "topright",  
  legend.title.txt = "Total population"
)
# layout
layoutLayer(title = "Population Distribution in Martinique",
            sources = "Sources: Insee and IGN, 2018",
            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE)
# north arrow
north(pos = "topleft")
# actual code ####

library(ggmap)
library(rstudioapi)

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

Ustation <- stations[which(stations$STATE == 'UT'),]

UBDS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'BD'),]

UCNGS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'CNG'),]

UELECS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'ELEC'),]

UE85S <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'E85'),]

UHYS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'HY'),] # no H so don't run

ULNGS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'LNG'),]

ULPGS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'LPG'),]

UTmap <- ggmap(get_googlemap(center = c(lon = -111.85298,lat =40.3958),
                             zoom = 8,
                             maptype = 'terrain',
                             color = 'color')) +
geom_point(data = Ustation, aes(x=LONGITUDE,y=LATITUDE), color = "red", size = .5)

print(UTmap) # this is the map of the wasatch front, dots are stations

# stuff sent by dr zahn 4/14 ####

library(tidyverse)
library(geosphere)

lat <- c(40.274055397791884, 40.270633598673044,40.304545775052695,40.30449431121125,40.31322297680429,40.307921618888116)
long <- c(-111.72627297459398, -111.72093015153435,  -111.7191187746866,  -111.69615599121808, -111.65769273421122, -111.65498132889809)

df <- data.frame(lat,long,site=factor(1:6))

dist(df[,1:2])

dmat <- distm(df[,c(2,1)]) %>% as.data.frame()

dmat[dmat==0] <- NA

mindists <- apply(dmat,1,min,na.rm=TRUE)

dmat

mins <- c()
x=1
for(i in 1:length(mindists)){
  mins[x] <- which(dmat[,i] == mindists[i])  
  x=x+1
}

df$closest <- mins

nearest_dist <- c()
for(i in 1:length(df$closest)){
  nearest_dist[i] <- distHaversine(df[i,c(2,1)],df[df$closest[i],c(2,1)])  
}

df$nearest_dist_m <- nearest_dist

df

################################ my application of it



UEDF <- data.frame(UELECS$LATITUDE,UELECS$LONGITUDE,site=factor(1:910))

DE <-  distm(UEDF[,c(2,1)]) %>%  as.data.frame()

DE[DE==0] <- NA

MEDis <- apply(DE,1,min,na.rm=TRUE)

minE <- c()
x = 1
for(i in 1:length(MEDis)){
  minE[x] <- which(DE[,i] == MEDis[i])  
  x=x+1
}

# I kept running into issues and I just started reworking it sunday night.

# I got issues with the size of the electric ds for Ut being 910x910 so what
# should I try to do there?


warnings()
# Warning messages: #############
1: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
2: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
3: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
4: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
5: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
6: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
7: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
8: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
9: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
10: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
11: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
12: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
13: In minE[x] <- which(DE[, i] == MEDis[i]) :
  number of items to replace is not a multiple of replacement length
################

elecDF$closest <- minE
