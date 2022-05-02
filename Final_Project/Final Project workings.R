# this is my key, please don't use it. I switch between computers
register_google(key = "AIzaSyCv8INptsfW-5J2qv2WAQBkbpEbE0233z4")


# guide to the abreviations used in the dataset
# https://afdc.energy.gov/data_download/alt_fuel_stations_format

# actual code ####

library(ggplot2)
library(geosphere)
library(tidyverse)
library(usmap)
library(ggmap)
library(rstudioapi)

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

Ustation <- stations[which(stations$STATE == 'UT'),]

Ustation <- Ustation[which(Ustation$ACCESS_CODE == "public"),]

Ustation <- Ustation[!duplicated(Ustation$LATITUDE) & !duplicated(Ustation$LONGITUDE),]

# BD information One station####

UBDS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'BD'),] # only one station

# ELEC information 802 stations####
UELECS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'ELEC'),]

UEDF <- data.frame(UELECS$LATITUDE,UELECS$LONGITUDE,site=factor(1:802))

DEC <-  distm(UEDF[,c(2,1)]) %>%  as.data.frame()

DEC[DEC==0] <- NA

MEDis <- apply(DEC,1,min,na.rm=TRUE)

minE <- c()
x = 1
for(i in 1:length(MEDis)){
  minE[x] <- which(DEC[,i] == MEDis[i])  
  x=x+1
}

UEDF$closest <- minE

nearest_distE <- c()
for(i in 1:length(UEDF$closest)){
  nearest_distE[i] <- distHaversine(UEDF[i,c(2,1)],UEDF[UEDF$closest[i],c(2,1)])  
}

UEDF$nearest_dist_m <- nearest_distE

UEDF

#Map of electric stations

UTEmap <- ggmap(get_googlemap(center = c(lon = -111.85298,lat =39.3958),
                              zoom = 7,
                              maptype = 'terrain',
                              color = 'color')) +
  geom_point(data = UEDF, aes(x=UELECS.LONGITUDE,y=UELECS.LATITUDE), 
             color = ifelse(UEDF$nearest_dist_m < 160934, "green", "red"), size = .5)

print(UTEmap)

# LNG information Two stations####

ULNGS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'LNG'),]

ULDF <- data.frame(ULNGS$LATITUDE,ULNGS$LONGITUDE,site=factor(1:2))

DLC <-  distm(ULDF[,c(2,1)]) %>%  as.data.frame()

DLC[DLC==0] <- NA

MLDis <- apply(DLC,1,min,na.rm=TRUE)

minL <- c()
x = 1
for(i in 1:length(MLDis)){
  minL[x] <- which(DLC[,i] == MLDis[i])  
  x=x+1
}

ULDF$closest <- minL

nearest_distL <- c()
for(i in 1:length(ULDF$closest)){
  nearest_distL[i] <- distHaversine(ULDF[i,c(2,1)],ULDF[ULDF$closest[i],c(2,1)])  
}

ULDF$nearest_dist_m <- nearest_distL

ULDF

#Map of electric stations

UTLDmap <- ggmap(get_googlemap(center = c(lon = -111.85298,lat =39.3958),
                               zoom = 7,
                               maptype = 'terrain',
                               color = 'color')) +
  geom_point(data = ULDF, aes(x=ULNGS.LONGITUDE,y=ULNGS.LATITUDE), 
             color = ifelse(ULDF$nearest_dist_m < 160934, "green", "red"), size = 2)

print(UTLDmap)

# CNG information 27 stations####

UCNGS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'CNG'),]

UCDF <- data.frame(UCNGS$LATITUDE,UCNGS$LONGITUDE,site=factor(1:27))

DC <-  distm(UCDF[,c(2,1)]) %>%  as.data.frame()

DC[DC==0] <- NA

MCDis <- apply(DC,1,min,na.rm=TRUE)

minC <- c()
x = 1
for(i in 1:length(MCDis)){
  minC[x] <- which(DC[,i] == MCDis[i])  
  x=x+1
}

UCDF$closest <- minC

nearest_distC <- c()
for(i in 1:length(UCDF$closest)){
  nearest_distC[i] <- distHaversine(UCDF[i,c(2,1)],UCDF[UCDF$closest[i],c(2,1)])  
}

UCDF$nearest_dist_m <- nearest_distC

UCDF

# cng map

UTCmap <- ggmap(get_googlemap(center = c(lon = -111.85298,lat =39.3958),
                              zoom = 7,
                              maptype = 'terrain',
                              color = 'color')) +
  geom_point(data = UCDF, aes(x=UCNGS.LONGITUDE,y=UCNGS.LATITUDE), 
             color = ifelse(UCDF$nearest_dist_m < 160934, "green", "red"), size = 1.5)

print(UTCmap)
# E85 information One station ####

UE85S <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'E85'),] # only one

# LPG information 38 stations####

ULPGS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'LPG'),]

ULPGDF <- data.frame(ULPGS$LATITUDE,ULPGS$LONGITUDE,site=factor(1:38))

DLPGC <-  distm(ULPGDF[,c(2,1)]) %>%  as.data.frame()

DLPGC[DLPGC==0] <- NA

MLPGDis <- apply(DLPGC,1,min,na.rm=TRUE)

minLPG <- c()
x = 1
for(i in 1:length(MLPGDis)){
  minLPG[x] <- which(DLPGC[,i] == MLPGDis[i])  
  x=x+1
}

ULPGDF$closest <- minLPG

nearest_distLPG <- c()
for(i in 1:length(ULPGDF$closest)){
  nearest_distLPG[i] <- distHaversine(ULPGDF[i,c(2,1)],ULPGDF[ULPGDF$closest[i],c(2,1)])  
}

ULPGDF$nearest_dist_m <- nearest_distLPG

ULPGDF

# lpg map

UTLmap <- ggmap(get_googlemap(center = c(lon = -111.85298,lat =39.3958),
                              zoom = 7,
                              maptype = 'terrain',
                              color = 'color')) +
  geom_point(data = ULPGDF, aes(x=ULPGS.LONGITUDE,y=ULPGS.LATITUDE), 
             color = ifelse(ULPGDF$nearest_dist_m < 160934, "green", "red"), size = 1.5)

print(UTLmap)
# HY information NO stations####

UHYS <- Ustation[which(Ustation$FUEL_TYPE_CODE == 'HY'),] # no stations

# Utah map ####
UTmap <- ggmap(get_googlemap(center = c(lon = -111.85298,lat =40.3958),
                             zoom = 8,
                             maptype = 'terrain',
                             color = 'color')) +
  geom_point(data = Ustation, aes(x=LONGITUDE,y=LATITUDE), color = "red", size = .5)

print(UTmap) # this is the map of the wasatch front, dots are stations
# Utah Population map ####

plot_usmap(data = countypop, regions = "counties", include = "UT", values = "pop_2015", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Population (2015)", label = scales::comma
  ) + theme(legend.position = "right") +
  labs(title = "Utah Population by County",
       subtitle = "2015 Census")
