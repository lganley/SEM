library(ggplot2)
library(ggmap)
library(maptools)
library(dplyr)
library(tidyr)
library(maps)
library(geosphere)
library(grid)
library(ggsn)
library(rgdal) 
library(raster)
library(rgeos)
library(marmap)


station_location <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/sem_data_station_locations.csv")
ecomon <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_calanus/EcoMon_Plankton_Data_v3_1.csv")
cpr <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_calanus/Gulf of Maine CPR (Feb 25, 2014 update)_from_chris_melrose.csv")
bedford_reg_strat <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/CTD_regional/Bedford_institute/clim_8598_a.csv")
trax <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/maps/tracklines.csv")


## filter bedford institute stratification data for 98 - 2017 and jan-may
bedford_strat_year_month <- filter(bedford_reg_strat, YEAR >= "1998") %>%
  filter(MONTH == "1" | MONTH == "2" | MONTH == "3" | MONTH == "4" | MONTH == "5") 

## filter CPR data for just 98 - 2017 and Jan - may
cpr_year_month <-  filter(cpr, Year >= "1998") %>%
  filter(Month == "1" | Month == "2" | Month == "3" | Month == "4" | Month == "5") %>%
  mutate(longitude = Longitude*(-1))

## separate date into year month day and filter ecomon data for just 98 - 2017 and Jan - May
ecomon_year_month <- separate(ecomon, date, c("dd", "MM", "YYYY"), sep = "-") %>%
  filter(YYYY == "98" | YYYY == "99" | YYYY == "00" | YYYY == "01" | YYYY == "02" |
           YYYY == "03" | YYYY == "04" | YYYY == "05" | YYYY == "06" | YYYY == "07" |
           YYYY == "08" | YYYY == "09" | YYYY == "10" | YYYY == "11" | YYYY == "12" |
            YYYY == "13" | YYYY == "14" | YYYY == "15" | YYYY == "16" | YYYY == "17") %>%
  filter(MM == "Jan" | MM == "Feb" | MM == "Mar" | MM == "Apr" | MM == "May")
  

## center the map around this location
CCB <- c(lon = -68.5361, lat = 43.7451)

## plot the buoys and stations being used for the SEM
map <- get_map(location = CCB , zoom = 7,  ##get_map pulls a blank map
               maptype = "watercolor", source = 'stamen', 
               color = "color")

ccb_zoom <- c(lon = -70.3, lat = 42.0)

map_ccb <- get_map(location = ccb_zoom, zoom = 9, maptype = "watercolor", 
                   source = "stamen", color = "color")

## pass the map we got from get_map into a ggplot plotting function
p <- ggmap(map) +
  geom_point(aes(x = lon, y = lat), data = ecomon_year_month, alpha = .05, colour = "purple", na.rm = T, show.legend = TRUE) +
  geom_point(aes(x = MIN_LONGITUDE, y = MIN_LATITUDE), data = bedford_strat_year_month, alpha = .05, colour = "orange", na.rm = T, show.legend = TRUE) +
  geom_point(aes(x = longitude, y = Latitude), data = cpr_year_month, alpha = .1, colour = "green", show.legend = TRUE) +
  geom_point(aes(x = long, y =  lat, color = data_type), 
             data = station_location, alpha = .8, na.rm = T, show.legend = TRUE) +
  scale_colour_manual(values = c("red", "black", "purple", "orange", "green")) +
  geom_text(aes(x=MIN_LONGITUDE, y=MIN_LATITUDE, label=AREA_NAME), data=bedford_strat_year_month, hjust=-0.5, size = 2.5)


## combine year and month columns so i can see if there is a data point for every month
## of every year
cpr_column_select <- cpr_year_month[ , 1:33]
cpr_unite <- unite(cpr_column_select, Yearmonth, Year, Month, sep = "-") %>%
  mutate(longitude = Longitude*(-1))

## PLOT OF CPR STATIONS BY YEAR AND MONTH
cpr_stations <- ggmap(map) +
  geom_point(aes(x = longitude, y = Latitude, color = as.factor(Yearmonth)), data = cpr_unite)


## combine year nad month columns of ecomon data so i can see if there is a data
## point for every month fo every year
ecomon_unite <- unite(ecomon_year_month, Yearmonth, YYYY, MM, sep = "-")

ecomon_stations <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, color = as.factor(Yearmonth)), data = ecomon_unite)


## create a polygon around the GOM
library(raster)
gom_poly <- Polygon(rbind(c(-73.80, 39.00), c(-65.55, 39.00), c( -65.55, 44.80), c(-73.80, 44.80)))
## create a polygon for CCB (we want to remove CCB from the GOM polygon so it gets hole = TRUE)
## ccb_poly is the poly I gave Dan for CCB SST data
ccb_poly <- Polygon(rbind(c(-70.63, 42.08), c(-70.09, 42.08), c( -70.02, 41.764), c(-70.59, 41.764)))
## this is the poly that ideally I would be able to get all the zpl data from.  I'm going to 
## compare the data from this poly with a poly that I make of southern new england
## gom_ideal_poly1 is the polygon I gave to Dan for SST data
gom_ideal_poly1 <- Polygon(rbind(c(-70.63, 42.08), c(-70.65, 43.35), c( -65.55, 43.50), c( -65.55, 42.08)))
gom_ideal_poly <- Polygon(rbind(c(-70.63, 42.08), c(-70.80, 43.50), c( -65.55, 43.50), c( -65.55, 43.00), c(-66.00, 43.00), c(-67.50, 42.50), c(-68.00, 42.10), c(-68.00, 42.10)))
gom_ideal_poly <- Polygon(rbind(c(-70.63, 42.08), c(-70.80, 42.55), c( -65.55, 43.50), c( -65.55, 43.00), c(-66.00, 43.00), c(-67.50, 42.50), c(-68.00, 42.10), c(-68.00, 42.10)))
## this is the southern new england polygon
southern_poly <- Polygon(rbind(c(-65.55, 42.08),  c( -65.55, 39.00), c(-74.70, 39.00), c(-74.20, 41.00), c(-70.59, 41.764), c( -70.02, 41.764), c(-70.09, 42.08)))

# convert the polygon to a spatial polygon
gom <- Polygons(list(gom_poly), "gom")
ccb <- Polygons(list(ccb_poly), "ccb")
gom_ideal <- Polygons(list(gom_ideal_poly), "northern_gom")
gom_ideal1 <- Polygons(list(gom_ideal_poly1), "northern_gom")
southernpoly <- Polygons(list(southern_poly), "southern")
gompolygon <- SpatialPolygons(list(gom))
gomccbpoly <- SpatialPolygons(list(gom, ccb))
ccbpoly <- SpatialPolygons(list(ccb))
gom_idealpoly <- SpatialPolygons(list(gom_ideal))
gom_idealpoly1 <- SpatialPolygons(list(gom_ideal1))
southern_poly_sp <- SpatialPolygons(list(southernpoly))


## Project the polygons to WGS84
proj4string(gompolygon) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84
proj4string(gomccbpoly) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84
proj4string(ccbpoly) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84
proj4string(gom_idealpoly) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84
proj4string(gom_idealpoly1) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84
proj4string(southern_poly_sp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

# Next the shapefile has to be converted to a dataframe for use in ggplot2
gom_shape <- fortify(gompolygon)
gomccb_shape <- fortify(gomccbpoly)
ccb_shape <- fortify(ccbpoly)
gom_ideal_shape <-fortify(gom_idealpoly)
gom_ideal_shape1 <-fortify(gom_idealpoly1)
southern_shape <- fortify(southern_poly_sp)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
gom_box_map <- ggmap(map) +
  geom_point(aes(x = lon, y = lat), data = ecomon_year_month, alpha = .2, colour = "purple", 
             na.rm = T, show.legend = TRUE) +
  geom_point(aes(x = MIN_LONGITUDE, y = MIN_LATITUDE), data = bedford_strat_year_month,
             alpha = .05, colour = "orange", na.rm = T, show.legend = TRUE) +
  geom_point(aes(x = longitude, y = Latitude), data = cpr_year_month, alpha = .2, 
             colour = "green", show.legend = TRUE) +
  geom_point(aes(x = long, y =  lat, color = data_type), 
             data = station_location, alpha = .8, na.rm = T, show.legend = TRUE) +
  scale_colour_manual(values = c("red", "black", "purple", "orange", "green")) +
  geom_text(aes(x=MIN_LONGITUDE, y=MIN_LATITUDE, label=AREA_NAME), 
            data=bedford_strat_year_month, hjust=-0.5, size = 2.5)+
  geom_polygon(data = gom_shape, aes(x = long, y = lat), colour="black", fill = .2) +
  geom_polygon(data = ccb_shape, aes(x = long, y = lat), colour = "black", fill = .2) +
  geom_polygon(data = gom_ideal_shape1, aes(x = long, y = lat), colour = "purple", fill = .2) +
  coord_map()

## map with the ideal gom polygon and southern polygon
gom_ideal_map <- ggmap(map) +
  geom_point(aes(x = lon, y = lat), data = ecomon_year_month, alpha = .2, colour = "purple", 
             na.rm = T, show.legend = TRUE) +
  geom_point(aes(x = MIN_LONGITUDE, y = MIN_LATITUDE), data = bedford_strat_year_month,
             alpha = .05, colour = "orange", na.rm = T, show.legend = TRUE) +
  geom_point(aes(x = longitude, y = Latitude), data = cpr_year_month, alpha = .2, 
             colour = "green", show.legend = TRUE) +
  geom_point(aes(x = long, y =  lat, color = data_type), 
             data = station_location, alpha = .8, na.rm = T, show.legend = TRUE) +
  scale_colour_manual(values = c("red", "black", "purple", "orange", "green")) +
  geom_text(aes(x=MIN_LONGITUDE, y=MIN_LATITUDE, label=AREA_NAME), 
            data=bedford_strat_year_month, hjust=-0.5, size = 2.5)+
  geom_polygon(data = gom_ideal_shape, aes(x = long, y = lat), colour="black", fill = .2) +
  geom_polygon(data = ccb_shape, aes(x = long, y = lat), colour = "purple", fill = .2) +
  geom_polygon(data = southern_shape, aes(x = long, y = lat), colour = "red", fill = .2) +
  coord_map()

## combine year and month so I can plot by yearmonth
ecomon_yrmonth_unite <- unite(ecomon_year_month, Yearmonth, YYYY, MM, sep = "-")

## convert hte ecomon data to a spatial data frame
xy <- ecomon_yrmonth_unite[,c(6,5)] ## pull out the lat longs and call them xy

## project them to WGS84
ecomon_sp <- SpatialPointsDataFrame(coords = xy, data = ecomon_yrmonth_unite,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


## clip ecomon stations inside gom shape
gom_inside <- ecomon_sp[gompolygon, ]
## select the ecomon stations that are inside the gom shape and inside ccb
sel <- over(gom_inside, ccbpoly)
## subset for everything but the points that are within ccb (so this is within the 
## GOM excluding CCB)
gom_outside <- gom_inside[is.na(sel),]

## get data just in ccb.  If they took CTDs at these stations this might help
## with some of my missing stratification data for CCB
ccb_ecomon <- as.data.frame(gom_inside[ccbpoly, ])


## clip the ecomon stations inside the gom ideal shape
gom_ideal_inside <- ecomon_sp[gom_idealpoly, ]
## select the ecomon stations that are inside the gom ideal shape and within ccb
sel_gomideal_ccb <- over(gom_ideal_inside, ccbpoly)
## subset for everything in the ideal gom poly except the points that are within ccb
gom_ideal_outside <- gom_ideal_inside[is.na(sel_gomideal_ccb), ]


## clip the ecomon stations inside the southern shape
southern_inside <- ecomon_sp[southern_poly_sp, ]



## plot the ecomon stations within the GOM but outside CCB
ecomon_in_gompoly <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, color = "Yearmonth"), 
             data = as.data.frame(coordinates(gom_outside)), 
             alpha = .8, show.legend = TRUE) +
  geom_polygon(data = gom_shape, aes(x = long, y = lat),
               colour = "black", fill = .2) +
  geom_polygon(data = ccb_shape, aes(x = long, y = lat),
               colour = "black", fill = .2)

## plot the ecomon stations within the ideal GOM and southern area
ecomon_ideal_gom <- ggmap(map) +
  geom_point(aes(x = lon, y = lat), 
             data = as.data.frame(coordinates(gom_ideal_outside)), 
             alpha = .8, show.legend = TRUE) +
  geom_point(aes(x = lon, y = lat), 
             data = as.data.frame(coordinates(southern_inside)), 
             alpha = .8, color = "blue", show.legend = TRUE) +
  geom_point(aes(x = longitude, y = Latitude), data = cpr_year_month, alpha = .2, 
             colour = "green", show.legend = TRUE) +
  geom_polygon(data = gom_ideal_shape, aes(x = long, y = lat),
               colour = "black", fill = .2) +
  geom_polygon(data = ccb_shape, aes(x = long, y = lat),
               colour = "black", fill = .2) +
  geom_polygon(data = southern_shape, aes(x = long, y = lat),
               colour = "black", fill = .2) 



## plot just the data that seems like it might be useable

## this is to put a box around ccb on the big gom map

ccb_poly_data <- rbind(c(-70.63, 42.08), c(-70.09, 42.08), c( -70.02, 41.764), 
                       c(-70.59, 41.764))
xlim <- range(ccb_poly_data[, 1]) + c(-1, 0.5)
ylim <- range(ccb_poly_data[,2]) + c(-0.5, 1)



###################
####don't use get_map basemaps
###################


# First read in the shapefile of the united states
us.shape <-readOGR("/Users/laura.ganley001/Documents/R_Projects/SEM/maps/us_state_shapefile/cb_2017_us_state_500k.shp")
## shapefile of canada
canada.shape <- readOGR("/Users/laura.ganley001/Documents/R_Projects/SEM/maps/canada_shapefile/gpr_000b11a_e.shp")
## bathy shapefile
dat <- getNOAA.bathy(-71.3, -65.0, 41.5, 46.0, res=1, keep=TRUE)
## clip these shapefiles to the region I need (hopefully this will make plotting faster)

e <- extent(-71.3, -65.0,41.5, 46.0) ## this is the extent of the map
us.crp <- crop(us.shape, e) ## crop the us shapefile to extent e
canada.crp <- crop(canada.shape, e) ## crop teh canada shapefile to extent e


# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df <- fortify(us.crp)
shapefile_canada <- fortify(canada.crp)



east.map.auto <- autoplot(dat, geom=c("r", "c"), 
                          colour="white", size=0.1, coast = TRUE) +
  scale_fill_etopo(guide = FALSE) +
 ## scale_fill_gradient("Bathymetry", low = "mediumblue", high = "lightblue") +
##  geom_polygon(data = shapefile_df, fill = "gray94", ## u.s. shapefile
##               aes(x = long, y = lat, group = group),
##               color = 'black', size = .5) +
##  geom_polygon(data = shapefile_canada, fill = "gray94", 
##              aes(x = long, y = lat, group = group),
##               color = 'black', size = .5) +
  geom_polygon(data = gom_ideal_shape1, 
               aes(x = long, y = lat, alpha = "Regional SST", fill = piece),
               colour = "black", fill = .1) + ##regional SST poly
  geom_polygon(data = ccb_shape, 
               aes(x = long, y = lat, alpha = "Local SST", fill = piece), 
               colour = "black", fill = .3) + ##CCB SST poly
  geom_path(data = trax, alpha = 1, 
            aes(x=long, y = lat, color = "Aerial survey tracklines")) + ## add tracklines
  geom_point(data = cpr_year_month, 
             aes(x = longitude, y = Latitude, color = "Regional Calanus finmarchicus")) + ## reg calfin
  labs(x = "", y = "", colour = "", alpha = "")  +
geom_point(aes(x = long, y =  lat, color = data_type), 
           data = station_location, na.rm = T) +
  scale_colour_manual(name = "Regional and Local variables",
                      labels = c("", "Aerial survey tracklines", 
                                 "Local stratification", "Local wind speed and direction",
                                 "Local zooplankton", "Regional Calanus finmarchicus", 
                                 "Regional wind speed and direction"),
                        values = c("white", "purple", "deeppink3", "red", "black", "blue", "darkgreen")) +
  ggsn::scalebar(shapefile_df, dist = 50, st.size=2, height=0.02, dd2km = TRUE,
                 location = "bottomright", model = 'WGS84', anchor = c(x = -66.0, y = 41.8)) +
  ggsn::north(shapefile_df,  location = "bottomright",  anchor = c(x = -65, y = 41.5), 
              symbol = 15, scale = 0.15) 


ccb_zoom <- autoplot(dat, geom=c("r", "c"), colour="white", size=0.1) +
  scale_fill_etopo() +
 geom_polygon(data = shapefile_df, fill = "forestgreen", ## u.s. shapefile
                           aes(x = long, y = lat, group = group),
                             color = 'black', size = .5) +
  geom_polygon(data = ccb_shape, aes(x = long, y = lat, ## CCB SST
                                    fill = piece),
                                    colour = "black", fill = .2) +
  geom_path(data = trax, aes(x=long, y = lat, color = "Aerial Survey Tracklines")) + ##trax
  geom_point(data = cpr_year_month, alpha = .2, ## CPR
             aes(x = longitude, y = Latitude, color = "Regional Calanus finmarchicus")) +
  geom_point(aes(x = long, y =  lat, color = data_type), 
             data = station_location, alpha = .8, na.rm = T) +
  scale_colour_manual(values = c("white", "purple", "deeppink3", 
                                 "red", "black", "blue", "darkgreen")) +
  theme_void() +
  theme(legend.position="none")  +
  xlab("") + ylab("") +
  coord_cartesian(xlim = c(-71.0, -69.8), ylim = c(41.5, 42.2))


maptheme <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black", size = 1),
  panel.background = element_blank()
)

maptheme2 <- theme(
  panel.grid = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black"),
  panel.background = element_blank()
)

## make CCB an inset of the larger GOM map
grid.newpage()
vp_b <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vp_a <- viewport(width = 0.25, height = 0.3, x = 0.22, y = 0.8)  # the inset in upper left
print(east.map.auto + maptheme2 , vp = vp_b)
print(ccb_zoom + maptheme, vp = vp_a)

