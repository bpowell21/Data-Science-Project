install.packages("maptools")
install.packages("rgdal")
install.packages("ggplot2")
install.packages("gpclib")

library(tidyverse)
library(maptools)
library(rgdal)
gpclibPermit()

#Sets the root folder for reading files
setwd("~/Documents/Marquette/Sophomore/Intro to Data Science/Pandas Project/")

county_shp <- readOGR(dsn="dtl_cnty", layer="dtl_cnty")

county_data <- county_shp@data
county_data[6:53] <- NULL

county_shp@data <- county_data

county_shp@data$id <- rownames(county_shp@data)
county_shp.points <- fortify(county_shp, region="id")
#county_shp.df <- join(county_shp.points, county_shp@data, by="id")

gg <- ggplot() + geom_map(data=county_shp.points, map = county_shp.points, aes(map_id=id, x = long, y = lat), 
                    fill="white", color="#7f7f7f", size=0.25)
gg
