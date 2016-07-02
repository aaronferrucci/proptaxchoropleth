# shp file downloaded from:
# https://earthworks.stanford.edu/catalog/stanford-gs418bw0551
# I downloaded the shapefile under the "Generated" heading... 
# because that data has latitude and longitude in degrees, rather than
# WGS84 (I think) in the not-generated shapefile data.

# Note on the parcel number format (apnnodash):
# Each Parcel is identified by an Assessor's Parcel Number (APN), which 
# is an 8 digit number separated by dashes (e.g. 049-103-12). The first 
# 3 digits represent the Assessor's mapbook containing the Parcel (Book 
# 049 in the above example). The next 2 digits represent the page number 
# within that mapbook (Page 10 in the example). The next digit represents 
# the block number on that page (Block 3 in the example). The last 2 
# digits represent the Assessor's Parcel Number on that block (12 in the 
# example)

library(maptools)
shpfile <- "gs418bw0551/gs418bw0551.shp"
shp <- readShapeSpatial(shpfile)

# Extract a few parcels (from the neighborhood?)
sm <- shp[grepl('^006', lapply(shp$apnnodash, as.character)),]
# rm(shp)

# Make some fake data
smdata <- data.frame(
  apnnodash = sm$apnnodash,
  id = sm$objectid,
  val = rnorm(length(sm), 10)
)

# test that sm and smdata have the same APNNODASH
all(sm$objectid == smdata$id)

library(ggplot2)
library(rgdal)
library(rgeos)
sm.f <- fortify(sm, region="objectid")
class(sm.f)

merge.shp.coef<-merge(sm.f, smdata, by="id", all.x=TRUE)
final.plot <- merge.shp.coef[order(merge.shp.coef$order), ] 

ggplot() +
  geom_polygon(data = final.plot, 
    aes(x = long, y = lat, group = group, fill = val), 
    color = "black", size = 0.25) + 
  coord_map()
