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
library(rvest)

get_apn_data <- function(apn) {
  html <- get_apn_html(apn)
  tax <- get_tax(html)
  addr <- get_address(html)
  type <- get_type(html)
  return(list(tax=tax, addr=addr, type=type))
}

get_apn_html <- function(apn) {
  prefix <- "http://sccounty01.co.santa-cruz.ca.us/ASR/ParcelList/linkHREF"
  # The website suggests this "outside" value...
  outside <- "outSide=true"
  apnquery <- paste("txtAPN", sep = "=", apn)
  query <- paste(apnquery, outside, sep = "&")
  url <- paste(prefix, query, sep="?")

  html <- read_html(url)
  return(html)
}

# return the total yearly tax as a numeric.
get_tax <- function(html) {
  nodes <- html_nodes(html, "body center table tr td table tr:nth-child(4) td")
  # Some parcels don't have tax data
  #  e.g. apn 00649211, of class "940-SCHOOL DISTRICT APN"
  proptax <- 0
  if (length(nodes) >= 6) {
    proptaxStr <- html_text(
      nodes[[6]]
    )
    # Drop '$', ','
    p1 <- sub('$', '', proptaxStr, fixed=TRUE)
    p2 <- sub(',', '', p1, fixed=TRUE)
    proptax <- as.numeric(p2)
  }
  return(proptax)
}

trim_info <- function (x) {
  # remove leading, trailing whitespace
  x <- gsub("^\\s+|\\s+$", "", x)
  # remove space before comma
  x <- gsub("\\s+,", ",", x)
  # collapse any remaining sequences of space to single space
  x <- gsub("\\s+", " ", x)
  return(x)
}

# return the parcel address as a character string.
get_address <- function(html) {
  return(get_parcel_info(html, 2))
}

get_type <- function(html) {
  return(get_parcel_info(html, 3))
}

get_parcel_info <- function(html, child) {
  selector <-
    paste0("body center table  tr td table tr:nth-child(6) td div:nth-child(2)
    div div.plmTbody div div:nth-child(", child, ")")
  info <- html_text(html_nodes(html, selector))
  return(trim_info(info))
}

if (file.exists("data/shp.rda")) {
  load("data/shp.rda")
} else {
  shpfile <- "gs418bw0551/gs418bw0551.shp"
  shp <- readShapeSpatial(shpfile)
  save(shp, file="data/shp.rda")
}

if (file.exists("data/final.plot.rda")) {
  load("data/final.plot.rda")
} else {
  # Extract a few parcels (from the neighborhood?)
  sm <- shp[grepl('^0064', lapply(shp$apnnodash, as.character)),]
  # rm(shp)

  # Make a df for parcel data
  len <- length(sm)
  parcel_data <- data.frame(
    apnnodash = sm$apnnodash,
    id = sm$objectid,
    tax = numeric(len),
    addr = character(len),
    type = character(len)
  )

  # hack - addr shouldn't be factor
  parcel_data$addr <- as.character(parcel_data$addr)
  parcel_data$type <- as.character(parcel_data$type)

  # Populate the df with parcel data
  for (i in 1:len) {
    apn <- parcel_data[i,]$apnnodash
    print(paste("scraping data", "(", i, "of", len, ") for parcel", apn))
    apn_data <- get_apn_data(parcel_data[i,]$apnnodash)
    parcel_data[i,]$tax <- apn_data$tax
    parcel_data[i,]$addr <- apn_data$addr
    parcel_data[i,]$type <- apn_data$type
  }

  # test that sm and parcel_data have the same APNNODASH
  all(sm$objectid == parcel_data$id)

  library(ggplot2)
  library(rgdal)
  library(rgeos)
  sm.f <- fortify(sm, region="objectid")
  class(sm.f)

  merge.shp.coef<-merge(sm.f, parcel_data, by="id", all.x=TRUE)
  final.plot <- merge.shp.coef[order(merge.shp.coef$order), ] 
  save(final.plot, file="data/final.plot.rda")
}

p <- ggplot() +
  geom_polygon(data = final.plot, 
    aes(x = long, y = lat, group = group, fill = tax), 
    color = "black", size = 0.25) + 
  coord_map() +
  scale_fill_gradient(low = "white", high = "red")
print(p)



