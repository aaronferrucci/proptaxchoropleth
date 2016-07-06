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
library(ggplot2)

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

get_apn_data <- function(html) {
  tax <- get_tax(html)
  addr <- get_address(html)
  type <- get_type(html)
  exemption <- get_exemption(html)
  return(list(tax=tax, addr=addr, type=type, exemption=exemption))
}

get_exemption <- function(html) {
  nodes <- html_nodes(html, "body center table tr td table tr:nth-child(6) td div:nth-child(3) div div.plmTbody_AOM div")
  exemption <- 0
  if (length(nodes) >= 6) {
    exStr <- html_text(nodes[6])
    exemption <- dollarsToNumeric(exStr) 
  }
  return(exemption)
}

dollarsToNumeric <- function(x) {
  # Drop '$', ','
  p1 <- sub('$', '', x, fixed=TRUE)
  p2 <- sub(',', '', p1, fixed=TRUE)
  return(as.numeric(p2))
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

trim_address <- function(addr) {
  addr <- gsub(", SANTA CRUZ,.*$", "", addr)
}

# return the parcel address as a character string.
get_address <- function(html) {
  addr <- get_parcel_info(html, 2)
  addr <- trim_address(addr) 
  return(addr)
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
    exemption = numeric(len),
    addr = character(len),
    type = character(len)
  )

  # hack - addr shouldn't be factor
  parcel_data$addr <- as.character(parcel_data$addr)
  parcel_data$type <- as.character(parcel_data$type)
 
  # Issue: loading saved parcel_html crashes R studio.
  if (FALSE && file.exists("data/parcel_html.rda")) {
    print("using cached html parcel data from data/parcel_html.rda")
    load("data/parcel_html.rda")
  } else {
    parcel_html <- vector("list", len)
    for (i in 1:len) {
      apn <- parcel_data[i,]$apnnodash
      print(paste("scraping data", "(", i, "of", len, ") for parcel", apn))
      parcel_html[[i]] <- get_apn_html(apn)
    }
    save(parcel_html, file="data/parcel_html.rda")
  }

  # Populate the df with parcel data
  for (i in 1:len) {
    html <- parcel_html[[i]]
    apn_data <- get_apn_data(html)
    parcel_data[i,]$tax <- apn_data$tax
    parcel_data[i,]$addr <- apn_data$addr
    parcel_data[i,]$type <- apn_data$type
    parcel_data[i,]$exemption <- apn_data$exemption
  }

  # some derived parcel data
  parcel_data$homeowner <- parcel_data$exemption == 7000

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

# ggvis experiment
# have a look at https://github.com/vega/vega; maybe this
# can export an svg file with tooltips.
library(ggvis)
get_row_by_group <- function(group) {
  rows <- final.plot[final.plot$group == group, ]
  row <- rows[1, ]

  row
}

tt <- function(x) {
  row <- get_row_by_group(x$group)
  paste0(row$apnnodash, "<br/>", row$addr, "<br/>", "tax: $", row$tax, "<br/>",
  "homeowner exemption: ", row$homeowner, "<br/>", row$type, "<br/>")
}

# make a palette from white, through yellow, to red.
library(RColorBrewer)
ramp <- colorRampPalette(
  c("white", brewer.pal(n=9, name="YlOrRd")),
  space="Lab"
)
# color for property tax
final.plot$taxColor <- as.character(
  cut(final.plot$tax, 20, include.lowest=TRUE, labels=ramp(20))
)

p2 <- final.plot %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(fill:=~taxColor, strokeOpacity := 0.5, strokeWidth := 0.25) %>%
  add_tooltip(tt, "hover") %>%
  set_options(width=100, height=600, keep_aspect=T)

# homeowner's exemption
final.plot$homeownerColor <- as.character(
  cut(0 + final.plot$homeowner, 2, include.lowest=TRUE, labels=ramp(2))
)
p3 <- final.plot %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(fill:=~homeownerColor) %>%
  add_tooltip(tt, "hover") %>%
  scale_numeric("fill", range=c("#bfd3e6", "#8c6bb1" ,"#4d004b")) %>%
  set_options(width=100, height=600, keep_aspect=T)

