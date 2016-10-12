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

get_apn_characteristics_url <- function(apn) {
  prefix <- "http://sccounty01.co.santa-cruz.ca.us/ASR/Characteristics/linkHREF"
  # The website suggests this "outside" value...
  outside <- "outSide=true"
  apnquery <- paste("txtAPN", sep = "=", apn)
  query <- paste(apnquery, outside, sep = "&")
  url <- paste(prefix, query, sep="?")
  
  return(url)
}

get_apn_characteristics_html <- function(apn) {
  url <- get_apn_characteristics_url(apn)
  
  html <- read_html(url)
  return(html)
}

get_apn_tax_url <- function(apn) {
  prefix <- "http://sccounty01.co.santa-cruz.ca.us/ASR/ParcelList/linkHREF"
  # The website suggests this "outside" value...
  outside <- "outSide=true"
  apnquery <- paste("txtAPN", sep = "=", apn)
  query <- paste(apnquery, outside, sep = "&")
  url <- paste(prefix, query, sep="?")
  
  return(url)
}

get_apn_tax_html <- function(apn) {
  url <- get_apn_tax_url(apn)

  html <- read_html(url)
  return(html)
}

get_characteristic_page_int <- function(html, i1, i2) {
  val <- get_characteristic_page_str(html, i1, i2)
  if (val == "None")
    return(NA)
  if (val == "N/A")
    return(NA)
  return(as.integer(val))
}

get_characteristic_page_str <- function(html, i1, i2) {
  selector <- paste0("body center table tr td table tr:nth-child(6) td div:nth-child(", i1, ") div div.plmTd_AOM")
  nodes <- html_nodes(html, selector)
  if (length(nodes) < i2) {
    return("None")
  }
  val <- html_text(nodes[[i2]])

  # for multi-unit parcels, choose the first.
  if (length(val) > 1)
    print("warning: only using first data for parcel")
  
  firstval <- val[[1]]
  return(trim_info(firstval))
}

get_year_built <- function(html) {
  return(get_characteristic_page_int(html, 1, 5))
}

get_effective_year <- function(html) {
  return(get_characteristic_page_int(html, 1, 6))
}

get_num_units <- function(html){
  return(get_characteristic_page_int(html, 1, 7))
}

get_main_area <- function(html) {
  return(get_characteristic_page_int(html, 2, 4))
}

get_num_rooms <- function(html) {
  return(get_characteristic_page_int(html, 2, 5))
}

get_bedrooms <- function(html) {
  return(get_characteristic_page_int(html, 2, 6))
}

get_bathrooms <- function(html) {
  return(get_characteristic_page_str(html, 2, 7))
}

get_roof <- function(html) {
  return(get_characteristic_page_str(html, 2, 8))
}

get_heat <- function(html) {
  return(get_characteristic_page_str(html, 2, 9))
}

get_fireplaces <- function(html) {
  return(get_characteristic_page_int(html, 1, 10))
}

get_pools <- function(html) {
  return(get_characteristic_page_str(html, 3, 6))
}

get_apn_characteristics_data <- function(html) {
  year_built <- get_year_built(html)
  effective_year <- get_effective_year(html)
  num_units <- get_num_units(html)
  num_rooms <- get_num_rooms(html)
  bedrooms <- get_bedrooms(html)
  bathrooms <- get_bathrooms(html)
  roof <- get_roof(html)
  heat <- get_heat(html)
  fireplaces <- get_fireplaces(html)
  pools <- get_pools(html)

  return(
    list(
      year_built=year_built,
      effective_year=effective_year,
      num_units=num_units,
      num_rooms=num_rooms,
      bedrooms=bedrooms,
      bathrooms=bathrooms,
      roof=roof,
      heat=heat,
      fireplaces=fireplaces,
      pools=pools
    )
  )
  
}

# Send in just the apn normally; pre-compute and provide the tax_html to avoid
# hitting the web site repeatedly during debugging.
# The apn must be always be provided: it's used to verify the returned data.
get_apn_tax_raw_data <- function(apn, tax_html=NULL) {
  if (is.null(tax_html)) {
    tax_url <- get_apn_tax_url(apn)
    tax_html <- read_html(tax_url)
  }
  
  all_nodes <- html_nodes(tax_html, ".tablePrintOnly .trPrintOnly")
  
  data <- sapply(all_nodes, function(n) strsplit(html_text(n), "\\r\\n\\s*", perl=T))
  # I expect to have a list of lists, like
  # [[1]]
  # [1] "Parcel Info"
  #
  # [[2]]
  # [1] "APN"           "Situs Address" "Class"        
  #
  # [[3]]
  # [1] ""                                                                 
  # [2] "00649103"                                                         
  # [3] "127 OTIS ST,  SANTA CRUZ                      , 95060-4245       "
  # [4] "711-OTHER CHURCH PROPERTY"                                        
  #
  # [[4]]
  # [1] "Assessed Value"
  #
  # [[5]]
  # [1] "Year"      "2015/2016"
  # ...
  # Unfortunately there are two years' worth of data munged in here. I'm going to assume
  # that the most recent year appears last.
  tidy <- list()
  if (data[[1]] != "Parcel Info") {
    print(paste0("Failed to find 'Parcel Info' in 1st element for apn ", apn))
    return(tidy)
  }
  if (!all(data[[2]] == list("APN", "Situs Address", "Class"))) {
    print(paste0("Didn't find expected keys in 2nd element"))
    print(data[[2]])
    return(tidy)
  }
  if (data[[3]][2] != apn) {
    print(paste0("Didn't find apn as data[[3]][2] (found:", data[[3]][2], ")"))
    return(tidy)
  }
  tidy[["Situs Address"]] <- data[[3]][3]
  tidy[["Class"]] <- data[[3]][4]
  
  if (data[[4]] == "Assessed Value") {
    for (i in 5:length(data)) {
      item <- data[[i]]
      if (length(item) == 2)
        tidy[[item[1]]] <- item[2]      
    }
  }
  tidy[['url']] <- tax_url
  return(tidy)
}

# Next: characteristics data
get_apn_tax_data <- function(apn, html=NULL) {
  raw_data <- get_apn_tax_raw_data(apn, html)
  
  tax <- getDollarsToNumeric(raw_data, "Total")
  addr <- trim_address(trim_info(raw_data[["Situs Address"]]))
  type <- raw_data[["Class"]]
  exemption <- getDollarsToNumeric(raw_data, "Homeowners  Exemption")
  assessment <- getDollarsToNumeric(raw_data, "Net Assessment")
  return(list(tax=tax, addr=addr, type=type, exemption=exemption, assessment=assessment))
}

get_apn_tax_data.old <- function(html) {
  tax <- get_tax(html)
  addr <- get_address(html)
  type <- get_type(html)
  exemption <- get_exemption(html)
  assessment <- get_assessment(html)
  return(list(tax=tax, addr=addr, type=type, exemption=exemption, assessment=assessment))
}

get_assessment <- function(html) {
  nodes <- html_nodes(html, "body center table tr td table tr:nth-child(6) td div:nth-child(3) div div.plmTbody_AOM div")
  assessment <- 0
  if (length(nodes) >= 7) {
    asStr <- html_text(nodes[7])
    assessment <- dollarsToNumeric(asStr) 
  }
  return(assessment)
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

get_tax2 <- function(tax_data) {
  total <- tax_data[["Total"]]
  if (is.null(total)) {
    total <- 0
  } else {
    total <- dollarsToNumeric(total)
  }
  
  return(total)
}

getDollarsToNumeric <- function(data, title) {
  total <- data[[title]]
  if (is.null(total)) {
    total <- 0
  } else {
    total <- dollarsToNumeric(total)
  }
  
  return(total)
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
  return(addr)
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

area <- "00649"
plot_file <- paste0("data/final.plot.", area, ".rda")
if (file.exists(plot_file)) {
  print(paste0("Using cached plot data from ", plot_file))
  load(plot_file)
} else {
  area_regexp <- paste0("^", area)
  sm <- shp[grepl(area_regexp, lapply(shp$apnnodash, as.character)),]
  # rm(shp)

  # Make a df for parcel data
  len <- length(sm)
  parcel_data <- data.frame(
    apnnodash = sm$apnnodash,
    id = sm$objectid,
    tax = numeric(len),
    exemption = numeric(len),
    assessment = numeric(len),
    addr = character(len),
    type = character(len),
    year_built = integer(len),
    effective_year = integer(len),
    num_units = integer(len),
    num_rooms = integer(len),
    bedrooms = integer(len),
    bathrooms = integer(len),
    roof = character(len),
    heat = character(len),
    fireplaces = integer(len),
    pools = integer(len)
  )

  # hack - a few fields shouldn't be factor
  parcel_data$addr <- as.character(parcel_data$addr)
  parcel_data$type <- as.character(parcel_data$type)
  parcel_data$roof <- as.character(parcel_data$roof)
  parcel_data$heat <- as.character(parcel_data$heat)
  
  for (i in 1:len) {
    apn <- parcel_data[i,]$apnnodash
    print(paste("scraping data", "(", i, "of", len, ") for parcel", apn))
    html <- get_apn_tax_html(tax_apn)
    char_html <- get_apn_characteristics_html(apn)

    print(paste("parsing data", "(", i, "of", len, ") for parcel", apn))
    apn_data <- get_apn_tax_data(tax_html)
    char_data <- get_apn_characteristics_data(char_html)
    
    parcel_data[i,]$tax <- apn_data$tax
    parcel_data[i,]$addr <- apn_data$addr
    parcel_data[i,]$type <- apn_data$type
    parcel_data[i,]$exemption <- apn_data$exemption
    parcel_data[i,]$assessment <- apn_data$assessment
    
    parcel_data[i,]$year_built = char_data$year_built
    parcel_data[i,]$effective_year = char_data$effective_year
    parcel_data[i,]$num_units = char_data$num_units
    parcel_data[i,]$num_rooms = char_data$num_rooms
    parcel_data[i,]$bedrooms = char_data$bedrooms
    parcel_data[i,]$bathrooms = char_data$bathrooms
    parcel_data[i,]$roof = char_data$roof
    parcel_data[i,]$heat = char_data$heat
    parcel_data[i,]$fireplaces = char_data$fireplaces
    parcel_data[i,]$pools = char_data$pools
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

  save(final.plot, file=plot_file)
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
  "homeowner exemption: ", row$homeowner, "<br/>", "year built: ", row$year_built, "<br/>", row$type, "<br/>")
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

final.plot$qtax <- round(final.plot$tax, -3)
p2 <- final.plot %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  # layer_paths(fill:=~taxColor, strokeOpacity := 0.5, strokeWidth := 0.25) %>%
  layer_paths(fill = ~qtax) %>%
  # scale_numeric("fill", range = final.plot$taxColor) %>%
  scale_numeric("fill", range = c("white", "red")) %>%
  add_legend("fill", title="annual property tax") %>%
  add_tooltip(tt, "hover") %>%
  set_options(width=100, height=600, keep_aspect=T)

# homeowner's exemption
ramp2 <- colorRampPalette(
  c("white", "green"),
  space="Lab"
)
final.plot$homeownerColor <- as.character(
  cut(0 + final.plot$homeowner, 2, include.lowest=TRUE, labels=ramp2(2))
)
p3 <- final.plot %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(fill:=~homeownerColor) %>%
  add_tooltip(tt, "hover") %>%
  set_options(width=100, height=600, keep_aspect=T)

p4 <- final.plot %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(fill = ~year_built) %>%
  add_tooltip(tt, "hover") %>%
  set_options(width=100, height=600, keep_aspect=T)
