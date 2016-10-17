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
library(rvest)

get_plot_data <- function(area = "006") {
  
  if (file.exists("data/shp.rda")) {
    load("data/shp.rda")
  } else {
    shpfile <- "gs418bw0551/gs418bw0551.shp"
    shp <- readShapeSpatial(shpfile)
    save(shp, file="data/shp.rda")
  }

  plot_file <- paste0("data/final.plot.", area, ".rda")
  if (file.exists(plot_file)) {
    print(paste0("Using cached plot data from ", plot_file))
    load(plot_file)
  } else {
    area_regexp <- paste0("^", area)
    sm <- shp[grepl(area_regexp, lapply(shp$apnnodash, as.character)),]

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
    
    active_record <- logical(len)
    inactive_apns <- character(len)
    for (i in 1:len) {
      apn <- parcel_data[i,]$apnnodash
      print(paste("scraping data", "(", i, "of", len, ") for parcel", apn))
      char_html <- get_apn_characteristics_html(apn)
      char_data <- get_apn_characteristics_data(apn, char_html)
      if (is.null(char_data)) {
        # Inactive parcel, or other form of "no data"
        active_record[i] <- FALSE
        inactive_apns[i] <- as.character(apn)
        next
      }
      active_record[i] <- TRUE
      print(paste("parsing data", "(", i, "of", len, ") for parcel", apn))
      tax_html <- get_apn_tax_html(apn)
      apn_data <- get_apn_tax_data(apn, tax_html)
      
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
    
    inactive_apns <- inactive_apns[inactive_apns != ""]
    if (length(inactive_apns > 0)) {
      print("Omitting these inactive apns:")
      print(inactive_apns[inactive_apns != ""])
      # Trim the inactives
      parcel_data <- parcel_data[active_record,]
      sm <- sm[active_record,]
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
  return(final.plot)
}

get_url <- function(page, apn) {
  prefix <- paste0("http://sccounty01.co.santa-cruz.ca.us/ASR/", page, "/linkHREF")
  # The website suggests this "outside" value...
  outside <- "outSide=true"
  apnquery <- paste("txtAPN", sep = "=", apn)
  query <- paste(apnquery, outside, sep = "&")
  url <- paste(prefix, query, sep="?")
  
  return(url)
}
get_apn_characteristics_url <- function(apn) {
  return(get_url("Characteristics", apn))
}

get_apn_tax_url <- function(apn) {
  return(get_url("ParcelList", apn))
}

get_apn_characteristics_html <- function(apn) {
  url <- get_apn_characteristics_url(apn)
  
  html <- read_html(url)
  return(html)
}

get_apn_tax_html <- function(apn) {
  url <- get_apn_tax_url(apn)
  
  html <- read_html(url)
  return(html)
}

# To do: what did apn 00654127 look like previously? It's "inactive"
get_apn_characteristics_data_raw <- function(apn, char_html=NULL) {
  char_url <- get_apn_characteristics_url(apn)
  if (is.null(char_html)) {
    char_html <- read_html(char_url)
  }
  
  all_nodes <- html_nodes(char_html, ".tablePrintOnly .trPrintOnly")
  
  data <- sapply(all_nodes, function(n) strsplit(html_text(n), "\\r\\n\\s*", perl=T))
  # expect a list of lists, like this:
  # [[1]]
  # [1] "Parcel Info"
  # 
  # [[2]]
  # [1] "APN"           "Situs Address" "Class"        
  #
  # [[3]]
  # [1] ""                                                                    "00649409"                                                           
  # [3] "105 WEEKS AVE,  SANTA CRUZ                      , 95060-4247       " "020-SINGLE RESIDENCE"                                               
  #
  # [[4]]
  # [1] "Parcel #" "00649409"
  #
  # [[5]]
  # [1] "View"    "NO VIEW"
  #
  # [[6]]
  # [1] "Topography" "LEVEL"     
  # ...
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
  if (grepl("(Inactive)", data[[3]][3])) {
    print(paste0("apn ", apn, " is (Inactive); omitting it."))
    return(list())
  }
  tidy[["Situs Address"]] <- data[[3]][3]
  tidy[["Class"]] <- data[[3]][4]
  # list elements 4 and up are more regular
  if (length(data) >= 4) {
    for (i in 4:length(data)) {
      item <- data[[i]]
      if (length(item) == 2)
        tidy[[item[1]]] <- item[2]      
    }
  }
  tidy[['url']] <- char_url
  return(tidy)
  
}

get_int_or_NA <- function(val) {
  if (is.null(val))
    return(NA)
  if (val == "None")
    return(NA)
  if (val == "N/A")
    return(NA)
  return(as.integer(val))
}

get_apn_characteristics_data <- function(apn, html=NULL) {
  raw_data <- get_apn_characteristics_data_raw(apn, html)
  if (length(raw_data) == 0) {
    return(NULL)
  }
  
  year_built <- get_int_or_NA(raw_data[["Year Built"]])
  effective_year <- get_int_or_NA(raw_data[["Effective Year"]])
  
  num_units <- get_int_or_NA(raw_data[["# of Units"]])
  num_rooms <- get_int_or_NA(raw_data[["Room Count"]])
  bedrooms <- get_int_or_NA(raw_data[["Bedrooms"]])
  bathrooms <- raw_data[["Bathrooms (F/H)"]]
  roof <- raw_data[["Roof"]]
  heat <- raw_data[["Heat"]]
  fireplaces <- get_int_or_NA(raw_data[["Fireplaces"]])
  pools <- raw_data[["Pool"]]
  
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

# Usually, send in just the apn; pre-compute and provide the tax_html to avoid
# hitting the web site repeatedly during debugging.
# The apn must be always be provided: it's used to verify the returned data.
get_apn_tax_data_raw <- function(apn, tax_html=NULL) {
  tax_url <- get_apn_tax_url(apn)
  if (is.null(tax_html)) {
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
  
  # After list element 4, things get more regular.
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

get_apn_tax_data <- function(apn, html=NULL) {
  raw_data <- get_apn_tax_data_raw(apn, html)
  
  tax <- getDollarsToNumeric(raw_data, "Total")
  addr <- trim_address(trim_info(raw_data[["Situs Address"]]))
  type <- raw_data[["Class"]]
  exemption <- getDollarsToNumeric(raw_data, "Homeowners  Exemption")
  assessment <- getDollarsToNumeric(raw_data, "Net Assessment")
  
  return(list(tax=tax, addr=addr, type=type, exemption=exemption, assessment=assessment))
}

dollarsToNumeric <- function(x) {
  # Drop '$', ','
  p1 <- sub('$', '', x, fixed=TRUE)
  p2 <- gsub(',', '', p1, fixed=TRUE)
  return(as.numeric(p2))
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
