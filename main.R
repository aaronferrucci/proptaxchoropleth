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
sm <- shp[grepl('^00649', lapply(shp$apnnodash, as.character)),]
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

p <- ggplot() +
  geom_polygon(data = final.plot, 
    aes(x = long, y = lat, group = group, fill = val), 
    color = "black", size = 0.25) + 
  coord_map()
print(p)

library(rvest)

get_apn_data <- function(apn) {
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
  proptaxStr <- html_text(
    html_nodes(
      html,
      "body center table tr td table tr:nth-child(4) td")[[6]]
  )
  # Drop '$', ','
  p1 <- sub('$', '', proptaxStr, fixed=TRUE)
  p2 <- sub(',', '', p1, fixed=TRUE)
  proptax <- as.numeric(p2)
  return(proptax)
}

trim_address <- function (x) {
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
  addr <-
    html_text(
      html_nodes(
        html,
        "body center table  tr td table tr:nth-child(6) td div:nth-child(2) div div.plmTbody div div:nth-child(2)"
      )
    )
    return(trim_address(addr))
}

# get_assessments <- function(apns) {
#}
# http://sccounty01.co.santa-cruz.ca.us/ASR/ParcelList/linkHREF?txtAPN=00649409
# http://sccounty01.co.santa-cruz.ca.us/ASR/ParcelList/linkHREF?txtAPN=00649204

#library(httr)
#url <- "http://sccounty01.co.santa-cruz.ca.us/ASR/ParcelList"
#uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
#session <- html_session(url, user_agent(uastring))
#form <- html_form(session)[[1]]
#form <- set_values(form, txtAPNNO = "00649409")
#new_url <- submit_geturl(session, form)
#new_session <- html_session(new_url, user_agent(uastring))
#
#appendList <- function (x, val)
#{
#  stopifnot(is.list(x), is.list(val))
#  xnames <- names(x)
#  for (v in names(val)) {
#    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
#      appendList(x[[v]], val[[v]])
#    else c(x[[v]], val[[v]])
#  }
#  x
#}
# 
## Simulating submit_form for GET requests
#submit_geturl <- function (session, form)
#{
#  query <- rvest:::submit_request(form)
#  query$method <- NULL
#  query$encode <- NULL
#  query$url <- NULL
#  names(query) <- "query"
# 
#  relativeurl <- XML::getRelativeURL(form$url, session$url)
#  basepath <- parse_url(relativeurl)
# 
#  fullpath <- appendList(basepath,query)
#  fullpath <- build_url(fullpath)
#  fullpath
#}
#
