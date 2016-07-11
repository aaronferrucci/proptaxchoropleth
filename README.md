# proptaxchoropleth

A choropleth of property tax (annual), per parcel in a neighborhood
in Santa Cruz, CA.

References:
  * shp file for Santa Cruz County: https://earthworks.stanford.edu/catalog/stanford-gs418bw0551
    * A drop-down offers downloads of various file formats, including
      "original" and "generated" shapefiles
    * I use the "generated" shapefile, because it uses latitude/longitude in
      degrees; the "original" shapefile appears to use WGS84 coordinates.
  * property tax and other data by parcel is taken from http://sccounty01.co.santa-cruz.ca.us/ASR/ParcelList

![alt text](https://github.com/aaronferrucci/proptaxchoropleth/blob/master/parcels.png "PNG example image")

[Interactive graph, hosted on shinyapps.io](https://aaronferrucci.shinyapps.io/proptaxchoropleth/ "Interactive graph, hosted on shinyapps.io")
