#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# TO do:
# single graph panel,
# sidebar with controls for selecting various graph
# options, e.g. how to color (tax, homeowner exemption),
# grid lines, lat/lon labels, selection of apn subset.

library(shiny)
library(ggvis)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   # Application title
   titlePanel('title'),
   
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("tax", ggvisOutput("taxPlot")),
        tabPanel("exemption", ggvisOutput("exemptPlot"))
      )
    )
))

load("../data/final.plot.rda")

# To do: extract these methods to a separate file for sourcing.
# make a palette from white, through yellow, to red.
ramp <- colorRampPalette(
  c("white", brewer.pal(n=9, name="YlOrRd")),
  space="Lab"
)
# color for property tax
final.plot$taxColor <- as.character(
  cut(final.plot$tax, 20, include.lowest=TRUE, labels=ramp(20))
)
# color for homeowner's exemption
final.plot$homeownerColor <- as.character(
  cut(0 + final.plot$homeowner, 2, include.lowest=TRUE, labels=ramp(2))
)

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

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    width <- 600
    height <- 800
    final.plot %>%
      ggvis(~long, ~lat) %>%
      add_axis("x", title = "longitude", grid=FALSE) %>%
      add_axis("y", title = "latitude", grid=FALSE) %>%
      group_by(group, id) %>%
      layer_paths(fill:=~taxColor) %>%
      add_tooltip(tt, "hover") %>%
      set_options(width=width, height=height, keep_aspect=T) %>%
      bind_shiny("taxPlot")

    final.plot %>%
      ggvis(~long, ~lat) %>%
      add_axis("x", title = "longitude", grid=FALSE) %>%
      add_axis("y", title = "latitude", grid=FALSE) %>%
      group_by(group, id) %>%
      layer_paths(fill:=~homeownerColor) %>%
      add_tooltip(tt, "hover") %>%
      scale_numeric("fill", range=c("#bfd3e6", "#8c6bb1" ,"#4d004b")) %>%
      set_options(width=width, height=height, keep_aspect=T) %>%
      hide_legend("fill") %>%
      bind_shiny("exemptPlot")

})

# Run the application 
shinyApp(ui = ui, server = server)

