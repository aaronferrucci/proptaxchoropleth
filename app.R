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

default_area <- "006"
areas <- c("006", "0062")
ui <- shinyUI(fluidPage(
  #  title
  # titlePanel('Santa Cruz Property Tax - Parcels 0064XXXX'),
  # titlePanel(paste0('Santa Cruz Property Tax - Parcels ', textOutput("selectedArea"))),
  titlePanel(textOutput("selectedArea")),
  
  # controls
  sidebarLayout(
    sidebarPanel(
      selectInput("area", "Area", areas, default_area),
      radioButtons("plotSelect", label = "Plotting Options",
                   choices = list("property tax" = 1, "homeowner's exemption" = 2), 
                   selected = 1),
      textOutput("summary"),
      br(),
      textOutput("hoverNote")
    ),

    # plot
    mainPanel(
      ggvisOutput("plot")
    )
  )
))

# color for property tax
rampTax <- colorRampPalette(
  c("white", brewer.pal(n=9, name="YlOrRd")),
  space="Lab"
)
# final.plot$taxColor <- as.character(
#   cut(final.plot$tax, 20, include.lowest=TRUE, labels=rampTax(20))
# )

# color for homeowner's exemption
rampHO <- colorRampPalette(
  c("white", "lime green"),
  space="Lab"
)
# final.plot$homeownerColor <- as.character(
#   cut(0 + final.plot$homeowner, 2, include.lowest=TRUE, labels=rampHO(2))
# )

width <- 800
height <- 800

server <- shinyServer(function(input, output) {

  reactive({
    file <- paste0("./data/final.plot.", input$area, ".rda")
    load(file)

    final.plot$qtax <- round(final.plot$tax, -3)
    final.plot$qhomeowner <- ifelse(final.plot$exemption == 7000, "Y", "N")

    # tooltip helper. Given a group, extract its row from the data frame.
    get_row_by_group <- function(group) {
      rows <- final.plot[final.plot$group == group, ]
      row <- rows[1, ]
      
      row
    }
    
    tt <- function(x) {
      if(is.null(x)) return(NULL)
      row <- get_row_by_group(x$group)
      paste0(row$apnnodash, "<br/>", row$addr, "<br/>", "tax: $", row$tax, "<br/>",
           "homeowner exemption: ", row$homeowner, "<br/>", row$type, "<br/>")
    }
    
    if (input$plotSelect == 1)
      plot <- final.plot %>%
        ggvis(~long, ~lat) %>%
        group_by(group, id) %>%
        layer_paths(fill = ~qtax) %>%
        scale_numeric("fill", range = c("white", "red")) %>%
        hide_axis("x") %>%
        hide_axis("y") %>%
        add_legend("fill", title="annual property tax") %>%
        add_tooltip(tt, "hover") %>%
        set_options(width=width, height=height, keep_aspect=T)
    else
      plot <- final.plot %>%
        ggvis(~long, ~lat) %>%
        group_by(group, id) %>%
        layer_paths(fill = ~qhomeowner) %>%
        scale_ordinal("fill", range = c("white", "limegreen")) %>%
        hide_axis("x") %>%
        hide_axis("y") %>%
        add_legend("fill", title="exemption") %>%
        add_tooltip(tt, "hover") %>%
        set_options(width=width-50, height=height, keep_aspect=T)
  
    return(plot)
  }) %>% bind_shiny("plot")

  output$summary <- renderText({
    if (input$plotSelect == 1)
      str <- "Annual Property tax, according to assessment value."
    else
      str <- "Homeowners can apply for an exemption - $7,000 off of the assessed value - so, about $70 less per year."
    str
  })
  output$hoverNote <- renderText({
    "Hover over a parcel for more information."
  })
  output$selectedArea <- renderText({
    paste0('Santa Cruz Property Tax - Parcels ', input$area, "XXXX")
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
