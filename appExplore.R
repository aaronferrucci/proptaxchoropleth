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

ui <- shinyUI(fluidPage(
  #  title
  titlePanel("Santa Cruz Property Tax - Book 006"),

  # controls
  sidebarLayout(
    sidebarPanel(
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
width <- 800
height <- 800
file <- "./data/final.plot.006.rda"
load(file)
final.plot$page <- substring(final.plot$apnnodash, 4, 5)
final.plot$page2 <- substring(final.plot$apnnodash, 4, 5)

final.plot[final.plot$page %in% c("44", "46", "47", "48", "49", "50"),]$page2 <-
"44, 46, 47, 48, 49, 50"
final.plot[final.plot$page %in% c("21", "22", "28", "41", "42", "43"),]$page2 <-
"21, 22, 28, 41, 42, 43"
final.plot[final.plot$page %in% as.character(c(17, 25, 35, 36, 37, 38, 39, 40, 51, 52, 54, 56, 57)),]$page2 <-
"17, 25, 35, 36, 37, 38, 39, 40, 51, 52, 54, 56, 57"
final.plot[final.plot$page %in% c("08", "15", "16"),]$page2 <-
"08, 15, 16"
final.plot[final.plot$page %in% as.character(c(18, 19, 20, 26, 31)),]$page2 <-
"18, 19, 20, 26, 31"


# tooltip helper. Given a group, extract its row from the data frame.
get_row_by_group <- function(group) {
  rows <- final.plot[final.plot$group == group, ]
  row <- rows[1, ]
  
  row
}

tt <- function(x) {
  if(is.null(x)) return(NULL)
  row <- get_row_by_group(x$group)
  paste0(row$page, "<br/>", row$page2, "<br/>")
}

server <- shinyServer(function(input, output) {

  reactive({
    plot <- final.plot %>%
      ggvis(~long, ~lat) %>%
      group_by(group, id) %>%
      layer_paths(fill = ~page2) %>%
      hide_legend("fill") %>%
      hide_axis("x") %>%
      hide_axis("y") %>%
      add_tooltip(tt, "hover") %>%
      set_options(width=width-50, height=height, keep_aspect=T)
    
    return(plot)
  }) %>% bind_shiny("plot")

  output$summary <- renderText({
    "exploring"
  })
  output$hoverNote <- renderText({
    "Hover over a parcel for more information."
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
