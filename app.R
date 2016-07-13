
library(shiny)
library(ggvis)
library(RColorBrewer)

page_sels <- list(
  "SCHS Area" = paste0(c("44", "46", "47", "48", "49", "50"), collapse=","),
  "Mission Hill Area" = paste0(c("21", "22", "28", "41", "42", "43"), collapse=","),
  "Mission Area" = paste0(as.character(c(17, 25, 35, 36, 37, 38, 39, 40, 51, 52, 54, 56, 57)), collapse=","),
  "West of Mission Hill" = paste0(c("08", "15", "16"), collapse=","),
  "Bay, King, Otis, Mission" = paste0(as.character(c(18, 19, 20, 26, 31)), collapse=","),
  "West of Westlake" = paste0(c("07", "13", "14", "27", "30"), collapse=","),
  "Bay, King, Sherman, Escalona" = paste0(c("11", "12", "29", "33"), collapse=","),
  "South of Westlake" = paste0(c("02", "03", "09", "10", "34"), collapse=","),
  "Westlake Park" = paste0(c("01", "06", "23", "24", "32", "53", "58", "60"), collapse=",")
)
default_page_sel <- page_sels[[1]]

ui <- shinyUI(fluidPage(
  #  title
  titlePanel(textOutput("selectedArea")),
  
  # controls
  sidebarLayout(
    sidebarPanel(
      selectInput("page_sel", "Pages", page_sels, default_page_sel),
      radioButtons("plotSelect",
                    label = "Plotting Options",
                    choices = list(
                    "property tax" = 1,
                    "homeowner's exemption" = 2,
                    "year built" = 3
                    ), 
                    selected = 1
                  ),
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

width <- 600
height <- 800
file <- "./data/final.plot.006.rda"
load(file)
final.plot$qtax <- round(final.plot$tax, -3)
final.plot$qhomeowner <- ifelse(final.plot$exemption == 7000, "Y", "N")
final.plot$qhomeowner[is.na(final.plot$qhomeowner)] <- "N"
final.plot$qhomeowner <- factor(final.plot$qhomeowner)

final.plot$page <- substring(final.plot$apnnodash, 4, 5)

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
       "homeowner exemption: ", row$homeowner, "<br/>", "year built: ", row$year_built, "<br/>", row$type, "<br/>")
}

server <- shinyServer(function(input, output) {

  df <- reactive({
    sel <- unlist(strsplit(input$page_sel, ",", fixed=TRUE))
    final.plot[final.plot$page %in% sel, ]
  })
  
  reactive({
    if (input$plotSelect == 1)
      plot <- df %>%
        ggvis(~long, ~lat) %>%
        group_by(group, id) %>%
        layer_paths(fill = ~qtax) %>%
        scale_numeric("fill", range = c("white", "red")) %>%
        hide_axis("x") %>%
        hide_axis("y") %>%
        add_legend("fill", title="annual property tax") %>%
        add_tooltip(tt, "hover") %>%
        set_options(width=width, height=height, keep_aspect=T)
    else if (input$plotSelect == 2)
      plot <- df %>%
        ggvis(~long, ~lat) %>%
        group_by(group, id) %>%
        layer_paths(fill = ~qhomeowner) %>%
        scale_ordinal("fill", range = c("white", "limegreen")) %>%
        hide_axis("x") %>%
        hide_axis("y") %>%
        add_legend("fill", title="homeowner") %>%
        add_tooltip(tt, "hover") %>%
        set_options(width=width-50, height=height, keep_aspect=T)
    else
      plot <- df %>%
        ggvis(~long, ~lat) %>%
        group_by(group, id) %>%
        layer_paths(fill = ~year_built) %>%
        scale_numeric("fill", range = c("white", "blue")) %>%
        hide_axis("x") %>%
        hide_axis("y") %>%
        add_legend("fill", title="year built", format="4d") %>%
        add_tooltip(tt, "hover") %>%
        set_options(width=width-50, height=height, keep_aspect=T)
    
    return(plot)
  }) %>% bind_shiny("plot")

  output$summary <- renderText({
    if (input$plotSelect == 1)
      str <- "Annual Property tax, according to assessment value."
    else if (input$plotSelect == 2)
      str <- "Homeowners can apply for an exemption - $7,000 off of the assessed value - so, about $70 less per year."
    else
      str <- "Year built, according to county records"
    str
  })
  output$hoverNote <- renderText({
    "Hover over a parcel for more information."
  })
  output$selectedArea <- renderText({
    paste0('Santa Cruz Property Tax - Book 006, Pages ', input$page_sel)
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
