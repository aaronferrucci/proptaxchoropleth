
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
      selectInput("page_sel", "Map Region", page_sels, default_page_sel),
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
      br()
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "README",
          fluidRow(
            br(),
            column(8, offset=2,
              "Parcel data for Santa Cruz County is available from the Assessor's web page. 
               The general public can look up various aspects of parcels in Santa Cruz, but
               data visualization options are limited. This app provides a
               view of some aspects of parcel data in an interactive map.",
              br(),
              br(),
              "Each parcel is identified by an Assessor's Parcel Number (APN), which 
              is an 8 digit number separated by dashes (e.g. 049-103-12). The first 
              3 digits represent the Assessor's mapbook containing the parcel (Book 
              049 in the above example). The next 2 digits represent the page number 
              within that mapbook (Page 10 in the example). The next digit represents 
              the block number on that page (Block 3 in the example). The last 2 
              digits represent the Assessor's Parcel Number on that block (12 in the 
              example).",
              br(),
              br(),
              "On the left are widgets which control the map area and plotting options
              for the parcel map. You can select a region of the map to display - a set
              of pages chosen to be contiguous. You can also color the map according to
              annual tax assessment, homeowner's exemption or year built.",
              br(),
              br(),
              "Hover over a parcel for more information.",
              br(),
              br()
            ),
            fluidRow(
              br(),
              column(8, offset=2,
                     tableOutput("url_table")
              ),
              br()
            )
          )
        ),
        # Plot panel
        tabPanel("Parcel Map", 
                 br(),
                 column(12, ggvisOutput('plot'))
        )
      )
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
        set_options(width=width-39, height=height, keep_aspect=T)
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
        set_options(width=width-5, height=height, keep_aspect=T)
    
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
  output$selectedArea <- renderText({
    paste0('Santa Cruz Property Tax - Book 006, Pages ', input$page_sel)
  })
  
  output$url_table <- renderTable({
    labels <- c(
      "Santa Cruz County Assessor's Website",
      "source code"
    )
    urls <- c(
      "http://sccounty01.co.santa-cruz.ca.us/ASR/",
      "https://github.com/aaronferrucci/proptaxchoropleth"
    )
    references <- paste0(
      labels, ": ", 
      "<a href='",  urls, "' target='_blank'>",
      urls, "</a>")
    
    data.frame(references)
    
  }, sanitize.text.function = function(x) x)
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)
