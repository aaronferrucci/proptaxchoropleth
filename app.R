
library(shiny)
library(ggvis)
library(RColorBrewer)

sep = ","
page_sels <- list(
  "SCHS Area" = paste0(c("44", "46", "47", "48", "49", "50"), collapse=sep),
  "Mission Hill Area" = paste0(c("21", "22", "28", "41", "42", "43"), collapse=sep),
  "Mission Area" = paste0(as.character(c(17, 25, 35, 36, 37, 38, 39, 40, 51, 52, 54, 56, 57)), collapse=sep),
  "West of Mission Hill" = paste0(c("08", "15", "16"), collapse=sep),
  "Bay, King, Otis, Mission" = paste0(as.character(c(18, 19, 20, 26, 31)), collapse=sep),
  "West of Westlake" = paste0(c("07", "13", "14", "27", "30"), collapse=sep),
  "Bay, King, Sherman, Escalona" = paste0(c("11", "12", "29", "33"), collapse=sep),
  "South of Westlake" = paste0(c("02", "03", "09", "10", "34"), collapse=sep),
  "Westlake Park" = paste0(c("01", "06", "23", "24", "32", "53", "58", "60"), collapse=sep),
  "All" = NA
)
default_page_sel <- names(page_sels)[[1]]

ui <- shinyUI(fluidPage(
  #  title
  titlePanel(textOutput("selectedArea")),
  
  # controls
  sidebarLayout(
    sidebarPanel(
      selectInput("page_sel", "Map Region", names(page_sels), default_page_sel),
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
      textOutput("hover_note"),
      br(),
      textOutput("pageSel"),
      br(),
      textOutput("version")
    ),

    mainPanel(
      tabsetPanel(
        # Plot panel
        tabPanel("Parcel Map", 
                 br(),
                 column(12, ggvisOutput('plot'))
        ),
        tabPanel(
          "About",
          fluidRow(
            br(),
            column(8, offset=2,
              h4("What's this for?"),
              "Parcel data for Santa Cruz County is available from the Assessor's web page. 
               The general public can look up various aspects of parcels in Santa Cruz, but
               data visualization options are limited. This app provides a
               view of some aspects of parcel data in an interactive map.",
              br(),
              h4("Parcel number encoding"),
              "Each parcel is identified by an Assessor's Parcel Number (APN), which 
              is an 8 digit number separated by dashes (e.g. 049-103-12). The first 
              3 digits represent the Assessor's mapbook containing the parcel (Book 
              049 in the above example). The next 2 digits represent the page number 
              within that mapbook (Page 10 in the example). The next digit represents 
              the block number on that page (Block 3 in the example). The last 2 
              digits represent the Assessor's Parcel Number on that block (12 in the 
              example).",
              br(),
              h4("FAQ"),
              "Q: What the heck is a choropleth?",
              br(),
              "A: It's a map colored according to data.",
              a(
                "https://en.wikipedia.org/wiki/Choropleth_map",
                href="https://en.wikipedia.org/wiki/Choropleth_map",
                target="_blank"
              ),
              br(),
              "Q: Can you display street names on the map?",
              br(),
              "A: Maybe... that would help navigation a lot.",
              br(),
              "Q: Why are the map regions named the way they are?",
              br(),
              "A: I divided the map up into approximately same-sized chunks,
              and came up with those names. Let me know if you have better
              names.",
              br()
            ),
            br(),
            fluidRow(
              br(),
              br(),
              column(8, offset=2,
                     tableOutput("url_table")
              ),
              br()
            )
          )
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

getSelectedPages <- function(sel_name) {
  sel <- page_sels[sel_name][[1]]
  if (is.na(sel)) return(NA)
  selected_pages <- unlist(strsplit(sel, sep, fixed=TRUE))
}

server <- shinyServer(function(input, output) {

  df <- reactive({
    sel_name <- input$page_sel
    selected_pages <- getSelectedPages(sel_name)
    if (anyNA(selected_pages))
      final.plot
    else
      final.plot[final.plot$page %in% selected_pages, ]
  })
  
  reactive({
    if (input$plotSelect == 1)
      plot <- df %>%
        ggvis(~long, ~lat) %>%
        group_by(group, id) %>%
        layer_paths(fill = ~qtax) %>%
        scale_numeric("fill", range = c("blue", "red")) %>%
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
        scale_ordinal("fill", range = c("orange", "limegreen")) %>%
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
        scale_numeric("fill", range = c("orange", "blue")) %>%
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

  output$hover_note <-
    renderText({"Hover over a parcel for more information."})

  output$selectedArea <- renderText({
    paste0('Santa Cruz Property Tax - Book 006, ', input$page_sel)
  })
  output$version <- renderText({
    "proptaxchoropleth v. 0.3beta"
  })
  output$pageSel <- renderText({
    sel <- getSelectedPages(input$page_sel)
    if (anyNA(sel))
      "Selected pages: all"
    else
      paste0("Selected pages: ", paste0(sel, collapse=sep))
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
