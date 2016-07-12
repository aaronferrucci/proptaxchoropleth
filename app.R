
library(shiny)
library(ggvis)
library(RColorBrewer)

page_sels <- list(
  "A" = c("44", "46", "47", "48", "49", "50"),
  "B" = c("21", "22", "28", "41", "42", "43"),
  "C" = as.character(c(17, 25, 35, 36, 37, 38, 39, 40, 51, 52, 54, 56, 57)),
  "D" = c("08", "15", "16"),
  "E" = as.character(c(18, 19, 20, 26, 31)),
  "F" = c("07", "13", "14", "27", "30"),
  "G" = c("11", "12", "29", "33"),
  "H" = c("02", "03", "09", "10", "34"),
  "I" = c("01", "06", "23", "24", "32", "53", "58", "60")
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

width <- 600
height <- 800
file <- "./data/final.plot.006.rda"
load(file)
final.plot$qtax <- round(final.plot$tax, -3)
final.plot$qhomeowner <- ifelse(final.plot$exemption == 7000, "Y", "N")
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
       "homeowner exemption: ", row$homeowner, "<br/>", row$type, "<br/>")
}

server <- shinyServer(function(input, output) {

  df <- reactive({
    final.plot[final.plot$page %in% input$page_sel, ]
    # re <- paste0("^006", input$page)
    # final.plot[grepl(re, final.plot$apnnodash),]
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
        add_legend("fill", title="exemption") %>%
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
        add_legend("fill", title="exemption") %>%
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
      str <- "something about year built"
    str
  })
  output$hoverNote <- renderText({
    "Hover over a parcel for more information."
  })
  output$selectedArea <- renderText({
    paste0('Santa Cruz Property Tax - Pages ', "006-", paste0(input$page_sel, collapse=", "))
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
