source("utils.R")

library(maptools)
library(rvest)
library(ggplot2)

final.plot <- get_plot_data()

p <- ggplot() +
  geom_polygon(data = final.plot, 
    aes(x = long, y = lat, group = group, fill = tax), 
    color = "black", size = 0.25) + 
  coord_map() +
  scale_fill_gradient(low = "white", high = "red")
print(p)

# ggvis experiment
# have a look at https://github.com/vega/vega; maybe this
# can export an svg file with tooltips.
library(ggvis)
get_row_by_group <- function(group) {
  rows <- final.plot[final.plot$group == group, ]
  row <- rows[1, ]

  row
}

tt <- function(x) {
  row <- get_row_by_group(x$group)
  paste0(row$apnnodash, "<br/>", row$addr, "<br/>", "tax: $", row$tax, "<br/>",
  "homeowner exemption: ", row$homeowner, "<br/>", "year built: ", row$year_built, "<br/>", row$type, "<br/>")
}

# make a palette from white, through yellow, to red.
library(RColorBrewer)
ramp <- colorRampPalette(
  c("white", brewer.pal(n=9, name="YlOrRd")),
  space="Lab"
)
# color for property tax
final.plot$taxColor <- as.character(
  cut(final.plot$tax, 20, include.lowest=TRUE, labels=ramp(20))
)

final.plot$qtax <- round(final.plot$tax, -3)
p2 <- final.plot %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  # layer_paths(fill:=~taxColor, strokeOpacity := 0.5, strokeWidth := 0.25) %>%
  layer_paths(fill = ~qtax) %>%
  # scale_numeric("fill", range = final.plot$taxColor) %>%
  scale_numeric("fill", range = c("white", "red")) %>%
  add_legend("fill", title="annual property tax") %>%
  add_tooltip(tt, "hover") %>%
  set_options(width=100, height=600, keep_aspect=T)

# homeowner's exemption
ramp2 <- colorRampPalette(
  c("white", "green"),
  space="Lab"
)
final.plot$homeownerColor <- as.character(
  cut(0 + final.plot$homeowner, 2, include.lowest=TRUE, labels=ramp2(2))
)
p3 <- final.plot %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(fill:=~homeownerColor) %>%
  add_tooltip(tt, "hover") %>%
  set_options(width=100, height=600, keep_aspect=T)

p4 <- final.plot %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(fill = ~year_built) %>%
  add_tooltip(tt, "hover") %>%
  set_options(width=100, height=600, keep_aspect=T)
