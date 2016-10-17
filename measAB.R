source("utils.R")
library(ggplot2)
library(maptools)
library(maps)
library(mapdata)
library(ggmap)

get_measAB <- function(assessment) {
  # Measures A and B are each estimated to add $29.50 per $100,000 of
  # assessed property value.
  return(29.50 * 2 * assessment / 100000)
}

data <- get_plot_data("006")

data$measAB <- get_measAB(data$assessment)

# include single residences and duplexes
# residences <- (data$type == "020-SINGLE RESIDENCE" | data$type == "030-SINGLE DUPLEX")
# data[!residences,]$measAB <- NA

# Bug: if max(data$measAB) < 800, cut does something strange. This affects small data sets.
data$measABcat <- cut(data$measAB, b=c(-1, 1, 50, 100, 250, 450, 800, max(data$measAB, na.rm=T) + 1))
legend_labels <- c("0", "1 to $50", "$51 to $100", "$101 to $250", "$251 to $450", "$451 to $800", "$801 to $1400")
cc <- scales::seq_gradient_pal("white", "red", "Lab")(seq(0,1,length.out=length(levels(data$measABcat))))

scmap <- get_map(location = c(-122.0550, 36.965, -122.0225, 36.9803), maptype = "roadmap", source = "google")

p <- ggmap(scmap) + ggtitle("Measures A & B Property Tax Increase") +
  geom_polygon(data = data, 
    aes(x = long, y = lat, group = group, fill = measABcat), 
    color = "black", size = 0.25) + 
  coord_map() +
  scale_fill_manual(values=cc, guide = guide_legend(title="Prop. Tax Increase"), labels = legend_labels)

qplot(data$measAB)