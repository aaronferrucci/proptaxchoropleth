source("utils.R")

get_measAB <- function(assessment) {
  return(60 * assessment / 100000)
}

final.plot <- get_plot_data("006")

final.plot$measAB <- get_measAB(final.plot$assessment)

# include single residences and duplexes
residences <- (final.plot$type == "020-SINGLE RESIDENCE" | final.plot$type == "030-SINGLE DUPLEX")
final.plot[!residences,]$measAB <- NA

final.plot$measABcat <- cut(final.plot$measAB, b=c(0, 1, 100, 250, 450, 800, max(final.plot$measAB) + 1))
cc <- scales::seq_gradient_pal("white", "red", "Lab")(seq(0,1,length.out=length(levels(final.plot$measABcat))))
p <- ggplot() +
  geom_polygon(data = final.plot, 
    aes(x = long, y = lat, group = group, fill = measABcat), 
    color = "black", size = 0.25) + 
  coord_map() +
  scale_fill_manual(values=cc)
#  scale_fill_gradient(low = "white", high = "red")

qplot(final.plot$measAB)