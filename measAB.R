source("utils.R")

get_measAB <- function(assessment) {
  return(60 * assessment / 100000)
}

final.plot <- get_plot_data("006")

final.plot$measAB <- get_measAB(final.plot$assessment)

# include single residences and duplexes
# residences <- (final.plot$type == "020-SINGLE RESIDENCE" | final.plot$type == "030-SINGLE DUPLEX")
# final.plot[!residences,]$measAB <- NA

final.plot$measABcat <- cut(final.plot$measAB, b=c(-1, 1, 50, 100, 250, 450, 800, max(final.plot$measAB, na.rm=T) + 1))
legend_labels <- c("0", "1 to $50", "$51 to $100", "$101 to $250", "$251 to $450", "$451 to $800", "$801 to $1400")
cc <- scales::seq_gradient_pal("white", "red", "Lab")(seq(0,1,length.out=length(levels(final.plot$measABcat))))

p <- ggplot() + ggtitle("Measures A & B Property Tax Increase") +
  geom_polygon(data = final.plot, 
    aes(x = long, y = lat, group = group, fill = measABcat), 
    color = "black", size = 0.25) + 
  coord_map() +
  scale_fill_manual(values=cc, guide = guide_legend(title="Prop. Tax Increase"), labels = legend_labels)

qplot(final.plot$measAB)