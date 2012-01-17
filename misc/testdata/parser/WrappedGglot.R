ggPlotDEscatter <- ggplot(diffexStatsFixedZero, aes(baseMeanA, baseMeanB, color=color_code)) + geom_point(size=0.6, alpha=0.7) +
facet_grid(layer.x ~ layer.y)  + opts(legend.position = "none")  +
scale_colour_manual(values = c("black","green")) + theme_bw()
