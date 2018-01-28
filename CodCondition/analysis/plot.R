library(cowplot)

load('interimsteps/pempties.rdata')
load('interimsteps/pstomw.rdata')
load('interimsteps/pcondition.rdata')

plot3by1 <- plot_grid(p.condition, p.stomw, p.empties,

                      labels=c("A", "B", "C"), ncol = 1)

save_plot("output/plot.png", plot3by1,

          ncol = 1, # we're saving a grid plot of 2 columns

          nrow = 4, # and 2 rows

          # each individual subplot should have an aspect ratio of 1.3

          base_aspect_ratio = 5

)
