library(pacman)
pacman::p_load(marmap, oce, raster, oceanmap, tmap,
               tmaptools, rgdal, maps, rworldmap, 
               mapplots, geoR, rworldxtra, tmap, raster, 
               viridis, rworldmap, colorRamps, rgdal)

# Load map

map <- getMap(resolution = "high")

# -------------------------------------------------------------------------
# Figures -----------------------------------------------------------------
# -------------------------------------------------------------------------


# Create folder to save figures

ifelse(!dir.exists(file.path('Figures')), 
       dir.create(file.path('Figures')), FALSE)


select <- c("depth", "SSSmean", "SSSran",  "SSTmean",
            "Velmean")

# Response curves ---------------------------------------------------------

pdf('Figures/Response_curves.pdf', width = 12)

par(mfrow = c(2,3), 
    mar = c(4,4,1,1))

for(i in seq_along(select))
{ 
  var  <- paste0(select[i])
  a    <- response(eval.models(e.mx.l)[[opt.seq$tune.args]], var = var, range = 'p',
                   ylim = c(0,1), rug = FALSE)
}

dev.off()

# Suitability maps --------------------------------------------------------

# Load files

scenarios <- list.files('Results/', pattern = '.tif', full.names = TRUE)
scenarios <- scenarios[c(1,12,14,15)]
sta       <- stack(scenarios)
title     <- c('Present', 'RCP 2.6', 'RCP 6.0', 'RCP 8.5')

pdf('Figures/Suitability_map.pdf')

par(mfrow = c(2, 2),
    mar = c(0.5, 0.5, 1, 0.1))

for(i in seq_along(scenarios))
{
  plot(map, xlim = c(-60, -20), 
       ylim = c(-45, 2), col = 'lightgray',
       lwd = 0.001, xaxt = 'n', yaxt = 'n',
       main = title[i], font = 2)
  image(sta[[i]], col = viridis(10), zlim = c(0,1),
        add = TRUE)
  box()
}

legend.krige(x.leg = c(-23.25,-21.25), y.leg = c(-45, -25), values = c(0,1), 
             vertical = TRUE, offset.leg = 1.1, col = viridis(10))

text(x = -25, y = -35, srt = 90, 'Suitability')

dev.off()

# Substraction maps -------------------------------------------------------

# Load files

scenarios <- list.files('Results/', pattern = 'Substraction', full.names = TRUE)
sta       <- stack(scenarios)
title     <- c('RCP 2.6', 'RCP 6.0', 'RCP 8.5')

col5               <- colorRampPalette(c('red', 'white', 'blue'))  #create color ramp starting from blue to red
color_levels       <- 10                                         #the number of colors to use
max_absolute_value <- 0.9 #what is the maximum absolute value of raster?

pdf('Figures/Suitability_map_change.pdf', height = 4, width = 8)

par(mfrow = c(1, 3),
    mar = c(0.5, 0.5, 2, 0.5))

for(i in seq_along(1:3))
{
  plot(map, xlim = c(-60, -35), 
       ylim = c(-45, 8), col = 'lightgray',
       lwd = 0.001, xaxt = 'n', yaxt = 'n',
       main = title[i], font = 2)
  
  image(sta[[i]], col = col5(n=color_levels), 
        breaks=seq(-max_absolute_value,max_absolute_value, 
                   length.out=color_levels+1), zlim = c(-0.9, 0.9),
        add = TRUE)
  box()
}

legend.krige(x.leg = c(-32.25,-30.25), y.leg = c(-45, -25), values = c(0,1), 
             vertical = TRUE, offset.leg = 1.1, col = col5(n=color_levels), 
             breaks=seq(-max_absolute_value,max_absolute_value, 
                        length.out=color_levels+1), zlim = c(-0.9, 0.9))

text(x = -35, y = -35, srt = 90, 'Suitability change')

dev.off()

# MESS maps ---------------------------------------------------------------

pdf('Figures/MESS.pdf')


scenarios <- list.files('Results/', pattern = 'Mess', full.names = TRUE)
sta       <- stack(scenarios)
title     <- c('Present', 'RCP 2.6', 'RCP 6.0', 'RCP 8.5')

col5               <- colorRampPalette(c('red', 'white', 'blue'))  #create color ramp starting from blue to red
color_levels       <- 10                                           #the number of colors to use
max_absolute_value <- 85                                           #what is the maximum absolute value of raster?

par(mfrow = c(2, 2),
    mar = c(0.5, 0.5, 1, 0.1))

for(i in seq_along(title))
{
  plot(map, xlim = c(-60, -20), 
       ylim = c(-45, 2), col = 'lightgray',
       lwd = 0.001, xaxt = 'n', yaxt = 'n',
       main = title[i], font = 2)
  image(sta[[i]], col =col5(n=color_levels), 
        breaks=seq(-max_absolute_value,max_absolute_value, 
                   length.out=color_levels+1), zlim = c(-85, 85),
        add = TRUE)
  box()
}

legend.krige(x.leg = c(-23.25,-21.25), y.leg = c(-45, -25), values = c(0,1), 
             vertical = TRUE, offset.leg = 1.1, col = col5(n=color_levels), 
             breaks=seq(-max_absolute_value,max_absolute_value, 
                        length.out=color_levels+1), zlim = c(-85, 85))


dev.off()

# Most dissimilar ---------------------------------------------------------

pdf('Results/MOD.pdf')

scenarios <- list.files('Results/', pattern = 'MOD', full.names = TRUE)
sta       <- stack(scenarios)
title     <- c('Present', 'RCP 2.6', 'RCP 6.0', 'RCP 8.5')
cols      <- RColorBrewer::brewer.pal(5, "Set1")

par(mfrow = c(2, 2),
    mar = c(0.5, 0.5, 1, 0.1))

for(i in seq_along(title))
{
  plot(map, xlim = c(-60, -20), 
       ylim = c(-45, 2), col = 'lightgray',
       lwd = 0.001, xaxt = 'n', yaxt = 'n',
       main = title[i], font = 2)
  image(sta[[i]], col =cols,
        add = TRUE)
  box()
}

legend("bottomright", title="Most different variables",
       c("depth","SSSmean","SSSran", "SSTmean", "Velmean"), fill = cols,
       cex=0.8)

dev.off()

