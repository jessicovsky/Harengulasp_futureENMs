# -------------------------------------------------------------------------
# Correlationship between rasters -----------------------------------------
# -------------------------------------------------------------------------

if(!require(raster)){install.packages("raster")}

# Load raster files

sta <- stack(list.files("M_variables/", pattern = '.tif', full.names = TRUE))

# Pearson correlationship between rasters

cor <- layerStats(sta, 'pearson', na.rm=T)

# Save correlationship results

write.csv(cor$`pearson correlation coefficient`, file = 'Correlationship.csv', row.names = FALSE)

# Delete files

remove <- c('M_variables/SSSmax.tif', 'M_variables/SSSmin.tif', 'M_variables/SSTmax.tif', 
            'M_variables/SSTmin.tif', 'M_variables/SSTran.tif', 'M_variables/Velmax.tif', 
            'M_variables/Velmin.tif', 'M_variables/Velran.tif')

file.remove(remove)

