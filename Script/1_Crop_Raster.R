# -------------------------------------------------------------------------
# Crop rasters ------------------------------------------------------------
# -------------------------------------------------------------------------

# Load or install libraries

if(!require(raster)){install.packages("raster")}
if(!require(sf)){install.packages("sf")}

# Load raster files

ras <- stack(list.files("Full_maps/Current/", pattern = '.asc', full.names = TRUE))

# Read shapefile

shape <- st_read("Shape_files/Pro.shp")

# Cropping and masking environmental variables

# ext   <- extent(-66, -26,  -42, 7)
ras_c <- crop(ras, shape)
ras_c <- mask(ras_c, shape)

# Second masking (distance to the coast)

# coas <- ras_c[[2]]
# coas[coas > 100] <- NA  
# ras_c <- mask(ras_c, coas)

# Loop for saving environmental files used for calibration

for(i in seq_along(1:(dim(ras_c)[3])))  
{
  ifelse(!dir.exists(file.path('Full_maps/8.5')), 
          dir.create(file.path('Full_maps/8.5')), FALSE) 
  f  <- paste0('Full_maps/8.5/', names(ras_c[[i]]), '.asc')
  writeRaster(ras_c[[i]], filename = f, overwrite = TRUE)
}

# Correlation between rasters

sta <- stack(list.files(path = 'Environmental_variables/Cropped/', pattern = 'tif', full.names = TRUE))
cor <- layerStats(sta, 'pearson', na.rm=T)
write.csv(cor$`pearson correlation coefficient`, file = 'Environmental_variables/Cropped/Correlationship.csv')

# Select relevant variables

# For present model

select <- c("Bathymetry", "Distance_To_Coast", "Sur_Chl_Max", "Sur_Chl_Min", "Sur_Iro_Ran", "Sur_Oxy_Max", 
            "Sur_Pho_Mean", "Sur_Pho_Min", "Sur_Pho_Ran",  "Sur_Sal_Max", 
            "Sur_Sal_Min", "Sur_Tem_Max", "Sur_Vel_Max", "Sur_Vel_Min")


for(i in seq_along(select))  
{  
  ifelse(!dir.exists(file.path('Environmental_variables', 'Current_model')), 
         dir.create(file.path('Environmental_variables', 'Current_model')), FALSE) 
  original <- list.files(path = 'Environmental_variables/Cropped/', pattern = select[i], full.names = TRUE)
  file.copy(from = paste0(original),
            to = paste0("Environmental_variables/Current_model/", select[i], '.tif'))
}


# For future model


select <- c("Bathymetry", "Distance_To_Coast", "Sur_Sal_Mean", "Sur_Sal_Ran",
            "Sur_Tem_Mean", "Sur_Vel_Mean")

for(i in seq_along(select))  
{  
  ifelse(!dir.exists(file.path('Environmental_variables', 'Future_model')), 
         dir.create(file.path('Environmental_variables', 'Future_model')), FALSE) 
  original <- list.files(path = 'Environmental_variables/Cropped/', pattern = select[i], full.names = TRUE)
  file.copy(from = paste0(original),
            to = paste0("Environmental_variables/Future_model/", select[i], '.tif'))
}



