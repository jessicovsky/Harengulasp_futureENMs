# -------------------------------------------------------------------------
# Filtering data ----------------------------------------------------------
# -------------------------------------------------------------------------

if(!require(sf)){install.packages("spThin")}
if(!require(raster)){install.packages("raster")}

# Read occurrence data

data <- read.csv('Ocurrence_data/South.csv')
OL_S<- data[data$Latitude <= -10, ]
OL_N<- data[data$Latitude >= -10, ]
  
write.csv(OL_S, 'South.csv', row.names = FALSE)
write.csv(OL_N, 'North.csv', row.names = FALSE)

# data$Species <- 'Harengula_sp'
# data <- data[,1:3]
# 
# # Extract data to localize NA
# 
# ras <- raster('Environmental_variables/Current_scenario/Sur_Tem_Mean.asc')
# ext <- raster::extract(ras, data[,2:3])
# ext <- cbind(data, ext)
# df  <- na.omit(ext)
# df  <- df[,1:3]

# write.csv(ext, 'Temperatura.csv', row.names = FALSE)

# Spatial filtering

filter   <- spThin::thin(loc.data  = data, 
                 lat.col  = "Latitude", 
                 long.col = "Longitude", 
                 spec.col = "Species",
                 thin.par = 40, #Buffer in km
                 reps     = 10000, 
                 locs.thinned.list.return = TRUE, 
                 write.files = TRUE, 
                 max.files   = 1, # número de archivos filtrados
                 out.dir     = "Ocurrence_data",
                 out.base    =  "Harengula_sp_South")
  