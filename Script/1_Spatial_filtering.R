# -------------------------------------------------------------------------
# Filtering data ----------------------------------------------------------
# -------------------------------------------------------------------------

# Install or load libraries -----------------------------------------------

if(!require(spThin)){install.packages("spThin")}

# Load occurrences --------------------------------------------------------

data <- read.csv('Datasets/Harengula_sp.csv')

# Subset data -------------------------------------------------------------

# Latitude

OL_S <- data[data$Latitude <= -10, ]
OL_N <- data[data$Latitude >= -10, ]
N_S  <- list(OL_S, OL_N)

# Name of files that will be created

nam  <- c('South.csv', 'North.csv')

# Distance buffer for south and north (km)

km   <- c(50, 10)

# Spatial filtering -------------------------------------------------------

set.seed(1)
for(i in seq_along(N_S))
{
filter   <- spThin::thin(loc.data  = N_S[[i]], 
                         lat.col  = "Latitude", 
                         long.col = "Longitude", 
                         spec.col = "Species",
                         thin.par = km[i], #Buffer in km
                         reps     = 10000, 
                         locs.thinned.list.return = TRUE, 
                         write.files = TRUE, 
                         max.files   = 1, # número de archivos filtrados
                         out.dir     = "Datasets/Filtered",
                         out.base    =  nam[i])
}

# Merging database --------------------------------------------------------

data  <- list.files(pattern = '.csv', path = 'Datasets/Filtered/', full.names = TRUE)
North <- read.csv(data[1])
South <- read.csv(data[2])
df    <- rbind(North, South)
write.csv(df, 'Datasets/Harengula_sp_joint.csv', row.names = FALSE)


