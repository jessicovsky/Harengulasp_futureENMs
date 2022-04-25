# -------------------------------------------------------------------------
# Model Calibration and Projection Current Scenario -----------------------
# -------------------------------------------------------------------------

# Install or load libraries -----------------------------------------------

if (!require(ENMeval)) install.packages("ENMeval")
if (!require(raster)) install.packages("raster")
if (!require(dplyr)) install.packages("dplyr")

# Load occurrences --------------------------------------------------------

occ <- read.csv('Datasets/Harengula_sp_joint.csv')
occ <- occ[,2:3]

# Load environmental variables --------------------------------------------

env <- stack(list.files('M_variables/', full.names= 'TRUE', pattern = '.tif'))

# Select background -------------------------------------------------------

set.seed(1)
bg            <- dismo::randomPoints(env, n = 10000) %>% as.data.frame()
colnames(bg)  <- colnames(occ)

# Model calibration  ------------------------------------------------------

e.mx.l <- ENMevaluate(occs = occ, envs = env, bg = bg, doClamp  = FALSE,
                      algorithm = 'maxent.jar', partitions = 'block', 
                      tune.args = list(fc = c("L", "LQ", "LQP", "LQH", "LQHP"), 
                                       rm = seq(1, 5, 0.5)))

# Select model with the best results --------------------------------------

# Create folder to save results

ifelse(!dir.exists(file.path('Results')), 
       dir.create(file.path('Results')), FALSE)

# Save all results

res <- eval.results(e.mx.l)
write.csv(res, 'Results/all_results.csv', row.names = FALSE)

# Select best model

opt.seq <- res %>% 
  filter(auc.val.avg > 0.85) %>%
  filter(or.10p.avg    < 0.10) %>% 
  filter(AICc == min(AICc))

# Save best model

write.csv(opt.seq, "Results/Optimal_model.csv", row.names = FALSE)

# Save importance of variables

Importance <- e.mx.l@variable.importance$fc.LQHP_rm.1.5
write.csv(Importance, "Results/Importance_Har_sp.csv")

# Save raster

writeRaster(e.mx.l@predictions$fc.LQHP_rm.1.5,
            'Results/Suitability_result_Har_sp.tif', overwrite = TRUE)

# Projections -------------------------------------------------------------

ras       <- list()
subfolder <- c('Current', '2.6', '6.0', '8.5')
names     <- c('Current', 'RCP2.6', 'RCP6.0', 'RCP8.5')

for(i in seq_along(subfolder))
{
f        <- paste0('G_variables/', subfolder[i])
ras[[i]] <- stack(list.files(f, pattern = 'tif', full.names = TRUE))
pre      <- dismo::predict(e.mx.l@models$fc.LQHP_rm.1.5, ras[[i]])
file     <- paste0('Results/', names[i], '.tif')
writeRaster(pre, filename = file, overwrite = TRUE)
}

# Delete auxiliary files created

remove <- list.files('Results/', pattern = 'aux', full.names = TRUE)
file.remove(remove)

# Suitability differences -------------------------------------------------

scenarios <- list.files('Results/', pattern = '.tif', full.names = TRUE)
scenarios <- scenarios[c(1,2,3,4)]
sta       <- stack(scenarios)

RCP2.6 <- sta[[2]] - sta[[1]]  
RCP6.0 <- sta[[3]] - sta[[1]]
RCP8.5 <- sta[[4]] - sta[[1]]

# Save substraction

writeRaster(RCP2.6, 'Results/Substraction_RCP_2.6.tif')
writeRaster(RCP6.0, 'Results/Substraction_RCP_6.0.tif')
writeRaster(RCP8.5, 'Results/Substraction_RCP_8.5.tif')

# Most different variables and MESS maps ----------------------------------

# Mess map

for(i in seq_along(scenarios))
{
  ras_rcp  <- ras[[i]]
  f        <- paste0('Results/', 'Mess_', names[i], '.tif')
  occ.sim  <- similarity(ras_rcp, env)
  writeRaster(occ.sim$similarity_min, filename = f)
  }

# Most different variable

for(i in seq_along(scenarios))
{
  ras_rcp  <- ras[[i]]
  f        <- paste0('Results/', 'MOD_', names[i], '.tif')
  occ.sim  <- similarity(ras_rcp, env)
  writeRaster(occ.sim$mod, filename = f)
}