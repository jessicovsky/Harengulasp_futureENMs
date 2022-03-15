# -------------------------------------------------------------------------
# Model Calibration and Projection Current Scenario -----------------------
# -------------------------------------------------------------------------

if (!require(ENMeval)) install.packages("ENMeval")
if (!require(dismo)) install.packages("dismo")
if (!require(raster)) install.packages("raster")
if (!require(rJava)) install.packages("rJava")
if (!require(kuenm)) install.packages("kuenm")
if (!require(oce)) install.packages("oce")
if (!require(dplyr)) install.packages("dplyr")
if (!require(ENMTools)) install.packages("ENMTools")


# Load ocurrences

occ <- read.csv('Datasets/Harengula_sp_joint_30.csv')
occ <- occ[,2:3]
# Load environmental variables

asciis <- list.files('M_variables/Set1/', full.names= 'TRUE', pattern = '.asc') 
env    <- stack(asciis)

# Visualize occurrence in environmental layer
x11()
plot(env[[1]])
points(occ, col = 'red')

# Select background

bg            <- dismo::randomPoints(env, n = 10000) %>% as.data.frame()
colnames(bg)  <- colnames(occ)
plot(env[[1]])
points(bg, pch = 20, cex = 0.2)


# Checkerboard
# Check groups

cb1    <- get.checkerboard1(occ, env, bg, aggregation.factor = 2)
evalplot.grps(pts = occ, pts.grp = cb1$occs.grp, envs = env)
groups <- cbind(occ, cb1$occs.grp)


# -------------------------------------------------------------------------
# Model calibration  ------------------------------------------------------
# -------------------------------------------------------------------------

e.mx.l <- ENMevaluate(occs = occ, envs = env, bg = bg, doClamp  = FALSE,
                      algorithm = 'maxent.jar', partitions = 'block', 
                      tune.args = list(fc = c("L", "LQ", "LQP", "LQH", "LQHP"), 
                                       rm = seq(1, 5, 0.5)))

# -------------------------------------------------------------------------
# Select model with the best results --------------------------------------
# -------------------------------------------------------------------------

# Create folder to save results

ifelse(!dir.exists(file.path('Results')), 
       dir.create(file.path('Results')), FALSE)


res <- eval.results(e.mx.l)
write.csv(res, 'Results/all_results.csv', row.names = FALSE)


opt.seq <- res %>% 
  filter(auc.val.avg > 0.85) %>%
  filter(or.10p.avg    < 0.06) %>% 
  filter(AICc == min(AICc))

# Save best model

write.csv(opt.seq, "Results/Optimal_model.csv", row.names = FALSE)

# Save important of variables

Importance <- e.mx.l@variable.importance$fc.LQHP_rm.1.5

write.csv(Importance, "Results/Importance_Har_sp.csv")

# Save raster

writeRaster(e.mx.l@predictions$fc.LQHP_rm.1.5, 'Results/Suitability_result_Har_sp.asc', overwrite = TRUE)



# Selec environmental variables

select <- c("depth", "SSSmean", "SSSran",  "SSTmean",
            "Velmean")

lis1 <- list()
lis2 <- list()
lis3 <- list()
lis4 <- list()
lis5 <- list()


# RCP 2.6 2100

for(i in seq_along(select))
{ 
  lis1[i] <- list.files('G_variables/Set1/2.6', pattern = select[i], full.names = TRUE)
}

# RCP 4.5 2100

for(i in seq_along(select))
{ 
  lis2[i] <- list.files('G_variables/Set1/4.5', pattern = select[i], full.names = TRUE)
}

# RCP 6 2100

for(i in seq_along(select))
{ 
  lis3[i] <- list.files('G_variables/Set1/6.0', pattern = select[i], full.names = TRUE)
}

# RCP 8.5 2100

for(i in seq_along(select))
{ 
  lis4[i]  <- list.files('G_variables/Set1/8.5', pattern = select[i], full.names = TRUE)
}

# RCP CURRENT

for(i in seq_along(select))
{ 
  lis5[i] <- list.files('G_variables/Set1/Current/', pattern = select[i], full.names = TRUE)
}



# Loop to plot all

final_list <- list(lis1,lis2,lis3,lis4, lis5)
names <- c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5', 'Current')

# Save  scenarios

for(i in seq_along(1:5))
{
  ras <- lapply(final_list[i], stack)
  pre <- dismo::predict(e.mx.l@models$fc.LQHP_rm.1.5, ras[[1]])
  f   <- paste0('Results/', names[i], '.asc')
  writeRaster(pre, filename = f)
}

# Difference in suitabilities

scenarios <- list.files('Results/', pattern = '.asc', full.names = TRUE)
scenarios <- scenarios[c(1,2,4,5)]
sta       <- stack(scenarios)

RCP2.6 <- sta[[2]] - sta[[1]]  
RCP6.0 <- sta[[3]] - sta[[1]]
RCP8.5 <- sta[[4]] - sta[[1]]

# Save substraction

writeRaster(RCP2.6, 'Results/Substraction_RCP_2.6.asc')
writeRaster(RCP6.0, 'Results/Substraction_RCP_6.0.asc')
writeRaster(RCP8.5, 'Result/Substraction_RCP_8.5.asc')


# -------------------------------------------------------------------------
# Mess analyses -----------------------------------------------------------
# -------------------------------------------------------------------------

# RCP 2.6 2100

for(i in seq_along(select))
{ 
  lis1[i] <- list.files('G_variables/Set1/2.6', pattern = select[i], full.names = TRUE)
}

# RCP 4.5 2100

for(i in seq_along(select))
{ 
  lis2[i] <- list.files('G_variables/Set1/4.5', pattern = select[i], full.names = TRUE)
}

# RCP 6 2100

for(i in seq_along(select))
{ 
  lis3[i] <- list.files('G_variables/Set1/6.0', pattern = select[i], full.names = TRUE)
}

# RCP 8.5 2100

for(i in seq_along(select))
{ 
  lis4[i]  <- list.files('G_variables/Set1/8.5', pattern = select[i], full.names = TRUE)
}

# RCP CURRENT

for(i in seq_along(select))
{ 
  lis5[i] <- list.files('G_variables/Set1/Current/', pattern = select[i], full.names = TRUE)
}

final_list <- list(lis1,lis2,lis3,lis4, lis5)

# Mess map

for(i in seq_along(1:5))
{
  ras      <- lapply(final_list[i], stack)
  f        <- paste0('Results/', 'Mess_', names[i], '.asc')
  occ.sim  <- similarity(ras[[1]], env)
  writeRaster(occ.sim$similarity_min, filename = f)
  }


# Most different variable

for(i in seq_along(1:5))
{
  ras      <- lapply(final_list[i], stack)
  f        <- paste0('Results/', 'MOD_', names[i], '.asc')
  occ.sim  <- similarity(ras[[1]], env)
  writeRaster(occ.sim$mod, filename = f)
}

