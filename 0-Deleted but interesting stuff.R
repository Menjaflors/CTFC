
#### 0. DELETED BUT INTERESTING STUFF ####

## Find VIFs and filter predictors  ----------------------------------------------------------

list_preds_df <- as.data.frame(list_preds) # transform the big list to dataframe to do the vifs

### Use only a x% of the rasters data to do it faster (discuss with Dani)
id_sel <- sample(nrow(list_preds_df), nrow(list_preds_df)*0.1) # we select only a sample of the df
list_preds_df <- list_preds_df[id_sel,]

### Calculate VIFCOR (from the David's book pp. 107)

vifcor(list_preds_df, th=.7) # this function removes the variables with collinearity problems. Threshold of 0.7 is convention.

### Calculate VIF
# It calculates multicolinearity, while vifcor calculates collinearity
v_vif <- HH::vif(list_preds_df)
v_vif # Values of VIF exceeding 5 are considered evidence of multicollinearity, it means the variable is redundant, it can be explained by the other variables

# the next while loop removes each time the variable with greater vif, and then it calculates the vif again until the vif values are < 5

while (any(v_vif > 5)) {
  y <- names(v_vif)[v_vif==max(v_vif)]
  list_preds_df <- list_preds_df[, -which(colnames(list_preds_df) %in% y)]
  v_vif <<- HH::vif(list_preds_df)
  print(v_vif)
  print("#####")
} # 5 is the convention, but we can discuss it, but it is often used 10 too.

### Filter predictors to pred raster layers

v_pos_preds <- names(list_preds@layers) %in% colnames(list_preds_df) %>% which # here we find the position of the layers that are filtered from the VIF analysis 

list_preds <- raster::subset(list_preds, subset=v_pos_preds, drop=TRUE) # subset only the layers filtered in the vif analyses

rm(y, v_pos_preds)




## Dades vectorials  ----------------------------------------------------------

list_species_vector <- pblapply(list_species, function(x,y) { subset(x, x$res == "vector") })

filter <- rep(NA, length(list_species_vector))
for (i in 1:length(list_species_vector)) { if(nrow(list_species_vector[[i]])<2 ) filter[i] <- 1 } # >1 because vector minimum of 2 coordinates.
list_species_vector <-list_species_vector[is.na(filter)]

### Transform vector list to spdf

for (i in 1:length(list_species_vector)) {
  print(paste(i, "of", length(list_species_vector)))
  z <- list_species_vector[[i]]
  lonlat <- cbind(z$X, z$Y)
  list_species_vector[[i]] <- spLines(lonlat, crs=proj_EPSG25831)
}

names(list_species_vector) <- gsub("/" , " " , names(list_species_vector)) # remove '/' from names

## Wallace: build and Evaluate Niche Model  ----------------------------------------------------------

### Create the models

fun_maxent_model <- function(x, y, z) {
  
  e <- ENMeval::ENMevaluate(occ = x[c('longitude', 'latitude')],
                            env = list_preds,
                            bg.coords = y,
                            RMvalues = rms, # we can change this parameter in the initial code
                            fc = c('LQ'), # See Philips et al 2006; Phillips and Dudik 2008 || if we want to use more than one, create parameter 'tune'args and then --> list(fc = c("L","LQ","LQH","H"), rm = 1:5)
                            method = 'user', 
                            occ.grp = z$occs.grp, # group of occurrences
                            bg.grp = z$bg.grp,  # group of pseudoabsences points (background)
                            clamp = TRUE, # clamping is when you continue a model into a range that is new, so it may be very inacurate
                            algorithm = "maxent.jar") # change if we want to use '.jar' maxent
  return(e)
}

list_maxent_model <- vector(mode="list", len=length(list_species_500m_df))

for (i in 1:length(list_species_500m_df)) { 
  print(paste(i, "of", length(list_species_500m_df)))
  list_maxent_model[i] <- pblapply(list_species_500m_df[i], fun_maxent_model, y=list_bg.xy[[i]], z=list_partition[[i]])
}
names(list_maxent_model) <- names(list_species_500m_df)

### Find the AUC, OR & AIC

list_evalTbl <- pblapply(list_maxent_model, function(x) { x@results }) 
names(list_evalTbl) <- names(list_maxent_model)

#### L
list_evalTblL <- vector(mode="list", len=length(list_evalTbl))

for (i in 1:length(list_evalTbl)) { 
  print(paste(i, "of", length(list_evalTbl)))
  list_evalTblL[[i]] <- list_evalTbl[[i]][list_evalTbl[[i]]$fc=="L",] # We take the first raster because it is from the training data of L (70% of occurrences)
}

names(list_evalPredsL) <- paste0(names(list_evalTbl), "_L")

#### LQ

list_evalTblLQ <- vector(mode="list", len=length(list_evalTbl))

for (i in 1:length(list_evalPredsLQ)) { 
  print(paste(i, "of", length(list_evalPredsLQ)))
  list_evalTblLQ[[i]] <- list_evalTbl[[i]][list_evalTbl[[i]]$fc=="LQ",] # We take the third raster because it is from the training data of LQ (70% of occurrences)
}

names(list_evalTblLQ) <- paste0(names(list_evalTbl), "_LQ")


### Find the parameters of TWO models

list_evalMods <- pblapply(list_maxent_model, function(x) { x@models }) 

for (i in 1:length(list_evalMods)) { 
  print(paste(i, "of", length(list_evalMods)))
  names(list_evalMods[[i]]) <- list_maxent_model[[i]]@tune.settings$tune.args
}

names(list_evalMods) <- names(list_maxent_model)

### Obtain the rasters of model predictions

list_evalPreds <- pblapply(list_maxent_model, function(x) { x@predictions }) # create 2 raster stack for each partition (training and testing) & 1 raster for each model
list_evalPreds <- pblapply(list_evalPreds, crs_fun) # set projection

#### L
list_evalPredsL <- vector(mode="list", len=length(list_evalPreds))

for (i in 1:length(list_evalPredsL)) { 
  print(paste(i, "of", length(list_evalPredsL)))
  list_evalPredsL[[i]] <- list_evalPreds[[i]][[1]] # We take the first raster because it is from the training data of L (70% of occurrences)
}

names(list_evalPredsL) <- paste0(names(list_maxent_model), "_L")

#### LQ

list_evalPredsLQ <- vector(mode="list", len=length(list_evalPreds))

for (i in 1:length(list_evalPredsLQ)) { 
  print(paste(i, "of", length(list_evalPredsLQ)))
  list_evalPredsLQ[[i]] <- list_evalPreds[[i]][[3]] # We take the third raster because it is from the training data of LQ (70% of occurrences)
}

names(list_evalPredsLQ) <- paste0(names(list_maxent_model), "_LQ")

### Create binary models prediction

fun_binary <- function(x, y) { x[x>=y] <- 1; x[x<y] <- 0; return(x) }

list_evalPredsL_bi <- pblapply(list_evalPredsL, fun_binary, y=0.7) # 0.7 is the threshold to create the binary L model prediction
list_evalPredsLQ_bi <- pblapply(list_evalPredsLQ, fun_binary, y=0.7) # 0.7 is the threshold to create the binary LQ model prediction

i
## Create df with the Evaluation Metrics  ----------------------------------------------------------

### AUC
# AUC values should only be considered as relative indicators of performance (e.g. for the same dataset of a given species)

# Trabajando sin ausencias reales hace que el AUC no sea apropiado para comparar ENTRE especies, pero sí nos da una idea de la calidad 
# del modelo. Otro problema es que pondera por igual los errores de comisión (ausencia 
# predicha como presencia, que no tiene por qué ser un error) y de omisión 
# (presencia predicha como ausencia, que SÍ es un errorazo).
# Aun con estas críticas, el AUC es un estadístico muy útil.

#### L
auc.val.avgL <- pbsapply(list_evalTblL,  function(x) { x$auc.val.avg[1] }) # mean of the k test AUCs (one for each partition)
auc.val.sdL <- pbsapply(list_evalTblL,  function(x) { x$auc.val.sd[1] }) # variance of the k test AUCs (one for each partition)
auc.diff.avgL <- pbsapply(list_evalTblL,  function(x) { x$auc.diff.avg[1] }) # mean of all differences between the k training and validation AUCs
auc.diff.sdL <- pbsapply(list_evalTblL,  function(x) { x$auc.diff.sd[1] }) # variance of all differences between the k training and validation AUCs

#### LQ
auc.val.avgLQ <- pbsapply(list_evalTblLQ,  function(x) { x$auc.val.avg[1] }) # mean of the k test AUCs (one for each partition)
auc.val.sdLQ <- pbsapply(list_evalTblLQ,  function(x) { x$auc.val.sd[1] }) # variance of the k test AUCs (one for each partition)
auc.diff.avgLQ <- pbsapply(list_evalTblLQ,  function(x) { x$auc.diff.avg[1] }) # mean of all differences between the k training and validation AUCs
auc.diff.sdLQ <- pbsapply(list_evalTblLQ,  function(x) { x$auc.diff.sd[1] }) # variance of all differences between the k training and validation AUCs


### Omissino Rate (OR)
# OR evaluates the ability of a binary classifier (apply a threshold to the model) to predict test localities
# Values of 0 indicates that all localities fall inside the prediction, whereas 1 indicates that all fall outside

#### L
or.mtp.avgL <- pbsapply(list_evalTblL,  function(x) { x$or.mtp.avg[1] }) # mean of all validation MTP omission rates
or.mtp.sdL <- pbsapply(list_evalTblL,  function(x) { x$or.mtp.sd[1] }) # standard deviation of all validation Minimum Training Presence omission rates
or.10p.avgL <- pbsapply(list_evalTblL,  function(x) { x$or.10p.avg[1] }) # mean of all validation of lowest suitability score after excluding the lowest 10% of them
or.10p.sdL <- pbsapply(list_evalTblL,  function(x) { x$or.10p.sd[1] }) # standard of all validation of lowest suitability score after excluding the lowest 10% of them

#### LQ
or.mtp.avgLQ <- pbsapply(list_evalTblLQ,  function(x) { x$or.mtp.avg[1] }) # mean of all validation MTP omission rates
or.mtp.sdLQ <- pbsapply(list_evalTblLQ,  function(x) { x$or.mtp.sd[1] }) # standard deviation of all validation Minimum Training Presence omission rates
or.10p.avgLQ <- pbsapply(list_evalTblLQ,  function(x) { x$or.10p.avg[1] }) # mean of all validation of lowest suitability score after excluding the lowest 10% of them
or.10p.sdLQ <- pbsapply(list_evalTblLQ,  function(x) { x$or.10p.sd[1] }) # standard of all validation of lowest suitability score after excluding the lowest 10% of them


### AIC
# Models with the lowest AIC are identified as optimal among candidate models

#### L
AICcL <- pbsapply(list_evalTblL,  function(x) { x$AICc[1] }) # the AIC value itself
delta.AICcL <- pbsapply(list_evalTblL,  function(x) { x$delta.AICc[1] }) # absolute difference between the lowest AICc and each AICc 
w.AICL <- pbsapply(list_evalTblL,  function(x) { x$w.AIC[1] }) # the AIC weight, can be used in model averaging

#### LQ
AICcLQ <- pbsapply(list_evalTblLQ,  function(x) { x$AICc[1] }) # the AIC value itself
delta.AICcLQ <- pbsapply(list_evalTblLQ,  function(x) { x$delta.AICc[1] }) # absolute difference between the lowest AICc and each AICc 
w.AICLQ <- pbsapply(list_evalTblLQ,  function(x) { x$w.AIC[1] }) # the AIC weight, can be used in model averaging


### Create dfs

#### L
eval_metrics_L_df <- data.frame(auc.val.avg=auc.val.avgL, # parameters of AUC
                                auc.val.sd=auc.val.sdL,
                                auc.diff.avg=auc.diff.avgL,
                                auc.diff.sd=auc.diff.sdL,
                                
                                or.mtp.avg=or.mtp.avgL, # parameters of OR
                                or.mtp.sd=or.mtp.sdL,
                                or.10p.avg=or.10p.avgL,
                                or.10p.sd=or.10p.sdL,
                                
                                AICc=AICcL, # parameters of AIC
                                delta.AICc=delta.AICcL,
                                w.AIC=w.AICL)

eval_metrics_L_df <- round(eval_metrics_L_df, 3)
rownames(eval_metrics_L_df) <- names(list_species_500m)


#### LQ
eval_metrics_LQ_df <- data.frame(auc.val.avg=auc.val.avgLQ, # parameters of AUC
                                 auc.val.sd=auc.val.sdLQ,
                                 auc.diff.avg=auc.diff.avgLQ,
                                 auc.diff.sd=auc.diff.sdLQ,
                                 
                                 or.mtp.avg=or.mtp.avgLQ, # parameters of OR
                                 or.mtp.sd=or.mtp.sdLQ,
                                 or.10p.avg=or.10p.avgLQ,
                                 or.10p.sd=or.10p.sdLQ,
                                 
                                 AICc=AICcLQ, # parameters of AIC
                                 delta.AICc=delta.AICcLQ,
                                 w.AIC=w.AICLQ)

eval_metrics_LQ_df <- round(eval_metrics_LQ_df, 3)
rownames(eval_metrics_LQ_df) <- names(list_species_500m)



## Create df with the Variables Importance/Contribution  ----------------------------------------------------------

### Create empty dataframes

N_varimp <- length(list_preds@layers) # number of predictors used
varImp_L_df <- data.frame(species=names(list_species_500m))
varImp_L_df <- as.data.frame(matrix(ncol = length(list_preds@layers))) # create empty columns
colnames(varImp_L_df) <- names(list_preds@layers)

varImp_LQ_df <- varImp_L_df # the same but with LQ

#### L

for (i in 1:length(list_maxent_model)) {
  print(paste(i, "of", length(list_maxent_model)))
  varImp_L_df[i, 1:N_varimp] <- list_maxent_model[[i]]@variable.importance[["rm.1_fc.L"]]$percent.contribution # each vector extracted from the predictors importance/contribution to the model, is sent to a row of the dataframe
}

varImp_L_df <- round(varImp_L_df, 3)
rownames(varImp_L_df) <- names(list_species_500m) # species as rownames

#### LQ

for (i in 1:length(list_maxent_model)) {
  print(paste(i, "of", length(list_maxent_model)))
  varImp_LQ_df[i, 1:N_varimp] <- list_maxent_model[[i]]@variable.importance[["rm.1_fc.LQ"]]$percent.contribution # each vector extracted from the predictors importance/contribution to the model, is sent to a row of the dataframe
}

varImp_LQ_df <- round(varImp_LQ_df, 3)
rownames(varImp_LQ_df) <- names(list_species_500m) # species as rownames


## Extract the value of each predictor as a column of the species (probably delete in the end)  ----------------------------------------------------------

fun_extract <- function(x) { x <- raster::extract(list_preds, x, sp=T); return(x) }  # sp=T hace que el resultado se añada como columnas en el objeto espacial de puntos

<<<<<<< HEAD
## Marxan WITH ZONES  ----------------------------------------------------------
## Initial code
# #### Costs for Strict Conservation
# list_SC <- list()
# 
# #### Costs for Mix-Purpose
# list_MP <- list()
# 
# #### Costs for Human Uses
# list_HU <- list()
# 

#### Targets
targets_features_df <- read_excel(paste0(dir, "/Dades_projecte.xlsx"),
                                  sheet = "Especies_HIC") %>% as.data.frame() # Targets for species.

targets_human_uses_df <- read_excel(paste0(dir, "/Dades_projecte.xlsx"),
                                    sheet = "Human_Uses") %>% as.data.frame() # Targets for human uses.

## Integrate costs 
### Three management zones
# stack_costs <- stack(list_SC, list_MP, list_HU) # create a rasterstack with the costs for each MZ
# plot(stack_costs, main = c("Strict Conservation", "Mix-Purpose", "Human Uses"), axes = FALSE, box = FALSE)


## Integrate targets
# Extinct (EX) --> NA
# Extinct in the wild (EW) --> NA
# Critically endangered (CR) --> 1
# Endangered (EN) --> 0.8
# Vulnerable (VU) --> 0.6
# Near threatened (NT) --> 0.4
# Least concern (LC) --> 0.2
# Data deficient (DD) --> NA
# Not evaluated (NE) --> NA

### Three management zones (target distribution)

## Target distribution  ----------------------------------------------------------

targets_distribution <- matrix(NA, ncol = 3, nrow = nrow(targets_features_df))

targets_distribution[,1] <- targets_features_df$SC
targets_distribution[,2] <- targets_features_df$MP

targets_distribution[,3] <- targets_features_df$HU

## Integrate fragmentation

### Three management zones

# Here we create a matrix that describe boundary penalties between PUs allocated to different zones.
# We want that SC will be surrounded by MP, and MP by HU, as done in Abarca et al. 2022.

m_penalties <- diag(3)
m_penalties[1, 2] <- 1 # cluster Mix-purpose with Strict Conservation
m_penalties[2, 1] <- 1 # cluster Mix-purpose with Strict Conservation
m_penalties[2, 3] <- 1 # cluster Mix-purpose with Human uses
m_penalties[3, 2] <- 1 # cluster Mix-purpose with Human uses

colnames(m_penalties) <- c("SC", "MP", "HU")
rownames(m_penalties) <- colnames(m_penalties)
print(m_penalties) # print matrix

## Problem 2: three management zones

# Strict Conservation (SC)
# Mix-Pruposes (MP)
# Human Uses (HU)

p_zones <- problem(stack_costs[[2]], stack_features[[1:5]]) %>%
  add_min_set_objective() %>% # we want to protect the maximum possible, so we set the minimum objective
  add_relative_targets(targets_distribution) %>% # we add the specific targets for each MZ
  add_locked_out_constraints(my_constraints) %>% # we add the constrains (human infraestructures)
  add_mandatory_allocation_constraints() %>% # we add this if we want to have all UPs with any MZ category. This is how Abarca et al. 2022 did it, so we follow the same procedure.
  add_boundary_penalties(penalty = my_connect, zones = m_penalties) %>% # high overall penalty, and clustering according to m_penalties matrix
  add_gurobi_solver(gap = 0) %>% # specify that Gurobi should be used to solve the problem, and specify an optimality gap of zero to obtain the optimal solution
  add_top_portfolio(number_solutions = 5) # Generate a portfolio of the best 5 solutions by storing feasible solutions found during the Gurobi optimization process


#### Solve problem

s_zones <- solve(p_zones) # download Guroby

#### calculate Feature Representation
fr_zones <- eval_feature_representation_summary(p_zones, s_zones)
print(r1)

#### plot solution
plot(s_zones, main = "Solution Three Management Zones", axes = FALSE, box = FALSE)

#### Checks

print(p_zones) # print problem
number_of_planning_units(p_zones) # print number of planning units
number_of_features(p_zones) # print number of features

eval_n_summary(p_zones, s_zones) # Calculate the number of planning units selected within a solution
eval_cost_summary(p_zones, s_zones) # Calculate the total cost of a solution.
eval_feature_representation_summary(p_zones, s_zones) # Calculate how well features are represented by a solution.
eval_target_coverage_summary(p_zones, s_zones) # Calculate how well feature representation targets are met by a solution.
eval_connectivity_summary(p_zones, s_zones, data = connectivity_matrix(sim_pu_raster, sim_features[[1]])) # Calculate the connectivity held within a solution.

targets_distribution[,3] <- targets_features_df$HU
