#### 2.1. SPECIES DISTRIBUTION MODELLING ####

## Meta  ----------------------------------------------------------

# Run wallace --> run_wallace()
# https://rspatial.org/raster/sdm/index.htm
# VIF --> https://www.analyticsvidhya.com/blog/2020/03/what-is-multicollinearity/
# https://support.bccvl.org.au/support/solutions/articles/6000127046-sdm-interpretation-of-model-outputs
# Discutir si reduir el biaix dels punts de presència (zones sobremostrejades). Tema 4 dels scripts del màster.
# Comprobar si amb Maxent s'usa l'AUC d'aquesta manera o està dintre del software.

## Initial code  ----------------------------------------------------------

### We delete all objects except lists of species

not_rm <- c("list_species", "list_species_100m", "list_species_500m", "list_species_1km") # concatenation of object names to remain

rm(list=ls()[! ls() %in% not_rm])

### Install packages in case they are not installed

list.of.packages <- c("tidyverse", "readr", "readxl", "rgdal", "sp", "raster", "sf", "rgeos", "MASS", "rlist", "pbapply",
                      "ggplot2", "cowplot", "plotly", "vegan", "geiger", "HH", "dismo", "wallace", "DescTools",
                      "spocc", "spThin", "ENMeval", "dplyr", "zonator", "usdm", "openxlsx", "rJava", "foreign")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # spatstats doesn't work with this version of R
if(length(new.packages)>0) {install.packages(new.packages)}

### Open libraries

invisible(lapply(list.of.packages, library, character.only = TRUE, logical.return=T))
rm(list.of.packages, new.packages) # remove objects we won't need anymore.

### Open functions from wallace

# source(system.file('shiny/funcs', 'functions.R', package = 'wallace')) # The following code opens several functions need to run wallace.

### Set constants and variables

n_pseudoabs <- 10000 # number of pseudoabsences
rms <- seq(1, # define the vector of regularization multipliers to test, it means the penalization of complex models. The first number is the beninning,
           2, # The second number is the finishing
           1) # The third nuomber is the distance to take to create each regularization. We can try to use 0.5

### Set directories
dir<-"C:/Users/guillem.pocull/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY"
dir_bioclim <- paste0(dir, "/PROCESSED/BIOCLIM_AMBIT_ASC") # directory bioclim layers
dir_raw <- paste0(dir,"/RAW") # directory simsy
dir_processed <- paste0(dir,"/PROCESSED") # directory simsy
dir_export <- paste0(dir, "/EXPORT")
dir_mdt <- paste0(dir_export, "/MDT_RASTER/asc/100x100m")
dir_habitat <- paste0(dir_export, "/HABITATS_RASTER/asc/CAT_HABITATS")

### Open files
#### Ambit
ambit_PN <- readOGR(paste0(dir, "/SIMSY/INFO_SONIA_141022/LIMIT_PARC_DECRET_21/LIMIT_2021_v2.shp"))
ambit_RB <- readOGR(paste0(dir, "/SIMSY/INFO_SONIA_141022/LimitsRB/DelimitacioShpED50RBM13_ETRS89.shp"))

# #### Bioclim tifs
# filenames <- list.files(dir_bioclim, pattern="*.tif", full.names=TRUE)
# list_bioclim <- pblapply(filenames, raster)

#### Grids
grid_100m_r <- raster(paste0(dir_export, "/GRIDS/grid_raster_100x100m.asc"))
grid_500m_r  <- raster(paste0(dir_export, "/GRIDS/grid_raster_500x500m.asc"))
grid_1km_r  <- raster(paste0(dir_export, "/GRIDS/grid_raster_1x1km.asc"))

#### MDT layers
filenames <- list.files(dir_mdt, pattern="*.asc", full.names=T)
list_mdts <- pblapply(filenames, raster)
filenames_short <- filenames %>% basename %>% file_path_sans_ext
names(list_mdts) <- filenames_short

### Estructura forestal
filenames <- list.files(paste0(dir_export, "/ESTRUCTURA_FOREST_RASTER/asc/100x100m"), pattern="*.asc", full.names=T)
list_estruc <- pblapply(filenames, raster)
filenames_short <- filenames %>% basename %>% file_path_sans_ext
names(list_estruc) <- filenames_short

#### Habitats layers
filenames <- list.files(dir_habitat, pattern="*.asc", full.names=T)
list_habs <- pblapply(filenames, raster)
filenames_short <- filenames %>% basename %>% file_path_sans_ext
names(list_habs) <- filenames_short

#### Rivers
filenames <- list.files(paste0(dir_export, "/HIDRO_RASTER/asc/"), pattern="*r_dist_rivers_100m.asc", full.names=T)
list_rivers <- pblapply(filenames, raster) # we create one element list to fix the name problem in the rasterstack
names(list_rivers) <- "dist_rivers"

#### Longitude and latitude predictors
# grid_lon_100m_r <- raster(paste0(dir_export, "/LON_LAT_RASTER/asc/grid_lon_100m_r.asc"))
# grid_lat_100m_r <- raster(paste0(dir_export, "/LON_LAT_RASTER/asc/grid_lat_100m_r.asc"))
# list_lonlat <- list(grid_lon_100m_r, grid_lat_100m_r)
# names(list_lonlat) <- c("grid_lon_100m_r", "grid_lat_100m_r")

#### Climate
filenames <- list.files(paste0(dir_export, "/CLIMATE_RASTER/asc/"), pattern="*.asc", full.names=T)
list_climate <- pblapply(filenames, raster)
filenames_short <- filenames %>% basename %>% file_path_sans_ext
names(list_climate) <- filenames_short

#### HIC grids
filenames <- list.files(paste0(dir_export, "/HIC_RASTER/asc"), pattern="*.asc", full.names=TRUE)
list_HIC <- pblapply(filenames, raster)

rm(filenames, filenames_short)

#### Subsubconques
subsubconques <- readOGR(paste0(dir, "/SIMSY/03_INFORMACIO_BASE/32_MEDI_NATURAL/32f_HIDROLOGIA/XarxaHidrografica/ConquesHidrografiquesMSY/SubConquesHidroMSY10_ETRS89.shp"))

#### Subconques
filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/asc/SUBCONQUES"), pattern="*.asc", full.names=T)
list_mask_subconques <- pblapply(filenames, raster)
filenames_short <- filenames %>% basename %>% file_path_sans_ext
names(list_mask_subconques) <- filenames_short

#### Miramon palette
palette_df <- read.dbf(paste0(dir, "/RAW/PALETA_MODELS_MIRAMON/aonc3_paleta_model.dbf"))

#### List species trees

v_species_trees <- read_delim(paste0(dir_raw, "/llista_arbres_nom_cientific.csv"), col_names = T,
                            delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame

v_species_trees <- v_species_trees[!is.na(v_species_trees$presencia_HIC),]
v_species_trees <- as.character(v_species_trees$element)

#### Targets or weights
dades_projecte_df <- read_excel(paste0(dir, "/Documents/Dades_projecte.xlsx"), sheet = "Elements") %>% as.data.frame() # Targets for species.


## Set projections  ----------------------------------------------------------

### Projections to use

proj_EPSG25831 <- crs(ambit_PN) # think if using the crs of a known layer or a standard one, as the next line of code 
proj_EPSG4326 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # typical world projection called WGS 84, synonim of EPSG:4326

### Reproject objects

# crs() is used to label a layer that we know that is created under a specific projection. DO not use to reproject.
# spTransform() This is the function used to reproject a shapefile
# To reproject a raster, the best way is to provide an existing raster, and use the function projectExtent()

fun_reproject <- function(x) {
  
  if(identical(crs(x), proj_EPSG25831)) {
    y <- spTransform(x, proj_EPSG25831)
    return(y)
  }
}

subsubconques <- fun_reproject(subsubconques)

ambit_RB <- spTransform(ambit_RB, proj_EPSG25831) # It doesn't work with ambit_RB so I do it without the function

#### Set crs rasters

crs_fun <- function(x) { crs(x) <- proj_EPSG25831; return(x) }

list_mdts <- pblapply(list_mdts, crs_fun)
list_estruc <- pblapply(list_estruc, crs_fun)
list_habs <- pblapply(list_habs, crs_fun)
list_rivers <- pblapply(list_rivers, crs_fun)
list_climate <- pblapply(list_climate, crs_fun)
list_mask_subconques <- pblapply(list_mask_subconques, crs_fun)

### Check all objects have the same projections

AllIdentical(proj_EPSG25831, crs(subsubconques), crs(ambit_RB), crs(list_mdts[[1]]), crs(list_estruc[[1]]), crs(list_rivers[[1]]),
             crs(list_climate[[1]]), crs(list_mask_subconques[[1]]))

## Extract coordinates from raster  ----------------------------------------------------------

# ### 100x100m
# for (i in 1:length(list_species_100m)) {
#   print(paste(i, "of", length(list_species_100m)))
#   x <- gridSample(list_species_100m[[i]]@coords, r=grid_100m_r, n=1)
#   list_species_100m[[i]]@coords <- x
# }
# 
# ### 500x500m
# for (i in 1:length(list_species_500m)) { 
#   print(paste(i, "of", length(list_species_500m)))
#   x <- gridSample(list_species_500m[[i]]@coords, r=grid_500m_r, n=1)
#   list_species_500m[[i]]@coords <- x
# }
# 
# ### 1x1km 
# # Discuss with Dani how we extract coordinates here

## Create list of dfs to introduce to Wallace  ----------------------------------------------------------

fun_df <- function(x) {
  
  df <- as.data.frame(x@coords)
  df$name <- x@data[["DESCRIPCIO_EPN"]][nrow(df)]
  
  colnames(df) <- c("longitude", "latitude", "name")
  df <- df[, c("name", "longitude", "latitude")]
  df$occID <- row.names(df)
  return(df)
}

list_species_100m_df <- pblapply(list_species_100m, fun_df)
names(list_species_100m_df) <- names(list_species_100m)

list_species_500m_df <- pblapply(list_species_500m, fun_df)
names(list_species_500m_df) <- names(list_species_500m)

### Join all resolutions together

list_species <- c(list_species_100m_df, list_species_500m_df) # do we join species with 1km resolution???? Also remove the species of 1km that are already in 100m or 500m???(fix it at the end)
#list_species <- list_species_500m_df # delete at the end

list_species <- list_species[order(names(list_species))] # sort alphabetically

### Delete in the end!!

# v <- c("Anoplodera sexguttata (Fabricius, 1775)", "Anoplotrupes stercorosus (Scriba, 1791)", "Miniopterus schreibersii", "Malpolon monspessulanus",
#        "Bryaxis mulsantii (Kiesenwetter, 1850)", "Fraxinus excelsior ", "Grynobius planus (Fabricius, 1787)", "Crataegus monogyna ", "Sylvia melanocephala")
# list_species <- list_species[v]


## Create predictors   ----------------------------------------------------------

### Create list of predictors
list_preds <- c(list_mdts, list_habs, list_estruc, list_rivers, list_climate) # big list of all the predictors. Add bioclim predictors at the end
list_preds <- raster::stack(list_preds)
names(list_preds@layers) <- names(c(list_mdts, list_habs, list_estruc, list_rivers, list_climate)) # introduce the names of the list to the stack
rm(list_mdts, list_habs, list_estruc, list_rivers, list_climate)

## Filter species and subconques to create the models ----------------------------------------------------------

### Remove tree species from models because they are inside HIC
matches <- grep(paste(v_species_trees, collapse="|"), names(list_species), value=TRUE)
matches <- matches[nchar(matches)< 60] # we remove all the HIC that could be contained inside 'matches'
list_species <- list_species[!names(list_species) %in% matches]

### Remove subconques rasters by tree species from models
matches <- grep(paste(v_species_trees, collapse="|"), names(list_mask_subconques), value=TRUE)
matches <- matches[nchar(matches)< 60] # we remove all the HIC that could be contained inside 'matches'
list_mask_subconques <- list_mask_subconques[!names(list_mask_subconques) %in% matches]

### Filter species with less than and more than 10 observations
records <- 10
filter <- rep(NA, length(list_species))
for (i in 1:length(list_species)) { if(nrow(list_species[[i]]) < records) filter[i] <- 1 }

list_species_less10 <-list_species[!is.na(filter)] # list of species with less than 10 obs.
list_species <-list_species[is.na(filter)] # list of species with 10 or more than 10 obs.


## Wallace: Process Enviromental data (pseudoabsences) ----------------------------------------------------------

# Here we take a random sample of background values from the study extent (ambit_RB)
# The better your background sample, the less variability you’ll have between runs.
# Unique pseudoabsences for each species, but always the same sample size

fun_pseudoabs <- function(x) {
  bg.xy <- dismo::randomPoints(list_preds[[1]], n_pseudoabs) # sample random background points
  bg.xy <- as.data.frame(bg.xy)  # convert matrix output to data frame
  colnames(bg.xy) <- c("longitude", "latitude")
  return(bg.xy)
}

list_bg.xy <- pblapply(list_species, fun_pseudoabs)

## Wallace: partition Occurrence Data ----------------------------------------------------------
# Here we can use the 'block' function, it is the only one that seemed to work.

fun_partition <- function(x, y) {
  occs.xy <- x[c('longitude', 'latitude')]
  group.data <- ENMeval::get.block(occ = occs.xy, bg = y)
  return(group.data)
} # code from Wallace

list_partition <- vector(mode = "list", length = length(list_species)) # create partition empty list

for (i in 1:length(list_species)) { 
  print(paste(i, "of", length(list_species)))
  list_partition[i] <- pblapply(list_species[i], fun_partition, y=list_bg.xy[[i]])
}

names(list_partition) <- names(list_species)

## Create big loop to build and Evaluate Niche Model  ----------------------------------------------------------

### Create objects previous to looping 

auc.val.avgLQT <- vector(mode="numeric", length=length(list_species))
auc.diff.avgLQT <- vector(mode="numeric", length=length(list_species))

N_varimp <- length(list_preds@layers) # number of predictors used
varImp_LQT_df <- data.frame(species=names(list_species))
varImp_LQT_df <- as.data.frame(matrix(nrow = length(list_species), ncol = length(list_preds@layers))) # create empty columns
colnames(varImp_LQT_df) <- names(list_preds@layers)
rownames(varImp_LQT_df) <- names(list_species) # species as rownames

my_height <- nrow(grid_100m_r)*5
my_width <- ncol(grid_100m_r)*5

### Palette: transform RGB to hex

rgb2hex = function(x){
  # function to apply to each row of input x
  ProcessColumn = function(row){
    rgb(x[row, 1], 
        x[row, 2], 
        x[row, 3], 
        maxColorValue = 255)
  }
  # Apply the function
  sapply(1:(nrow(x)-1), ProcessColumn)
} # function to transform rgb colors to hex

palette_df$hex <- c(rgb2hex(palette_df), "NA") # we concatenate because the last row is incompatible with rgb2hex function.

### Create list_species to sp
# We create this list to plot the points inside the png
list_species_sp <- pblapply(list_species, function(x) { SpatialPointsDataFrame(coords = x[, c("longitude", "latitude")],
                                                                               data = x,
                                                                               proj4string =proj_EPSG25831) } )

### Loop itself

for (i in 1:length(list_species)) {
  
  tryCatch({
  print(paste(i, "of", length(list_species)))
  
  ### Wallace: build and Evaluate Niche Model
  
  fun_maxent_model <- function(x, y, z) {
    
    e <- ENMeval::ENMevaluate(occ = x[c('longitude', 'latitude')],
                              env = list_preds,
                              bg.coords = y,
                              RMvalues = rms, # we can change this parameter in the initial code
                              fc = 'LQT', # See Philips et al 2006; Phillips and Dudik 2008 || if we want to use more than one, create parameter 'tune'args and then --> list(fc = c("L","LQ","LQH","H"), rm = 1:5)
                              method = 'user', 
                              occ.grp = z$occs.grp, # group of occurrences
                              bg.grp = z$bg.grp,  # group of pseudoabsences points (background)
                              clamp = TRUE, # clamping is when you continue a model into a range that is new, so it may be very inacurate
                              algorithm = "maxent.jar") # change if we want to use '.jar' maxent
    return(e)
  }
  
  maxent_model <- fun_maxent_model(x=list_species[[i]], y=list_bg.xy[[i]], z=list_partition[[i]])

  ### Find the AUC
  evalTbl <- maxent_model@results 
  
  #### LQT
  evalTblLQT <- evalTbl[evalTbl$fc=="LQT",] # We take the forth raster because it is from the test data of T (30% of occurrences to test)
  
  ### Find the parameters of TWO models
  evalMods <- maxent_model@models 
  name_evalMods <- maxent_model@tune.settings$tune.args
  
  ### Obtain the rasters of model predictions
  evalPreds <- maxent_model@predictions # create 2 raster stack for each partition (training and testing) & 1 raster for each model
  evalPreds <- crs_fun(evalPreds) # set projection
  
  #### LQT
  evalPredsLQT <- evalPreds@layers[[2]] # We take the second raster because it is from the test data of T (30% of occurrences)
  
  ### Add Evaluation Metrics
  #### LQT
  auc.val.avgLQT[i] <- evalTblLQT$auc.val.avg[2] # mean of the k test AUCs (we take the test data)
  auc.diff.avgLQT[i] <- evalTblLQT$auc.diff.avg[2]  # mean of all differences between the k training and validation AUCs (we take the test data)
  
  ### Add Variables Importance/Contribution
  #### LQT
  varImp_LQT_df[i, 1:N_varimp] <- maxent_model@variable.importance[["rm.2_fc.LQT"]]$percent.contribution # each vector extracted from the predictors importance/contribution to the model, is sent to a row of the dataframe
  
  ### Remove SDM on certain conditions
  
  if (auc.val.avgLQT[i] > 0.7) {
    

    ### Categories
    # Firstly, we cut the SDM by the mean value of the 10% lower POINT values --> '0'
    # Secondly, we cut the SDM by the mean of the non '0' values --> '1' or 'adequat'
    # Thirdly, we cut the SDM by the mean of the '1' or 'adecuat' values --> '2' or 'bo'
    # Forthly, we create the categorical sdm with 0, 1, 2 & 3 values
    
    fun_categories_SDM <- function(r) {
      
      r1<-r
      
      valor <- raster::extract(r1, list_species_sp[[i]]) # extract the values of all the points
      valorq10 <- valor[valor < quantile(valor, 0.1)] # values lower of threhold Quantile 0.1 
      min_points <- mean(valorq10) # mean of the values lower of Q10

      r2 <- r
      r2[r2[] < min_points] <- 0 # set values lower of the mean of Q10 to 0
      m2 <- mean(r2[r2!=0]) # m2 is the mean of the non-zero new values (adequat-bo)
      
      r3 <- r
      r3[r3[] < m2] <- 0  # set values lower of the mean2 to 0
      m3 <- mean(r3[r3!=0]) # m3 is the mean of the non-zero new values (bo-optim)
      
      r[r[] <= min_points]  <- 0 # here we transform the original raster
      r[r[] >= m3] <- 3 # here we transform the original raster
      r[r[] > min_points & r[] <= m2]  <- 1 # here we transform the original raster
      r[r[] > m2 & r[] < m3]  <- 2 # here we transform the original raster
      
      return(r)
    }
    
    evalPredsLQT_cat <- fun_categories_SDM(evalPredsLQT)
    
    ### Marxan categories
    # Firstly, we cut the SDM by the mean value of the 10% lower POINT values --> '0'
    # Secondly, we cut the SDM by the mean of the non '0' values --> '1' or 'adequat'
    # Thirdly, we cut the SDM by the mean of the '1' or 'adecuat' values --> '2' or 'bo'
    # Forthly, we create the categorical sdm with 0, 1 inside 0, 2 & 3 values
    
    fun_categories_marxan <- function(r) {
      
      r1<-r
      
      valor <- raster::extract(r1, list_species_sp[[i]]) # extract the values of all the points
      valorq10 <- valor[valor < quantile(valor, 0.1)] # values lower of threhold Quantile 0.1 
      min_points <- mean(valorq10) # mean of the values lower of Q10
      
      
      r2 <- r
      r2[r2[] < min_points] <- 0 # set values lower of the mean of Q10 to 0
      m2 <- mean(r2[r2!=0]) # m2 is the mean of the non-zero new values (adequat)
      
      r3 <- r
      r3[r3[] < m2] <- 0  # set values lower of the mean2 to 0
      m3 <- mean(r3[r3!=0]) # m3 is the mean of the non-zero new values (bo)
      
      r[r[] <= m2]  <- 0 # here we transform the original raster
      r[r[] >= m3] <- 1 # here we transform the original raster
      r[r[] > m2 & r[] < m3]  <- 0.5 # here we transform the original raster
      
      return(r)
      
    } # marxan only wants 'bo' and 'optim' categories
    
    evalPredsLQT_marxan <- fun_categories_marxan(evalPredsLQT)
    
    ### Mask the SDM by subconques
    evalPredsLQT <- evalPredsLQT * list_mask_subconques[[i]] # We take the second raster because it is from the test data of T (30% of occurrences)
    evalPredsLQT_cat <- evalPredsLQT_cat * list_mask_subconques[[i]] # We take the second raster because it is from the test data of T (30% of occurrences)
    evalPredsLQT_marxan <- evalPredsLQT_marxan * list_mask_subconques[[i]] # We take the second raster because it is from the test data of T (30% of occurrences)
    
    ### Export rasters model prediction
    name_export <- names(list_species)[i] # Create name for the raster exports
    
    ### Export rasters of model prediction on asc
    
    writeRaster(evalPredsLQT, paste0(dir_export, "/PREDICTION_RASTER/asc/continuous/", substr(name_export, 1, 40), ".asc"), format='ascii', overwrite=T)
    writeRaster(evalPredsLQT_cat, paste0(dir_export, "/PREDICTION_RASTER/asc/cats/", substr(name_export, 1, 40), ".asc"), format='ascii', overwrite=T)
    writeRaster(evalPredsLQT_marxan, paste0(dir_export, "/PREDICTION_RASTER/asc/marxan/", substr(name_export, 1, 40), ".asc"), format='ascii', overwrite=T)
    
    ### Export rasters of model prediction on png
    
    dev.off()
    
    png(paste0(dir_export, "/PREDICTION_RASTER/png/continuous/", substr(name_export, 1, 40), ".png"), height=my_height, width=my_width)
    plot(evalPredsLQT, breaks=palette_df$MAX_VALOR, col = palette_df$hex); plot(ambit_RB, border="red", add=T);
    plot(ambit_PN, add=T); plot(list_species_sp[[i]], pch = 1, col = "orange", lwd=2, cex=2, add=T)
    dev.off()
    
    png(paste0(dir_export, "/PREDICTION_RASTER/png/cats/", substr(name_export, 1, 40), ".png"), height=my_height, width=my_width)
    plot(evalPredsLQT_cat, breaks=palette_df$MAX_VALOR, col = palette_df$hex); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T);
    plot(list_species_sp[[i]], pch = 3, col= "orange", add=T)
    dev.off()
    
    png(paste0(dir_export, "/PREDICTION_RASTER/png/marxan/", substr(name_export, 1, 40), ".png"), height=my_height, width=my_width)
    plot(evalPredsLQT_marxan, breaks=palette_df$MAX_VALOR, col = palette_df$hex); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T); plot(list_species_sp[[i]], pch = 3, col= "orange", add=T)
    dev.off()
    
    ### Export Response Curve on png
    
    png(paste0(dir_export, "/RESPONSE_CURVE/", substr(name_export, 1, 40), ".png"), height=my_height, width=my_width, pointsize = 20)
    dismo::response(evalMods[["rm.2_fc.LQT"]], col="black", lwd=0.5, expand=0) # we export the test data model (30%)
    dev.off()
    
    ### Export Variable Contribution on png
    
    png(paste0(dir_export, "/VARIABLES_CONTRIBUTION/", substr(name_export, 1, 40), ".png"), height=my_height, width=my_width, pointsize = 20)
    plot(evalMods[["rm.2_fc.LQT"]])
    dev.off()
    
    
  }
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # this line of code is used to continue even if there is errors.
} 

## Join all models into one layer  ----------------------------------------------------------

### Import all asc to do any change

fun_import <- function(y){
  x <- vector(mode="list")
  filenames <- list.files(paste0(dir_export, "/PREDICTION_RASTER/asc/", y, "/"), pattern="*.asc", full.names=T) # for now we will use the LQ models from 500 to 100m
  x <- pblapply(filenames, raster)
  names(x) <- filenames %>% basename %>% file_path_sans_ext
  x <- x[names(x) != "grid_all_SDM_LQT"]
  return(x)
}

list_evalPredsLQT <- fun_import("continuous")
list_evalPredsLQT_cat <- fun_import("cats")
list_evalPredsLQT_marxan <- fun_import("marxan")

### Create list of species without SDM

filter_no_SDM <- !substr(names(list_species), 1, 40) %in% names(list_evalPredsLQT) # filter species < 10 obs and species with AUC < 0.7

list_species_no_sdm <- append(list_species_less10, list_species[filter_no_SDM]) # append to a new list with all species without SDM

### Create list of species without SDM and only with conservation status

filter_conserv_status <- dades_projecte_df[!is.na(dades_projecte_df$estatus_conserv) & dades_projecte_df$tractament != "SDM", "nom_element"]  # remove NAs because they are elements without conservation status

list_species_no_sdm_conserv_status <- list_species_no_sdm[filter_conserv_status]

list_species_no_sdm_conserv_status[sapply(list_species_no_sdm_conserv_status, is.null)] <- NULL


### Create rasters species without SDM 

fun_to_spdf <- function(x) {
  coordinates(x) <- c("longitude", "latitude") 
  crs(x) <- proj_EPSG25831
  return(x)
}
fun_point_presence <- function(pts, r){
  r2 <- r
  r2[] <- 0 # make the inout raster of zeroes
  counts <- table(cellFromXY(r,pts)) # get the number of points inside each cell
  counts[] <- 1 # change all elements to 1, as presences.
  r2[as.numeric(names(counts))] <- counts # fill in the raster with the counts from the number of points inside each cell
  return(r2)
} # Point presence

list_species_no_sdm <- pblapply(list_species_no_sdm, fun_to_spdf)
list_species_no_SDM_raster <- pblapply(list_species_no_sdm, fun_point_presence, r=grid_100m_r)

### Create rasters species without SDM and only with conservation status

list_species_no_sdm_conserv_status <- pblapply(list_species_no_sdm_conserv_status, fun_to_spdf)
list_species_no_sdm_conserv_status_raster <- pblapply(list_species_no_sdm_conserv_status, fun_point_presence, r=grid_100m_r)


### Transform marxan to absence/presence

# list_evalPredsLQT_marxan <- pblapply(list_evalPredsLQT_marxan, function(x) { x[x!=0] <- 1; return(x) }) # discuss that in the end

### Create each sum grid for species and HICs 

grid_richness_SDM <- calc(stack(list_evalPredsLQT_marxan), sum) # sum species marxan categories
grid_richness_HICs <- calc(stack(list_HIC), sum) # Create sum of all HICs
grid_richness_no_SDM <- calc(stack(list_species_no_SDM_raster), sum) # Create sum of species < 10 obs
grid_richness_no_SDM_conserv_status <- calc(stack(list_species_no_sdm_conserv_status_raster), sum) # Create sum of species < 10 obs

### Create sum of species + HICs + species < 10 obs

grid_all_richness <- calc(stack(grid_richness_SDM, grid_richness_HICs, grid_richness_no_SDM), sum) # Create sum of all HICs

fun_scale <- function(r) { (r - cellStats(r, "min")) / (cellStats(r, "max") - cellStats(r, "min") - 0) * 1 } # function to rescale/normalize data to 0-1
grid_all_richness_scaled <- fun_scale(grid_all_richness) # continuous

## Export rasters ----------------------------------------------------------

#### asc
writeRaster(grid_richness_SDM, paste0(dir_export, "/RICHNESS_RASTER/asc/", "grid_richness_SDM"), format='ascii', overwrite=T)
writeRaster(grid_richness_HICs, paste0(dir_export, "/RICHNESS_RASTER/asc/", "grid_richness_HICs"), format='ascii', overwrite=T)
writeRaster(grid_richness_no_SDM, paste0(dir_export, "/RICHNESS_RASTER/asc/", "grid_richness_no_SDM"), format='ascii', overwrite=T)
writeRaster(grid_richness_no_SDM_conserv_status, paste0(dir_export, "/RICHNESS_RASTER/asc/", "grid_richness_no_SDM_conserv_status"), format='ascii', overwrite=T)

writeRaster(grid_all_richness, paste0(dir_export, "/RICHNESS_RASTER/asc/", "grid_all_richness"), format='ascii', overwrite=T)
writeRaster(grid_all_richness_scaled, paste0(dir_export, "/RICHNESS_RASTER/asc/", "grid_all_richness_scaled"), format='ascii', overwrite=T)


#### png
png(paste0(dir_export, "/RICHNESS_RASTER/png/grid_richness_SDM.png"), height=my_height, width=my_width)
plot(grid_richness_SDM); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/RICHNESS_RASTER/png/grid_richness_HICs.png"), height=my_height, width=my_width)
plot(grid_richness_HICs); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/RICHNESS_RASTER/png/grid_richness_no_SDM.png"), height=my_height, width=my_width)
plot(grid_richness_no_SDM); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/RICHNESS_RASTER/png/grid_richness_no_SDM_conserv_status.png"), height=my_height, width=my_width)
plot(grid_richness_no_SDM); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/RICHNESS_RASTER/png/grid_all_richness.png"), height=my_height, width=my_width)
plot(grid_all_richness); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/RICHNESS_RASTER/png/grid_all_richness_scaled.png"), height=my_height, width=my_width)
plot(grid_all_richness_scaled); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

## Create dataframes ----------------------------------------------------------
### Evaluation metrics: AUC
# AUC values should only be considered as relative indicators of performance (e.g. for the same dataset of a given species)
# Trabajando sin ausencias reales hace que el AUC no sea apropiado para comparar ENTRE especies, pero sí nos da una idea de la calidad 
# del modelo. Otro problema es que pondera por igual los errores de comisión (ausencia 
# predicha como presencia, que no tiene por qué ser un error) y de omisión 
# (presencia predicha como ausencia, que SÍ es un errorazo).
# Aun con estas críticas, el AUC es un estadístico muy útil.

#### LQT
eval_metrics_LQT_df <- data.frame(auc.val.avg=auc.val.avgLQT, auc.diff.avg=auc.diff.avgLQT) # parameters of AUC

eval_metrics_LQT_df <- round(eval_metrics_LQT_df, 3)
rownames(eval_metrics_LQT_df) <- names(list_species)

### Variables Importance/Contribution
#### LQT
varImp_LQT_df <- round(varImp_LQT_df, 3)

## Export Excel  ----------------------------------------------------------

OUT <- createWorkbook() # Create a blank workbook

addWorksheet(OUT, "eval_metrics_LQT") # Add some sheets to the workbook
addWorksheet(OUT, "var_contri_LQT") # Add some sheets to the workbook

writeData(OUT, sheet = "eval_metrics_LQT", x = eval_metrics_LQT_df, rowNames = T) # Write the data to the sheets
writeData(OUT, sheet = "var_contri_LQT", x = varImp_LQT_df, rowNames = T) # Write the data to the sheets

saveWorkbook(OUT, paste0(dir_export,"/taula_model_", Sys.Date(), ".xlsx"), overwrite=T) # Export the file

## Export environment  ----------------------------------------------------------

save.image(paste0("Z:/R/Pla_Proteccio_Montseny/environment_SDM", Sys.Date(), ".RData"))



## Create distributions that are not SDM but OBS ----

#Open filenames for species at 100m, 500m and 1km, from script 1.4
filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/asc/1x1km"), pattern="*.asc", full.names=T)
filenames <- filenames[-grep("00", filenames)] #Delete names that are all_features
llista1km <- pblapply(filenames, raster)
filenames_short <- filenames %>% basename %>% file_path_sans_ext
names(llista1km) <- filenames_short

filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/asc/500x500m"), pattern="*.asc", full.names=T)
filenames <- filenames[-grep("all_species", filenames)] #Delete names that are all_features
llista500m <- pblapply(filenames, raster)
filenames_short <- filenames %>% basename %>% file_path_sans_ext
names(llista500m) <- filenames_short

filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/asc/100x100m"), pattern="*.asc", full.names=T)
filenames <- filenames[-grep("all_species", filenames)] #Delete names that are all_features
llista100m <- pblapply(filenames, raster)
filenames_short <- filenames %>% basename %>% file_path_sans_ext
names(llista100m) <- filenames_short

llista_tot<-c(llista100m, llista500m, llista1km) #ajuntem les llistes, amb les dades de més concretes a menys. 
first_occurrences <- !duplicated(names(llista_tot)) # seleccionem la primera vegada que apareix cada nom. 
llista_tot <- llista_tot[first_occurrences] #Ens petem les aparicions que no són la primera (100m>500m>1km)

noms_abans<-names(llista_tot)

#Remove features that are not inside the PN Montseny:
raster_masked_list <- pblapply(llista_tot, function(x) { #emmascarem els rasters amb la forma del PN
  r_masked <- mask(x, PU_conques) #AQUEST PU CONQUES HA DE SER JA EL RETALLAT AL PN
  return(r_masked)
})

rasters_with_1_idx <- which(sapply(raster_masked_list, function(x) any(getValues(x) == 1)))
llista_tot <- raster_masked_list[rasters_with_1_idx]
noms_despres<-names(llista_tot)

no_presents <- setdiff(noms_abans, noms_despres) #Names of the species that are not inside the PN. This will be used in 2.4, to fill the table. 

llista_PU<-list()

#Here we create, from the occupied pixels, rasters with 0 in the subconques that are not occupied by the feature, and 1 in those with presence of the feature.
for (i in 1:length(llista_tot)) {
  extret<- raster::extract(llista_tot[[i]], PU_conques, fun=sum, sp=T) #S'afegeix un camp de 1 i 0 a PU_conques amb 0 i 1 segons pres/abs
  extret@data[,18] <- ifelse(is.na(extret@data[,18]), 0, extret@data[,18])
  prova <- extret[extret@data[,18]!= 0,] #seleccionem el que no sigui 0
  llista_PU[[i]]<- prova #Ho portem a una llista resultat
  llista_PU[[i]]<- raster::rasterize(prova, grid_100m_r, field =extret@data[,18] ) #I rasteritzem la ultima columna
  llista_PU[[i]][llista_PU[[i]] == 0] <- 1 # com que tot es 0 i NODATA, reclassifiquem 0 a 1 i NODATA a 0
  llista_PU[[i]][is.na(llista_PU[[i]])] <- 0
  writeRaster(llista_PU[[i]], paste0(dir_export,"/PREDICTION_RASTER/asc/TOT/",noms_despres[i],".asc"), format='ascii', overwrite=T)
}
