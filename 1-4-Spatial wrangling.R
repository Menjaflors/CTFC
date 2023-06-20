
#### 1.4. RASTERS WRANGLING ####

## Meta  ----------------------------------------------------------

# https://www.worldclim.org/data/bioclim.html --> website that explains the bioclim variables

# https://rspatial.org/raster/sdm/index.htm
# https://rspatial.org/raster/spatial/6-crs.html --> to understand projections

## Initial code  ----------------------------------------------------------

### Delete all objects

not_rm <- c("list_species", "list_liquens_100m", "list_species_100m", "list_species_500m", "list_species_1km") # concatenation of object names to remain
rm(list=ls()[! ls() %in% not_rm])

### Instal packages in case they are not installed

list.of.packages <- c("tidyverse", "readr", "rgdal", "sp", "raster", "sf", "rgeos", "readxl", "DescTools",
                      "Matrix", "openxlsx", "maptools", "PBSmapping", "pbapply", "tools", "climateStability",
                      "prioritizr", "spdep", "nngeo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}

### Open libraries

invisible(lapply(list.of.packages, library, character.only = TRUE, logical.return=T))
rm(list.of.packages, new.packages) # remove objects we won't need anymore.

### Set directories

dir<-"C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY"
dir_ambit_PN <- paste0(dir, "/SIMSY/07_PER_INCORPORAR_AL_SIMSY/ESPAISNATURALS_ENPE/ESPAISNATURALS_ENPE.shp") # directory ambit layer
dir_ambit_RB <- paste0(dir, "/RAW/AMBIT/MAB.shp") # directory ambit layer
dir_simsy <- paste0(dir, "/SIMSY") # directory simsy
dir_hidro <- paste0(dir, "/SIMSY/03_INFORMACIO_BASE/32_MEDI_NATURAL/32f_HIDROLOGIA") # directory ambit layer
dir_aus <- paste0(dir_simsy, "/XXSeguretat/Fauna/Vertebrats/Aus") # directory ambit layer
dir_info_base <- paste0(dir_simsy, "/03_INFORMACIO_BASE")
dir_raw <- paste0(dir, "/RAW")
dir_processed <- paste0(dir, "/PROCESSED") # directory processed
dir_export <- paste0(dir, "/EXPORT")

### Open files

#### Ambits
ambit_PN <- shapefile(paste0(dir, "/SIMSY/INFO_SONIA_141022/LIMIT_PARC_DECRET_21/LIMIT_2021_v2.shp"))  # G: official EBBA2 countries and masks
ambit_RB <- shapefile(paste0(dir, "/SIMSY/INFO_SONIA_141022/LimitsRB/DelimitacioShpED50RBM13_ETRS89.shp")) # G: official EBBA2 countries and masks

#### Climate
filenames <- list.files(paste0(dir_processed, "/ACDC"), pattern="*tif", full.names=TRUE) # import as asci
list_climate <- pblapply(filenames, raster)
filenames_short <- filenames %>% basename %>% file_path_sans_ext
names(list_climate) <- filenames_short

#### Grid
grid_1km <- shapefile(paste0(dir_export, "/GRIDS/grid_1x1km_modified.shp"))

#### Ponds and rivers
ponds <- shapefile(paste0(dir_hidro, "/Basses/InventariBasses/InventariBassesMSY11v2_ETRS89.shp"))
rivers <- shapefile(paste0(dir_processed, "/HIDRO/xarxa_hidro_nivells2-6_processat.shp"))

#### Boscos madurs
bosc_madur <- shapefile(paste0(dir_raw, "/BOSCOS_MADURS/Boscos_madurs_JSA_1.shp"))

#### Habitats
HIC <- shapefile(paste0(dir_export, "/HABITATS_SHP/habitats_HIC_extent.shp"))
habitats <- shapefile(paste0(dir_export, "/HABITATS_SHP/habitats_cat_extent.shp"))
habitats_cat <- read_excel(paste0(dir, "/Documents/Dades_projecte.xlsx"), sheet = "Predictors") %>% 
  as.data.frame()

#### Hidrology
subsubconques <- shapefile(paste0(dir_processed, "/PUs/PU_conques.shp")) # we will use the subsubconques as polygon shapefile Planning Unit
# subconques <- shapefile(paste0(dir_processed, "/HIDRO/subconques_per_a_grans_conques.shp")) # DO NOT DELETE THIS! I don't open it because it is a heavy file
hidro_network <- shapefile(paste0(dir_processed, "/HIDRO/xarxa_hidro_bona.shp"))

#### MDTs
r_elevacio <- raster(paste0(dir, "/RAW/ICGC/MDT/mdt_5x5.tif"))
r_orient <- raster(paste0(dir, "/RAW/ICGC/MDT/aspect_5x5.tif"))
r_pendent <- raster(paste0(dir, "/RAW/ICGC/MDT/slope_5x5.tif"))
r_ombra <- raster(paste0(dir, "/RAW/ICGC/MDT/hillshade_5x5.tif"))
r_south <- raster(paste0(dir, "/RAW/ICGC/MDT/southness_5x5.tif"))

list_mdts <- list(r_elevacio, r_orient, r_pendent, r_ombra, r_south)
names(list_mdts) <- c("r_elevacio", "r_orient", "r_pendent", "r_ombra", "r_south")
rm(r_elevacio, r_orient, r_pendent, r_ombra, r_south)

#### Estructura forestal
filenames <- list.files(paste0(dir_processed, "/ESTRUCTURA_FOREST"), pattern="*.tif", full.names=T)
list_estruc <- pblapply(filenames, raster)
filenames_short <- filenames %>% basename %>% file_path_sans_ext
names(list_estruc) <- filenames_short

rm(filenames, filenames_short)

#### Flora amenaçada
flora_amen <- shapefile(paste0(dir_processed, "/AIFF/flora_amenacada.shp"))

#### Nius

nius_09 <- shapefile(paste0(dir_aus, "/Aus2009/Rapinyaires2009/CartografiaRapinyairesMSY09/RapinyairesNiusMSY09_ETRS89.shp"))
nius_10 <- shapefile(paste0(dir_aus, "/Aus2010/Rapinyaires2010/CartografiaRapinyairesMSY10/RapinyairesEstatNiusMSY10_ETRS89.shp"))
nius_11 <- shapefile(paste0(dir_aus, "/Aus2011/Rapinyaires2011/CartografiaRapinyairesMSY11/Nius_estat_2011_ETRS89.shp"))
nius_12 <- shapefile(paste0(dir_aus, "/Aus2012/Rapinyaires2012/CartografiaRapinyairesMSY12/Nius_estat_2012_ETRS89.shp"))
nius_13 <- shapefile(paste0(dir_aus, "/Aus2013/Rapinyaires2013/Nius_estat_2013_ETRS89.shp"))
nius_14 <- shapefile(paste0(dir_aus, "/Aus2014/Rapinyaires_2014/Nius_estat_2014_ETRS89.shp"))
nius_15 <- shapefile(paste0(dir_aus, "/Aus2015/Rapinyaires_2015/Nius_estat_2015_ETRS89.shp"))
nius_16 <- shapefile(paste0(dir_aus, "/Aus2016/Rapinyaires_2016/Nius_estat_2016_ETRS89.shp"))
nius_17 <- shapefile(paste0(dir_aus, "/Aus2017/NiusOcells2017_18.shp"))
nius_18 <- shapefile(paste0(dir_aus, "/Aus2018/Rapinyaires_2018/Nius_estat_2018.shp"))
nius_21 <- shapefile(paste0(dir_aus, "/Aus2021/Rapinyaires_2021/Nius_estat_2021.shp"))
nius_22 <- shapefile(paste0(dir_aus, "/Aus2021_22/Nius_estat_2021_22v1.shp"))

nius_09$Any <- "2009"
nius_10$Any <- "2010"
nius_11$Any <- "2011"
nius_12$Any <- "2012"
nius_13$Any <- "2013"
nius_14$Any <- "2014"
nius_15$Any <- "2015"
nius_16$Any <- "2016"
nius_17$Any <- "2017"
colnames(nius_17@data)[colnames(nius_17@data) == 'NOM_COMU'] <- 'Especie' # this one has different column name for 'Especie'
nius_18$Any <- "2018"
nius_21$Any <- "2021"
nius_22$Any <- "2022"

list_nius <- mget(ls(pattern = "nius_"))
rm(list=ls(pattern = "nius_"))

##### Nius dataframe

nius_df <- read_excel(paste0(dir, "/Documents/taula_nius.xlsx"), sheet = "taula_nius") %>% as.data.frame()

#### Costs
# Titularitat pública
cost_public <- shapefile(paste0(dir_info_base, "/31_FINQUES/FINQUES_PUBLIQUES/SHP/2020/finques_publiques_msy_POL.shp"))

# Sup. alta freqüentació
cost_alta_freq <- shapefile(paste0(dir_info_base, "/37_US_PUBLIC/ACTIVITATS_US_PUBLIC/Frequentacio/ZonesElevadaFreqMSY08_ETRS89.shp"))

# Gestió forestal
cost_gest_forest <- shapefile(paste0(dir_simsy, "/NOVA_INFO_SONIA/PTGMF_2020/SHP/INCENDIS_CPF_CANVI_US_201231.shp")) # capa de canvi d'usos del sol
cost_gest_forest$DESC_CU_NUM <- as.numeric(as.factor(cost_gest_forest$DESC_CU)) 

# Carreteres (i ferrocarrils?) 
ferrocarrils <- shapefile(paste0(dir_info_base, "/34_INFRASTRUCTURES_SERVEIS/XARXA_TRANSPORT PUBLIC/FerrocarrilMSY08_ETRS89.shp"))
tgv <- shapefile(paste0(dir_info_base, "/34_INFRASTRUCTURES_SERVEIS/XARXA_TRANSPORT PUBLIC/TGVMSY08_ETRS89.shp"))
carreteres <- shapefile(paste0(dir_info_base, "/34_INFRASTRUCTURES_SERVEIS/XARXA_VIARIA/Shape/CarreteresMSY08_ETRS89.shp"))

# Zones urbanes
zones_urbanes <- shapefile(paste0(dir_raw, "/AMBIT/ZonificacioPLA_ESPECIAL08/ZONIFICACIOMSY08_ETRS89.shp")) # isolate the urban areas
zones_urbanes$ID_ZONIFIC_NUM <- as.numeric(as.factor(zones_urbanes$ID_ZONIFIC)) 
zones_urbanes$id <- 1:nrow(zones_urbanes)
zones_urbanes <- zones_urbanes[zones_urbanes$ID_ZONIFIC == "URBA", "id"] # we just want the urban areas and the column 'id' to then merge

nucli_pob <- shapefile(paste0(dir_raw, "/NUCLIS_POBLACIO/NuclisPoblacio_ETRS89.shp")) # population settlements
nucli_pob$id <- 1:nrow(nucli_pob)
nucli_pob <- nucli_pob[, "id"]



# Join costs inside list
list_costs <- list(cost_public, cost_alta_freq, cost_gest_forest, ferrocarrils, tgv, carreteres, nucli_pob, zones_urbanes) 

names(list_costs) <- c("cost_public", "cost_alta_freq", "cost_gest_forest", "ferrocarrils", "tgv", "carreteres", "nucli_pob", "zones_urbanes")


### Cobertes del sol
# cobertes_sol_r <- raster(paste0(dir_processed, "/COBERTES_SOL/cobertes_sol_2017_MSY.tif"))

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

grid_1km <- fun_reproject(grid_1km)
ponds <- fun_reproject(ponds)
bosc_madur <- fun_reproject(bosc_madur)
HIC <- fun_reproject(HIC)
habitats <- fun_reproject(habitats)
subsubconques <- fun_reproject(subsubconques)
subconques <- fun_reproject(subconques)
rivers <- fun_reproject(rivers)
flora_amen <- fun_reproject(flora_amen)

ambit_RB <- spTransform(ambit_RB, proj_EPSG25831) # It doesn't work with ambit_RB so I do it without the function

#### Set crs rasters
crs_fun <- function(x) { crs(x) <- proj_EPSG25831; return(x) }

list_mdts <- lapply(list_mdts, crs_fun)
list_estruc <- lapply(list_estruc, crs_fun)
list_climate <- lapply(list_climate, crs_fun)
list_costs <- lapply(list_costs, crs_fun)
list_nius <- lapply(list_nius, crs_fun)

### Check all objects have the same projections

AllIdentical(proj_EPSG25831, crs(grid_1km), crs(ponds), crs(bosc_madur), crs(HIC), crs(habitats), crs(subsubconques), crs(subconques), crs(ambit_RB),
             crs(list_mdts[[1]]), crs(list_estruc[[1]]), crs(list_costs[[1]]), crs(rivers), crs(flora_amen))

## Create grids (resolution and conques)  ----------------------------------------------------------

### Ambit
ambit_RB_b <- gBuffer(ambit_RB, width=1000) # widht is in meters because we are using utms

### Create raster grid

grid_1km_r <- raster(extent(grid_1km), nrow=36, ncol=41, crs = crs(grid_1km)) # we know 'nrow' and 'ncol' counting manually           
grid_1km_r[] <- runif(ncell(grid_1km_r), min=0, max=10)
res(grid_1km_r) <- 1000
grid_1km_r <- mask(grid_1km_r, mask=ambit_RB_b) # mask the grid on the ambit
grid_1km_r[grid_1km_r>0] <- 1
print(grid_1km_r)

### Create 500x500m grid

grid_500m_r <- raster(extent(grid_1km), nrow=36*2, ncol=41*2, crs = crs(grid_1km)) # we know 'nrow' and 'ncol' counting manually           
grid_500m_r[] <- runif(ncell(grid_500m_r), min=0, max=10)
res(grid_500m_r) <- 500
grid_500m_r <- mask(grid_500m_r, mask=ambit_RB_b) # mask the grid on the ambit
grid_500m_r[grid_500m_r>0] <- 1
print(grid_500m_r)

### Create 100x100m grid

grid_100m_r <- raster(extent(grid_1km), nrow=36*10, ncol=41*10, crs = crs(grid_1km)) # we know 'nrow' and 'ncol' counting manually           
grid_100m_r[] <- runif(ncell(grid_100m_r), min=0, max=10)
res(grid_100m_r) <- 100
grid_100m_r <- mask(grid_100m_r, mask=ambit_RB_b) # mask the grid on the ambit
grid_100m_r[grid_100m_r>0] <- 1
print(grid_100m_r)

### Create 25x25m grid

grid_25m_r <- raster(extent(grid_1km), nrow=36*10*4, ncol=41*10*4, crs = crs(grid_1km)) # we know 'nrow' and 'ncol' counting manually           
grid_25m_r[] <- runif(ncell(grid_25m_r), min=0, max=10)
res(grid_25m_r) <- 25
grid_25m_r <- mask(grid_25m_r, mask=ambit_RB_b) # mask the grid on the ambit
grid_25m_r[grid_25m_r>0] <- 1
print(grid_25m_r)

### Create subsubconques grid

subsubconques$ID_GRAFIC_FACTOR <- as.factor(as.character(subsubconques$ID_GRAFIC))
subsubconques_r <- rasterize(subsubconques, grid_100m_r, field="ID_GRAFIC_FACTOR")

### Create subconques grid (to use as mask in SDM)
v <- c("050", "051", "052", "060", "061", "062", "106", "109", "160") # subconques present inside ambit_RB_b
subconques <- subconques[subconques@data$CODI_SUBCO %in% v, ]
subconques$CODI_SUBCO <- as.factor(as.character(subconques$CODI_SUBCO))
levels(subconques$CODI_SUBCO)[c(7,9)] <- "106_160" # we join two subconques into one (Dani advice)
subconques_r <- rasterize(subconques, grid_100m_r, field="CODI_SUBCO") # create raster
subconques_r <- mask(subconques_r, mask=ambit_RB_b) # mask the grid on the ambit

## Create costs ----------------------------------------------------------

### Fix things on costs list
list_costs <- pblapply(list_costs, function(x){ x[ambit_RB_b,] }) # clip

list_costs[["tgv"]] <- list_costs[["tgv"]][, "TIPUS"]
colnames(list_costs[["carreteres"]]@data) <- "TIPUS"
list_costs[["transport"]] <- do.call(rbind, list_costs[c("ferrocarrils", "tgv", "carreteres")])                         
list_costs[c("ferrocarrils", "tgv", "carreteres")] <- NULL

list_costs[["cost_urba"]] <- do.call(rbind, list_costs[c("nucli_pob", "zones_urbanes")])                         

### Create list_constraints for further use
list_constraints <- list_costs # before creating rasters we create another identical list for building the constraints raster

### Create the function to find the log distance
fun_log_dist <- function(x) {
  
  y <- grid_100m_r
  dist_v <-  gDistance(list_costs[[x]], as(y, "SpatialPoints"), byid=T) # finds vector with the distance (in meters) to the closest transport line
  y[(values(y) == 1)] <- apply(dist_v, 1, min) # add distaces to raster pixels
  y[values(y) < 1] <- 1
  y <- log(y) # we will use natural logarithm (neperià) because it doesn't change the output, due to the rescaling on the following lines.
  y <- ((y - maxValue(y)) * -1) + minValue(y) # values inversed
  y <- (y - cellStats(y, "min"))/(cellStats(y, "max") - cellStats(y, "min")) # values scaled to 0-1
  function(r){ ((r-cellStats(r,"min"))/(cellStats(r,"max")-cellStats(r,"min"))) }
  return(y)
}

### Gestió forestal
cost_gest_forest_r <- fun_log_dist("cost_gest_forest")

### Titularitat pública
cost_public_r <- rasterize(list_costs[["cost_public"]], grid_100m_r)                       
cost_public_r[cost_public_r >= 1] <- 0.5 # this layer weights (+) the final cost layer 
cost_public_r[is.na(cost_public_r)] <- 2  # this layer weights (-) the final cost layer 

### Zones urbanes
cost_urba_r <- fun_log_dist("cost_urba")

### Sup. alta freqüentació
r_dist_alt_freq <- fun_log_dist("cost_alta_freq")

### Carreteres (i ferrocarrils?) 
r_dist_transport <- fun_log_dist("transport")

### Join all into one layer
stack_costs <- stack(cost_gest_forest_r, cost_urba_r, r_dist_alt_freq, r_dist_transport)
grid_all_costs <- calc(stack_costs, sum)

grid_all_costs <- (grid_all_costs - cellStats(grid_all_costs, "min")) / (cellStats(grid_all_costs, "max") - cellStats(grid_all_costs, "min"))
plot(grid_all_costs)

grid_all_costs_public <- grid_all_costs * cost_public_r # to weight the stack
plot(grid_all_costs_public)


## Create lon and lat grids  ----------------------------------------------------------

### Create raster grid
r <- grid_100m_r
r[] <- 0

### Make Raster gradient for longitude
grid_lon_100m_r <- as.matrix(r)
grid_lon_100m_r <- apply(grid_lon_100m_r, 1, function(x) seq(xmin(r)+res(r)[1], xmax(r), res(r)[1]))
grid_lon_100m_r <- raster(t(grid_lon_100m_r), xmn = xmin(r), xmx = xmax(r), ymn = ymin(r), ymx = ymax(r))
res(grid_lon_100m_r) <- res(r)
crs(grid_lon_100m_r) <- proj_EPSG25831

### Make Raster gradient for latitude
grid_lat_100m_r <- as.matrix(r)
grid_lat_100m_r <- apply(grid_lat_100m_r, 2, function(x) rev(seq(ymin(r)+ res(r)[2], ymax(r), res(r)[2]))) 
grid_lat_100m_r <- raster(grid_lat_100m_r, xmn = xmin(r), xmx = xmax(r), ymn = ymin(r), ymx = ymax(r))
res(grid_lat_100m_r) <- res(r)
crs(grid_lat_100m_r) <- proj_EPSG25831

rm(r)

### Transform to 500x500m
grid_lon_500m_r <- aggregate(grid_lon_100m_r, fact=5, fun=mean)
grid_lat_500m_r <- aggregate(grid_lat_100m_r, fact=5, fun=mean)

### Transform to 1x1km
grid_lon_1km_r <- aggregate(grid_lon_100m_r, fact=10, fun=mean)
grid_lat_1km_r <- aggregate(grid_lat_100m_r, fact=10, fun=mean)

## Create presence species and subconques on Planning Units  ----------------------------------------------------------

### Create functions
fun_point_presence <- function(r, pts){
  r2 <- r 
  r2[] <- 0 # make the inout raster of zeroes
  counts <- table(cellFromXY(r,pts)) # get the number of points inside each cell
  counts[] <- 1 # change all elements to 1, as presences.
  r2[as.numeric(names(counts))] <- counts # fill in the raster with the counts from the number of points inside each cell
  return(r2)
} # Point presence

fun_point_presence_subsubconca <- function(r, pts){
  r2 <- r 
  #r2[] <- 0 # make the inout raster of zeroes
  counts <- as.numeric(names(table(cellFromXY(r,pts)))) # get the number of points inside each cell
  z <- unique(r2[counts])
  z <- z[!is.na(z)]
  r2[!r2@data@values %in% z] <- 0
  r2[r2@data@values %in% z] <- 1
  return(r2)
} # Point presence on subsubconques layer

fun_point_presence_subsubconca_vector <- function(pts, pol){
  
  pol2 <- pol
  x <- over(pts, pol2)
  pol2[pol2$ID_GRAFIC %in% unique(x$ID_GRAFIC), "presence"] <- 1
  return(pol2)
}

### Liquens 100m
list_grid_presence_liquens_100m <- vector(mode = "list", length=length(list_liquens_100m))

for (i in 1:length(list_liquens_100m)) {
  print(paste(i, "of", length(list_liquens_100m)))
  list_grid_presence_liquens_100m[[i]] <- fun_point_presence(grid_100m_r, list_liquens_100m[[i]])
}

grid_all_presence_100m <- do.call("sum", list_grid_presence_liquens_100m)

### 100 x 100m
list_grid_presence_100m <- vector(mode = "list", length=length(list_species_100m))

for (i in 1:length(list_species_100m)) {
  print(paste(i, "of", length(list_species_100m)))
  list_grid_presence_100m[[i]] <- fun_point_presence(grid_100m_r, list_species_100m[[i]])
}

grid_all_presence_100m <- do.call("sum", list_grid_presence_100m)

### 500 x 500m
list_grid_presence_500m <- vector(mode = "list", length=length(list_species_500m))

for (i in 1:length(list_species_500m)) {
  print(paste(i, "of", length(list_species_500m)))
  list_grid_presence_500m[[i]] <- fun_point_presence(grid_500m_r, list_species_500m[[i]])
}

grid_all_presence_500m <- do.call("sum", list_grid_presence_500m)

### 1 x 1km
list_grid_presence_1km <- vector(mode = "list", length=length(list_species_1km))

for (i in 1:length(list_species_1km)) {
  print(paste(i, "of", length(list_species_1km)))
  list_grid_presence_1km[[i]] <- fun_point_presence(grid_1km_r, list_species_1km[[i]])
}

grid_all_presence_1km <- do.call("sum", list_grid_presence_1km)

### subsubconques
list_grid_presence_subsubconca <- vector(mode = "list", length=2)
list_vector_presence_subsubconca <- vector(mode = "list", length=2)

for (i in 1:length(list_species_500m)) {
  print(paste(i, "of", length(list_species_500m)))
  list_grid_presence_subsubconca[[1]][[i]] <- fun_point_presence_subsubconca(subsubconques_r, list_species_500m[[i]])
  list_vector_presence_subsubconca[[1]][[i]] <- fun_point_presence_subsubconca_vector(list_species_500m[[i]], subsubconques)
}

for (i in 1:length(list_species_100m)) {
  print(paste(i, "of", length(list_species_100m)))
  list_grid_presence_subsubconca[[2]][[i]] <- fun_point_presence_subsubconca(subsubconques_r, list_species_100m[[i]])
  list_vector_presence_subsubconca[[2]][[i]] <- fun_point_presence_subsubconca_vector(list_species_100m[[i]], subsubconques)
}

list_grid_presence_subsubconca_unlisted <- unlist(list_grid_presence_subsubconca) #we first unlist because if not there is a bug in the next line 
grid_all_presence_subsubconca <- do.call("sum", list_grid_presence_subsubconca_unlisted)
names(list_grid_presence_subsubconca[[1]]) <- names(list_species_500m)
names(list_grid_presence_subsubconca[[2]]) <- names(list_species_100m)
list_grid_presence_subsubconca <- unlist(list_grid_presence_subsubconca) # we do it again but now properly 

list_vector_presence_subsubconca <- unlist(list_vector_presence_subsubconca) #we unlist the vectors
names(list_vector_presence_subsubconca) <- names(list_grid_presence_subsubconca)
rm(list_grid_presence_subsubconca_unlisted) # we do not need it anymore

### subconques
list_grid_mask_subconca <- vector(mode = "list", length=2)

for (i in 1:length(list_species_100m)) {
  print(paste(i, "of", length(list_species_100m)))
  list_grid_mask_subconca[[1]][[i]] <- fun_point_presence_subsubconca(subconques_r, list_species_100m[[i]])
}

for (i in 1:length(list_species_500m)) {
  print(paste(i, "of", length(list_species_500m)))
  list_grid_mask_subconca[[2]][[i]] <- fun_point_presence_subsubconca(subconques_r, list_species_500m[[i]])
}

list_grid_mask_subconca_unlisted <- unlist(list_grid_mask_subconca) #we first unlist because if not there is a bug in the next line 
names(list_grid_mask_subconca[[1]]) <- names(list_species_100m)
names(list_grid_mask_subconca[[2]]) <- names(list_species_500m)
list_grid_mask_subconca <- unlist(list_grid_mask_subconca) # we do it again but now properly 
rm(list_grid_mask_subconca_unlisted) # we do not need it anymore

## Rasterize 'flora amenaçada'  ----------------------------------------------------------

### Split shp by species
list_flora_amen <- split(flora_amen, flora_amen@data$ESPECIE) # we create a list where each element is a species

### Rasterize to 25m
list_flora_amen <- pblapply(list_flora_amen, rasterize, y=grid_25m_r)

### Transform values
list_flora_amen <- pblapply(list_flora_amen, function(x) { x[is.na(x)] <- 0; return(x) }) # non-zero values to 1
list_flora_amen <- pblapply(list_flora_amen, function(x) { x[x>0] <- 1; return(x) }) # non-zero values to 1

### Aggregate to 100m
list_flora_amen <- pblapply(list_flora_amen, aggregate, fact=4, fun=mean)

## Create ponds and river raster  ----------------------------------------------------------

### Ponds
fun_point_presence <- function(r, pts){
  r2 <- r 
  r2[] <- 0 # make the inout raster of zeroes
  counts <- table(cellFromXY(r,pts)) # get the number of points inside each cell
  counts[] <- 1 # change all elements to 1, as presences.
  r2[as.numeric(names(counts))] <- counts # fill in the raster with the counts from the number of points inside each cell
  return(r2)
} # Point presence

r_ponds  <- fun_point_presence(grid_100m_r, ponds)
m <- matrix(0.1, 3, 3); m[2,2] <- 1 # matrix to modify the pixels adjacent to pixels with some values.
r_ponds <- focal(r_ponds, w=m, mean)
r_ponds <- rescale0to1(r_ponds) # rescale values of raster to 0-1
plot(r_ponds)

### Rivers

rivers@data <- rivers@data["NUMORDRE"] 

#### This part is done because it occupy to much memory

# r_dist_rivers_100m <- grid_100m_r
# r_dist_rivers_100m[] <- 0
# dist_v <-  gDistance(rivers, as(r_dist_rivers_100m, "SpatialPoints"), byid=T) # finds vector with the distance (in meters) to the closer river
# r_dist_rivers_100m[] = apply(dist_v,1,min) # add distaces to raster pixels
# rm(dist_v)

#### We do this instead
r_dist_rivers_100m <- raster(paste0(dir_export, "/HIDRO_RASTER/asc/r_dist_rivers_100m.asc")) # do it if because the "Error: cannot allocate vector of size 3.0 Gb"
r_dist_rivers_500m <- aggregate(r_dist_rivers_100m, fact=5, fun=mean)

## Create Boscos Madurs raster  ----------------------------------------------------------
r_bosc_madur  <- rasterize(bosc_madur, grid_100m_r)
r_bosc_madur[r_bosc_madur@data@values>1] <- 1
plot(r_bosc_madur)

## Find aquatic and terrestrial species  ----------------------------------------------------------

fun_hidro_coef <- function(r2, r1) {
  r2[r2==0] <- NA
  r4 = prod(r2,r1,na.rm=F) # the multiplication prod() acts as a mask to filter only the pixels where the species is present.
  freq_m <- terra::freq(r4, digits=1) # to find the frequency of terrestrial pixels (value of 1) and aquatic ones (value of 2)
  freq_m <- freq_m[!is.na(freq_m[,1]),]
  
  if(class(freq_m)[1] == "numeric") { waterness <- freq_m[1] } # for cases that there is only one category
  if(class(freq_m)[1] == "matrix") { waterness <- sum(freq_m[,1]*freq_m[,2])/sum(freq_m[,2]*maxValue(r1)) } # we multiply the value and number of pixels of each value. Then we divide the result for the total number of pixes multiplied by its maximum value
  
  return(waterness)
}

fun_hidro_coef_II <- function(x, r) {
  xy <- x@coords
  v_extract <- extract(r, xy)
  waterness <- mean(v_extract, na.rm=T)
  return(waterness)
}

### 100x100m 

list_hidro_coef_100m <- pblapply(list_species_100m, fun_hidro_coef_II, r=r_dist_rivers_100m) # creates list with the coefficient of waterness for each species
v_hidro_coef_100m <- sapply(list_hidro_coef_100m, function(x) {round(x[], 2)}) # transform list to vector

### 500x500m

list_hidro_coef_500m <- pblapply(list_species_500m, fun_hidro_coef_II, r=r_dist_rivers_100m) # creates list with the coefficient of waterness for each species
v_hidro_coef_500m <- sapply(list_hidro_coef_500m, function(x) {round(x[], 2)}) # transform list to vector

### 1x1km


## Create HICs  ----------------------------------------------------------

### Mask HICs shapefile to ambit_RB_b
HIC <- raster::intersect(HIC, ambit_RB_b)

### Separate HIC categories
cols2split <- c("HIC1", "HIC2", "HIC3", "HIC4", "HIC5", "HIC6", "HIC7", "HIC8", "HIC9", "HIC10") # columns to use for splitting

list_HIC_cat <- vector(mode="list")

for (i in 1:length(cols2split)) {
  x <- split(HIC, HIC@data[[cols2split[i]]])
  list_HIC_cat <- append(list_HIC_cat, list(x))
}

names(list_HIC_cat) <- cols2split

### Delete empty elements of the list
filter <- rep(NA, length(list_HIC_cat))
for (i in 1:length(list_HIC_cat)) { if(length(list_HIC_cat[[i]]) == 0) filter[i] <- 1 }
list_HIC_cat <-list_HIC_cat[is.na(filter)]
rm(filter)

### Delete non-necessary columns
fun_extract_cols <- function(y,i) {  z <- y@data
filter <- endsWith(colnames(z), cols2split[i])
z <- z[, filter]
y@data <- z
return(y) }

for (i in 1:length(list_HIC_cat)) { list_HIC_cat[[i]] <- pblapply(list_HIC_cat[[i]], fun_extract_cols, i=i) }

### Unlist and Change column names
list_HIC <- unlist(list_HIC_cat)
list_HIC <- pblapply(list_HIC, function(x) { colnames(x@data) <- substring(colnames(x@data), 1, nchar(colnames(x@data))-1); return(x) })

### Rasterize elements to 25x25 m
list_HIC_raster <- pblapply(list_HIC, rasterize, y=grid_25m_r, field="RHIC")
list_HIC_raster <- pblapply(list_HIC_raster, function(x) { x[is.na(x)] <- 0; return(x) })

### Join rasters of the same categories
list_HIC_raster <- tapply(list_HIC_raster, substr(names(list_HIC_raster), 6, 9), identity) # nest the list to have the same rasters together
list_HIC_raster <- pblapply(list_HIC_raster, function(x) { y <- calc(stack(x), sum); return(y) }) # join by sum

### Aggregate to 100x100 m
list_HIC_raster <- pblapply(list_HIC_raster, aggregate, fact=4, fun=mean)

### Reduce any value > 10 to 10
for (i in 1:length(list_HIC_raster)) { print(list_HIC_raster[[i]]@data@max) } # there is no value bigger than 10

### Add r_ponds as HIC too
list_HIC_raster[["r_ponds"]] <- r_ponds

## Create habitats as predictors  ----------------------------------------------------------

### Clip habitat shapefile to the ambit

habitats <- raster::intersect(habitats, ambit_RB_b)

### Create habitat categories dataframe

habitats_cat <- habitats_cat[habitats_cat$tema == "Habitat", c("codi_predictor", "observacions")]

hab_cat_list <- vector(mode = "list", length = nrow(habitats_cat))

for (i in 1:length(hab_cat_list)) { hab_cat_list[i] <- strsplit(habitats_cat[i,2], split = ", ") }

names(hab_cat_list) <- habitats_cat[,1]


hab_cat_df <- data.frame(codi_predictor=rep(names(hab_cat_list), as.vector(pblapply(hab_cat_list, length))), 
                         subcategories=unlist(hab_cat_list, use.names=FALSE))      

hab_cat_df$subcategories <- gsub(",", "", hab_cat_df$subcategories)  

rm(habitats_cat, hab_cat_list)

### Filter only interesting habitats from Abarca et al. 2022
# nothing for now

### Add categories to the spdf

habitats <- merge(habitats, hab_cat_df,  by.x = 'H1', by.y = 'subcategories', all=T)

### Split habitats into list

unique_hab <- unique(habitats$H1) # Select the column of the attribute table that will determine the split of the shp
list_habitats <- vector(mode = "list", length=length(unique_hab)) # create empty list with the length of unique habitats

for (i in 1:length(unique_hab)) { list_habitats[[i]] <- habitats[habitats$H1 == unique_hab[i], ] } # create new elements of the list for each habitat

names(list_habitats) <- unique_hab # add the habitats coding as the name of the list elements.

### Split categories of habitats into list

unique_cat <- unique(habitats$codi_predictor) # Select the column of the attribute table that will determine the split of the shp
unique_cat <- unique_cat[!is.na(unique_cat)]
list_cat_habitats <- vector(mode = "list", length=length(unique_cat)) # create empty list with the length of unique habitats

habitats <- habitats[!is.na(habitats$codi_predictor),] # here we have to remove habitats that has no categories

for (i in 1:length(unique_cat)) { list_cat_habitats[[i]] <- habitats[habitats$codi_predictor == unique_cat[i], ] }# create new elements of the list for each habitat

### Create rasterize function
fun_poly_presence <- function(l, r){
  l2 <- l
  r2 <- rasterize(l2, r)
  r2[!is.na(r2@data@values)] <- 1
  r2[is.na(r2@data@values)] <- 0
  return(r2)
} # polygon presence

#### Habitats
list_grid_habitats_25m <- pblapply(list_habitats, fun_poly_presence, r=grid_25m_r)

#### Categories of habitats
list_grid_cat_habitats_25m <- pblapply(list_cat_habitats, fun_poly_presence, r=grid_25m_r)


## Number of pixels for each habitat and category ----------------------------------------------------------

### Habitats
npixels_hab <- pblapply(list_grid_habitats_25m, cellStats, stat=sum) # total number of pixels of each habitat
npixels_hab <- sapply(npixels_hab, function(x) {x[]}) # transform list to vector
nhabitats <- sapply(list_habitats, function(x) {unique(x@data$H1)}) # transform list to vector
descr_habs <- sapply(list_habitats, function(x) {unique(x@data$text_H1)}) # transform list to vector
npixels_hab_df <- data.frame(habitats=nhabitats, descripcio=descr_habs , npixels=npixels_hab)
hab_df <- merge(hab_cat_df, npixels_hab_df, by.x="subcategories", by.y="habitats", all=T)

hab_df <- hab_df[ , c("codi_predictor", names(hab_df)[names(hab_df) != "codi_predictor"])] # Reorder data frame

## Find covering of habitats  ----------------------------------------------------------

# The following code aggregate the 25m res grid to 100m res. It means that it takes the 16 pixels of 25m res and finds
# the mean to create the 100m res grid.

list_grid_habitats_100m <- pblapply(list_grid_habitats_25m, aggregate, fact=4, fun=mean)
names(list_grid_habitats_100m) <- unique_hab

list_grid_cat_habitats_100m <- pblapply(list_grid_cat_habitats_25m, aggregate, fact=4, fun=mean)
names(list_grid_cat_habitats_100m) <- unique_cat

## Edit the nius layers  ----------------------------------------------------------

list_nius <- pblapply(list_nius, function(x){ x <- x[, c("Any", "Especie", "X", "Y")]; return(x) })
list_nius <- pblapply(list_nius, function(x){ x$Especie <- iconv(x$Especie, from="UTF-8", to="ASCII//TRANSLIT"); return(x) }) # to remove the accents

nius_spdf <- do.call(rbind, list_nius) # bind all the species into one spdf

nius_spdf <- nius_spdf[nius_spdf$X != 0, ] # remove rows without 'x' coordinate
nius_spdf <- nius_spdf[nius_spdf$Y != 0, ] # remove rows without 'y' coordinate
nius_spdf <- nius_spdf[nius_spdf$Y > 4e6, ] # remove rows with unreal 'y' coordinate
nius_spdf <- nius_spdf[!is.na(nius_spdf$Especie), ] # remove observations without the species name

nius_spdf <- merge(nius_spdf, nius_df, by.x="Especie", by.y="nom_comu", all.x=T, all.y=F) # merge the two columns to have the scientific name

nius_spdf@data <- nius_spdf@data[!is.na(nius_spdf$nom_cientific), ]

list_nius <- split(nius_spdf, nius_spdf$nom_cientific) # split the big spdf into separate elements by species name

### Remove repeated rows and create the rep_number column

fun_rep_points <- function(x) {
  
  y <- st_as_sf(x)
  nn <- st_nn(y, y, k = nrow(x), maxdist = 20) # Compute the number of points within 20 meters of each point
  nn <- lapply(nn, sort)
  
  nn_unique <- nn[!duplicated(nn, by = function(x) paste(x, collapse = ","))]
  
  nn_year <- nn_unique
  for (i in 1:length(nn_year)) {
    nn_val <- nn_unique[[i]]
    year_v <- length(unique(x@data[nn_val, "Any"]))
    nn_year[[i]] <- year_v
  }
  
  first_p <- sapply(nn_unique, function(x){x[1]}) # Extract the count of nearest neighbors for each point
  z <- x[first_p, ]
  z$count <- unlist(nn_year)

  return(z)
}

list_nius2 <- pblapply(list_nius, fun_rep_points)

### Presence of nius in the subconques

list_nius_subconques <- pblapply(list_nius2, fun_point_presence_subsubconca_vector, subsubconques)

## Edit the MDT layers  ----------------------------------------------------------

list_mdts_res <- pblapply(list_mdts, resample, y=grid_100m_r) # resample to match extent, resolution and dimensions

list_mdts <- pblapply(list_mdts, mask, mask=ambit_RB_b) # crop the raster to the limits of the RB
list_mdts_res <- pblapply(list_mdts_res, mask, mask=ambit_RB_b) # crop the raster to the limits of the RB

## Edit the estructura forestal layers  ----------------------------------------------------------

### Extent and resolution
list_estruc_100m <- pblapply(list_estruc, aggregate, fact=5) # resample to match extent, resolution (100x100m) and dimensions

### 'NA' values to '0'
list_estruc_100m <- pblapply(list_estruc_100m, function(x) { x[is.na(x)] <- 0; return(x) }) # resample to match extent, resolution (100x100m) and dimensions

### Crop
list_estruc <- pblapply(list_estruc, mask, mask=ambit_RB_b) # crop the raster to the limits of the RB
list_estruc_100m <- pblapply(list_estruc_100m, mask, mask=ambit_RB_b) # crop the original raster to the limits of the RB

## Edit constraints layer  ----------------------------------------------------------

### Create vector of constraints
list_constraints[["transport"]] <- gBuffer(list_constraints[["transport"]], width=20) # some roads are 10m and others 30m wide, so the mean. 20m is a good average for train rails.
constraints_v <- bind(list_constraints[["cost_urba"]], list_constraints[["transport"]])

constraints_v <- constraints_v[ambit_PN,]

constraints_v <- raster::intersect(constraints_v, ambit_PN)
     
### Urban constrains
list_constraints[["cost_urba"]] <-  rasterize(list_constraints[["cost_urba"]], grid_100m_r)                       
list_constraints[["cost_urba"]][list_constraints[["cost_urba"]] > 1, ] <- 1  
list_constraints[["cost_urba"]] <- is.na(list_constraints[["cost_urba"]])

r1 <- crop(r1, extent(ambit_RB)) # crop on extent
r1 <- mask(r1, mask=ambit_RB) # mask the grid on the PN ambit
r1[r1<0.65] <- 0 
r1[r1 >= 0.65] <- 1 
plot(r1)

r2 <- crop(r2, extent(ambit_RB)) # crop on extent
r2 <- mask(r2, mask=ambit_RB) # mask the grid on the PN ambit
r2[r2<0.9] <- 0 
r2[r2 >= 0.9] <- 1 
plot(r2)

grid_all_constraints_100m <- r1+r2
grid_all_constraints_100m[grid_all_constraints_100m == 0] <- 3 
grid_all_constraints_100m[grid_all_constraints_100m < 3] <- 0 
grid_all_constraints_100m[grid_all_constraints_100m == 3] <- 1 
plot(grid_all_constraints_100m)

#### Merge raster into total constraints layer
list_constraints <- list_constraints[names(list_constraints) %in% c("cost_urba", "transport")] # Use only costs that are constrain
grid_all_constraints_100m <- calc(stack(list_constraints), prod)
grid_all_constraints_100m <- crop(grid_all_constraints_100m, ambit_RB_b) # mask to ambit
grid_all_constraints_100m <- mask(grid_all_constraints_100m, mask=ambit_RB_b) # mask to ambit

## Edit climate layers  ----------------------------------------------------------

### Crop bioclim files
list_climate <- pblapply(list_climate, mask, mask=ambit_RB_b) # crop the raster to the limits of the RB

### Change extent
list_climate <- pblapply(list_climate, crop, y=grid_100m_r)

### Change resolution to 100m
list_climate <- pblapply(list_climate, resample, y=grid_100m_r)

## Number of pixels and resolution for each species and HICs  ----------------------------------------------------------

### 100x100m
npixels_100m <- pbsapply(list_grid_presence_100m, cellStats, stat=sum) # total number of pixels of each species
nrecords_100m <- pbsapply(list_species_100m, nrow) # total number of records of each species
species_100m <- names(list_species_100m)
taxon_100m <- pbsapply(list_species_100m, function(x) {x@data$DES_TIPUS_EPN[1]})
conserv_100m <- pbsapply(list_species_100m, function(x) {x@data$INT_CONS_MSY_FINAL[1]}) # conservation status
hidro_cats_100m <- ifelse(v_hidro_coef_100m < 100, "aquatica", "terrestre") # we determine that an species is aquatic when its mean distances are lower than 100m
species_100m_df <- data.frame(categoria="species100m", taxon=taxon_100m, element=species_100m, conserv=conserv_100m,
                              npixels=npixels_100m, nrecords=nrecords_100m, hidro_coef=v_hidro_coef_100m, hidro_categories=hidro_cats_100m)

### 500x500m
npixels_500m <- pbsapply(list_grid_presence_500m, cellStats, stat=sum) # total number of pixels of each species
nrecords_500m <- pbsapply(list_species_500m, nrow) # total number of records of each species
species_500m <- names(list_species_500m)
taxon_500m <- pbsapply(list_species_500m, function(x) {x@data$DES_TIPUS_EPN[1]})
conserv_500m <- pbsapply(list_species_500m, function(x) {x@data$INT_CONS_MSY_FINAL[1]}) # conservation status
hidro_cats_500m <- ifelse(v_hidro_coef_500m < 100, "aquatica", "terrestre") # we determine that an species is aquatic when its mean distances are lower than 100m
species_500m_df <- data.frame(categoria="species500m", taxon=taxon_500m, element=species_500m, conserv=conserv_500m,
                              npixels=npixels_500m, nrecords=nrecords_500m, hidro_coef=v_hidro_coef_500m, hidro_categories=hidro_cats_500m)

### 1x1km
npixels_1km <- pbsapply(list_grid_presence_1km, cellStats, stat=sum) # total number of pixels of each species
nrecords_1km <- pbsapply(list_species_1km, nrow) # total number of records of each species
species_1km <- names(list_species_1km)
taxon_1km <- pbsapply(list_species_1km, function(x) {x@data$DES_TIPUS_EPN[1]})
conserv_1km <- pbsapply(list_species_1km, function(x) {x@data$INT_CONS_MSY_FINAL[1]}) # conservation status
species_1km_df <- data.frame(categoria="species1km", taxon=taxon_1km, element=species_1km, conserv=conserv_1km,
                             npixels=npixels_1km, nrecords=nrecords_1km, hidro_coef=NA, hidro_categories=NA)


### Flora amenaçada
npixels_floramen <- pbsapply(list_flora_amen, function(x) { length(x[x > 0])}) # total number of pixels of each species
nrecords_floramen <- NA # it is NA because it comes from polygons, we only want the area (taken with npixels)
species_floramen <- names(list_flora_amen)
taxon_floramen <- "Flora"
conserv_floramen <- "amenaçada" # amenaçada perquè és el mateix nom de la capa de flora amenaçada
species_floramen_df <- data.frame(categoria="floramen", taxon=taxon_floramen, element=species_floramen, conserv=conserv_floramen,
                                  npixels=npixels_floramen, nrecords=nrecords_floramen, hidro_coef=NA, hidro_categories=NA)


### HICs
npixels_HIC <- pbsapply(list_grid_HIC, cellStats, stat=sum) # total number of positive pixels of each HIC
HIC_names <- names(list_grid_HIC) # transform list of HIC names to vector
HIC_100m_df <- data.frame(categoria="HIC", taxon=NA, element=HIC_names, conserv=NA, npixels=npixels_HIC,
                          nrecords=NA, hidro_coef=NA, hidro_categories=NA)

### Create table

table_df <- rbind(species_100m_df, species_500m_df, species_1km_df, HIC_100m_df, species_floramen_df)

### Filter species of 1km res list ----------------------------------------------------------
# This section wants to filter species highly represented on 1x1km resolution AND species with medium, high and very high conservation value


### Filter species with high conservation value
conserv_1km <- c("Mig", "Alt", "Molt Alt") # we can discuss these values
species_1km_df <- species_1km_df[species_1km_df$conserv %in% conserv_1km, -c(6,7)]

### Filter species with high percentage of values on 1km resolution

merge_species_df <- merge(species_1km_df, species_500m_df, by="element", all.x=T, all.y=F)
merge_species_df <- merge(merge_species_df, species_100m_df, by="element", all.x=T, all.y=F)

merge_species_df[is.na(merge_species_df$npixels.y), "npixels.y"] <- 0
merge_species_df[is.na(merge_species_df$npixels), "npixels"] <- 0

merge_species_df$percen_1km_vs_500m <- merge_species_df$npixels.x / (merge_species_df$npixels.x + merge_species_df$npixels.y) # importance pixels relation 1km vs.500m
merge_species_df$percen_1km_vs_100m  <- merge_species_df$npixels.x / (merge_species_df$npixels.x + merge_species_df$npixels) # importance pixels relation 1km vs. 100m

merge_species_df$filter_percen <- pmin(merge_species_df$percen_1km_vs_500m, merge_species_df$percen_1km_vs_100m) # take the higher value of two columns

v_species_1km_filtered <- merge_species_df[merge_species_df$filter_percen > 0.5, "element"] # take species with higher than 50% of pixels in the 1 km res list

### Delete elements of 1 km list according to the species that prove the conditions

list_species_1km <-list_species_1km[names(list_species_1km) %in% v_species_1km_filtered]


## Parameters  ----------------------------------------------------------

my_height <- nrow(grid_100m_r)*5 # 1800
my_width <- ncol(grid_100m_r)*5 # 2050

## Export rasters grids  ----------------------------------------------------------
writeRaster(grid_100m_r, paste0(dir_export, "/GRIDS/grid_raster_100x100m.asc"),
            format = "ascii", overwrite=T)

writeRaster(grid_500m_r, paste0(dir_export, "/GRIDS/grid_raster_500x500m.asc"),
            format = "ascii", overwrite=T)

writeRaster(subsubconques_r, paste0(dir_export, "/GRIDS/grid_raster_subsubconques.asc"),
            format = "ascii", overwrite=T)


## Export rasters lon lat grids  ----------------------------------------------------------
### asc
writeRaster(grid_lon_100m_r, paste0(dir_export, "/LON_LAT_RASTER/asc/grid_lon_100m_r.asc"),
            format = "ascii", overwrite=T)

writeRaster(grid_lat_100m_r, paste0(dir_export, "/LON_LAT_RASTER/asc/grid_lat_100m_r.asc"),
            format = "ascii", overwrite=T)

writeRaster(grid_lon_500m_r, paste0(dir_export, "/LON_LAT_RASTER/asc/grid_lon_500m_r.asc"),
            format = "ascii", overwrite=T)

writeRaster(grid_lat_500m_r, paste0(dir_export, "/LON_LAT_RASTER/asc/grid_lat_500m_r.asc"),
            format = "ascii", overwrite=T)

writeRaster(grid_lon_1km_r, paste0(dir_export, "/LON_LAT_RASTER/asc/grid_lon_1km_r.asc"),
            format = "ascii", overwrite=T)

writeRaster(grid_lat_1km_r, paste0(dir_export, "/LON_LAT_RASTER/asc/grid_lat_1km_r.asc"),
            format = "ascii", overwrite=T)

### png

png(paste0(dir_export, "/LON_LAT_RASTER/png/grid_lon_100m_r.png"),
    height=my_height, width=my_width)
plot(grid_lon_100m_r); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/LON_LAT_RASTER/png/grid_lat_100m_r.png"),
    height=my_height, width=my_width)
plot(grid_lat_100m_r); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/LON_LAT_RASTER/png/grid_lon_500m_r.png"),
    height=my_height, width=my_width)
plot(grid_lon_500m_r); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/LON_LAT_RASTER/png/grid_lat_500m_r.png"),
    height=my_height, width=my_width)
plot(grid_lat_500m_r); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/LON_LAT_RASTER/png/grid_lon_1km_r.png"),
    height=my_height, width=my_width)
plot(grid_lon_1km_r); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/LON_LAT_RASTER/png/grid_lat_1km_r.png"),
    height=my_height, width=my_width)
plot(grid_lat_1km_r); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()


## Export rasters species  ----------------------------------------------------------

### Export species grids on asc

#### 100x100m

for (i in 1:length(list_species_100m)) {
  
  print(paste(i, "of", length(list_species_100m)))
  writeRaster(list_grid_presence_100m[[i]], paste0(dir_export, "/SPECIES_RASTER/asc/100x100m/", substr(names(list_species_100m)[i], 1, 40), ".asc"),
              format='ascii', overwrite=T)
}

writeRaster(grid_all_presence_100m, paste0(dir_export, "/SPECIES_RASTER/asc/100x100m/00_all_species.asc"),
            format='ascii', overwrite=T)

#### 500x500m

for (i in 1:length(list_species_500m)) {
  
  print(paste(i, "of", length(list_species_500m)))
  
  writeRaster(list_grid_presence_500m[[i]], paste0(dir_export, "/SPECIES_RASTER/asc/500x500m/", substr(names(list_species_500m)[i], 1, 40), ".asc"),
              format='ascii', overwrite=T)
}

writeRaster(grid_all_presence_500m, paste0(dir_export, "/SPECIES_RASTER/asc/500x500m/00_all_species.asc"),
            format='ascii', overwrite=T)

#### 1x1km

for (i in 1:length(list_species_1km)) {
  
  print(paste(i, "of", length(list_species_1km)))
  
  writeRaster(list_grid_presence_1km[[i]], paste0(dir_export, "/SPECIES_RASTER/asc/1x1km/", substr(names(list_species_1km)[i], 1, 40), ".asc"),
              format='ascii', overwrite=T)
}

writeRaster(grid_all_presence_1km, paste0(dir_export, "/SPECIES_RASTER/asc/1x1km/00_all_species.asc"),
            format='ascii', overwrite=T)

### Export species grids on png

#### 100x100m

for (i in 1:length(list_species_100m)) {
  
  print(paste(i, "of", length(list_species_100m)))
  png(paste0(dir_export, "/SPECIES_RASTER/png/100x100m/", substr(names(list_species_100m)[i], 1, 40), ".png"),
      height=my_height, width=my_width)
  plot(list_grid_presence_100m[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}

png(paste0(dir_export, "/SPECIES_RASTER/png/100x100m/00_all_species.png"),
    height=my_height, width=my_width)
plot(grid_all_presence_100m); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

#### 500x500m

for (i in 1:length(list_species_500m)) {
  
  print(paste(i, "of", length(list_species_500m)))
  
  png(paste0(dir_export, "/SPECIES_RASTER/png/500x500m/", substr(names(list_species_500m)[i], 1, 40), ".png"),
      height=my_height, width=my_width)
  plot(list_grid_presence_500m[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
  
}

png(paste0(dir_export, "/SPECIES_RASTER/png/500x500m/00_all_species.png"),
    height=my_height, width=my_width)
plot(grid_all_presence_500m); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

#### 1x1km
for (i in 1:length(list_species_1km)) {
  
  print(paste(i, "of", length(list_species_1km)))
  
  png(paste0(dir_export, "/SPECIES_RASTER/png/1x1km/", substr(names(list_species_1km)[i], 1, 40), ".png"),
      height=my_height, width=my_width)
  plot(list_grid_presence_1km[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}

png(paste0(dir_export, "/SPECIES_RASTER/png/1x1km/00_all_species.png"),
    height=my_height, width=my_width)
plot(grid_all_presence_1km); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

### Export species on subsubconques grid on asc

for (i in 1:length(list_grid_presence_subsubconca)) {
  
  print(paste(i, "of", length(list_grid_presence_subsubconca)))
  
  writeRaster(list_grid_presence_subsubconca[[i]], paste0(dir_export, "/SPECIES_RASTER/asc/SUBSUBCONQUES_GRID/", substr(names(list_grid_presence_subsubconca)[i], 1, 40), ".asc"), format='ascii', overwrite=T)
}

writeRaster(grid_all_presence_subsubconca, paste0(dir_export, "/SPECIES_RASTER/asc/SUBSUBCONQUES/00_all_species.asc"), format='ascii', overwrite=T)

### Export species on subsubconques grid on shp

for (i in 1:length(list_grid_presence_subsubconca)) {
  
  print(paste(i, "of", length(list_grid_presence_subsubconca)))

  shapefile(list_vector_presence_subsubconca[[i]], paste0(dir_export, "/SPECIES_RASTER/shp/SUBSUBCONQUES_VECTOR/",
            substr(names(list_grid_presence_subsubconca)[i], 1, 40), ".shp"), overwrite=T)
  
}


### Export species on subsubconques grid on png

for (i in 1:length(list_species_500m)) {
  
  print(paste(i, "of", length(list_species_500m)))
  
  png(paste0(dir_export, "/SPECIES_RASTER/png/SUBSUBCONQUES_GRID/", substr(names(list_grid_presence_subsubconca)[i], 1, 40), ".png"), height=my_height, width=my_width)
  plot(list_grid_presence_subsubconca[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T); plot(list_species_500m[[i]], add=T)
  dev.off()
  
  png(paste0(dir_export, "/SPECIES_RASTER/png/SUBSUBCONQUES_VECTOR/", substr(names(list_grid_presence_subsubconca)[i], 1, 40), ".png"), height=my_height, width=my_width)
  plot(list_vector_presence_subsubconca[[i]]); plot(list_vector_presence_subsubconca[[i]][!is.na(list_vector_presence_subsubconca[[i]]$presence),], col="darkgreen", add=T); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T); plot(list_species_500m[[i]], col="orange", add=T)
  dev.off()
}

for (i in (length(list_species_500m)+1):(length(list_species_100m)+length(list_species_500m))) {
  
  print(paste(i, "of", length(list_species_100m)+length(list_species_500m)))
  
  png(paste0(dir_export, "/SPECIES_RASTER/png/SUBSUBCONQUES_GRID/", substr(names(list_grid_presence_subsubconca)[i], 1, 40), ".png"), height=my_height, width=my_width)
  plot(list_grid_presence_subsubconca[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T); plot(list_species_100m[[i-length(list_species_500m)]], add=T)
  dev.off()
  
  png(paste0(dir_export, "/SPECIES_RASTER/png/SUBSUBCONQUES_VECTOR/", substr(names(list_vector_presence_subsubconca)[i], 1, 40), ".png"), height=my_height, width=my_width)
  plot(list_vector_presence_subsubconca[[i]]); plot(list_vector_presence_subsubconca[[i]][!is.na(list_vector_presence_subsubconca[[i]]$presence),], col="darkgreen", add=T);
  plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T); plot(list_species_100m[[i-length(list_species_500m)]], col="orange", add=T)
  dev.off()
}

png(paste0(dir_export, "/SPECIES_RASTER/png/SUBSUBCONQUES/00_all_species.png"), height=my_height, width=my_width)
plot(grid_all_presence_subsubconca); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()


### Export species on subconques grid on asc

for (i in 1:length(list_grid_mask_subconca)) {
  print(paste(i, "of", length(list_grid_mask_subconca)))
  writeRaster(list_grid_mask_subconca[[i]], paste0(dir_export, "/SPECIES_RASTER/asc/SUBCONQUES/", substr(names(list_grid_mask_subconca)[i], 1, 40), ".asc"), format='ascii', overwrite=T)
}

### Export species on subconques grid on png

for (i in 1:length(list_grid_mask_subconca)) {
  print(paste(i, "of", length(list_grid_mask_subconca)))
  png(paste0(dir_export, "/SPECIES_RASTER/png/SUBCONQUES/", substr(names(list_grid_mask_subconca)[i], 1, 40), ".png"), height=my_height, width=my_width)
  plot(list_grid_mask_subconca[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}

## Export flora amenaçada  ----------------------------------------------------------
### Export flora amenaçada on grid on asc

for (i in 1:length(list_flora_amen)) {
  
  print(paste(i, "of", length(list_flora_amen)))
  
  writeRaster(list_flora_amen[[i]],
              paste0(dir_export, "/SPECIES_RASTER/asc/flora_amen/", names(list_flora_amen)[i], ".asc"),
              format='ascii', overwrite=T)
}

### Export flora amenaçada on grid on png

for (i in 1:length(list_flora_amen)) {
  
  print(paste(i, "of", length(list_flora_amen)))
  
  png(paste0(dir_export, "/SPECIES_RASTER/png/flora_amen/", names(list_flora_amen)[i], ".png"),
      height=my_height, width=my_width)
  plot(list_flora_amen[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}

## Export rasters HICs  ----------------------------------------------------------
### Export HICs on grid on asc

for (i in 1:length(list_HIC_raster)) {
  
  print(paste(i, "of", length(list_HIC_raster)))
  
  writeRaster(list_HIC_raster[[i]],
              paste0(dir_export, "/HIC_RASTER/asc/", names(list_HIC_raster)[i], ".asc"),
              format='ascii', overwrite=T)
}

### Export HICs on grid on png

for (i in 1:length(list_HIC_raster)) {
  
  print(paste(i, "of", length(list_HIC_raster)))
  
  png(paste0(dir_export, "/HIC_RASTER/png/", names(list_HIC_raster)[i], ".png"),
      height=my_height, width=my_width)
  plot(list_HIC_raster[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}

## Export rasters habitats  ----------------------------------------------------------
### Export habitats

#### Export habitat on asc

for (i in 1:length(list_grid_habitats_100m)) {
  
  print(paste(i, "of", length(list_grid_habitats_100m)))
  
  writeRaster(list_grid_habitats_100m[[i]],
              paste0(dir_export, "/HABITATS_RASTER/asc/HABITATS/", names(list_grid_habitats_100m)[i], ".asc"),
              format='ascii', overwrite=T)
}


#### Export habitat on png

for (i in 1:length(list_grid_habitats_100m)) {
  
  print(paste(i, "of", length(list_grid_habitats_100m)))
  
  png(paste0(dir_export, "/HABITATS_RASTER/png/HABITATS/", names(list_grid_habitats_100m)[i], ".png"),
      height=my_height, width=my_width)
  plot(list_grid_habitats_100m[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}


### Export habitats categories

#### Export habitat categories on asc

for (i in 1:length(list_grid_cat_habitats_100m)) {
  
  print(paste(i, "of", length(list_grid_cat_habitats_100m)))
  
  writeRaster(list_grid_cat_habitats_100m[[i]],
              paste0(dir_export, "/HABITATS_RASTER/asc/CAT_HABITATS/", names(list_grid_cat_habitats_100m)[i], ".asc"),
              format='ascii', overwrite=T)
}


#### Export habitat categories on png

for (i in 1:length(list_grid_cat_habitats_100m)) {
  
  print(paste(i, "of", length(list_grid_cat_habitats_100m)))
  
  png(paste0(dir_export, "/HABITATS_RASTER/png/CAT_HABITATS/", names(list_grid_cat_habitats_100m)[i], ".png"),
      height=my_height, width=my_width)
  plot(list_grid_cat_habitats_100m[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}


## Export nius  ----------------------------------------------------------

#### shp
for (i in 1:length(list_nius2)) {
  
  print(paste(i, "of", length(list_nius2)))
  shapefile(list_nius2[[i]], paste0(dir_export, "/SPECIES_RASTER/shp/NIUS/points/", names(list_nius2)[i], ".shp"), overwrite=T)
  shapefile(list_nius_subconques[[i]], paste0(dir_export, "/SPECIES_RASTER/shp/NIUS/subconques/", names(list_nius_subconques)[i], ".shp"), overwrite=T)
  
}

#### png
for (i in 1:length(list_nius2)) {
  
  print(paste(i, "of", length(list_nius2)))
  dat <- list_nius2[[i]]
  png(paste0(dir_export, "/SPECIES_RASTER/png/NIUS/", names(list_nius2)[i], ".png"), height=1800, width=2050, pointsize = 25)
  plot(ambit_RB, border="grey", lwd=2)
  plot(ambit_PN, lwd=1, add=T)
  plot(st_as_sf(list_nius_subconques[[i]][, "presence"]), add=T)
  plot(dat, cex = (log(dat$count)+1)*2, col="red", pch=1, add=T)
  dev.off()

}


## Export rasters MDTs  ----------------------------------------------------------

### Ancient resolution

#### asc

for (i in 1:length(list_mdts)) {
  
  print(paste(i, "of", length(list_mdts)))
  
  writeRaster(list_mdts[[i]],
              paste0(dir_export, "/MDT_RASTER/asc/original/", names(list_mdts)[i], ".asc"),
              format='ascii', overwrite=T)
}

#### png

for (i in 1:length(list_mdts)) {
  
  print(paste(i, "of", length(list_mdts)))
  
  png(paste0(dir_export, "/MDT_RASTER/png/original/", names(list_mdts)[i], ".png"),
      height=my_height, width=my_width)
  plot(list_mdts[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}

### 100x100m

#### asc

for (i in 1:length(list_mdts_res)) {
  
  print(paste(i, "of", length(list_mdts_res)))
  
  writeRaster(list_mdts_res[[i]],
              paste0(dir_export, "/MDT_RASTER/asc/100x100m/", names(list_mdts_res)[i], ".asc"),
              format='ascii', overwrite=T)
}

#### png

for (i in 1:length(list_mdts_res)) {
  
  print(paste(i, "of", length(list_mdts_res)))
  
  png(paste0(dir_export, "/MDT_RASTER/png/100x100m/", names(list_mdts_res)[i], ".png"),
      height=my_height, width=my_width)
  plot(list_mdts_res[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}

## Export rasters estructura forestal  ----------------------------------------------------------

#### asc

for (i in 1:length(list_estruc)) {
  
  print(paste(i, "of", length(list_estruc)))
  
  writeRaster(list_estruc[[i]],
              paste0(dir_export, "/ESTRUCTURA_FOREST_RASTER/asc/original/", names(list_estruc)[i], ".asc"),
              format='ascii', overwrite=T)
}

for (i in 1:length(list_estruc_100m)) {
  
  print(paste(i, "of", length(list_estruc_100m)))
  
  writeRaster(list_estruc_100m[[i]],
              paste0(dir_export, "/ESTRUCTURA_FOREST_RASTER/asc/100x100m/", names(list_estruc_100m)[i], ".asc"),
              format='ascii', overwrite=T)
}

#### png

for (i in 1:length(list_estruc)) {
  
  print(paste(i, "of", length(list_estruc)))
  
  png(paste0(dir_export, "/ESTRUCTURA_FOREST_RASTER/png/original/", names(list_estruc)[i], ".png"),
      height=my_height, width=my_width)
  plot(list_estruc[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}


for (i in 1:length(list_estruc_100m)) {
  
  print(paste(i, "of", length(list_estruc_100m)))
  
  png(paste0(dir_export, "/ESTRUCTURA_FOREST_RASTER/png/100x100m/", names(list_estruc_100m)[i], ".png"),
      height=my_height, width=my_width)
  plot(list_estruc_100m[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}

## Export rasters climate  ----------------------------------------------------------

#### asc

for (i in 1:length(list_climate)) {
  
  print(paste(i, "of", length(list_climate)))
  
  writeRaster(list_climate[[i]],
              paste0(dir_export, "/CLIMATE_RASTER/asc/", names(list_climate)[i], ".asc"),
              format='ascii', overwrite=T)
}

#### png

for (i in 1:length(list_climate)) {
  
  print(paste(i, "of", length(list_climate)))
  
  png(paste0(dir_export, "/CLIMATE_RASTER/png/", names(list_climate)[i], ".png"),
      height=my_height, width=my_width)
  plot(list_climate[[i]]); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
  dev.off()
}



## Export raster hidro ----------------------------------------------------------

#### asc

writeRaster(r_dist_rivers_100m, paste0(dir_export, "/HIDRO_RASTER/asc/", "r_dist_rivers_100m.asc"),
            format='ascii', overwrite=T)

writeRaster(r_dist_rivers_500m, paste0(dir_export, "/HIDRO_RASTER/asc/", "r_dist_rivers_500m.asc"),
            format='ascii', overwrite=T)

#### png

png(paste0(dir_export, "/HIDRO_RASTER/png/", "r_dist_rivers_100m.png"), height=my_height, width=my_width)
plot(r_dist_rivers_100m); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/HIDRO_RASTER/png/", "r_dist_rivers_500m.png"), height=my_height, width=my_width)
plot(r_dist_rivers_500m); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

## Export raster boscos madurs ----------------------------------------------------------

#### asc

writeRaster(r_bosc_madur, paste0(dir_export, "/BOSCOS_MADURS/", "r_boscos_madurs.asc"),
            format='ascii', overwrite=T)

#### png

png(paste0(dir_export, "/BOSCOS_MADURS/", "r_boscos_madurs.png"), height=my_height, width=my_width)
plot(ambit_PN); plot(r_bosc_madur, add=T)
dev.off()

## Export raster costs ----------------------------------------------------------

#### asc
writeRaster(cost_gest_forest_r, paste0(dir_export, "/COSTS_RASTER/asc/", "cost_gest_forest.asc"),
            format='ascii', overwrite=T)

writeRaster(cost_public_r, paste0(dir_export, "/COSTS_RASTER/asc/", "cost_public.asc"),
            format='ascii', overwrite=T)

writeRaster(cost_urba_r, paste0(dir_export, "/COSTS_RASTER/asc/", "cost_urba.asc"),
            format='ascii', overwrite=T)

writeRaster(r_dist_alt_freq, paste0(dir_export, "/COSTS_RASTER/asc/", "cost_alt_freq.asc"),
            format='ascii', overwrite=T)

writeRaster(r_dist_transport, paste0(dir_export, "/COSTS_RASTER/asc/", "cost_transport.asc"),
            format='ascii', overwrite=T)

writeRaster(grid_all_costs, paste0(dir_export, "/COSTS_RASTER/asc/", "cost_total_sense_public.asc"),
            format='ascii', overwrite=T)

writeRaster(grid_all_costs_public, paste0(dir_export, "/COSTS_RASTER/asc/", "cost_total_amb_public.asc"),
            format='ascii', overwrite=T)

#### png
png(paste0(dir_export, "/COSTS_RASTER/png/", "cost_gest_forest.png"),
    height=my_height, width=my_width)
plot(cost_gest_forest_r); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/COSTS_RASTER/png/", "cost_public.png"),
    height=my_height, width=my_width)
plot(cost_public_r); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/COSTS_RASTER/png/", "cost_urba.png"),
    height=my_height, width=my_width)
plot(cost_urba_r); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/COSTS_RASTER/png/", "cost_alt_freq.png"),
    height=my_height, width=my_width)
plot(r_dist_alt_freq); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/COSTS_RASTER/png/", "cost_transport.png"),
    height=my_height, width=my_width)
plot(r_dist_transport); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/COSTS_RASTER/png/", "cost_total_sense_public.png"),
    height=my_height, width=my_width)
plot(grid_all_costs); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

png(paste0(dir_export, "/COSTS_RASTER/png/", "cost_total_amb_public.png"),
    height=my_height, width=my_width)
plot(grid_all_costs_public); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()


## Export raster constraints ----------------------------------------------------------

#### asc
writeRaster(grid_all_constraints_100m, paste0(dir_export, "/CONSTRAINTS_RASTER/asc/", "grid_all_constraints_100m"),
            format='ascii', overwrite=T)

# writeRaster(grid_all_constraints_500m, paste0(dir_export, "/CONSTRAINTS_RASTER/asc/", "grid_all_constraints_500m"),
#             format='ascii', overwrite=T)


#### png
png(paste0(dir_export, "/CONSTRAINTS_RASTER/png/", "grid_all_constraints_100m.png"),
    height=my_height, width=my_width)
plot(grid_all_constraints_100m); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
dev.off()

# png(paste0(dir_export, "/CONSTRAINTS_RASTER/png/", "grid_all_constraints_500m.png"),
#     height=my_height, width=my_width)
# plot(grid_all_constraints_500m); plot(ambit_RB, border="red", add=T); plot(ambit_PN, add=T)
# dev.off()

### shp
shapefile(constraints_v, paste0(dir_export, "/CONSTRAINTS_RASTER/shp/grid_all_constraints_100m.shp"), overwrite=T)



## Export excels  ----------------------------------------------------------

#### Number of pixels excel

OUT <- createWorkbook() # Create a blank workbook

addWorksheet(OUT, "especies_HIC") # Add some sheets to the workbook
addWorksheet(OUT, "habitats") # Add some sheets to the workbook
addWorksheet(OUT, "especies_1km") # Add some sheets to the workbook

writeData(OUT, sheet = "especies_HIC", x = table_df) # Write the data to the sheets
writeData(OUT, sheet = "habitats", x = hab_df) # Write the data to the sheets
writeData(OUT, sheet = "especies_1km", x = merge_species_df) # Write the data to the sheets

saveWorkbook(OUT, paste0(dir_export,"/taula_especies_habitats_HICs.xlsx"), overwrite=T) # Export the file

### Connectivity df

write.csv(connect_df, paste0(dir_export, "/CONNECTIVITY/dist_centroids_grid.csv"), row.names=F)

## Create distributions that are not SDM but OBS ----

### We fill the occupied conques by each species, using the locations of the species. We will have to load objects that are above these lines. 

fun_point_presence_subsubconca <- function(r, pts){
  r2 <- r 
  #r2[] <- 0 # make the inout raster of zeroes
  counts <- as.numeric(names(table(cellFromXY(r,pts)))) # get the number of points inside each cell
  z <- unique(r2[counts])
  z <- z[!is.na(z)]
  r2[!r2@data@values %in% z] <- 0
  r2[r2@data@values %in% z] <- 1
  return(r2)
} # Point presence on subsubconques layer

list_grid_presence_subsubconca <- vector(mode = "list", length=3)

#List_species_100m, 500m and 1km comes from 1.3 Remember to run 1.3 first in R console 

for (i in 1:length(list_species_100m)) { #Apply fun_point_presence_subconca to mark occupied conques by each species. 
  print(paste(i, "of", length(list_species_100m)))
  list_grid_presence_subsubconca[[1]][[i]] <- fun_point_presence_subsubconca(subsubconques_r, list_species_100m[[i]])
}

for (i in 1:length(list_species_500m)) {
  print(paste(i, "of", length(list_species_500m)))
  list_grid_presence_subsubconca[[2]][[i]] <- fun_point_presence_subsubconca(subsubconques_r, list_species_500m[[i]])
}

for (i in 1:length(list_species_1km)) {
  print(paste(i, "of", length(list_species_1km)))
  list_grid_presence_subsubconca[[3]][[i]] <- fun_point_presence_subsubconca(subsubconques_r, list_species_1km[[i]])
}

list_grid_presence_subsubconca_unlisted <- unlist(list_grid_presence_subsubconca) #we first unlist because if not there is a bug in the next line 
grid_all_presence_subsubconca <- do.call("sum", list_grid_presence_subsubconca_unlisted)
names(list_grid_presence_subsubconca[[1]]) <- names(list_species_100m)
names(list_grid_presence_subsubconca[[2]]) <- names(list_species_500m)
names(list_grid_presence_subsubconca[[3]]) <- names(list_species_1km)
list_grid_presence_subsubconca <- unlist(list_grid_presence_subsubconca) # we do it again but now properly 

rm(list_grid_presence_subsubconca_unlisted) # we do not need it anymore

first_occurrences <- !duplicated(names(list_grid_presence_subsubconca)) # Select the first time that a name appears here. Hierarchy 100m>500m>1km
list_grid_presence_subsubconca <- list_grid_presence_subsubconca[first_occurrences] #Delete second and third apparitions (100m>500m>1km)
nom<-names(list_grid_presence_subsubconca)

names(list_grid_presence_subsubconca) <- substr(names(list_grid_presence_subsubconca), 1, 40) #keep only the first 40 characters

raster_masked_list <- pblapply(list_grid_presence_subsubconca, function(x) { #Mask rasters with the shape of PN
  r_masked <- mask(x, ambit_PN) 
  return(r_masked)
})

rasters_with_1_idx <- which(sapply(raster_masked_list, function(x) any(getValues(x) == 1))) #Select those rasters that contain values==1
list_grid_presence_subsubconca <- raster_masked_list[rasters_with_1_idx] #And delete those that do not. 
noms_despres<-names(list_grid_presence_subsubconca)

no_presents <- setdiff(nom, noms_despres) #Names of the species that are not inside the PN. This will be used in 2.4, to fill the table with 0. 

for (i in 1:length(list_grid_presence_subsubconca)) { #Write rasters
  writeRaster(list_grid_presence_subsubconca[[i]], paste0(dir_export,"/SPECIES_RASTER/asc/OBSERVACIONS/",noms_despres[[i]],".asc"), format="ascii")
}


## Old way (using raster data directly, not vector)
#Open filenames for species at 100m, 500m and 1km, from script 1.4
# filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/asc/1x1km"), pattern="*.asc", full.names=T)
# filenames <- filenames[-grep("00", filenames)] #Delete names that are all_features
# llista1km <- pblapply(filenames, raster)
# filenames_short_1km <- filenames %>% basename %>% file_path_sans_ext
# names(llista1km) <- filenames_short
# 
# filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/asc/500x500m"), pattern="*.asc", full.names=T)
# filenames <- filenames[-grep("all_species", filenames)] #Delete names that are all_features
# llista500m <- pblapply(filenames, raster)
# filenames_short_500m <- filenames %>% basename %>% file_path_sans_ext
# names(llista500m) <- filenames_short
# 
# filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/asc/100x100m"), pattern="*.asc", full.names=T)
# filenames <- filenames[-grep("all_species", filenames)] #Delete names that are all_features
# llista100m <- pblapply(filenames, raster)
# filenames_short_100m <- filenames %>% basename %>% file_path_sans_ext
# names(llista100m) <- filenames_short
# 
# noms<-c(filenames_short_100m, filenames_short_500m, filenames_short_1km)
# 
# llista_tot<-c(llista100m, llista500m, llista1km) #ajuntem les llistes, amb les dades de més concretes a menys. 
# first_occurrences <- !duplicated(names(llista_tot)) # seleccionem la primera vegada que apareix cada nom. 
# llista_tot <- llista_tot[first_occurrences] #Ens petem les aparicions que no són la primera (100m>500m>1km)
# 
# noms_abans<-names(llista_tot)
# 
# #Remove features that are not inside the PN Montseny:
# raster_masked_list <- pblapply(llista_tot, function(x) { #emmascarem els rasters amb la forma del PN
#   r_masked <- mask(x, PU_conques) #AQUEST PU CONQUES HA DE SER JA EL RETALLAT AL PN
#   return(r_masked)
# })
# 
# rasters_with_1_idx <- which(sapply(raster_masked_list, function(x) any(getValues(x) == 1)))
# llista_tot <- raster_masked_list[rasters_with_1_idx]
# noms_despres<-names(llista_tot)
# 
# no_presents <- setdiff(noms_abans, noms_despres) #Names of the species that are not inside the PN. This will be used in 2.4, to fill the table. 
# 
# llista_PU<-list()
# 
# #Here we create, from the occupied pixels, rasters with 0 in the subconques that are not occupied by the feature, and 1 in those with presence of the feature.
# for (i in 1:length(llista_tot)) {
#   extret<- raster::extract(llista_tot[[i]], PU_conques, fun=sum, sp=T) #S'afegeix un camp de 1 i 0 a PU_conques amb 0 i 1 segons pres/abs
#   extret@data[,18] <- ifelse(is.na(extret@data[,18]), 0, extret@data[,18])
#   prova <- extret[extret@data[,18]!= 0,] #seleccionem el que no sigui 0
#   llista_PU[[i]]<- prova #Ho portem a una llista resultat
#   llista_PU[[i]]<- raster::rasterize(prova, grid_100m_r, field =extret@data[,18] ) #I rasteritzem la ultima columna
#   llista_PU[[i]][llista_PU[[i]] == 0] <- 1 # com que tot es 0 i NODATA, reclassifiquem 0 a 1 i NODATA a 0
#   llista_PU[[i]][is.na(llista_PU[[i]])] <- 0
#   writeRaster(llista_PU[[i]], paste0(dir_export,"/PREDICTION_RASTER/asc/TOT/",noms_despres[i],".asc"), format='ascii', overwrite=T)
# }
