
#### 2.5 SYSTEMATIC CONSERVATION PLANNING FOR MULTIZONES AND COSTS ####

##  Meta  ----------------------------------------------------------

# https://prioritizr.net/
# https://cran.r-project.org/web/packages/prioritizr/vignettes/prioritizr.html

##  Initial code  ----------------------------------------------------------

### Errors to warnings
# If we see that some warnings are turned to errors, run the following:
options(warn=1)

### Install packages in case they are not installed

list.of.packages <- c("tidyverse", "readr", "rgdal", "sp", "raster", "sf", "rgeos", "prioritizr", "readxl",
                      "maptools", "PBSmapping", "pbapply", "tools", "DescTools", "Rsymphony", "spdep", "openxlsx", "tidyr",
                      "climateStability", "spdep", "parallel", "foreach", "zoom", "ggplot2","RColorBrewer","gurobi","doParallel",
                      "htmlwidgets", "terra", "geodata", "leaflet", "leafpop", "leaflet.extras", "leafem", "leaflet.minicharts")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}

# install.packages('c:/gurobi952/win64/R/gurobi_9.5-2.zip', repos=NULL) # installing gurobi (however we need it in the computer before installing it in R!)

### Open libraries

invisible(lapply(list.of.packages, library, character.only = TRUE, logical.return=T))
# library(gurobi)
rm(list.of.packages, new.packages) # remove objects we won't need anymore

### Number of cores of the computer
numCores <- detectCores()
numCores # 16 for serverbio and 4 for CTFC local
numCores <- numCores - 1 # recommended to leave one free core for other tasks

### Set default options for printing tabular data

options(tibble.width = Inf)

### Directories
dir<-"C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY"
dir_processed <- paste0(dir, "/PROCESSED") # directory processed
dir_export <- paste0(dir, "/EXPORT") # directory exportation
dir_sdm <- paste0(dir_export, "/PREDICTION_RASTER/asc") # directory species distribution model rasters
dir_marxan <- paste0(dir_export, "/MARXAN_RASTER/STRICT_CONSERVATION") # directory species distribution model rasters
dir_hic <- paste0(dir_export, "/HIC_RASTER/png") # directory species distribution model rasters
dir_richness <- paste0(dir_export, "/RICHNESS_RASTER/asc") # directory species distribution model rasters

### Open files

#### Ambits
ambit_PN <- shapefile(paste0(dir, "/SIMSY/INFO_SONIA_141022/LIMIT_PARC_DECRET_21/LIMIT_2021_v2.shp"))  # G: official EBBA2 countries and masks
ambit_RB <- shapefile(paste0(dir, "/SIMSY/INFO_SONIA_141022/LimitsRB/DelimitacioShpED50RBM13_ETRS89.shp")) # G: official EBBA2 countries and masks

#### Planning units
PU_conques <- shapefile(paste0(dir_processed, "/PUs/PU_conques.shp")) # G: official EBBA2 countries and masks

### Raster grid
grid_100m_r <- raster(paste0(dir_export, "/GRIDS/grid_raster_100x100m.asc"))

### Richness
grid_all_richness <- raster(paste0(dir_richness, "/grid_all_richness.asc")) # the sum of HIC + SDM + species <10 obs richnesses
grid_richness_HIC <- raster(paste0(dir_richness, "/grid_richness_HICs.asc")) # richness HICs
grid_richness_SDM <- raster(paste0(dir_richness, "/grid_richness_SDM.asc")) # richness SDM
grid_richness_no_SDM <- raster(paste0(dir_richness, "/grid_richness_no_SDM_conserv_status.asc")) # richness <10 obs 

### Finques públiques
finques_pub<-readOGR("C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY/SIMSY/NOVA_INFO_SONIA/CARTO_REFERENCIA/PARCELARI/FINQUES_PUBLIQUES/finques_publiques_msySHP_MAIG23.shp")

#### OBSERVACIONS. Here we add and remove features, change names, etc. in order to match the table with the names of the archives.
filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/asc/OBSERVACIONS"), pattern="*.asc", full.names=T) # for now we will use the LQ models from 500 to 100m
# paralelize the following lines
cl <- makeCluster(numCores) # create cluster
clusterExport(cl=cl, varlist="filenames") # import the objects and functions for the paralelization
clusterEvalQ(cl, library(raster)) # VERY IMPORTANT to call the libraries too
list_obs <- parLapply(cl, filenames, raster)
stopCluster(cl) # stop paralelizing
filenames_obs <- sub(".asc$", "", filenames %>% basename %>% file_path_sans_ext)
names(list_obs) <- filenames_obs
names(list_obs) <- substr(names(list_obs), 1, 40) #We do this because in SDM archive names are cutted, but not in OBS. 

###Remove names that the first two words are repeated (We will write them 3 lines later. )
# extract the first two words of each name
raster_names_2words <- sub("^([[:alnum:]]+\\s+[[:alnum:]]+).*", "\\1", names(list_obs)) #Search of elements that contain the same 2 first words
duplicated_names <- raster_names_2words[duplicated(raster_names_2words)] #Duplicated names. We will have to remove elements from list_obs that are repeated names but written different. subsp, habitats or names that are not well written will remain

remove<-c("Oberea linearis (Linnaeus, 1758)","Tylopsis lilifolia", "Melitaea deione Duponchel, 1832", "Poblaments submersos d’asprelles (Chara ","Plebeius argus") #Plebejus, plebeius is not well written. 
list_obs<-list_obs[! names(list_obs) %in% remove] #Remove repeated names

names_to_change <- c("Buxbaumia viridis (Moug. ex Lam. & DC.) ","Lobaria pulmonaria (L.) Hoffm.","Aquila pennata","Cecropis daurica", "Carduelis cannabina") #This names are different in list_flora_amen or nius
# select the elements to change by name
new_names <- c("Buxbaumia viridis","Lobaria pulmonaria","Hieraaetus pennatus", "Hirundo daurica", "Linaria cannabina") 
pos <- match(names_to_change, names(list_obs))
names(list_obs)[pos] <- new_names #We change this names to remove duplicates later.

#Here we sum Aquila fasciata and Hieraaetus fasciatus observations because they are synonyms and data comes from 500x500m
raster_names <- c("Aquila fasciata", "Hieraaetus fasciatus") #In fact we use SDM, but we can leave it here.
final_raster_name <- "Hieraaetus fasciatus"
rasters_to_sum <- list_obs[raster_names]# Extract the rasters from the list based on their names

final_raster <- overlay(rasters_to_sum[[1]], rasters_to_sum[[2]], fun=sum) # Sum the rasters using the overlay function
list_obs[[raster_names[1]]] <- NULL
list_obs[[final_raster_name]] <- final_raster


#### SDM
filenames <- list.files(paste0(dir_sdm, "/marxan"), pattern="*.asc", full.names=T) # for now we will use the LQ models from 500 to 100m
# paralelize the following lines
cl <- makeCluster(numCores) # create cluster
clusterExport(cl=cl, varlist="filenames") # import the objects and functions for the paralelization
clusterEvalQ(cl, library(raster)) # VERY IMPORTANT to call the libraries too
list_sdm <- parLapply(cl, filenames, raster)
stopCluster(cl) # stop paralelizing
filenames_SDM <- sub(".asc$", "", filenames %>% basename %>% file_path_sans_ext)
names(list_sdm) <- filenames_SDM

setdiff(names(list_sdm), names(list_obs)) #Only Charadrius dubius that it is not inside the PN

remove<-c("Prats amb Cynosurus cristatus, mesòfils,","Prats dalladors amb fromental (Arrhenath","Avetoses del territori de les fagedes, a",
          "Vernedes medioeuropees","Prats calcíoles i mesòfils amb Festuca n","Prats acidòfils i mesòfils, amb Agrostis",
          "Prats silicícoles i mesòfils, amb dominà","Boscos de roure sessiliflor (Quercus pet","Vernedes amb Carex remota, que es fan a ",
          "Rosalia alpina","Austropotamobius pallipes (Lereboullet, ","Calotriton arnoldi") #Remove habitats and species locked in. They have SDM but we will use observations
list_sdm<-list_sdm[! names(list_sdm) %in% remove]

#Change name of Aquila pennata to Hieraaetus pennatus
names(list_sdm)[names(list_sdm) == "Aquila pennata"] <- "Hieraaetus pennatus"

list_sdm <- pblapply(list_sdm, function(x) { x[x<0.6] <- 0; return(x) }) #0.6 to 0, not to sobreestimate the SDM

#### HICs
filenames <- list.files(paste0(dir_export, "/HIC_RASTER/asc"), pattern="*.asc", full.names=T)
list_HIC <- pblapply(filenames, raster)
names(list_HIC) <- sub(".asc$", "", filenames %>% basename %>% file_path_sans_ext)

pos <- match("9100", names(list_HIC))
names(list_HIC)[pos] <- "91E0" #Change 9100 to 91E0, for concordance.

#### Boscos madurs
bosc_madur <- raster(paste0(dir_export, "/BOSCOS_MADURS/r_boscos_madurs.asc"))
bosc_madur <- raster::reclassify(bosc_madur, cbind(NA, 0))

#### Catalegs 
filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/asc/flora_amen"), pattern="*.asc", full.names=T)
list_flora_amen <- pblapply(filenames, raster)
names(list_flora_amen) <- sub(".asc$", "", filenames %>% basename %>% file_path_sans_ext)

#### Nius 
filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/shp/NIUS/subconques/"), pattern="*.shp", full.names=T)
cl <- makeCluster(numCores) # create cluster
clusterExport(cl=cl, varlist="filenames") # import the objects and functions for the paralelization
clusterEvalQ(cl, library(raster)) # VERY IMPORTANT to call the libraries too
list_nius <- parLapply(cl, filenames, shapefile)
stopCluster(cl) # stop paralelizing
names(list_nius) <- sub(".shp$", "", filenames %>% basename %>% file_path_sans_ext)
names(list_nius)[30]<-"Aegithalos caudatus" #Canviem el nom de mallerenga cuallarga al nom científic. 



#OPEN THE EXCEL
abans<-read_excel(paste0(dir, "/documents/NOVA_taula_zonacio_msy.xlsx"), sheet="elements_montseny")
zonacio_df<-read_excel(paste0(dir, "/documents/taula_zonacio_MSY_230512_v5 .xlsx"), sheet = "elements_montseny_V4v")
zonacio_df2<-zonacio_df
#duplicates <- despres$element[duplicated(zonacio_df$element)] #search duplicates. Aplicate setdiff to the element column, we want the new table to contain the same names as the ancient

#Aconseguir que la nova taula tingui els mateixos noms i valors que la taula antiga.
noms<-names(abans)
merged_df <- merge(zonacio_df, abans[, c("element", "tipus_informacio", "area_ocupacio (ha)", "num_PUs")], by = "element", all.x = TRUE)

columns_to_remove <- grep("\\.x$", names(merged_df), value = TRUE)
merged_df <- merged_df[, !(names(merged_df) %in% columns_to_remove)]

names_to_change <- grep("\\.y$", names(merged_df), value = TRUE)
new_names <- sub("\\.y$", "", names_to_change)

names(merged_df)[names(merged_df) %in% names_to_change] <- new_names
zonacio_df <- merged_df[, noms]



#### Locked-in
# sp_lockin: Calotriton arnoldi, Rosalia alpina, Austropotamobius pallipes and Buxbaumia viridis
filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/shp/LOCK_IN"), pattern="*.shp", full.names=T)
list_conques_lockin <- pblapply(filenames, shapefile, verbose=F)
names(list_conques_lockin) <- sub(".shp$", "", filenames %>% basename %>% file_path_sans_ext)
#list_conques_lockin<-list_conques_lockin[-1] W use this line to delete Austrapotamobius pallipes

#### Locked-out
lock_out <- raster(paste0(dir_export, "/CONSTRAINTS_RASTER/asc/grid_all_constraints_100m.asc")) # we have to previously create a raster layer with the same resolution and binary info to lock out or not.
lock_out_v <- shapefile(paste0(dir_export, "/CONSTRAINTS_RASTER/shp/grid_all_constraints_100m.shp")) # G: official EBBA2 countries and masks

# If constraints layer is a vector, we can do the following: locked_out_raster <- sim_pu_polygons[sim_pu_polygons$locked_out == 1, ]

##  Costs for grid ----
cost_grid <- raster(paste0(dir_export, "/COSTS_RASTER/asc/cost_total_amb_public.asc")) 

load("C:/Users/david.munoz/Downloads/Pla_Proteccio_MSY-main/Pla_Proteccio_MSY-main/environment_cost_public_privat.RData")
cost_public_r <- rasterize(list_costs[["cost_public"]], grid_100m_r)
cost_public_r[cost_public_r >= 1] <- 0.2 # this layer weights (+) the final cost layer
cost_public_r[is.na(cost_public_r)] <- 2  # this layer weights (-) the final cost layer
grid_all_costs_public <- grid_all_costs * cost_public_r # to weight the stack
cost_grid_changed<-grid_all_costs_public


##  Set projections  ----------------------------------------------------------

### Projections to use
proj_EPSG25831 <- crs(ambit_PN) # think if using the crs of a known layer or a standard one, as the next line of code 
proj_EPSG4326 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # typical world projection called WGS 84, synonim of EPSG:4326

### Reproject objects

fun_reproject <- function(x) {
  
  if(identical(crs(x), proj_EPSG25831)) {
    y <- spTransform(x, proj_EPSG25831)
    return(y)
  }
}

ambit_RB <- spTransform(ambit_RB, proj_EPSG25831) # It doesn't work with ambit_RB so I do it without the function

#### Set crs to unique rasters
crs(lock_out) <- proj_EPSG25831
crs(lock_out_v) <- proj_EPSG25831
crs(grid_100m_r) <- proj_EPSG25831
crs(PU_conques) <- proj_EPSG25831
crs(grid_richness_no_SDM) <- proj_EPSG25831
crs(cost_grid) <- proj_EPSG25831
crs(cost_grid_changed) <- proj_EPSG25831
crs(bosc_madur) <- proj_EPSG25831
crs(finques_pub) <- proj_EPSG25831

#### Set crs rasters from lists
crs_fun <- function(x) { crs(x) <- proj_EPSG25831; return(x) }

list_obs <- pblapply(list_obs, crs_fun)
list_sdm <- pblapply(list_sdm, crs_fun)
list_nius <- pblapply(list_nius, crs_fun)
list_HIC <- pblapply(list_HIC, crs_fun)
list_flora_amen <- pblapply(list_flora_amen, crs_fun)
list_conques_lockin <- pblapply(list_conques_lockin, crs_fun)

##  Edit features, planning units and add cost ----------------------------------------------------------

### Grid

grid_100m_PN_r <- crop(grid_100m_r, extent(ambit_PN)) # crop on extent
grid_100m_PN_r <- mask(grid_100m_PN_r, mask=ambit_PN) # mask the grid on the PN ambit

PU_grid <- rasterToPolygons(grid_100m_PN_r) # vectorize raster to grid polygon
PU_grid <- intersect(PU_grid, ambit_PN) # crop the layer to the shape of the PN
PU_grid <- PU_grid[PU_grid$SHP == 0,] # delete the holes

grid_100m_PN_r <- rasterize(PU_grid, grid_100m_PN_r, 'SHP')
grid_100m_PN_r[grid_100m_PN_r==0] <- 1
grid_100m_PN_r <- crop(grid_100m_PN_r, extent(PU_grid)) # crop on extent

PU_grid <- rasterToPolygons(grid_100m_PN_r) # we transform to raster again to have the same number of cells (30901)
PU_grid@data$id <- 1:nrow(PU_grid@data)

#### Use interesting columns and remove holes
PU_grid <- intersect(PU_grid, ambit_PN) # crop again the layer to the shape of the PN because there are a lot of cells that are not quadrants
PU_grid <- PU_grid[PU_grid$SHP == 0,] # delete the holes again

#PU_grid@data[, c("layer" ,"SHP")] <- list(NULL) # remove non-interesting columns

### Subconques

colnames(PU_conques@data)[colnames(PU_conques@data) %in% "ID_GRAFIC"] <- "id"
PU_conques@data$id <- as.character(PU_conques@data$id) 

PU_conques <- intersect(PU_conques, ambit_PN)  # crop the layer to the shape of the PN
PU_conques <- PU_conques[PU_conques$SHP == 0, ] # delete the holes

#### Add column of costs
PU_conques <- raster::extract(cost_grid, PU_conques, method="simple", fun=mean, sp=T)
PU_conques <- raster::extract(cost_grid_changed, PU_conques, method="simple", fun=mean, sp=T)

names(PU_conques@data)[which(names(PU_conques@data) %in% c("cost_total_amb_public", "layer"))] <- c("public_4", "public_10") #zona_1 costs 2*zona_2.


### Nius
list_nius <- pblapply(list_nius, raster::rasterize, grid_100m_r, 'presence')  # crop the layer to the shape of the PN

list_nius <- pblapply(list_nius, function(x) {
  x[is.na(x)] <- 0
  return(x)
})

raster_masked_list <- pblapply(list_nius, function(x) { #emmascarem els rasters amb la forma del montseny
  r_masked <- mask(x, ambit_PN) #Remove the birds with nests out of the PN. 
  return(r_masked)
}) 

filter_nius <- pbsapply(raster_masked_list, function(x) { return(1 %in% values(x)) })
list_nius <- raster_masked_list[filter_nius] # remove empty raster nius

##  Create connectivity   ----------------------------------------------------------

### Dataframe framework to find the euclidian distance between the centroid of each cell and its 8 neighbours

centroids <- gCentroid(PU_grid, byid=TRUE) # find centroids

coords <- coordinates(centroids) # extract the coordinates of the centroids

id2 <- poly2nb(PU_grid, queen=T) # to find the id of the neighbour centroid

dsts <- nbdists(id2, coords) # find the distance from id to id2
dsts <- unlist(dsts) # unlist distances to insert into the df
dsts <- dsts / 100 # from meters to hectometers. This way, the range values are similar to the range of connectivity 
dsts <- 1/dsts # we do the inverse to find connectivity (which is higher on closer squares)

length_id1 <- sapply(id2, function(x) { length(x) }) # how many neighbours do each id have
id1 <- rep(PU_grid$id, length_id1) # vector of id names repeated
id2 <- unlist(id2) # unlist id2 to insert into the df

id2[id2==0] <- 2634 # little change on one planning zone that wasn't touching any planning zone
dsts <- append(dsts, 1/(149/1000), after=21636) # little change on one planning zone that wasn't touching any planning zone

connect_grid <- data.frame(id1=id1, id2=id2, boundary=dsts)

rm(centroids, coords, id2, dsts, length_id1, id1)

#### Find the euclidean distance between the centroid of the subconques

centroids_subsubconques <- gCentroid(PU_conques, byid=TRUE) # find centroids

touching <- gTouches(PU_conques, byid=TRUE)

colnames(touching) <- PU_conques$ID_GRAFIC
rownames(touching) <- PU_conques$ID_GRAFIC

gdis <- pointDistance(centroids_subsubconques, lonlat=F) # find the distances between all centroids
connect_conques <- gdis*touching # we only want the distances of the adjacent polygons
connect_conques[connect_conques==0] <- NA # we only want the distances of the adjacent polygons
connect_conques <- connect_conques / 100 # from meters to hm to have a range similar to the area_costs
connect_conques <- 1/connect_conques # we do the inverse to find connectivity (which is higher on closer conques)
connect_conques <- round(connect_conques, 3)
connect_conques[is.na(connect_conques)] <- 0 # we only want the distances of the adjacent polygons

print(connect_conques[1:10, 1:10]) # to visualize

rm(centroids_subsubconques, touching, gdis)

##  Create and integrate elements  ----------------------------------------------------------
#### Little edit of "bosc madur"
bosc_madur <- as.list(bosc_madur)
names(bosc_madur) <- "bosc_madur"


hierarchy <- list(list_nius, list_flora_amen, list_HIC, list_sdm, list_obs, bosc_madur) # Define the hierarchy of the lists

merged_list <- list()# Initialize the merged list

# Loop over the hierarchy and add elements to merged_list if they are not already present
for (lst in hierarchy) {
  for (name in names(lst)) {
    if (!(name %in% names(merged_list))) {
      merged_list[[name]] <- lst[[name]]
    }
  }
}

names(merged_list) <- str_trim(names(merged_list), side = "right") #We remove blank spaces
duplicated_names <- names(merged_list)[duplicated(names(merged_list))] #And search for duplicates. Only flora_amen are repeated.

first_occurrences <- !duplicated(names(merged_list)) #We select the first time that a name appears in the list (using the previous hierarchy) 

list_features<- merged_list[first_occurrences] #And remove the following appearences. 
list_features <- list_features[order(names(list_features))]


## Change names of certain habitats. 
new_names <- c("41.774", "22.3417","35.23","54.112","22.422","24.43","44.637+","37.26+","42.132","37.22",
               "44.515+","38.112","44.62","35.124+","36.317+","22.414","38.23","22.441","41.2A+",
               "22.442","44.3","44.316+","22.3414","38.24+","42.A75","44.3432+","44.3431+","34.32611+",
               "22.3233","44.128+")

# select the elements to change by name
names_to_change <- c("Boscos de roure africà (Quercus canarien","Comunitats amb Anagallis tenella o altre","Comunitats de Corynephorus canescens, de",
                     "Comunitats fontinals sovint dominades pe","Comunitats submerses d'herbes petites o","Comunitats submerses, amb Potamogeton de",
                     "Freixenedes de Fraxinus angustifolia, de","Herbassars amb cua de cavall (Equisetum","Avetoses del territori de les fagedes, a",
                     "Jonqueres de Juncus acutiflorus, acidòfi","Lloredes o vernedes amb llor (Laurus nob","Prats amb Cynosurus cristatus, mesòfils,",
                     "Omedes de terra baixa","Prats acidòfils i mesòfils, amb Agrostis","Prats silicícoles i mesòfils, amb dominà",
                     "Poblaments d’Utricularia vulgaris o U. a","Prats dalladors amb fromental (Arrhenath","Poblaments submersos d'asprelles (Chara",
                     "Boscos de roure sessiliflor (Quercus pet","Poblaments submersos de Nitella, de bass","Vernedes medioeuropees",
                     "Vernedes amb Carex remota, que es fan a","Pradells terofítics de petites serranes","Prats dalladors, generalment amb Gaudini",
                     "Teixedes ibèriques","Vernedes (de vegades pollancredes) amb o","Vernedes (i pollancredes) amb Circaea lu",
                     "Prats calcíoles i mesòfils amb Festuca n","Pradells de teròfits amb dominància d'al","Gatelledes (boscos, generalment baixos,")

pos <- match(names_to_change, names(list_features))
names(list_features)[pos] <- new_names

list_features <- list_features[order(names(list_features))] #Reorder the list using the new names

##Check if table names and list_features are the same.
noms_features<-as.vector(names(list_features))

zonacio_df$element <- substr(zonacio_df$element, 1, 40)
zonacio_df$element <- str_trim(zonacio_df$element, side = "right")
noms_taula<-zonacio_df$element
noms_taula<-noms_taula[order(noms_taula)]

diferencies<-setdiff(noms_features, noms_taula)

list_features <- list_features[!names(list_features) %in% diferencies] #Remove all these names

list_features <- pblapply(list_features, crop, extent(ambit_PN))
list_features <- pblapply(list_features, mask, ambit_PN)

list_features <- pblapply(list_features, function(x) { x[x>0] <- 1; return(x) }) #0.5 to 1 (SDM previously are 0 and 1.)

### Remove empty features
filter_features <- pbsapply(list_features, function(x) { cellStats(x, max, na.rm=T) != 0 }) # we remove elements that are all 0 on the study area
list_features <- list_features[filter_features]

##  Create lock-in and lock-out   ----------------------------------------------------------

### Lock-in

bloquejades_tot <- zonacio_df[zonacio_df$bloquejada_zonacio == 1 & complete.cases(zonacio_df$bloquejada_zonacio), ] #Warning: it is better that there are no SDM here
remove<-c("Austropotamobius pallipes (Lereboullet,","Rosalia alpina","Buxbaumia viridis", "Calotriton arnoldi") #We remove this 4 species as the occupied conques were calculated before.
bloquejades<-bloquejades_tot[!bloquejades_tot$element %in% remove,] 
list_conques_2<- list_features[bloquejades$element]

for (i in 1:length(list_conques_2)) {
  polygon_values <- raster::extract(list_conques_2[[i]], PU_conques, na.rm = TRUE)
  layer_values <- as.numeric(sapply(polygon_values, function(x) any(x == 1)))
  layer_values[is.na(layer_values)] <- 0
  
  list_conques_2[[i]] <- PU_conques
  list_conques_2[[i]]$PRESENCIA <- layer_values
  print(i)
}

#list_conques_2 <- list_conques_2[-12] #Here we remove the big "VERNEDES"

#### Conques
list_conques_lockin <- pblapply(list_conques_lockin, intersect, ambit_PN)
list_conques_lockin <- pblapply(list_conques_lockin, function(x) { return(x[x$SHP == 0, ]) })
list_conques_lockin <- pblapply(list_conques_lockin, function(x) { x[is.na(x$PRESENCIA), "PRESENCIA"] <- 0; return(x) }) # presence and absences as 0 and 1s

llista_lock_in_tot<-c(list_conques_lockin, list_conques_2)

PU_conques@data$locked_in <- NA
PU_conques$locked_in <- rowSums(sapply(llista_lock_in_tot, `[[`, "PRESENCIA")) # we create a matrix with each species as column and then we sum the rows of the matrix 
PU_conques$locked_in <- as.logical(PU_conques$locked_in) # transformed as logical because it is the standard way for 'lock in' procedure

### Lock-out

#### Conques
PU_conques$lock_out <- raster::extract(lock_out, PU_conques, method="bilinear", fun=mean, sp=F, na.rm=T)[, 1] #NA.RM added.
PU_conques$lock_out <- ifelse(PU_conques$lock_out < 0.5, T, F) # here we are saying that if more that half the area is covered by transport or urban area, set them to lock-out.
table(PU_conques@data$lock_out) # we just have locked-out two subconques


##  Problem 1  ----------------------------------------------------------

load("C:/Users/david.munoz/Downloads/Pla_Proteccio_MSY-main/Pla_Proteccio_MSY-main/workspace_canvi_vernedes.RData")

targets <- c(0.6, 0.4, 0) #Targets of the protected features. In zona estricta, zona laxa and zona perifèrica (always 0)
zonacio_df <- zonacio_df %>% mutate(inclosa_zonacio = ifelse(element == "91E0", 0, inclosa_zonacio)) #Change status of vernedes

change_zone<-c("Parvospeonomus canyellesi Lagar, 1974") #,"Pinguicula vulgaris","Emberiza citrinella","Plecotus auritus","Circus pygargus","37.22") #Then, species vulnerables that are in high % in lock in, we change them to level 4 in status.

elements_conserv <- zonacio_df
elements_conserv <- elements_conserv[is.na(elements_conserv$bloquejada_zonacio)|elements_conserv$bloquejada_zonacio==0, ] #Remove lock_in species
elements_conserv <- elements_conserv[elements_conserv$`area_ocupacio (ha)`!=0, ] #Remove species that are out
elements_conserv <- elements_conserv[elements_conserv$tipus_informacio !="Sense_dades", ] #Remove species of which we do not have spatial data
elements_conserv <- elements_conserv[elements_conserv$inclosa_zonacio==1, ]
elements_conserv$estatus_zonacio[elements_conserv$element %in% change_zone & elements_conserv$estatus_zonacio == 3] <- 4 #dues espècies, una de valor 3 i una de valor 4 ocupen NOMÉS la mateixa PU. És incompatible que la PU sigui zona 1 i 2 alhora.

elements_conserv <- elements_conserv[order(elements_conserv$element),]
elements_conserv_name <- elements_conserv$element # we extract the name of elements with conservation status

features<-names(list_features) %in% elements_conserv_name
features <- list_features[features]
stack_features<-stack(features)

# Set up a parallel backend using all available cores
cl <- makeCluster(detectCores())
registerDoParallel(cl)

calculate_coverage <- function(layer) {
  valors <- raster::extract(layer, PU_conques[,"locked_in"], fun=sum, sp=T)
  covered <- sum(valors@data[,2][valors@data$locked_in], na.rm = T)
  all <- sum(valors@data[,2], na.rm=T)
  covered <- covered/all
  return(covered)
}

cov_list <- foreach(i = 1:nlayers(stack_features), .combine = c) %dopar% {
  calculate_coverage(stack_features[[i]])
}

stopCluster(cl)
elements_conserv$cobert_lock_in <- cov_list

elements_conserv$estatus_zonacio<- ifelse(is.na(elements_conserv$estatus_zonacio) & elements_conserv$'tipus zona' == "Protecció Laxa", 3, elements_conserv$estatus_zonacio)
elements_conserv$estatus_zonacio<- ifelse(elements_conserv$estatus_zonacio==4 & elements_conserv$`tipus zona`=="Protecció Laxa", 3, elements_conserv$estatus_zonacio)
elements_conserv$estatus_zonacio[elements_conserv$estatus_zonacio==1|elements_conserv$estatus_zonacio==2]<-3

tots_elements<-elements_conserv #We save all the elements selected to add them to a final table, because there are features that will not be used in the exercice as their targets are accomplished by lock_in

elements_conserv <- elements_conserv[!(elements_conserv$estatus_zonacio ==3 & elements_conserv$cobert_lock_in > targets[2]),] #remove features with value of 3 if its targets are accomplished by lock_in
elements_conserv <- elements_conserv[!(elements_conserv$estatus_zonacio == 4 & elements_conserv$cobert_lock_in > targets[1]),] #remove features with value of 4 if its targets are accomplished by lock_in
elements_conserv_name <- elements_conserv$element

elements_coberts<-tots_elements[!tots_elements$element %in% elements_conserv$element,]  

features<-names(list_features) %in% elements_conserv_name #Delete elements that are already covered by lock_in
features <- list_features[features]
stack_features<-stack(features)

#Targets
elements_conserv$zona_1<-ifelse(elements_conserv$estatus_zonacio == 4, targets[1], 0) #Zona 1 will be the targets that we should accomplish after resting final_targets-already covered by lock_in
elements_conserv$zona_2<-ifelse(elements_conserv$estatus_zonacio == 3, targets[2]-elements_conserv$cobert_lock_in, 0) #Zona 2 will be the targets that we should accomplish after resting final_targets-already covered by lock_in
#elements_conserv$zona_1<-ifelse(elements_conserv$element=="91E0", target_vernedes-elements_conserv$cobert_lock_in, elements_conserv$zona_1)
elements_conserv$zona_3<-0
targets<-dplyr::select(elements_conserv, zona_1, zona_2, zona_3) #We invent targets, if status_zonacio =4, targets will be 0.5 in the strict zone, if its 4, target will be 0.25 in the lax zone

targets<-as.matrix(targets)
rownames(targets)<-elements_conserv$element

#Costs
PU_conques@data$cost_meitat_public_4 <- PU_conques@data$public_4 / 2
PU_conques@data$cost_meitat_public_10 <- PU_conques@data$public_10 / 2
PU_conques@data$cost0 <- rep(0, nrow(PU_conques@data))

#Lock in and lock_out
locked_in<-PU_conques@data$locked_in
locked_in<-data.frame(zona_1=locked_in, zona_2=F, zona_3=F) #All locked in zones must be at zona 1 (strict)
locked_in<-as.matrix(locked_in)

locked_out<-PU_conques@data$lock_out
locked_out<-data.frame(zona_1=F, zona_2=locked_out, zona_3=F) #Locked out zones are locked out from zona 1 and zona 2. 
locked_out<-as.matrix(locked_out)

#Zones. En aquest cas, les dues zones són el mateix stack, tal i com surt als tutorials. 
zones <- zones(stack_features, stack_features, stack_features, zone_names = c("zona_1", "zona_2","zona_3"), feature_names = elements_conserv_name)

#Boundary penalties
clumping <- matrix(c(0.7, 0.5, -0.7, 0.5, 0.5, 0, -0.7, 0, 1), nrow = 3, ncol = 3) #Zona 1 should be very clumped, zona 2 a little less, and between zona 1 and zona 2, less. 
print(clumping)

nom<-"public10_alts_mitjana"

p1 <- problem(PU_conques, zones, cost_column = c("public_10", "cost_meitat_public_10", "cost0")) %>% #We have to change these columns if we want to change the costs layer.
  add_min_set_objective() %>% # we minimize the cost of the solution whilst ensuring that all targets are met
  add_relative_targets(targets) %>% # we add the specific targets that we want to meet (between 0 and 1)
  add_locked_in_constraints(locked_in) %>% # here we lock in the places there are highly important features
  add_locked_out_constraints(locked_out) %>% # here we lock out the places that are very occupated by humans
  add_binary_decisions() %>% # binary decision of protecting or not protecting certain planning unit
  add_gurobi_solver(gap = 0.05, time_limit = 120, verbose = T) %>%
  add_boundary_penalties(penalty = 1, zones = clumping, data = connect_conques) # Here we penalize solutions that are fragmented and not connected..

print(p1)

#### Solve problem

s1<- solve(p1)

#### plot solution

#Plot in two colors: green
my_height <- nrow(grid_100m_r)*3
my_width <- ncol(grid_100m_r)*3

s1_sf <- st_as_sf(s1)

s1_sf$fill_color <- ifelse(s1_sf$solution_1_zona_1 == 1, "darkgreen", "white") # Create a variable for the fill color based on the solution_1_zona_1 and solution_1_zona_2 columns
s1_sf$fill_color <- ifelse(s1_sf$solution_1_zona_2 == 1, "darkolivegreen1", s1_sf$fill_color)

# Plot the polygons with different fill colors based on the solution_1_zona_1 and solution_1_zona_2 columns
png(file=paste0(dir_export, "/SOLUCIONS_MONTSENY/PROPOSTA_1/",nom,".png"), width=my_width, height=my_height)
par(mar=c(1,1,1,1))
ggplot() +
  geom_sf(data = s1_sf, aes(fill = fill_color)) +
  ggtitle(nom) +
  scale_fill_manual(name = "Zones",
                    values = c("darkgreen", "darkolivegreen1","white"),
                    labels = c("Protecció estricta", "Protecció laxa","No protecció específica"))
dev.off()
## plot also locked_in in black

s1_sf <- st_as_sf(s1)

s1_sf$fill_color <- ifelse(s1_sf$solution_1_zona_1 == 1, "darkgreen", "white")
s1_sf$fill_color <- ifelse(s1_sf$solution_1_zona_2 == 1, "darkolivegreen1", s1_sf$fill_color)
s1_sf$fill_color <- ifelse(s1$locked_in == TRUE, "black", s1_sf$fill_color)

png(file=paste0(dir_export, "/SOLUCIONS_MONTSENY/PROPOSTA_1/",nom,"_bloquejades.png"), width=my_width, height=my_height)
par(mar=c(1,1,1,1))
ggplot() + 
  geom_sf(data = s1_sf, aes(fill = fill_color)) +
  ggtitle(paste0(nom,"_bloquejades")) +
  scale_fill_manual(name = "Zones", 
                    values = c("black","darkgreen", "darkolivegreen1", "white"), 
                    labels = c("Bloquejades","Protecció estricta", "Protecció laxa", "Sense protecció específica"),
                    drop = FALSE)
dev.off()

## PLOT AMB FINQUES PÚBLIQUES 
s1_sf$fill_color <- ifelse(s1_sf$solution_1_zona_1 == 1, "darkgreen", "white")
s1_sf$fill_color <- ifelse(s1_sf$solution_1_zona_2 == 1, "darkolivegreen1", s1_sf$fill_color)

s2_sf <- st_as_sf(finques_pub)
s2_sf <- mutate(s2_sf, fill_color = "red")

png(file=paste0(dir_export, "/SOLUCIONS_MONTSENY/PROPOSTA_1/",nom,"_finques_publiques.png"), width=my_width, height=my_height)
par(mar=c(1,1,1,1))
ggplot() + 
  geom_sf(data = s1_sf, aes(fill = fill_color)) +
  ggtitle(paste0(nom,"_finques_publiques")) +
  geom_sf(data = s2_sf, aes(fill = fill_color), alpha = 0.3) +
  scale_fill_manual(name = "Zones", 
                    values = c("darkgreen", "darkolivegreen1", "red", "white"), 
                    labels = c("Protecció estricta", "Protecció laxa", "Finques públiques", "Sense protecció específica"),
                    drop = FALSE)
dev.off()

#### Checks
nPU_protegides<-eval_n_summary(p1, s1[, c("solution_1_zona_1", "solution_1_zona_2", "solution_1_zona_3")])
cost<-eval_cost_summary(p1, s1[, c("solution_1_zona_1", "solution_1_zona_2", "solution_1_zona_3")]) # Calculate the total cost of a solution.
r2<-eval_target_coverage_summary(p1, s1[, c("solution_1_zona_1", "solution_1_zona_2", "solution_1_zona_3")]) # Calculate how well feature representation targets are met by a solution.
colnames(r2)[1]<-"element"
connectivitat<-eval_connectivity_summary(p1, s1[, c("solution_1_zona_1", "solution_1_zona_2", "solution_1_zona_3")], data = connect_conques) # Calculate the connectivity held within a solution. Using asymetric function, the result is exactly the same.
boundary<-eval_boundary_summary(p1, s1[, c("solution_1_zona_1", "solution_1_zona_2", "solution_1_zona_3")]) #spatial fragmentation of each zone

## CÀLCUL DELS TARGETS ACONSEGUITS

junt<-dplyr::inner_join(elements_conserv, r2, by="element") #Join the 2 dataframe
junt$zone_extracted <- sapply(junt$zone, paste, collapse = ",")

by_features <- aggregate(relative_held ~ element + estatus_zonacio + zone_extracted+cobert_lock_in, data = junt, sum) # Aggregate by element, estatus_zonacio, and the extracted zone values

#Preparem un dataframe nou
unique_elements <- unique(by_features$element) 

df_desired <- data.frame(
  element = character(length(unique_elements)),
  estatus_zonacio = numeric(length(unique_elements)),
  zona_1 = numeric(length(unique_elements)),
  zona_2 = numeric(length(unique_elements)),
  suma = numeric(length(unique_elements)),
  stringsAsFactors = FALSE
)

for (i in seq_along(unique_elements)) {
  element <- unique_elements[i]
  df_element <- by_features[by_features$element == element, ]
  df_desired[i, "element"] <- element
  df_desired[i, "estatus_zonacio"] <- df_element$estatus_zonacio[df_element$zone_extracted == "zona_1"]
  df_desired[i, "zona_1"] <- df_element$relative_held[df_element$zone_extracted == "zona_1"]
  df_desired[i, "zona_2"] <- df_element$relative_held[df_element$zone_extracted == "zona_2"]
  df_desired[i, "suma"] <- df_element$relative_held[df_element$zone_extracted == "zona_3"]
}

df_desired$suma<-df_desired$zona_1+df_desired$zona_2
by_features<-df_desired

elements_NO_target <- subset(elements_conserv, select = -c(zona_1, zona_2, zona_3, cobert_lock_in)) #Per a unir-ho amb la resta de columnes de la taula gran (per als features que han fet l'exercici)

by_features<-dplyr::full_join(by_features, elements_NO_target, by=c("element", "estatus_zonacio"))

#Ara ho voldrem ajuntar amb els features que ja quedaven coberts per lock in
names(elements_coberts)[names(elements_coberts) == "cobert_lock_in"] <- "zona_1" #Preparem la taula
elements_coberts$zona_2<-rep(0, nrow(elements_coberts))
elements_coberts$suma<-rowSums(elements_coberts[,c("zona_1","zona_2")], na.rm=TRUE)

by_features<-dplyr::union(by_features,elements_coberts) #I les ajuntem

#Ara afegim els features bloquejats. 
bloquejades_tot$zona_1<-rep(1, nrow(bloquejades_tot))
bloquejades_tot$zona_2<-rep(0, nrow(bloquejades_tot))
bloquejades_tot$suma<-rep(1, nrow(bloquejades_tot))

by_features<-dplyr::union(by_features, bloquejades_tot)

#CÀLCUL DE L'ÀREA PROTEGIDA AL MONTSENY, EN ZONES PÚBLIQUES I PRIVADES

s1_sf <- st_as_sf(s1)
s2_sf <- st_as_sf(finques_pub)

s1_s2_intersect_sf <- st_intersection(s1_sf, s2_sf)

zona_1_publiques <- sum(st_area(filter(s1_s2_intersect_sf, solution_1_zona_1 == 1)))/10000
zona_2_publiques <- sum(st_area(filter(s1_s2_intersect_sf, solution_1_zona_2 == 1)))/10000

zona_1 <- sum(st_area(filter(s1_sf, solution_1_zona_1 == 1)))/10000
zona_2 <- sum(st_area(filter(s1_sf, solution_1_zona_2 == 1)))/10000

area_PN <- sum(st_area(s1_sf))/10000

taula <- data.frame(
  "Àrea total" = c(zona_1, zona_2),
  "Àrea pública" = c(zona_1_publiques, zona_2_publiques),
  "percentage públic" = c(zona_1_publiques/zona_1, zona_2_publiques/zona_2),
  "percentatge PN" = c((zona_1/area_PN), (zona_2/area_PN))
)
taula <- apply(taula, 2, as.numeric)
rownames(taula)<-c("Zona_1", "Zona_2")

options(scipen=999)

print(taula)
area_total_protegida<-c(sum(taula[,"percentatge.PN"]),sum(taula[,"Àrea.total"]))
print(area_total_protegida)



##  Calculate the area covered for all features. Ancient, but it has to run. ----

stack_all_features<-stack(list_features) #stack amb totes les espècies

taula_cobriment <- data.frame(element = names(list_features), #Create a new df to fill later
                              zona_1 = rep(0, nlayers(stack_all_features)),
                              zona_2 = rep(0, nlayers(stack_all_features))
)


for (i in 1:nlayers(stack_all_features)) {
  raster_subset <- subset(stack_all_features, i)  # Select the first layer of the stack
  
  raster_subset[raster_subset == 0] <- NA #change the 0 to NA
  raster_polygons <- raster::rasterToPolygons(raster_subset, dissolve = TRUE) #Polygonize the raster
  raster_polygons <- as(raster_polygons, "SpatialPolygons")  # Convert to SpatialPolygons
  
  zona1 <- s1[s1$solution_1_zona_1 == "1",] #Select zona 1 and 2. We have to put the result that we want, here.
  zona2 <- s1[s1$solution_1_zona_2 == "1",]
  
  intersection_1 <- tryCatch(intersect(zona1, raster_polygons), error = function(e) {NULL}) #Intersect, but if they can not be intersected, do not give an error
  intersection_2 <- tryCatch(intersect(zona2, raster_polygons), error = function(e) {NULL})
  
  if (is.null(intersection_1)) { #If there is no intersection 0. If there is intersection, then put the sum of the area
    total_area_1 <- 0
  } else {
    total_area_1 <- sum(area(intersection_1))
  }
  
  if (is.null(intersection_2)) {
    total_area_2 <- 0
  } else {
    total_area_2 <- sum(area(intersection_2))
  }
  
  total_raster <- sum(cellStats(raster_subset, sum) * res(raster_subset)[1]^2) #Calculate the sum of the surface occupied by the species
  
  area_final_1 <- total_area_1 / total_raster #And the proportion that is protected
  area_final_2 <- total_area_2 / total_raster
  
  if (is.null(intersection_1)) { #Put it on the table
    taula_cobriment$protegit_zona1[i] <- 0
  } else {
    taula_cobriment$protegit_zona1[i] <- area_final_1
  }
  
  if (is.null(intersection_2)) {
    taula_cobriment$protegit_zona2[i] <- 0
  } else {
    taula_cobriment$protegit_zona2[i] <- area_final_2
  }
  print(i)
}

taula_cobriment$suma<-taula_cobriment$protegit_zona1+ taula_cobriment$protegit_zona2 #Create the sum
taula_cobriment<-subset(taula_cobriment[,-c(2:3)])
names(taula_cobriment)<-c("element", "zona_1", "zona_2", "suma")


#Change the value of Rosalia alpina and cranc de riu because they are introduced as lockin, and the method of the calculus was different. Checked!
# matching_elements <- match(by_features$element, by_features_all$element)
# difference <- by_features_all$suma[matching_elements] - by_features$suma
# difference_df <- data.frame(element = by_features$element, difference = difference)

by_features_all<-dplyr::full_join(zonacio_df, taula_cobriment, by="element") #Connect the two tables by element
by_features_all <- by_features_all %>% 
  mutate(zona_1 = ifelse(element == "Austropotamobius pallipes (Lereboullet," | element == "Rosalia alpina", 1, zona_1),
         zona_2 = ifelse(element == "Austropotamobius pallipes (Lereboullet," | element == "Rosalia alpina", 0, zona_2),
         suma = ifelse(element == "Austropotamobius pallipes (Lereboullet," | element == "Rosalia alpina", 1, suma)) 


by_features_final<-by_features_all[by_features_all$element %in% by_features$element,] 
by_features_final$estatus_zonacio<-ifelse(by_features_final$estatus_zonacio=="1"| by_features_final$estatus_zonacio=="2" | is.na(by_features_final$estatus_zonacio), 3,by_features_final$estatus_zonacio)
by_estatus <- aggregate(suma ~ estatus_zonacio, data = by_features_final, mean) #Mitjana dels targets aconseguits amb l'exercici. SI VOLEM TENIR EN COMPTE LES LOCK IN, POSAR 10 LÍNIES MÉS AVALL!!!!!
print(by_estatus) 



#save by_features_all
write.xlsx(by_features_all, paste0(dir_export, "/SOLUCIONS_MONTSENY/PROPOSTA_1/ALL_ELEMENTS_PROPOSTA_1.xlsx")) 

#Save by_features

by_features_final$bloquejada_zonacio<-ifelse(is.na(by_features_final$bloquejada_zonacio), 0, by_features_final$bloquejada_zonacio)
by_features_final$estatus_zonacio<-ifelse(is.na(by_features_final$estatus_zonacio), 4, by_features_final$estatus_zonacio) #for 1 habitat without estatus_zonacio
by_features_final$estatus_zonacio<-ifelse(by_features_final$estatus_zonacio==3 & by_features_final$bloquejada_zonacio==1, 4, by_features_final$estatus_zonacio)

write.xlsx(by_features_final, paste0(dir_export,"/SOLUCIONS_MONTSENY/PROPOSTA_1/SELECTED_ELEMENTS_PROPOSTA_1.xlsx"))

#SAVE SHP
solucio <- subset(s1, select = c("solution_1_zona_1", "solution_1_zona_2"))                                       
solucio$solution_1_zona_1[solucio$solution_1_zona_2 == 1 & solucio$solution_1_zona_1 == 0] <- 2 #Si zona estricta és 0 i zona laxa és 1, a la zona estricta li posem un 1.
solucio<-subset(solucio, select = "solution_1_zona_1")

names(solucio)<-"solucio"
solucio$solucio <- ifelse(solucio$solucio == 2, "ZIC", ifelse(solucio$solucio == 1, "ZRN", solucio$solucio))
writeOGR(solucio, dsn=paste0(dir_export,"/SOLUCIONS_MONTSENY/PROPOSTA_1"), layer = "PROPOSTA_1", driver = "ESRI Shapefile", overwrite_layer = T)




llista<-list(p1, solucio, taula, area_total_protegida, cost, by_features_final, by_features_all, by_estatus, boundary, clumping)
names(llista)<-c("Problema", "Solució", "Taula", "Àrea total protegida", "cost total", "Targets per espècies seleccionades","targets per totes les especies", "Targets per estatus", "connectivitat", "clumping")
assign(nom, llista)

llista_exercicis<-list(baixa_connectivitat, mitja_connectivitat, alta_connectivitat, baixa_connectivitat_cost, mitja_connectivitat_cost, alta_connectivitat_cost)
names(llista_exercicis)<-c("baixa_connectivitat", "mitja_connectivitat", "alta_connectivitat","baixa_connectivitat_cost","mitja_connectivitat_cost","alta_connectivitat_cost")

#Calcul de l'area ocupada per lock_in i lock_out
# pol_lock_in <- PU_conques[PU_conques$locked_in == TRUE, ]
# pol_lock_out <- PU_conques[PU_conques$lock_out == TRUE, ]
# 
# sup_locked_in<-sum(pol_lock_in@data$AREA)/10000
# sup_locked_out <- sum(pol_lock_out@data$AREA)/10000


##  Creation of the final SHP including the solution and the protected species involved in each PU. ALSO, NEW BY_FEATURES ----

#No sé si això serveix d0alguna cosa:
# zona1 <- public10_alts_mitjana[["Solució"]][public10_alts_mitjana[["Solució"]]$solution_1_zona_1 == "1",] #Select zona 1 and 2. We have to put the result that we want, here.
# zona2 <- public10_alts_mitjana[["Solució"]][public10_alts_mitjana[["Solució"]]$solution_1_zona_2 == "1",]
# zona3 <- public10_alts_mitjana[["Solució"]][public10_alts_mitjana[["Solució"]]$solution_1_zona_2 == "0" & public10_alts_mitjana[["Solució"]]$solution_1_zona_1 == "0",]


load("C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY/scripts/Pla_Proteccio_MSY-main/workspace_final.RData")

tots_elements<-dplyr::full_join(bloquejades_tot, tots_elements) #All the elements: those that enter to the model but also the locked_in
tots_elements$estatus_blo<-ifelse(tots_elements$bloquejada_zonacio=="1" & !is.na(tots_elements$bloquejada_zonacio), "Bloquejada", tots_elements$estatus_zonacio) #New column

features<-names(list_features) %in% tots_elements$element
features <- list_features[features]
stack_tots<-stack(features)

#HERE, CHANGE PROPOSTA 1 AND 2. THEN, ALSO CHANGE PUBLIC10alts/mitjans like 150 lines after. 
spdf<-readOGR("C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY/RESULTS/PROPOSTA_1/PROPOSTA_1_ID.shp")

# OBSERVATIONS: WE USE THE CENTER OF THE PIXELS ----

areas <- sapply(spdf@polygons, function(p) {
  poly <- SpatialPolygons(list(p))
  return(poly@polygons[[1]]@area)
})
spdf$area <- areas/10000

subset_list <- list()  # Empty list to store the subsets

# Loop through each SpatialPolygonsDataFrame in the list
for (i in seq_along(stack_tots)) {
  
  raster_cells <- stack_tots[[i]]
  raster_cells[raster_cells == 0] <- NA
  centroids <- rasterToPoints(raster_cells, spatial = TRUE)
  
  centroids_sp <- SpatialPoints(coords = centroids, proj4string = CRS(proj4string(stack_tots)))
  centroids_df <- data.frame(ID = 1:length(centroids))
  centroids_spdf <- SpatialPointsDataFrame(centroids_sp, centroids_df)
  
  overlapping_polygons <- over(centroids_spdf, spdf)
  
  indices <- !duplicated(overlapping_polygons$id)
  subset_df <- overlapping_polygons$id[indices]
  
  subset_list[[i]] <- subset_df
  
}

noms <- by_features$element[order(by_features$element)]
names(subset_list)<-noms
#Ara ja tinc una llista amb els noms de l'element i les subconques que ocupa. 

my_dataframe <- data.frame(Name = character(), Value = numeric(), area=numeric(), stringsAsFactors = FALSE) #New dataframe

for (name in names(subset_list)) {
  values <- subset_list[[name]]
  temp_df <- data.frame(element = rep(name, length(values)), id = values, stringsAsFactors = FALSE)
  my_dataframe <- rbind(my_dataframe, temp_df)
}

rownames(my_dataframe) <- NULL

spdf_df <- as.data.frame(spdf)
num_unique <- length(unique(my_dataframe$Value))

proposta_2<- merge(spdf_df, my_dataframe, by.x = "id", by.y = "id", all.x=T)
proposta_2 <- left_join(proposta_2, select(by_features, element, tipus_informacio), by = "element")

total_area<- proposta_2 %>%
  group_by(element) %>%
  summarize(total_area = sum(area))
proposta_2 <- left_join(proposta_2,total_area, by = "element")

proposta_2$percentatge<-(proposta_2$area/proposta_2$total_area)*100

proposta_2 <- left_join(by_features %>% select(element, estatus_zonacio), proposta_2, by = "element")
proposta_2 <- left_join(proposta_2, PU_conques@data %>% select(id, locked_in, lock_out), by = "id")

proposta_2 <- proposta_2 %>%
  filter(!(element == "Austropotamobius pallipes (Lereboullet," & locked_in == F)) %>%
  filter(!(element == "Rosalia alpina" & locked_in == F)) %>%
  filter(!(element == "Buxbaumia viridis" & locked_in == F)) %>%
  filter(!(element == "Calotriton arnoldi" & locked_in == F)) %>%
  group_by(element) %>%
  mutate(total_area = sum(area),
         percentatge = area / total_area * 100)


noms_canviar<-c("Alauda arvensis","Anthus campestris", "Ctenodecticus masferreri","Euphydryas aurinia (Rottemburg, 1775)","Hypericum pulchrum",
                "Lanius meridionalis", "Melitaea diamina (Lang, 1789)", "Polygonum bistorta", "Rana temporaria",  "Sambucus racemosa")
proposta_2$tipus_informacio <- ifelse(proposta_2$element %in% c(noms_canviar), "observacions", proposta_2$tipus_informacio)
proposta_2$tipus_informacio <- ifelse(proposta_2$element=="Phylloscopus collybita", "SDM", proposta_2$tipus_informacio)

obs<-dplyr::filter(proposta_2, !tipus_informacio=="SDM" & !tipus_informacio=="poligons" & !tipus_informacio=="polígons (flora amenaçada)")
#sdm ens servirà després per a comprovacions i seleccionar els mateixos polígons que aquí: 
sdm<-dplyr::filter(proposta_2, tipus_informacio=="SDM" | tipus_informacio=="poligons" | tipus_informacio=="polígons (flora amenaçada)")


# SDM CALCULATE SURFACE OCCUPIED FOR EACH ELEMENT ----
noms_sdm<-unique(sdm$element)
noms_sdm<-sort(noms_sdm)

stack_sdm <- stack(stack_tots[[24]], stack_tots[[25]], stack_tots[[31]], stack_tots[[32]],stack_tots[[38]],stack_tots[[42]],stack_tots[[44]],
                   stack_tots[[45]],stack_tots[[48]],stack_tots[[55]],stack_tots[[58]],stack_tots[[62]],stack_tots[[63]],stack_tots[[64]],
                   stack_tots[[71]],stack_tots[[74]],stack_tots[[79]],stack_tots[[82]],stack_tots[[83]],stack_tots[[87]],stack_tots[[89]],
                   stack_tots[[96]],stack_tots[[103]],stack_tots[[104]],stack_tots[[110]],stack_tots[[116]],stack_tots[[118]],stack_tots[[119]])


sdm_df <- data.frame(Name = character(), Value = numeric(), area = numeric(), stringsAsFactors = FALSE) #New DF

for (i in 1:nlayers(stack_sdm)) {
  
  extracted_values <- raster::extract(stack_sdm[[i]], spdf, fun = sum)
  new_row <- data.frame(Name = noms_sdm[i], Value = extracted_values, area = spdf$area, id=spdf$id)
  sdm_df <- rbind(sdm_df, new_row)
  print(i)
}

suma_values <- sdm_df %>% # Calculate the sum for each unique Name
  group_by(Name) %>%
  summarize(suma = sum(Value, na.rm = T))

sdm_df <- merge(sdm_df, suma_values, by = "Name", all.x = TRUE)

sdm_df$percentatge <- sdm_df$Value / sdm_df$suma * 100

sdm_df<-dplyr::filter(sdm_df, Value!=0)

sdm_df <- left_join(sdm_df, spdf@data  %>% select(id, solucio), by = "id")
sdm_df <- left_join(sdm_df, select(PU_conques@data, id, locked_in, lock_out), by = "id")

# lockinout<-dplyr::select(PU_conques@data, id, locked_in, lock_out)
# df_merged<-dplyr::left_join(df_merged, lockinout, by="id")
# 

#VEIEM QUE HI HA ALGUNS POLÍGONS QUE NO ESTAVEN A SDM I SÍ QUE ESTAN A DF MERGED
# provasdm<-dplyr::filter(sdm, element=="Tyto alba")
# provadf<-dplyr::filter(df_merged, Name=="Tyto alba")
# setdiff(provadf$id, provasdm$id) COMPROVACIÓ, 

#SÓN CANVIS MINÚSCULS, PERÒ COM QUE VOLEM CALCULAR-HO TOT IGUAL, ENS QUEDEM NOMÉS AMB ELS POLÍGONS QUE HA ESTAVEN CALCULATS AMB EL PRIMER MÈTODE.
#CANVIEN MENYS D'UN 1 PER CENT, excepte R_PONDS (1,1%) i Barbus (1,5%). Es calcula aquí:

# for (i in unique(sdm$element)) {
#   kkdf <- df2_subset %>% filter(Name == i)
#   sum_percentatge <- sum(kkdf$percentatge)
#   print(sum_percentatge)
# }

#Ens quedem només amb les conques que hem vist que ocupaven amb el mètode de les observacions (utilitzant sdm, original)
unique_elements <- unique(sdm$element)
unique_ids <- unique(sdm$id)

sdm_df <- subset(sdm_df, Name %in% unique_elements & id %in% unique_ids)

colnames(sdm_df)[which(names(sdm_df) == "Name")] <- "element"
colnames(sdm_df)[which(names(sdm_df) == "suma")] <- "total_area"
colnames(sdm_df)[which(names(sdm_df) == "locked_in.x")] <- "locked_in"
colnames(sdm_df)[which(names(sdm_df) == "lock_out.x")] <- "lock_out"

sdm_df<-sdm_df[ ,  !names(sdm_df) %in%  c("Value", "locked_in.y", "lock_out.y")]

sdm_df <- sdm_df %>%
  left_join(select(by_features, element, tipus_informacio), by = "element") %>%
  left_join(select(by_features, element, estatus_zonacio), by = "element")


# TOT ÉS LA TAULA PER ANALITZAR ELS % DE CADASCUN DELS ELEMENTS EN CADA CONCA ----

tot<-dplyr::union(obs, sdm_df)
tot <- left_join(tot, by_features %>% select(element, id_simsy), by = "element", na.rm = FALSE)

tot$tipus_informacio<-ifelse(tot$element=="Phylloscopus collybita", "SDM", tot$tipus_informacio)

# ARA ACTUALITZAREM LA TAULA DE LA PROPOSTA ----

sum_by_category <- aggregate(percentatge ~ element, data = tot, FUN = sum) #Comprovació que els percentatges donin 100%. Hi ha algun 99 i a minim 98.5%
row_counts <- table(tot$element) #Nombre de conques ocupades per cada element

sum_by_zone_element <- aggregate(percentatge ~ solucio + element, data = tot, FUN = sum) #Ja tenim el percentatge protegit, però no té l'estructura que vull. Ara ho fem:


all_elements <- unique(sum_by_zone_element$element)
all_solucio <- unique(sum_by_zone_element$solucio)
expanded_df <- expand.grid(element = all_elements, solucio = all_solucio)

sum_by_zone_element_expanded <- merge(expanded_df, sum_by_zone_element, by = c("element", "solucio"), all.x = TRUE)
sum_by_zone_element_expanded$percentatge[is.na(sum_by_zone_element_expanded$percentatge)] <- 0
spread_df <- spread(sum_by_zone_element_expanded, solucio, percentatge)

NOVATAULA<- dplyr::left_join(public10_alts_mitjana[["Targets per espècies seleccionades"]], spread_df %>% select(element, ZRN, ZIC, "0"), by="element")
#NOVATAULA$zona_1*100 - NOVATAULA$ZRN Comprovació que els resultats són molt similars, però millors aquí, respecte la taula anterior

NOVATAULA$tipus_informacio<-ifelse(NOVATAULA$element=="Phylloscopus collybita", "SDM", NOVATAULA$tipus_informacio)
noms_canviar<-c("Alauda arvensis","Anthus campestris", "Ctenodecticus masferreri","Euphydryas aurinia (Rottemburg, 1775)","Hypericum pulchrum",
                "Lanius meridionalis", "Melitaea diamina (Lang, 1789)", "Polygonum bistorta", "Rana temporaria",  "Sambucus racemosa")
NOVATAULA$tipus_informacio <- ifelse(NOVATAULA$element %in% c(noms_canviar), "observacions", NOVATAULA$tipus_informacio)

NOVATAULA<-NOVATAULA[ , !names(NOVATAULA) %in% c("zona_1","zona_2","suma")]
NOVATAULA$suma <- apply(NOVATAULA[, c("ZRN", "ZIC")], 1, sum)


#Eliminar columnes del tesaure (tot_proposta) i afegir si les conques estan bloquejades al SHP. Separar-ho en proposta 1 i 2
tot_proposta1<-tot

proposta1<-NOVATAULA
proposta1$`area_ocupacio (ha)` <- tot_proposta1$total_area[match(proposta1$element, tot_proposta1$element)]

tot_proposta1<-subset(tot_proposta1, select=c("id", "id_simsy", "element", "percentatge"))

proposta1$num_PUs <- row_counts[as.character(proposta1$element)]


spdf <- merge(spdf, PU_conques[c("id", "locked_in", "lock_out")], by = "id", all.x = TRUE)
spdf_proposta1<-spdf

#desired_order <- c("id", "solucio", "area", "locked_in", "lock_out") Si cal canviar ordres
#reordered_spdf <- spdf_proposta2[, c(desired_order, setdiff(names(spdf_proposta2), desired_order))]



#EXPORT

write.xlsx(tot_proposta1, "C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY/RESULTATS_NOUS/PROPOSTA_1/TESAURE_PROPOSTA_1.xlsx")
write.xlsx(tot_proposta2, "C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY/RESULTATS_NOUS/PROPOSTA_2/TESAURE_PROPOSTA_2.xlsx")


write.xlsx(proposta1, "C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY/RESULTATS_NOUS/PROPOSTA_1/PROPOSTA_1.xlsx")
write.xlsx(proposta2, "C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY/RESULTATS_NOUS/PROPOSTA_2/PROPOSTA_2.xlsx")


writeOGR(spdf_proposta1, "C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY/RESULTATS_NOUS/PROPOSTA_1", layer = "PROPOSTA_1", driver = "ESRI Shapefile") 
writeOGR(reordered_spdf, "C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY/RESULTATS_NOUS/PROPOSTA_2", layer = "PROPOSTA_2", driver = "ESRI Shapefile", overwrite_layer = T) 

ambit_PNkk<-spTransform(ambit_PN, crs("EPSG:4326"))
proposta1kk<-spTransform(spdf_proposta1, crs("EPSG:4326"))
proposta2kk<-spTransform(spdf_proposta2, crs("EPSG:4326"))

opacity_values <- function(solucio) {
  # Define opacity values for each level in solucio
  # Customize the opacity values as per your requirements
  opacity <- ifelse(solucio == "0", 0.5,
                    ifelse(solucio == "ZIC", 0.7,
                           ifelse(solucio == "ZRN", 0.7, 1)))
  return(opacity)
}

pal <- colorFactor(palette = c("#ffffff", "#b2df8a", "#006400"), 
                   levels = c("0", "ZIC", "ZRN"))



leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") |>
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") |>
  addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") |>
  addProviderTiles(providers$Stamen.Terrain, group = "Stamen.Terrain") |>
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorldMap") |>
  addLayersControl(baseGroups = c("Blanc","OpenStreetMap","Esri.WorldImagery","OpenTopoMap", "Stamen.Terrain", "Esri.NatGeoWorldMap"), overlayGroups = c("proposta 1", "proposta 2"), options = layersControlOptions(collapsed = F))|>  # try also collapsed = TRUE
  addPolygons(data = ambit_PNkk, col="white", opacity = 0.6, weight = 2)|>
  addPolygons(data = proposta1kk, popup = leafpop::popupTable(proposta1kk), col="black", fillColor = ~ pal(solucio), weight = 1, opacity = 0.3, fillOpacity = opacity_values(proposta1kk$solucio), group = "proposta 1", smoothFactor = 0)|>
  addPolygons(data = proposta2kk, popup = leafpop::popupTable(proposta2kk), col="black", fillColor = ~ pal(solucio), weight = 1, opacity = 0.3, fillOpacity = opacity_values(proposta2kk$solucio), group = "proposta 2", smoothFactor = 0)|>
  hideGroup("proposta 1") |> # try also without this
  leaflet.extras::addSearchOSM() |>
  addMiniMap(position = "bottomleft") |>
  addScaleBar(position = "bottomright") |>
  leafem::addHomeButton(group = "proposta 1", position = "bottomright")|>
  addLegend(position = "bottomright", colors = c("#ffffff", "#b2df8a", "#006400"),labels = c("0", "ZIC", "ZRN")) |>
  onRender("function(el, x) {el.style.background = '#FFF';}")|> #Afegir el fons blanc
  addControl(html = "<input id=\"OpacitySlide\" type=\"range\" min=\"0\" max=\"1\" step=\"0.1\" value=\"0.5\">") |>   # Add Slider
  htmlwidgets::onRender(
    "function(el,x,data){
                     var map = this;
                     var evthandler = function(e){
                        var layers = map.layerManager.getVisibleGroups();
                        console.log('VisibleGroups: ', layers); 
                        console.log('Target value: ', +e.target.value);
                        layers.forEach(function(group) {
                          var layer = map.layerManager._byGroup[group];
                          console.log('currently processing: ', group);
                          Object.keys(layer).forEach(function(el){
                            if(layer[el] instanceof L.Polygon){;
                            console.log('Change opacity of: ', group, el);
                             layer[el].setStyle({fillOpacity:+e.target.value});
                            }
                          });
                          
                        })
                     };
              $('#OpacitySlide').mousedown(function () { map.dragging.disable(); });
              $('#OpacitySlide').mouseup(function () { map.dragging.enable(); });
              $('#OpacitySlide').on('input', evthandler)}
          ")

# #Old way (cutre)
# 
# zona3$elements_coberts <- NA
# zona1$elements_coberts <- NA
# zona2$elements_coberts <- NA
# 
# for (i in 1:length(zona2)) {
#   polygon <- zona2[i, ]
#   raster_names_modified <- character(0)
#   
#   for (j in 1:nlayers(stack_tots)) {
#     raster_layer <- stack_tots[[j]]
#     
#     values <- extract(raster_layer, polygon)
#     
#     if (any(unlist(values) == 1, na.rm = TRUE)) {
#       raster_name <- names(stack_tots)[j]
#       
#       if (raster_name %in% c("r_ponds", "bosc_madur")) {
#         matching_element <- raster_name
#         matching_index <- match(matching_element, tots_elements$element)
#         estatus_blo <- tots_elements$estatus_blo[matching_index]
#       } else if (startsWith(raster_name, "X") && !endsWith(raster_name, ".")) {
#         modified_name <- substring(raster_name, 2)
#         
#         matching_element <- modified_name
#         matching_index <- match(matching_element, tots_elements$element)
#         estatus_blo <- tots_elements$estatus_blo[matching_index]
#       } else if (startsWith(raster_name, "X") && endsWith(raster_name, ".")) {
#         
#         modified_name <- gsub("^X", "", raster_name)
#         modified_name <- gsub("\\.$", "+", modified_name)
#         
#         matching_element <- modified_name
#         matching_index <- match(matching_element, tots_elements$element)
#         estatus_blo <- tots_elements$estatus_blo[matching_index]
#       } else {
#         modified_name <- gsub("^X", "", raster_name)
#         
#         modified_name <- gsub("\\.+", " ", modified_name)
#         modified_name <- gsub("\\s+", " ", modified_name)
#         
#         modified_name <- paste(word(modified_name, 1, 2), collapse = " ")
#         
#         matching_element <- modified_name
#         matching_index <- match(matching_element, word(tots_elements$element, 1, 2))
#         estatus_blo <- tots_elements$estatus_blo[matching_index]
#       }
#       
#       if (estatus_blo == 3) {
#         estatus <- "(Alt)"
#       } else if (estatus_blo == 4) {
#         estatus <- "(Molt alt)"
#       } else if (estatus_blo == "Bloquejada") {
#         estatus <- "(Bloquejada)"
#       } else {
#         estatus <- ""
#       }
#       
#       modified_name <- paste(modified_name, estatus, sep = " ")
#       
#       raster_names_modified <- c(raster_names_modified, modified_name)
#     }
#   }
#   
#   raster_names_concat <- paste(raster_names_modified, collapse = ", ")
#   
#   zona2$elements_coberts[i] <- raster_names_concat
# }
# 
# zona_protegida<-rbind(zona1, zona2, zona3)
# zona_protegida$solution_1_zona_1[s1_change$solution_1_zona_2 == 1 & s1_change$solution_1_zona_1 == 0] <- 2 #Si zona estricta és 0 i zona laxa és 1, a la zona estricta li posem un 1.
# 
# writeOGR(zona_protegida, dsn="C:/Users/david.munoz/Downloads/BotrychiumMSY", layer="shape_final", driver="ESRI Shapefile", overwrite_layer = T)
# 


#Per a fer-ho amb un altre format 
# zona1_sf <- st_as_sf(zona1)
# zona1_sf$elements_coberts_long <- I(zona1$elements_coberts)
# writeOGR(zona1_sf, dsn=, layer = "zona1", driver = "GPKG")



##  Comprovacio ----
features<-names(list_features) %in% tots_elements$element #seleccionem els 105 elements
features <- list_features[features]
stack_tots<-stack(features)

sol1<-raster::rasterize(s1, stack_tots[[1]], field="solution_1_zona_1") #rasteritzem la zona 1 i també la zona 2
sol2<-raster::rasterize(s1, stack_tots[[1]], field="solution_1_zona_2")
sol1[sol1==0] <-NA #Els 0 seran NA
sol2[sol2==0] <-NA


protegit_zona1<-vector(length = nlayers(stack_tots))
protegit_zona2<-vector(length = nlayers(stack_tots))
protegit_total<-vector(length = nlayers(stack_tots))

for (i in 1:nlayers(stack_tots)) { #per a cada feature
  
  
  raster1<-stack_tots[[i]]
  
  raster1[raster1==0] <-NA
  
  kk1<-raster1*sol1 #Multipliquem el feature per la solucio 1 i per la 2
  kk2<-raster1*sol2
  
  condition1 <- !is.na(kk1) & kk1 == 1 #Si a tot arreu hi ha 1, es quedarà un 1. Si hi ha un NA, serà NA. 
  condition2 <- !is.na(kk2) & kk2 == 1
  
  sumkk1 <- cellStats(kk1 * condition1, sum, na.rm = TRUE) #Sumem, i després ho guardem en un vector (suma/total)
  sumkk2 <- cellStats(kk2 * condition2, sum, na.rm = TRUE)
  
  protegit_zona1[i]<-sumkk1/cellStats(stack_tots[[i]], stat=sum)
  protegit_zona2[i]<-sumkk2/cellStats(stack_tots[[i]], stat=sum)
  protegit_total[i]<-protegit_zona1[i]+protegit_zona2[i]
  print(i)
  
}

ue<-data.frame(element=names(features), #Creem un dataframe amb les noves dades per a comprovar
               zona_1<-protegit_zona1,
               zona_2<-protegit_zona2,
               suma<-protegit_total)

pra <- by_features[order(by_features$element),] #I ordenem la taula que fa el programa
restesona1<-ue$zona_1....protegit_zona1-pra$zona_1 #Resta de la zona 1. Molts valors s'acosten al 0, els que no és degut a que
#Pixels i mida de pixel de la PU conca és diferent. ENTENC QUE MILLOR PU CONCA, PEL QUE ENS QUEDEM AMB LA TAULA QUE JA TENÍEM


tail(sort(restesona1),5) #Per a veure els valors més alts
                