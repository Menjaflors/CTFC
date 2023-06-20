
#### 1.3. DATA WRANGLING ####

## Meta  ----------------------------------------------------------

# This script contains the cleaning, filtering and fine tunning of the dataframes and shapefiles

# https://rspatial.org/raster/sdm/index.htm


## Initial code  ----------------------------------------------------------

### Delete all objects

rm(list=ls())

### Instal packages in case they are not installed

list.of.packages <- c("tidyverse", "readr", "rgdal", "sp", "raster", "sf", "rgeos", "RODBC", "plyr", "pbapply",
                      "MASS", "ggplot2", "cowplot", "plotly", "vegan", "readxl", "bit64", "utils")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}

### Open libraries

invisible(lapply(list.of.packages, library, character.only = TRUE, logical.return=T))

rm(list.of.packages, new.packages) # remove objects we won't need anymore.

### Set constants and variables
dir<-"C:/Users/david.munoz/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY"
dir_bioclim <- paste0(dir, "/BIOCLIM/BIOCLIM_AMBIT") # directory bioclim layers
dir_db <- paste0(dir, "/RAW/EXPORT_SIMSY/BdDPatrimoniNaturalMSY120111.accdb") # directory ambit layer
dir_simsy <- paste0(dir, "/SIMSY") # directory simsy
dir_processed <- paste0(dir, "/PROCESSED") # directory simsy
dir_export <- paste0(dir, "/EXPORT") # directory simsy

### Open files

#### Ambit

ambit_PN <- readOGR(paste0(dir, "/SIMSY/INFO_SONIA_141022/LIMIT_PARC_DECRET_21/LIMIT_2021_v2.shp"))
ambit_RB <- readOGR(paste0(dir, "/SIMSY/INFO_SONIA_141022/LimitsRB/DelimitacioShpED50RBM13_ETRS89.shp"))

#### SIMSY

index_simsy <- read_excel(paste0(dir_simsy, "/INDEX/INDEXSIMSY_original.xlsx" ), sheet=2)

db <- odbcConnectAccess2007(dir_db) # open access database, it is 'mdb' because the file is quite old

tbls <- sqlTables(db, tableType = "TABLE")
tbls$TABLE_NAME # to display the name of all the tables

ubi_db <- sqlFetch(db, "TAULA_UBICACIONS_TOTAL") # to import a sheet into R. It requires two arguments: the connection, and the table name.
amenaces_db <- sqlFetch(db, "TAULA_AMENACES")
tesaure_epn_db <- sqlFetch(db, "TESAURE_EPN")
tesaure_interes_conserv_db <- sqlFetch(db, "TAULA_MOTIU_INTERES")
# unio_epn_estatus_db <- sqlFetch(db, "TAULA_UNIO_EPN_ESTATUS")
# tesaure_estatus_db <- sqlFetch(db, "TESAURE_ESTATUS")

odbcCloseAll() # Close connection to Access databases

#### Grids
grid_1km <- readOGR(paste0(dir, "/EXPORT/GRIDS/grid_1x1km_modified.shp"))

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

ambit_RB <- spTransform(ambit_RB, proj_EPSG25831) # It doesn't work with ambit_RB so I do it without the function

## Wrangling of presence species  ----------------------------------------------------------

### Add species scientific name and taxons to the list
name_species_df <- tesaure_epn_db[, c("ID_EPN", "DESCRIPCIO_EPN", "DES_TIPUS_EPN", "DES_TIPUS_EXOTIC")]
ubi_db <- merge(ubi_db, name_species_df, by='ID_EPN', all.x=T, all.y=F )

### Merging presence species with conservation interest value
ubi_db <- merge(ubi_db, tesaure_interes_conserv_db, by='ID_EPN', all.x=T, all.y=F )

### Stay only with interesting columns
ubi_db <- ubi_db[, c("ID_EPN", "TIPUS_DADA", "X", "Y", "YEAR", "TIPUS1", "DESCRIPCIO_EPN", "DES_TIPUS_EPN",
                     "INT_CONS_MSY_FINAL", "DES_TIPUS_EXOTIC")]

### Delete exotic species
ubi_db <- ubi_db[is.na(ubi_db$DES_TIPUS_EXOTIC), ]

### Create column to know the resolution
ubi_db$res <- ifelse(ubi_db$TIPUS_DADA == "UTM X,Y"|ubi_db$TIPUS_DADA == "UTM X,Y (metre)", "100m",
                     ifelse(ubi_db$TIPUS_DADA == "UTM 1X1", "1km",
                            ifelse(ubi_db$TIPUS_DADA == "VECTOR SHAPEFILE", "vector", "no_res")))

### Split species into list
list_species <- base::split(ubi_db, ubi_db$DESCRIPCIO_EPN)

### Remove NAs and species without spatial data
fun_NAs <- function(x) { x[which(!is.na(x$X) &     # remove if it does not containt latitude
                                   !is.na(x$Y) &   # remove if it does not containt longitude
                                   !is.na(x$YEAR)  # remove if it does not containt year
),] }

list_species <- pblapply(list_species, fun_NAs)
nsamples <- sapply(list_species, function(x) {nrow(x[])}) # find species with and without spatial data
list_species <- list_species[as.vector(!nsamples==0)] # remove species without spatial data

rm(nsamples)

### Add species from outside simsy

#### Saltarela
saltarela_db_21 <- as.data.frame(read_excel(paste0(dir, "/RAW/DADES_ROSALIA_CTENODECTICUS/Cites Saltarel·la 2021.xlsx"), sheet=1)) # Ctenodecticus masferreri
saltarela_db_20 <- as.data.frame(read_excel(paste0(dir, "/RAW/DADES_ROSALIA_CTENODECTICUS/Cites Saltarel·la 2021.xlsx"), sheet=2)) # Ctenodecticus masferreri
saltarela_db <- rbind(saltarela_db_20, saltarela_db_21)
saltarela_db$Data <- as.Date(saltarela_db$Data, format="%Y-%m-%d")
saltarela_db$YEAR <- format(saltarela_db$Data, format="%Y")
saltarela_db$TIPUS_DADA <- "UTM, X,Y" # I check on QGIS that the coordinates are "UTM, X,Y"
saltarela_db$res <- "100m" # Then the resolution is 100x100m
saltarela_db$DES_TIPUS_EPN <- "Fauna" # Then the resolution is 100x100m
saltarela_db$ID_EPN <- "91003984" # it is the same species, so they have the same ID_EPN
saltarela_db$DESCRIPCIO_EPN <- "Ctenodecticus masferreri" # name of the species
saltarela_db$DES_TIPUS_EXOTIC <- "" # not an invasive species
colnames(saltarela_db)[which(names(saltarela_db) == "Exemplars")] <- "ABUNDANCIA" # same name as the list_species column
colnames(saltarela_db)[which(names(saltarela_db) == "Data")] <- "DATA" # same name as the list_species column
saltarela_db <- saltarela_db[, !names(saltarela_db) %in% c("N", "Altura", "Sexe", "Estat")] # extract interesting columns
list_species[["Ctenodecticus masferreri"]] <- rbind.fill(list_species[["Ctenodecticus masferreri"]], saltarela_db)
list_species[["Ctenodecticus masferreri Bolívar, 1894"]] <- NULL # we delete the element from SIMSY that had few observations

#### Rosalia
rosalia_db <- as.data.frame(read_excel(paste0(dir, "/RAW/DADES_ROSALIA_CTENODECTICUS/Recull de dades de Rosalia alpina al Montseny.xlsx"), sheet=1)) # Rosalia alpina
rosalia_db$TIPUS_DADA <- "UTM, X,Y" # I check on QGIS that the coordinates are "UTM, X,Y"
rosalia_db$res <- "100m" # Then the resolution is 100x100m
rosalia_db$DES_TIPUS_EPN <- "Fauna" # Then the resolution is 100x100m
rosalia_db$ID_EPN <- "91000681" # it is the same species, so they have the same ID_EPN
rosalia_db$DESCRIPCIO_EPN <- "Rosalia alpina" # name of the species
rosalia_db$DES_TIPUS_EXOTIC <- "" # not an invasive species
colnames(rosalia_db)[which(names(rosalia_db) == "Data")] <- "DATA" # same name as the list_species column
colnames(rosalia_db)[which(names(rosalia_db) == "Total ex.")] <- "ABUNDANCIA" # same name as the list_species column
rosalia_db <- rosalia_db[, !names(rosalia_db) %in% c( "Alçada",  "Topònim", "Exemplars", "Observador/s", "Referència")] # extract interesting columns
list_species[["Rosalia alpina"]] <- rbind.fill(list_species[["Rosalia alpina"]], rosalia_db)
list_species[["Rosalia alpina (Linnaeus, 1758)"]] <- NULL  # we delete the element from SIMSY that had few observations

rm(rosalia_db, saltarela_db, saltarela_db_20, saltarela_db_21)

### Filter points inside RB (extension)

fun_filter_inside_ambit <- function(x) { 
  
  y <- ambit_RB@polygons[[1]]@Polygons[[1]]@coords
  v <- point.in.polygon(x$X, x$Y, y[,1], y[,2], mode.checked=FALSE)
  x <- x[v==1,]
  return(x)
}

list_species <- pblapply(list_species, fun_filter_inside_ambit)

### Delete elements of list without rows

filter <- rep(NA, length(list_species))
for (i in 1:length(list_species)) { if(nrow(list_species[[i]])==0) filter[i] <- 1 }
list_species <-list_species[is.na(filter)]

## Separate data to different resolutions  ----------------------------------------------------------

### Separate resolutions from the others
list_liquens_100m <- pblapply(list_species, function(x,y,z) { subset(x, x$res=="100m" & x$DES_TIPUS_EPN=="Líquens")}) # only liquens to 100m (we separate them because they have less than 10 individual per species)
list_species_100m <- pblapply(list_species, function(x,y,z) { subset(x, x$res=="100m" & x$DES_TIPUS_EPN!="Fauna")}) # all except fauna to 100m
list_species_500m <- pblapply(list_species, function(x,y,z) { subset(x, x$res=="100m" & x$DES_TIPUS_EPN=="Fauna")}) # Fauna to 500m
list_species_1km  <- pblapply(list_species, function(x,y) { subset(x, x$res=="1km") })

#### Delete elements of list without rows

filter <- rep(NA, length(list_liquens_100m))
for (i in 1:length(list_liquens_100m)) { if(nrow(list_liquens_100m[[i]]) == 0) filter[i] <- 1 } # for liquens we take all the species with non-zero records.
list_liquens_100m <-list_liquens_100m[is.na(filter)]

filter <- rep(NA, length(list_species_100m))
for (i in 1:length(list_species_100m)) { if(nrow(list_species_100m[[i]]) == 0) filter[i] <- 1 }
list_species_100m <-list_species_100m[is.na(filter)]

filter <- rep(NA, length(list_species_500m))
for (i in 1:length(list_species_500m)) { if(nrow(list_species_500m[[i]]) == 0) filter[i] <- 1 }
list_species_500m <-list_species_500m[is.na(filter)]

filter <- rep(NA, length(list_species_1km))
for (i in 1:length(list_species_1km)) { if(nrow(list_species_1km[[i]]) == 0) filter[i] <- 1 }
list_species_1km <-list_species_1km[is.na(filter)]

rm(filter)

### Transform lists to spdf

fun_to_spdf <- function(x) {
  coordinates(x) <- c("X", "Y") 
  crs(x) <- proj_EPSG25831
  return(x)
}

list_liquens_100m <- pblapply(list_liquens_100m, fun_to_spdf) # liquens to 100m
list_species_100m <- pblapply(list_species_100m, fun_to_spdf) # 100m
list_species_500m <- pblapply(list_species_500m, fun_to_spdf) # 500m
list_species_1km  <- pblapply(list_species_1km, fun_to_spdf) # 1km

### remove '/' from names

names(list_species) <- gsub("/" , " " , names(list_species))
names(list_liquens_100m) <- gsub("/" , " " , names(list_liquens_100m))
names(list_species_1km) <- gsub("/" , " " , names(list_species_1km))
names(list_species_100m) <- gsub("/" , " " , names(list_species_100m))
names(list_species_500m) <- gsub("/" , " " , names(list_species_500m))


llista_final<-c(list_liquens_100m,list_species_100m,list_species_1km,list_species_500m)

duplicated_names <- duplicated(names(llista_final))

# Print the duplicated names
repetits<-print(names(llista_final)[duplicated_names])

