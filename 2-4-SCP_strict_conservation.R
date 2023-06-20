
#### 2.4 SYSTEMATIC CONSERVATION PLANNING FOR STRICT CONSERVATION####

##  Meta  ----------------------------------------------------------

# https://prioritizr.net/
# https://cran.r-project.org/web/packages/prioritizr/vignettes/prioritizr.html



##  Functions ----------------------------------------------------------

### Delete all objects
rm(list=ls())

### Function of minimum objective
fun_min_prioritizr <- function(PU, filter, targets, name_list, connect, blm) {
  
  # PU <- PU_conques
  # filter <- elements_conserv_name_status
  # targets <- rel_targets
  # name_list <- "list_conques_blm_status"
  # connect <- connect_conques
  # blm <- increase_blm[i]
  
  start <- Sys.time()
  
  features <- list_features[filter] # we filter elements to match with relative targets
  features[sapply(features, is.null)] <- NULL # remove NULL elements
  features <- stack(features)
  
  # Start problem
  p1 <- problem(
    PU, features, cost_column = "mean_cost") %>% # we start with a few to not saturate the software
    add_locked_in_constraints(which(PU$locked_in)) %>% # here we lock in the places there are highly important features
    add_locked_out_constraints(which(PU$lock_out)) %>% # here we lock in the places there are highly important features
    add_min_set_objective() %>% # we minimize the cost of the solution whilst ensuring that all targets are met. 
    add_relative_targets(targets) %>% # we add the specific targets that we want to meet (between 0 and 1)
    add_binary_decisions() %>% # binary decision of protecting or not protecting certain planning unit
    add_rsymphony_solver(gap = 0.05, time_limit=7200, verbose=T) %>% # for now we will use the RSymphony software to solve the problems. Time limit is in seconds and I would remove it in the end
    add_boundary_penalties(penalty = blm, data = connect) # Here we penalize solutions that are fragmented and not connected..
  
  # Solve problem
  s1 <- solve(p1) 
  
  # Plot solution
  fun_plotting(s1)
  
  # Checks
  print(p1) # print problem
  number_of_planning_units(p1) # print number of planning units
  number_of_features(p1) # print number of features
  eval_n_summary(p1, s1[, "solution_1"]) # Calculate the number of planning units selected within a solution
  print(eval_cost_summary(p1, s1[, "solution_1"])) # Calculate the total cost of a solution.
  fr1 <- eval_feature_representation_summary(p1, s1[, "solution_1"]) %>% list # Calculate how well features are represented by a solution.
  eval_target_coverage_summary(p1, s1[, "solution_1"]) # Calculate how well feature representation targets are met by a solution.
  #eval_connectivity_summary(p1, s1[, "solution_1"], data = connectivity_matrix(PU, stack_features[[1]])) # Calculate the connectivity held within a solution.
  blm1 <- eval_boundary_summary(p1, s1[, "solution_1"])
  
  # Add to the actual lists
  assign(paste0(name_list, "_p"), append(get(paste0(name_list, "_p")), p1), envir = .GlobalEnv)
  assign(paste0(name_list, "_s"), append(get(paste0(name_list, "_s")), s1), envir = .GlobalEnv)
  assign(paste0(name_list, "_fr"), append(get(paste0(name_list, "_fr")), fr1), envir = .GlobalEnv)
  assign(paste0(name_list, "_blm"), append(get(paste0(name_list, "_blm")), blm1), envir = .GlobalEnv)
  
  print(round(Sys.time() - start, 2))
  
}

### Function of maximum objective
fun_max_prioritizr <- function(PU, filter, targets, budget, name_list, connect, blm) {
  
  start <- Sys.time()
  
  features <- list_features[filter] # we filter elements to match with relative targets
  features[sapply(features, is.null)] <- NULL # remove NULL elements
  features <- stack(features)
  
  # Start problem
  p1 <- problem(
    PU, features, cost_column = "mean_cost") %>% # we start with a few to not saturate the software
    add_locked_in_constraints(which(PU$locked_in)) %>% # here we lock in the places there are highly important features
    add_locked_out_constraints(which(PU$lock_out)) %>% # here we lock in the places there are highly important features
    add_max_features_objective(budget) %>% # we have a maximum budget (the ha we want to protect) and then we maximize the species and habitats we can protected inside this area. 
    add_relative_targets(targets) %>% # we add the specific targets that we want to meet (between 0 and 1)
    add_binary_decisions() %>% # binary decision of protecting or not protecting certain planning unit
    add_rsymphony_solver(gap = 0.05, time_limit=7200, verbose=T) %>% # for now we will use the RSymphony software to solve the problems. Time limit is in seconds and I would remove it in the end
    add_boundary_penalties(penalty = blm, edge_factor = 0.5, data = connect) # Here we penalize solutions that are fragmented and not connected..
  
  # Solve problem
  s1 <- solve(p1)
  
  # Plot solution
  fun_plotting(s1)
  
  # Checks
  print(p1) # print problem
  number_of_planning_units(p1) # print number of planning units
  number_of_features(p1) # print number of features
  eval_n_summary(p1, s1[, "solution_1"]) # Calculate the number of planning units selected within a solution
  print(eval_cost_summary(p1, s1[, "solution_1"])) # Calculate the total cost of a solution.
  fr1 <- eval_feature_representation_summary(p1, s1[, "solution_1"]) %>% list # Calculate how well features are represented by a solution.
  eval_target_coverage_summary(p1, s1[, "solution_1"]) # Calculate how well feature representation targets are met by a solution.
  #eval_connectivity_summary(p1, s1[, "solution_1"], data = connectivity_matrix(PU, stack_features[[1]])) # Calculate the connectivity held within a solution.
  blm1 <- eval_boundary_summary(p1, s1[, "solution_1"])
  
  # Add to the actual lists
  assign(paste0(name_list, "_p"), append(get(paste0(name_list, "_p")), p1), envir = .GlobalEnv)
  assign(paste0(name_list, "_s"), append(get(paste0(name_list, "_s")), s1), envir = .GlobalEnv)
  assign(paste0(name_list, "_fr"), append(get(paste0(name_list, "_fr")), fr1), envir = .GlobalEnv)
  assign(paste0(name_list, "_blm"), append(get(paste0(name_list, "_blm")), blm1), envir = .GlobalEnv)
  
  
  print(round(Sys.time() - start, 2))
  
}

### Function to plot the solutions
fun_plotting <- function(x) { 
  
  if (identical(sort(unique(x$solution_1)), c(0,1))) { plot(st_as_sf(x[, "solution_1"]), main = "Prioritization", pal = c("grey90", "darkgreen")) }
  
  if (identical(unique(x$solution_1), 1)) {  
    x$solution_1 <- as.factor(as.character(x$solution_1)) # transform solution to factor to plot only
    levels(x$solution_1) <- c("1", "0")
    plot(st_as_sf(x[, "solution_1"]), main = "Prioritization", pal = c("darkgreen", "grey90")) }
  
  if (identical(unique(x$solution_1), 0)) { 
    x$solution_1 <- as.factor(as.character(x$solution_1)) # transform solution to factor to plot only
    levels(x$solution_1) <- c("0", "1")
    plot(st_as_sf(x[, "solution_1"]), main = "Prioritization", pal = c("grey90", "darkgreen")) }
  
  
}

### Function to create lists
fun_create <- function(name_list) {
  assign(paste0(name_list, "_p"), vector("list"), envir = .GlobalEnv) # save the problem inside new list to the environment
  assign(paste0(name_list, "_s"), vector("list"), envir = .GlobalEnv) # save the solution inside new list to the environment
  assign(paste0(name_list, "_fr"), vector("list"), envir = .GlobalEnv) # save the feature representation inside new list to the environment
  assign(paste0(name_list, "_blm"), vector("list"), envir = .GlobalEnv) # save the Boundary Length Modifier inside new list to the environment
}

### Function to put the targets in a matrix
fun_targets <- function(v, base) {
  
  m <-  matrix(NA, nrow = nrow(base), ncol = length(v))
  
  targets <- as.numeric(as.character(base$targets))
  increase <- base$increase
  
  for (j in 1:length(v)) { m[,j] <- targets * (v[j] * (1 + increase)) }
  
  m[m > 1] <- 1 # no relative targets greater than 1!
  colnames(m) <- names(v)
  
  return(m)
  
}

### Function to list the problem, solution and feature representation lists
fun_list <- function(x) {
  names_lists <- grep(x, ls(), value=T)
  list3 <- mget(names_lists)
  list3 <- pblapply(list3, function(x) { return(x[[1]]) })
  assign(x, list3, envir = .GlobalEnv)
  rm(list=names_lists)
}

#### Function to find and wrangle the representation targets 
fun_fr <- function(x, rel_targets) { 
  
  x <- as.data.frame(x)
  x$initial_targets <- as.factor(rel_targets) # here we add the relative targets as a column
  x <- x[x$total_amount > 0, ] # IMPORTANT! Here we remove features that have 0 pixels inside the PN, but we had them because probably were inside RB
  
  if(length(levels(x$initial_targets)) >= 4) { levels(x$initial_targets)[1] <- levels(x$initial_targets)[2] }
  
  x$cats <- x$initial_targets
  if(length(levels(x$initial_targets)) == 4) {
    levels(x$cats) <- c("baix/no status", "mig", "alt", "molt alt") } else { 
      levels(x$cats) <- c("mig", "alt", "molt alt") }
  
  summary_min_mid_conques_fr <- x %>%
    group_by(initial_targets) %>%
    summarise(mean_rel_held = mean(relative_held)) %>%
    as.data.frame
  
  if(nrow(summary_min_mid_conques_fr) == 4) {
    cats <- c("baix/no status", "mig", "alt", "molt alt"); summary_min_mid_conques_fr <- cbind(cats, summary_min_mid_conques_fr) } else {
      cats <- c("mig", "alt", "molt alt"); summary_min_mid_conques_fr <- cbind(cats, summary_min_mid_conques_fr) }
  print(summary_min_mid_conques_fr)
  print(head(x))
  
  return(x)
}

### Function to export the curve plot
fun_curve_plot <-  function(set, list1, list_export) {
  
  list2 <- get(list1) # from character to actual object
  
  mean_targets <- round(pbapply(m_rel_targets, 2, mean), 3) # number of PU that have value '1'
  num1 <- pbsapply(list2, function(x) { table(x$solution_1)[which(names(table(x$solution_1)) == 1)] }) # number of PU that have value '1'
  area1 <- pbsapply(list2, function(x) { sum(area(x)[x$solution_1 == 1])/10000 }) # sum of the hectares that have value '1'
  print((area1/30900)*100)
  
  png(paste0(dir_export, "/MARXAN_RASTER/STRICT_CONSERVATION/", set, "/curves/", list_export, ".png"), height=1800, width=2050, pointsize=24)
  plot(mean_targets, area1, type = "b", frame = FALSE, pch = 19, xlim= c(0, max(mean_targets)*1.3), ylim=c(0, 32000), lwd=2,
       col = "blue", xlab = "targets weighted", ylab = "area subconques protected", main="sum area of conques vs. increase targets") # we can add the line of grid in the end
  
  if (length(list2[[1]]) == 30901) { abline(h=sum(area(PU_grid))/10000, col="red", lwd=1.7, lty=2) } else { # add asymptote to total area
    abline(h=sum(area(PU_conques))/10000, col="red", lwd=1.7, lty=2) } # add asymptote to total area
  
  dev.off()
}

### Function to create and export blm plot
fun_blm <-  function(set, list1, list_export) {
  
  list_p <- get(paste0(list1, "_p")) # from character to actual object
  list_s <- get(paste0(list1, "_s")) # from character to actual object
  list_blm <- get(paste0(list1, "_blm")) # from character to actual object
  
  cost_area <- pbmapply(function(x,y) {eval_cost_summary(x, y[, "solution_1"]) }, x=list_p, y=list_s) 
  cost_area <- unlist(cost_area[2,])
  
  boundary_length <- unlist(list_blm)
  
  boundary_length <- boundary_length[seq(2, length(boundary_length), 2)]
  boundary_length <- as.numeric(boundary_length)
  
  png(paste0(dir_export, "/MARXAN_RASTER/STRICT_CONSERVATION/", set, "/blm/", list1, ".png"), height=1800, width=2050, pointsize=24)
  plot(cost_area, boundary_length, type = "b", xlim=c(min(cost_area)*0, max(cost_area)*1.2), ylim=c(min(boundary_length)*0.8, max(boundary_length)*1.2), frame = FALSE, pch = 19, lwd=2, col = "blue",
       xlab = "solution cost (equivalent to area)", ylab = "boundary length") # we can add the line of grid in the end
  text(cost_area + 10, boundary_length + 5000, labels=round(increase_blm, 1))
  dev.off()
}

### Fun area
fun_area_df <- function(PU, features, escenario, listx) {
  
  area1 <- sum(raster::area(listx[listx$solution_1 == 1, "solution_1"]))/10000 # # sum of the hectares that have value '1'
  
  row1 <- c(PU, features, escenario, round(area1, 1))
  area_df <<- rbind(area_df, row1)
  
}

### Function to export excel and png
fun_export <- function(set, x) {
  
  y <- get(x)
  
  ##### Excel of feature representation
  OUT <- createWorkbook() # Create a blank workbook
  
  addWorksheet(OUT, "df") # Add some sheets to the workbook
  # addWorksheet(OUT, "resum") # Add some sheets to the workbook
  
  writeData(OUT, sheet = "df", x = y[[3]]) # Write the data to the sheets
  # writeData(OUT, sheet = "resum", x = summary_min_mid_conques_fr) # Write the data to the sheets
  
  saveWorkbook(OUT, paste0(dir_export, "/FEATURES_REPRESENTATION/", set, "/", x, ".xlsx"), overwrite=T) # Export the file
  
  ##### shp
  shapefile(y[[2]], paste0(dir_export, "/MARXAN_RASTER/STRICT_CONSERVATION/", set, "/shp/", x), overwrite=T)
  
  ##### png
  png(paste0(dir_export, "/MARXAN_RASTER/STRICT_CONSERVATION/", set, "/png/", x, ".png"), height=1800, width=2050, pointsize=18)
  fun_plotting(y[[2]])
  dev.off()
  
}

### Rescale raster to 0-1
fun_rescale <- function(x) {(x - cellStats(x, "min"))/(cellStats(x, "max") - cellStats(x, "min"))}

check_repeated_names <- function(names) {
  # Create a data frame to store the first 2 words and their frequency
  name_df <- data.frame(name = character(), freq = numeric(), stringsAsFactors = FALSE)
  
  # Loop through each name in the list
  for (name in names) {
    # Extract the first 2 words of the name
    words <- strsplit(name, " ")[[1]][1:2]
    if (length(words) == 2) {
      first2 <- paste(words, collapse = " ")
      
      # Check if the first 2 words already exist in the data frame
      if (first2 %in% name_df$name) {
        # Increment the frequency of the first 2 words
        name_df[name_df$name == first2, "freq"] <- name_df[name_df$name == first2, "freq"] + 1
      } else {
        # Add a new row to the data frame with the first 2 words and a frequency of 1
        name_df <- rbind(name_df, data.frame(name = first2, freq = 1))
      }
    }
  }
  
  # Return a data frame with the repeated first 2 words and their frequency
  return(name_df[name_df$freq > 1, ])
}

##  Initial code  ----------------------------------------------------------

### Errors to warnings
# If we see that some warnings are turned to errors, run the following:
options(warn=1)

### Install packages in case they are not installed

list.of.packages <- c("tidyverse", "readr", "rgdal", "sp", "raster", "sf", "rgeos", "prioritizr", "readxl",
                      "maptools", "PBSmapping", "pbapply", "tools", "DescTools", "Rsymphony", "spdep", "openxlsx",
                      "climateStability", "spdep", "parallel", "foreach", "zoom")
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

#dir<-"C:/Users/guillem.pocull/OneDrive - ctfc.cat/PLANIFICACIO_MONTSENY/Pla_Proteccio_MSY"
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

#### HICs
filenames <- list.files(paste0(dir_export, "/HIC_RASTER/asc"), pattern="*.asc", full.names=T)
list_HIC <- pblapply(filenames, raster)
names(list_HIC) <- sub(".asc$", "", filenames %>% basename %>% file_path_sans_ext)

pos <- match("9100", names(list_HIC))
names(list_HIC)[pos] <- "91E0" #Change 9100 to 91E0, for concordance.

#### Boscos madurs
bosc_madur <- raster(paste0(dir_export, "/BOSCOS_MADURS/r_boscos_madurs.asc"))

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

setdiff(names(list_nius), names(list_obs))
#zonacio_df <- rbind(especies_df, habitats_df)
zonacio_df<-read_excel(paste0(dir, "/documents/NOVA_taula_zonacio_msy.xlsx"), sheet="elements_montseny")

old_names<-c("22.3417","22.414","22.422","35.23","41.774","42.132")
new_names<-c("22.3417","22.414","22.422","35.23","41.774","42.132")
zonacio_df$element <- ifelse(zonacio_df$element %in% old_names, new_names[match(zonacio_df$element, old_names)], zonacio_df$element) #Change elements that are 42.199999
zonacio_df2<-zonacio_df

#### Locked-in
# sp_lockin: Calotriton arnoldi, Rosalia alpina, Austropotamobius pallipes and Buxbaumia viridis
filenames <- list.files(paste0(dir_export, "/SPECIES_RASTER/shp/LOCK_IN"), pattern="*.shp", full.names=T)
list_conques_lockin <- pblapply(filenames, shapefile, verbose=F)
names(list_conques_lockin) <- sub(".shp$", "", filenames %>% basename %>% file_path_sans_ext)

#### Locked-out
lock_out <- raster(paste0(dir_export, "/CONSTRAINTS_RASTER/asc/grid_all_constraints_100m.asc")) # we have to previously create a raster layer with the same resolution and binary info to lock out or not.
lock_out_v <- shapefile(paste0(dir_export, "/CONSTRAINTS_RASTER/shp/grid_all_constraints_100m.shp")) # G: official EBBA2 countries and masks

# If constraints layer is a vector, we can do the following: locked_out_raster <- sim_pu_polygons[sim_pu_polygons$locked_out == 1, ]

#### Costs for grid
cost_grid <- raster(paste0(dir_export, "/COSTS_RASTER/asc/cost_total_amb_public.asc")) 

##### Finques públiques
# It is used to export the exercice with the "public/privat" behind
finques_publiques <- raster(paste0(dir_export, "/COSTS_RASTER/asc/cost_public.asc")) 
finques_publiques[finques_publiques == 2] <- 0
finques_publiques[finques_publiques == 0.5] <- 1


##  Set projections  ----------------------------------------------------------

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

ambit_RB <- spTransform(ambit_RB, proj_EPSG25831) # It doesn't work with ambit_RB so I do it without the function

#### Set crs to unique rasters
crs(lock_out) <- proj_EPSG25831
crs(lock_out_v) <- proj_EPSG25831
crs(grid_100m_r) <- proj_EPSG25831
crs(PU_conques) <- proj_EPSG25831
crs(grid_richness_no_SDM) <- proj_EPSG25831
crs(cost_grid) <- proj_EPSG25831
crs(bosc_madur) <- proj_EPSG25831
crs(finques_publiques) <- proj_EPSG25831

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

cost_grid_PN <- crop(cost_grid, extent(grid_100m_PN_r))
cost_grid_PN <- grid_100m_PN_r * cost_grid_PN

# list_costs <- pblapply(list_costs, crop, extent(grid_100m_PN_r))
# list_costs <- pblapply(list_costs, function(x) { return(x*grid_100m_PN_r) })

PU_grid <- rasterToPolygons(grid_100m_PN_r) # we transform to raster again to have the same number of cells (30901)
PU_grid@data$id <- 1:nrow(PU_grid@data)

#### Add columns to PU_grid
PU_grid@data$area_cost <- round(raster::area(PU_grid)/10000, 3) # we add the area costs in hectarees, to have range similar to connectivity
PU_grid@data$locked_in <- NA
PU_grid@data$real_cost <- cost_grid_PN[!is.na(cost_grid_PN)]
# for (i in names(list_costs)) { PU_grid@data[, i] <- list_costs[[i]][!is.na(list_costs[[i]])] } # we add the multiple costs to PUs

#### Use interesting columns and remove holes
PU_grid <- intersect(PU_grid, ambit_PN) # crop again the layer to the shape of the PN because there are a lot of cells that are not quadrants
PU_grid <- PU_grid[PU_grid$SHP == 0,] # delete the holes again

PU_grid@data[, c("layer" ,"SHP")] <- list(NULL) # remove non-interesting columns

### Subconques

colnames(PU_conques@data)[colnames(PU_conques@data) %in% "ID_GRAFIC"] <- "id"
PU_conques@data$id <- as.character(PU_conques@data$id) 
# PU_conques@data$area_cost <- raster::area(PU_conques)/10000 # in this case, use the area of each conques

PU_conques <- intersect(PU_conques, ambit_PN)  # crop the layer to the shape of the PN
PU_conques <- PU_conques[PU_conques$SHP == 0, ] # delete the holes

#### Add column of costs
PU_conques <- raster::extract(cost_grid, PU_conques, method="simple", fun=mean, sp=T)
colnames(PU_conques@data)[ncol(PU_conques@data)] <- "mean_cost"

### Nius
# list_nius <- pblapply(list_nius, intersect, ambit_PN)  # crop the layer to the shape of the PN
# list_nius <- pblapply(list_nius, crop, extent(ambit_PN))  # crop the layer to the shape of the PN
list_nius <- pblapply(list_nius, raster::rasterize, grid_100m_r, 'presence')  # crop the layer to the shape of the PN

raster_masked_list <- pblapply(list_nius, function(x) { #emmascarem els rasters amb la forma del montseny
  r_masked <- mask(x, ambit_PN) #Remove the birds with nests out of the PN. 
  return(r_masked)
}) 

filter_nius <- pbsapply(raster_masked_list, function(x) { return(1 %in% values(x)) })
list_nius <- list_nius[filter_nius] # remove empty raster nius

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

### Matrix framework to find the euclidian distance between the centroid of each cell and its 8 neighbours on grid

# rook_dis <- names(table(connect_df$boundary))[1] %>% as.numeric # the first one is the rook distance (in m)
# diag_dis <- names(table(connect_df$boundary))[2] %>% as.numeric # the first one is the diagonal distance (in m)
# 
# centroids <- sf::st_coordinates(suppressWarnings(sf::st_centroid(st_as_sf(PU_grid))))
# 
# centroids <- SpatialPoints(coords = centroids)
# crs(centroids) <- proj_EPSG25831
# 
# m_rook <- proximity_matrix(centroids, rook_dis + 1)
# m_queen <- proximity_matrix(centroids, diag_dis + 1)
# sum_dist <- m_rook + m_queen
# 
# for (i in 1:ncol(sum_dist)) {
#   print(paste0(round(i/ncol(sum_dist)*100,1) , "%"))
#   sum_dist[,i][sum_dist[,i]==2] <- rook_dis
#   sum_dist[,i][sum_dist[,i]==1] <- diag_dis
# }
# connect_grid_m <- sum_dist

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


##  Create lock-in and lock-out   ----------------------------------------------------------

### Lock-in

#### Conques
list_conques_lockin <- pblapply(list_conques_lockin, intersect, ambit_PN)
list_conques_lockin <- pblapply(list_conques_lockin, function(x) { return(x[x$SHP == 0, ]) })
list_conques_lockin <- pblapply(list_conques_lockin, function(x) { x[is.na(x$PRESENCIA), "PRESENCIA"] <- 0; return(x) }) # presence and absences as 0 and 1s

PU_conques@data$locked_in <- NA
PU_conques$locked_in <- rowSums(sapply(list_conques_lockin, `[[`, "PRESENCIA")) # we create a matrix with each species as column and then we sum the rows of the matrix 
PU_conques$locked_in <- as.logical(PU_conques$locked_in) # transformed as logical because it is the standard way for 'lock in' procedure

#### Grid
# list_grid_lockin <- pblapply(list_conques_lockin, raster::rasterize, grid_100m_PN_r, 'PRESENCIA')
# PU_grid$locked_in <- rowSums(sapply(list_grid_lockin, function(x){ x[!is.na(x)] })) # we create a matrix with each cell values of each species as column and then we sum the rows of the matrix 
# PU_grid$locked_in <- as.logical(PU_grid$locked_in) # transformed as logical because it is the standard way for 'lock in' procedure

### Lock-out

#### Conques
PU_conques$lock_out <- raster::extract(lock_out, PU_conques, method="bilinear", fun=mean, sp=F)[, 1]
PU_conques$lock_out <- ifelse(PU_conques$lock_out < 0.5, T, F) # here we are saying that if more that half the area is covered by transport or urban area, set them to lock-out.
table(PU_conques@data$lock_out) # we just have locked-out two subconques

# lock_out_v_sf <- st_as_sf(lock_out_v)
# PU_conques_sf <- st_as_sf(PU_conques)
# 
# st_agr(lock_out_v_sf) = "constant"
# st_agr(PU_conques_sf) = "constant"
# 
# intersect_sf <- st_intersection(lock_out_v_sf, PU_conques_sf) # intersect lock-out areas to the subsubconques
# 
# intersect_sf <- intersect_sf %>% 
#   mutate(area = st_area(.) %>% as.numeric()) # add in areas in m2
# 
# intersect_df <- intersect_sf %>% 
#   as_tibble() %>% 
#   group_by(ID_GRAFIC) %>% 
#   summarize(area = sum(area)) %>% as.data.frame # for each PU_conques_sf, get area per lock_out_v type
# 
# 
# total_area_df <- PU_conques_sf %>% 
#   mutate(area = st_area(.) %>% as.numeric()) # add in areas in m2
# 
# total_area_df <- total_area_df %>% 
#   as_tibble() %>% 
#   group_by(ID_GRAFIC) %>% 
#   summarize(area = sum(area)) %>% as.data.frame # for each PU_conques_sf, get area per lock_out_v type
# 
# total_area_df <- merge(intersect_df, total_area_df, by="ID_GRAFIC", all.y=T)
# 
# total_area_df$prop <- round(total_area_df$area.x/total_area_df$area.y, 3) # proportion of area of lock-out that falls on each subsubconca
# 
# total_area_df$prop_bi <- ifelse(total_area_df$prop > 0.8, 1, 0)


# PU_conques$locked_out <- lock_out[!is.na(lock_out),] # we create a matrix with each species as column and then we sum the rows of the matrix 
# PU_conques$locked_out <- as.logical(PU_conques$locked_out) # transformed as logical because it is the standard way for 'lock in' procedure

#### Grid
# lock_out <- crop(lock_out, extent(grid_100m_PN_r))
# lock_out <- grid_100m_PN_r * lock_out
# PU_grid$locked_out <- lock_out[!is.na(lock_out),] 
# PU_grid$locked_out <- as.logical(PU_grid$locked_out) # transformed as logical because it is the standard way for 'lock out' procedure


##  Create budget  ----------------------------------------------------------

# Pla Protecció Montseny 2008

# Zona Reserva Natural (super protected): 2606 ha
# Zona Alt Interès Natural, Ecològic i Paisatgístic (medium protection): 6459 ha
# Zona Interès Natural (basic protection): 21768 ha --> we now don't create this exercice
# Zona Urbana (outside any protection): 254 ha

a_total_Pla_Protect_2008 <- 2606 + 6459 + 21768 # we do not sum 'Zona Urbana' because it is excluded from the area of PU_conques

coef_lax <- 2606 / a_total_Pla_Protect_2008 # coefficient of how much area was ZRN in the Pla Protecció Montseny 2008
coef_mid <- (6459 + 2606) / a_total_Pla_Protect_2008 # coefficient of how much area was ZAINEP and ZRN in the Pla Protecció Montseny 2008
coef_amb <- 21768 / a_total_Pla_Protect_2008 # coefficient of how much area was ZIN in the Pla Protecció Montseny 2008


### Conques

filter_lockout <- PU_conques$lock_out != T
filter_lockout[is.na(filter_lockout)] <- T
PU_conques_lockinout <- PU_conques[filter_lockout, ]

filter_lockin <- PU_conques_lockinout$locked_in != T 
filter_lockin[is.na(filter_lockin)] <- T
PU_conques_lockinout <- PU_conques_lockinout[filter_lockin, ]


cost_total_conques <- sum(PU_conques_lockinout$mean_cost, na.rm=T)

budget_conques <- c(coef_lax * cost_total_conques, # maximum budget for laxed budget on conques,
                    coef_mid * cost_total_conques, # maximum budget for medium budget on conques
                    coef_amb * cost_total_conques) # maximum budget for medium budget on conques


### Grid
cost_total_grid <- sum(PU_grid$real_cost, na.rm=T)
budget_grid <- c(coef_lax * cost_total_grid, # maximum budget for laxed budget on conques,
                 coef_mid * cost_total_grid, # maximum budget for medium budget on conques
                 coef_amb * cost_total_grid) # maximum budget for medium budget on conques


##  Create scenarios   ----------------------------------------------------------

# Pla Protecció Montseny 2008
# Zona Reserva Natural (super protected): 2606 ha
# Zona Alt Interès Natural, Ecològic i Paisatgístic (medium protection): 6459 ha
# Zona Interès Natural (basic protection): 21768 ha --> we now don't create this exercice
# Zona Urbana (outside any protection): 254 ha

esc_df <- data.frame(escenario = c("lax", "mid", "amb"), # escenarios
                     
                     position = c(2, # lax targets, so we strictly protect only ZRN
                                  4, # mid targets, so we strictly protect ZAINEP
                                  9), # ambitious targets, so we strictly protect all ZIN
                     
                     budget_conques = budget_conques,
                     budget_grid = budget_grid) 

##  Create and integrate elements  ----------------------------------------------------------

# #### Edit richness 'puntual' species
# m <- matrix(1, 5, 5)
# m[2,2] <- m[2,3] <- m[2,4] <- m[3,4] <- m[4,4] <- m[3,2] <- m[4,2] <- m[4,3] <- 2
# m[3,3] <- 3

# ### For richness without SDM
# grid_richness_no_SDM <- focal(grid_richness_no_SDM, w=m, mean) # smooth the raster
# grid_richness_no_SDM <- fun_rescale(grid_richness_no_SDM) # rescale between 0 and 1
# grid_richness_no_SDM[grid_richness_no_SDM>=0.1] <- 1  # we remove the places with really low richness
# grid_richness_no_SDM[grid_richness_no_SDM<0.1] <- 0  # we remove the places with really low richness
# list_richness <- list(grid_richness_no_SDM)
# names(list_richness) <- "grid_richness_no_SDM"
# 
# ### For richness without SDM and only conservation status
# grid_richness_no_SDM <- focal(grid_richness_no_SDM, w=m, mean) # smooth the raster
# grid_richness_no_SDM <- fun_rescale(grid_richness_no_SDM) # rescale between 0 and 1
# grid_richness_no_SDM[grid_richness_no_SDM>=0.1] <- 1  # we remove the places with really low richness
# grid_richness_no_SDM[grid_richness_no_SDM<0.1] <- 0  # we remove the places with really low richness
# list_richness <- list(grid_richness_no_SDM)
# names(list_richness) <- "grid_richness_no_SDM"

### Integrate features

#### Little edit of "bosc madur"
bosc_madur <- as.list(bosc_madur)
names(bosc_madur) <- "bosc_madur"

### Hierarchical decision
# nius = flora amenaçada > SDM > observacions
#Before adding observations 
#list_sdm <- list_sdm[! word(names(list_sdm), start = 1, end = 2, sep = fixed(" ")) %in% # use the first two words
                       #names(list_nius)] # here we remove the SDM elements that have nius

#list_sdm <- list_sdm[! word(names(list_sdm), start = 1, end = 2, sep = fixed(" ")) %in% # use the first two words
                       #names(list_flora_amen)] # here we remove the SDM elements that have flora amenaçada

hierarchy <- list(list_nius, list_flora_amen, list_HIC, list_sdm, list_obs) # Define the hierarchy of the lists

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
setdiff(noms_taula, noms_features)
diferencies #We accept only species with more than 1 scientific name
list_features <- list_features[!names(list_features) %in% diferencies] #Remove all these names

list_features <- pblapply(list_features, crop, extent(ambit_PN))
list_features <- pblapply(list_features, mask, ambit_PN)

#### Paralelize the following function

list_features <- pblapply(list_features, function(x) { x[x>0] <- 1; return(x) }) #0.5 to 1

### Remove empty features
filter_features <- pbsapply(list_features, function(x) { cellStats(x, max, na.rm=T) != 0 }) # we remove elements that are all 0 on the study area
list_features_later <- list_features[filter_features]

missing_elements <- setdiff(names(list_features), names(list_features_later))

list_features<-list_features_later
check_repeated_names(names = names(list_features)) #This function search if there are elements with the same 2 first words. There are elements that are repeated because they include subsp. But there are errors, too. NOT NOW!

stack_features <- stack(list_features) # create a rasterstack

### Integrate conservation status
#estatus_conserv <- names(table(dades_projecte_df$estatus_conserv))
#percent <- c(3, 3, 3, 1, NA, NA, NA, NA, 1, 2, 5, 1, 2, 3, 3) # this would work for "Alt", "Amenaçada però amb poblacions que no disminueixen", "Baix", "Dades Insuficients", "Extingida al Montseny", "Extingit", "LC", "Mig", "Molt Alt", "NT", "Prioritari"
#percent <- round(percent/max(percent, na.rm = T)/3, 2) # we divide by 3 because to have a small targets on the beginning
# increase <- c(0.25, 0.25, 0.25, 0, NA, NA, NA, NA, 0, 0.15, 0.75, 0, 0.15, 0.25, 0.25) # increase of the percent when doing the curve
# targets_df <- data.frame(percent=percent, estatus_conserv=estatus_conserv, increase=increase)
# rm(estatus_conserv, percent, increase)

zonacio_df <- zonacio_df[zonacio_df$inclosa_zonacio == 1, ] # we just want species that are selected

zonacio_df$estatus_zonacio <- as.numeric(zonacio_df$estatus_zonacio)
zonacio_df$targets <- round(zonacio_df$estatus_zonacio/max(zonacio_df$estatus_zonacio, na.rm = T)/10, 2) # here we find the targets for the smaller scenario (not lax, even smaller)
# zonacio_df[is.na(zonacio_df$targets), "targets"] <- min(zonacio_df$targets, na.rm = T)
zonacio_df$targets <- as.factor(zonacio_df$targets)

zonacio_df$increase <- zonacio_df$targets
levels(zonacio_df$increase) <- c("0", "0.15", "0.25", "0.75") # these are how increases each target
zonacio_df$increase <- as.numeric(as.character(zonacio_df$increase))

zonacio_df <- zonacio_df %>% relocate(targets, .after = last_col())

### Clean zonacio_df
zonacio_df$element <- substr(zonacio_df$element, 1, 40)
zonacio_df$element <- str_trim(zonacio_df$element, side = "right")

### Create objects to run the exercices
v_curve <- rep("curve", 14)
v_curve <- paste0(v_curve, 1:14)

### Increasing value for the loop
increase_weight <- seq(0.1, 2.7, 0.2) # this value will increase on each loop
names(increase_weight) <- v_curve

#### For protected/threatened features
elements_conserv <- zonacio_df
elements_conserv <- elements_conserv[is.na(elements_conserv$bloquejada_zonacio), ]
elements_conserv <- elements_conserv[order(elements_conserv$element),]
elements_conserv_name <- elements_conserv$element # we extract the name of elements with conservation status
m_rel_targets <- fun_targets(increase_weight, elements_conserv)

#### For all features

##  Check layer properties  ----------------------------------------------------------

# resample(raster1, raster2, method="bilinear") # if some raster has different resolution and dimensions, use this formula

### Dimensions
identical(extent(PU_grid)[1:4], extent(lock_out)[1:4])

### CRS
AllIdentical(as.character(crs(stack_features)), as.character(crs(lock_out)),
             as.character(crs(PU_grid)), as.character(crs(PU_conques)))  

crs(PU_grid) <- crs(lock_out)
crs(PU_conques) <- crs(lock_out)

AllIdentical(as.character(crs(stack_features)), as.character(crs(lock_out)), as.character(crs(PU_grid))) 

##  Solve problems to find the optimal blm (connectivity value)   ----------------------------------------------------------

### Set blm parameters
n_blm <- 10
v_blm <- rep("blm", n_blm)
v_blm <- paste0(v_blm, 1:n_blm)

### Increasing value for the loop
# Here I am using the method taken from Steward and Possingham (2005), explained on the page 48 of the Marxan-User-Manual-2021.
lseq <- function(from=1, to=100000, length.out=6) { exp(seq(log(from), log(to), length.out = length.out)) } # function to create logarithmic sequences.
increase_blm <- round(lseq(0.0001, 1000, n_blm), 6)
names(increase_blm) <- v_blm
increase_blm

##### Create objects conques
fun_create("list_conques_blm_lax")
fun_create("list_conques_blm_mid")


##### Develope solution
for (i in 1:length(v_blm)) {
  
  print(paste0(i, " of ", length(v_blm)))
  
  rel_targets <- round(m_rel_targets[, 2], 2) # lax
  
  fun_min_prioritizr(PU_conques, elements_conserv_name, rel_targets, "list_conques_blm_lax", connect_conques, increase_blm[i]) # for the 'subconques'
  # fun_min_prioritizr(PU_grid,  elements_conserv_name, rel_targets, "list_grid_blm_lax", connect_grid, increase_blm[i]) # for the 'grid'
  
  rel_targets <- round(m_rel_targets[, 4], 2) # mid
  
  fun_min_prioritizr(PU_conques, elements_conserv_name, rel_targets, "list_conques_blm_mid", connect_conques, increase_blm[i]) # for the 'subconques'
  # fun_min_prioritizr(PU_grid,  elements_conserv_name, rel_targets, "list_grid_blm_mid", connect_grid, increase_blm[i]) # for the 'grid'
  
}



#### Export the blm plots as png
##### Conques
fun_blm("min", "list_min_conques_blm_no_status_lax", "conques_no_status_lax")
fun_blm("min", "list_min_conques_blm_no_status_mid", "conques_no_status_mid")

##  Solve problems to find the targets vs. area curve   ----------------------------------------------------------

### Minimum set
#### Create objects
fun_create("list_min_conques_curve")

#### Develope solution
for (i in 1:length(v_curve)) {
  
  print(paste0(i, " of ", v_curve[length(v_curve)]))
  rel_targets <- round(m_rel_targets[, i], 2)
  
  fun_min_prioritizr(PU_conques, elements_conserv_name, rel_targets, "list_conques_curve", connect_conques, 3) # for the 'subconques'

}

# #### Find the status for each target category
# targets_summary <- apply(m_rel_targets, 2, function(x) { unique(x) }) # here we print a summary of the increasing targets
# targets_summary <- apply(targets_summary, 2, function(x) { sort(x) }) # sort results from 'baix' to alt'
# targets_summary <- round(targets_summary * 100, 1) # as percentage
# if(nrow(targets_summary) == 3) { rownames(targets_summary) <- c("baix", "mig", "alt") }
# if(nrow(targets_summary) == 4) { rownames(targets_summary) <- c("baix", "mig", "alt", "molt_alt") }

print(targets_summary)

#### Export the curve plots as png
fun_curve_plot("min", "list_conques_curve_s", "conques")


### Maximum set

#### Create scenarios for a sequence of protected areas
# cost_total_conques <- sum(PU_conques$mean_cost, na.rm=T)
seq_budget <- seq(0.1, 1, 0.1) * cost_total_conques

#### Create objects
fun_create("list_max_conques_curve")

for (i in 1:length(seq_budget)) {
  
  rel_targets <- round(m_rel_targets[, 10], 2) # in maximum set, we use only the proportion of targets
  
  fun_max_prioritizr(PU_conques, elements_conserv_name, rel_targets, seq_budget[i], "list_max_conques_curve", connect_conques, 3)
  
}


#### Export the curve plots as png

fun_curve_plot("max", "list_max_conques_curve_s", "conques")


### Find the mean of the representation features

a <- list_max_conques_curve_fr[1][[1]]

merge(a, zonacio_df, "feature", "targets", by.x=T, by.y=)


b <- list_max_conques_curve_fr[2][[1]]

##  Solving unique problems   ----------------------------------------------------------

### Set the parameters

#### areas

area_df <- data.frame(PU=NA, features=NA, escenario=NA, area=NA )
area_df <- area_df[-1,]

### Solving conques ----------------------------------------------------------

fun_create("list_max_conques_lax")
fun_create("list_max_conques_mid")

fun_max_prioritizr(PU_conques, elements_conserv_name, rel_targets, seq_budget[1], "list_max_conques_lax", connect_conques, 0) # for the 'subconques'
fun_max_prioritizr(PU_conques, elements_conserv_name, rel_targets, seq_budget[1], "list_max_conques_lax", connect_conques, 50) # for the 'subconques'

fun_max_prioritizr(PU_conques, elements_conserv_name, rel_targets, 174.6853, "list_max_conques_mid", connect_conques, 0.01) # for the 'subconques'

### Solving grid ----------------------------------------------------------
# 
# for(i in 1:nrow(esc_df)) {
#   
#   print(esc_df[i, "escenario"])
#   
#   esc <- esc_df[i, "position"]
#   name_esc <- esc_df[i, "escenario"]
#   
#   #### Create lists and solve problem
#   fun_create("list1")
#   rel_targets <- round(m_rel_targets_status[, esc], 2)
#   fun_min_prioritizr(PU_grid, elements_conserv_name_status,  rel_targets, "list1", connect_grid, 0.8)
#   
#   #### Find representation targets 
#   list1_fr <- fun_fr(list1_fr, m_rel_targets_status[, esc])
#   
#   #### Find area
#   fun_area_df("grid", "status", name_esc, list1_s)
#   
#   #### Nest list and remove the others
#   list1 <- list(list1_p[[1]], list1_s[[1]], list1_fr)
#   assign(paste0("_grid_status", name_esc), list1, envir = .GlobalEnv)
# 
#   #### Export
#   fun_export(paste0("grid_status", name_esc))
#   
# }
# 
##  Integrate solutions ----------------------------------------------------------

### Table with the numeric results


##  Export results  ----------------------------------------------------------
### Parameters

my_height <- nrow(grid_100m_r)*5
my_width <- ncol(grid_100m_r)*5

### Export solutions as shapefiles

#### Area vs. target curves
for (i in 1:length(increase_weight)) {
  
  print(paste(i, "of", length(increase_weight)))
  
  shapefile(list_conques_curve_status_s[[i]], paste0(dir_export, "/MARXAN_RASTER/STRICT_CONSERVATION/shp/list_conques_curve_status/", i), overwrite=T)
  
  # writeOGR(list_grid_curve_status_s[[i]], paste0(dir_export, "/MARXAN_RASTER/STRICT_CONSERVATION/shp/list_grid_curve_status/"),
  #          i, driver="ESRI Shapefile", overwrite_layer=T)
}


#### Area excel

write.csv(area_df, paste0(dir_export, "/MARXAN_RASTER/STRICT_CONSERVATION/area/area_df.csv"), row.names = FALSE)

### Export solutions as png

#### Area vs. target curves
for (i in 1:length(increase_weight)) {
  
  print(paste(i, "of", length(increase_weight)))
  
  png(paste0(dir_export, "/MARXAN_RASTER/STRICT_CONSERVATION/png/list_conques_curve_status/",
             i, ".png"), height=1800, width=2050, pointsize=18)
  fun_plotting(list_conques_curve_status_s[[i]])
  dev.off()
  
  # png(paste0(dir_export, "/MARXAN_RASTER/STRICT_CONSERVATION/png/list_grid_curve_status/",
  #            i, ".png"), height=1800, width=2050, pointsize=18)
  # fun_plotting(list_grid_curve_status_s[[i]])
  # dev.off()
}

### Export as vectorized postcript (the last phase to present)
# setEPS()
# postscript(file=paste0(dir_export, "/MARXAN_RASTER/eps/s1.png"), width=2050, height=1800)
# par(mar=c(4,5,2,2))
# plot(s1, main = "s1", axes = FALSE, box = FALSE)
# dev.off()

##  Modify the project table ----

### Count cells occupied by all features. 
count <- cellStats(stack_features, stat = "sum", na.rm = TRUE)  #sum occupied cells by element
names(count)<-names(list_features)
occupied_cells <- data.frame(element = names(count), occupied_cells = count, row.names = NULL) #DF from the sum.

rst_values <- raster::extract(stack_features, PU_conques, df=TRUE, fun=sum, na.rm=T) #Sum the values of the SDM for PU
rst_values <- dplyr::select(rst_values, -ID)
rst_values<-colSums(rst_values != 0) #How many PU contain at least one 1 value for each feature
names(rst_values)<-names(list_features)
occupied_conques <- data.frame(element = names(rst_values), occupied_conques = rst_values, row.names = NULL) #DF

occupancy<-dplyr::inner_join(occupied_cells, occupied_conques, by="element") #Join the 2 dataframe

zonacio_df$element <- substr(zonacio_df$element, 1, 40) #we cut the name of species, becoming 40 characters
zonacio_df$element <- str_trim(zonacio_df$element, side = "right") #we remove the empty characters in the right
zonacio_df<-dplyr::left_join(zonacio_df, occupancy, by="element")

#Give 0 to elements that are not physically in PN MSY
rows_to_replace <- zonacio_df$element %in% missing_elements
zonacio_df$occupied_cells[rows_to_replace] <- 0
zonacio_df$occupied_conques[rows_to_replace] <- 0

#Same, but this comes from the last chunk of script 1.4. 
rows_to_replace <- zonacio_df$element %in% no_presents
zonacio_df$occupied_cells[rows_to_replace] <- 0
zonacio_df$occupied_conques[rows_to_replace] <- 0

zonacio_df<-dplyr::select(zonacio_df, -c("area_ocupacio (ha)", "num_PUs"))
colnames(zonacio_df)[c(14,15)]<-c("area_ocupacio (ha)", "num_PUs")

#Change order of columns
zonacio_df <- zonacio_df %>%
  relocate(`area_ocupacio (ha)`, .before = 9) %>%
  relocate(num_PUs, .before = 10)

zonacio_df <- zonacio_df[!(zonacio_df$element %in% c("Tylopsis lilifolia", "Oberea linearis (Linnaeus, 1758)", "Melitaea deione Duponchel, 1832")), ] #Remove repeated elements.

#Fill with "sense dades" those features for which we do not have spatial information. 
zonacio_df$`area_ocupacio (ha)` <- ifelse(is.na(zonacio_df$`area_ocupacio (ha)`), "Sense_dades", zonacio_df$`area_ocupacio (ha)`)
zonacio_df$num_PUs <- ifelse(is.na(zonacio_df$num_PUs), "Sense_dades", zonacio_df$num_PUs) 
zonacio_df$tipus_informacio <- ifelse(zonacio_df$`area_ocupacio (ha)` == "Sense_dades", "Sense_dades", zonacio_df$tipus_informacio) #If we don't have data for a species, we write sense dades

# Write occupation area of "Bosc Madur"
index <- which(zonacio_df$element == "bosc_madur")
zonacio_df$`area_ocupacio (ha)`[index] <- cellStats(bosc_madur[[1]], stat="sum") #Fill the occupancy of bosc madur
zonacio_df$tipus_informacio[index] <- "poligons"
zonacio_df$num_PUs[index] <- 0 #0 per no tenir el sense_dades
zonacio_df$tipus_informacio <- ifelse(!is.na(zonacio_df$`area_ocupacio (ha)`) & is.na(zonacio_df$tipus_informacio), "observacions",
                                       ifelse(is.na(zonacio_df$tipus_informacio), "Sense_dades", zonacio_df$tipus_informacio))


#Write the table
write.xlsx(zonacio_df, "C:/Users/david.munoz/Downloads/comprovacio_estat_MSY/novataula.xlsx", col_names=T)


## Check status_conservation and synonyms in the table
taula$specie_assessment <- str_extract(taula$element, "\\w+\\W+\\w+")
catalegs<-read.xlsx("C:/Users/david.munoz/Downloads/comprovacio_estat_MSY/species_status_ACTBIO.xlsx")

coincideixen<-dplyr::inner_join(catalegs, taula, by="specie_assessment")
coincideixen<-dplyr::select(coincideixen, specie_assessment, status, assessment, estatus_cat, estatus_eu)
no_coincideixen<-dplyr::anti_join(taula, catalegs) #Aqui dins hem de buscar els sinonims d'altres taules

catalegs2<-read.xlsx("C:/Users/david.munoz/Downloads/comprovacio_estat_MSY/20230309_Tspecies.xlsx")
catalegs2<-filter(catalegs2, specie_synonym  =='TRUE') #especies amb noms diferents entre GBIF i nom cientific

#Aquí ja entrem al nivell de sinònims
matching_positions <- c()

# Loop over each row in df1
for (i in 1:nrow(catalegs2)) {
  # Split the elements in col1 into two words using `strsplit`
  words <- unlist(strsplit(as.character(catalegs2[i, "scientific_name"]), " "))
  word1 <- words[1]
  word2 <- words[2]
  
  # Check if any element in col2 of df2 contains both word1 and word2
  for (j in 1:nrow(no_coincideixen)) {
    if (grepl(word1, as.character(no_coincideixen[j, "element"])) && grepl(word2, as.character(no_coincideixen[j, "element"]))) {
      # If the condition is true, store the index of the matching position
      matching_positions <- c(matching_positions, j)
    }
  }
}

#Matching positions és quines espècies de no_coincideixen són iguals a espècies que presenten sinònims al tesaure.
#Busquem les matching positions als catalegs i veurem quines espècies tenen sinònims i per què. 
