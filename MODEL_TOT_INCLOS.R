#####################################################################################################################
#####################################################################################################################
############################################### MODEL 6 MESOS OBS ###################################################
#####################################################################################################################
#####################################################################################################################

library(ROCR)
library(dismo)
library(raster)
library(rgdal)
library(ENMeval)
library(nhSDM)
library(dplyr)
library(sp)
library(spocc)
library(spThin)
library(rgeos)
library(readr)
library(rJava)
library(wallace)
library(move)
source(system.file('shiny/funcs', 'functions.R', package = 'wallace'))
library(nhSDM)


##################### SELECCIÓ OBSERVACIONS

#Entra la taula de GPS i filtrem. Podria se la d'OBS, però caldria filtrar altres coses. 
taulatot<-read.csv("Z:/OBSERVACIONS/ANTIC/BASE_ORIGINAL/Radiotracking_ossos_1996_2020_taula_final.csv", header = T, dec = ",", sep = ";")
mascles<-dplyr::filter(taulatot, Sex=="F")
mascles<-dplyr::filter(mascles, With_cubs_estimated =="<6month")

#Fem que la taula sigui acceptada per les funcions de després. 
colselect<-c("x_long", "y_lat", "Age_class", "ID_obs")
tallat<-mascles %>% dplyr::select(colselect)
crs_output<-"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
caracters <- c("longitude","latitude")
colselect<-c("ID_obs", "name", "longitude.1", "latitude.1")

x<-mascles[["Age_class"]]
x<-as.character(x)
x[x == 1|2|3|NA] <- "Ursus"
mascles[["Age_class"]]<-x
mascles<-rename(mascles, "name" = "Age_class")
mascles<-rename(mascles, "longitude" = "x_long")
mascles<-rename(mascles, "latitude" = "y_lat")
mascles[caracters] <- sapply(mascles[caracters],as.numeric)

#Passem de dataframe a punts reprojectats. 
mascles <- SpatialPointsDataFrame(coords = mascles[,16:17], data = mascles,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
mascles <- spTransform(mascles, crs_output)
mascles<-as.data.frame(mascles)
mascles<-mascles %>% dplyr::select(-c("longitude","latitude"))
mascles$longitude.1<-round(mascles$longitude.1)
mascles$latitude.1<-round(mascles$latitude.1)
mascles<-rename(mascles, "longitude" = "longitude.1")
mascles<-rename(mascles, "latitude" = "latitude.1")

#Apliquem SPTHIN. Canviar valors, segons projeccions, etc. 

output <- spThin::thin(mascles, 'latitude', 'longitude', 'name', thin.par = 100, reps = 10, 
                       locs.thinned.list.return = TRUE, write.files = FALSE, verbose = FALSE)
maxThin <- which(sapply(output, nrow) == max(sapply(output, nrow)))
maxThin <- output[[ifelse(length(maxThin) > 1, maxThin[1], maxThin)]]  
mascles <- mascles[as.numeric(rownames(maxThin)),]  

#Seleccionem els individus que ens interessi analitzar
taulames<-mascles[mascles$Bear_name %in% c('Ziva','Melba','Hvala','Sorita'),]

consulta<-dplyr::count(mascles, Bear_name) #Veiem quantes observacions té cada individu

#Seleccionem el nombre d'obs de l'individu que en té menys. Fem 10 seleccions aleatòries i independents. 
for (i in 1:10) {
  assign(paste0("mesdeu", i), (taulames %>% group_by(Bear_name) %>% slice_sample(n=56, replace = F)))
  assign(paste0("mesdeu", i), (distinct(get(paste0("mesdeu", i)))))
}

#Ajuntem les taule i les guardem. 
taules<-list(mesdeu1,mesdeu2,mesdeu3,mesdeu4,mesdeu5,mesdeu6,mesdeu7,mesdeu8,mesdeu9,mesdeu10)
colselect<-c("ID_obs", "name", "longitude", "latitude")
taules<-lapply(taules, "[", colselect)
for (i in 1:10) {
  write.csv(taules[[i]], paste0("Z:/MODELS_R/ASPECTE/obs/GPS_SPTHIN",i,".csv"))
}

#Canvis de nom i occs.xy jajaaj
occs <- taules
colselect<-c("longitude", "latitude")
occs.xy<-lapply(occs, "[", colselect)

#Entrada de les variables ambientals i seleccionem coordenades pels background samplings
d.envs <- 'Z:/MODELS_R/PREDICTORS_FINALS_PYR'
userRas.paths<- list.files(d.envs, pattern="*.tif", full.names=TRUE)
userRas.paths<-userRas.paths[c(1:5,8,14:23,25)]
envs <- raster::stack(userRas.paths)
crs(envs)<- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

bg.xy<-list(1:length(occs))
for (i in 1:length(occs)) {
  bg.xy[[i]] <- dismo::randomPoints(envs, 10000)
  bg.xy[[i]] <- as.data.frame(bg.xy[[i]])  
  colnames(bg.xy[[i]]) <- c("longitude", "latitude")
  write.csv(bg.xy[[i]], file = paste0("Z:/MODELS_R/ASPECTE/RESULTATS_MODELS/bg_samplings/bg_samplings_",i,".csv"))
}

#Es fan els grups per avaluar dins de cada iteració els models. 
group.data<-list(1:length(occs.xy))
for (i in 1:length(occs.xy)) {
  group.data[[i]] <- ENMeval::get.randomkfold(occ = occs.xy[[i]], bg = bg.xy, kfolds = 4)
}

occs.grp<-list(1:length(occs.xy))
bg.grp<-list(1:length(occs.xy))

for (i in 1:length(group.data)) {
  occs.grp[[i]] <- group.data[[i]][[1]]
  bg.grp[[i]] <- group.data[[i]][[2]]
}

#Definim els paràmetres del model. 
rms <- seq(1, 2, 1)
e<- vector(mode="list", length=length(occs)) 
evalTbl<-list(1:length(occs))
evalMods<-list(1:length(occs))
evalPreds<-list(1:length(occs))

#I fem córrer el model i el guardem. 
for (i in 1:length(occs)) {   
  e[[i]] <- ENMeval::ENMevaluate(occ = occs.xy[[i]], env = envs, bg.coords = bg.xy[[i]],
                            RMvalues = rms, fc = c('L','LQ'), method = 'user', 
                            occ.grp = occs.grp[[i]], bg.grp = bg.grp[[i]], 
                            clamp = TRUE, algorithm = "maxent.jar")
  
  evalTbl[[i]] <- e[[i]]@results
  evalMods[[i]] <- e[[i]]@models
  names(evalMods[[i]]) <- e[[i]]@tune.settings$tune.args
  evalPreds[[i]] <- e[[i]]@predictions
  readr::write_rds (e[[i]], paste0("Z:/MODELS_R/ASPECTE/RESULTATS_MODELS/Model_6mesos_GPS_",i,".rds"))
}

#Guardem les taules de contribucio de les variables i evaluacio dels models, mapes, etc. 
model<-list(1:length(occs))
importancia<-list(1:length(occs))
evaluacio<-list(1:length(occs))
lambdas<-list(1:length(occs))

for (i in 1:length(occs)) {  
  model[[i]]<-evalPreds[[i]][[3]]
  crs(model[[i]])<- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
  writeRaster(model[[i]], paste0("Z:/MODELS_R/ASPECTE/RESULTATS_MODELS/Mapes/prediccio_6mesos_",i,".tif"), 
              overwrite= T)
  
  importancia[[i]]<-as.data.frame(e[[i]]@variable.importance[[3]]) #######RECORDEM QUE AQUEST 3 ES POT CANVIAR########
  write.csv(importancia[[i]], paste0("Z:/MODELS_R/ASPECTE/RESULTATS_MODELS/Contribucio_variables/Cont_6mesos_GPS_",
                                     i,".csv"))
  evaluacio[[i]]<-as.data.frame(evalTbl[[i]])
  write.csv(evaluacio[[i]], paste0("Z:/MODELS_R/ASPECTE/RESULTATS_MODELS/evaluacio_models/eval_6mesos_GPS_",i,
                                   ".csv"))
  
  png(filename=paste0("Z:/MODELS_R/ASPECTE/RESULTATS_MODELS/Corbes_resposta/corbes_6mesos_GPS_",i,".png"), 
      width=1000, height=1000)
  dismo::response(e[[i]]@models[[3]], col="black", lwd=0.5, expand=0, range="p")
  dev.off()
  
  png(filename=paste0("Z:/MODELS_R/ASPECTE/RESULTATS_MODELS/Contribucio_variables/contribucio_6mesos_GPS_",i,
                      ".png"),
      width=600, height=600)
  plot(evalMods[[i]][[3]], vars = envs, type = "cloglog") #Variable importance a nivell gr?fic. 
  dev.off()
  
  lambdas[[i]]<-as.data.frame(e[[i]]@models$rm.1_fc.LQ@lambdas, sep=",") ##RECORDEM QUE AQUEST MODEL ES POT CANVIAR#
  write.csv(lambdas[[i]], paste0("Z:/MODELS_R/ASPECTE/RESULTATS_MODELS/lambdas/lambdas_6mesos_GPS_",i,".csv"), dec = ",")
}

save.image(file = "Z:/MODELS_R/ASPECTE/RESULTATS_MODELS/WSmodel6mesos_GPS.RData")

#Mitjana de les prediccions
d.preds <- 'Z:/MODELS_R/ASPECTE/RESULTATS_MODELS/Mapes'
llista_preds<- list.files(d.preds, pattern="*.tif", full.names=TRUE)
llista_preds<- llista_preds[c(1,3,5,7,9,11,13,15,17,19)]
llista_preds <- raster::stack(llista_preds)
mitjana_preds<- calc(llista_preds, fun = mean, na.rm = T)
writeRaster(mitjana_preds, filename = "Z:/MODELS_R/ASPECTE/RESULTATS_MODELS/Mapes/mitjana_prediccions.tif",  overwrite= T)

########################## APLIQUEM THRESHOLDS AL MODEL. 
observacions<-occs.xy
mitjana<-mitjana_preds
valors<-list(0:10)
adequades<-list(0:10)
bones<-list(0:10)
optimes<-list(0:10)
minim<-list(0:10)
mesquebones<-list(0:10)
valorsretall<-list(0:10)

#Càlcul del mínim per definir zones adequades. Seran la mitjana del 10% de les obs amb menys idoneïtat. 
for (i in 1:10) {
  valors[[i]]<-raster::extract(mitjana, observacions[[i]])     
  valorsretall[[i]]<-valors[[i]][valors[[i]]<quantile(valors[[i]], 0.1)]
  minim[[i]]<-mean(valorsretall[[i]])
}

adequades<-as.numeric(minim)
adequades<-mean(adequades)

rcl1<-c(0,adequades)
rcl2<-c(adequades,1)
rcl3<-c(NA,1)
rcl<-data.frame(rcl1,rcl2,rcl3)

reclassificatadequat<-reclassify(mitjana, rcl)

adequat<-reclassificatadequat*mitjana

llindarbo <- cellStats(adequat, stat='mean', na.rm=TRUE)

rcl1<-c(0,llindarbo)
rcl2<-c(llindarbo,1)
rcl3<-c(NA,1)
rcl<-data.frame(rcl1,rcl2,rcl3)

reclassificatbo<-reclassify(adequat, rcl)

bo<-reclassificatbo*mitjana

llindaroptim <- cellStats(bo, stat='mean', na.rm=TRUE)

rcl1<-c(0,llindaroptim)
rcl2<-c(llindaroptim,1)
rcl3<-c(NA,1)
rcl<-data.frame(rcl1,rcl2,rcl3)

reclassificatoptim<-reclassify(bo, rcl)

rcl1<-c(0,adequades,llindarbo,llindaroptim)
rcl2<-c(adequades,llindarbo,llindaroptim,1)
rcl3<-c(0:3)
rcl<-data.frame(rcl1,rcl2,rcl3)

reclassificatfinal<-reclassify(mitjana, rcl)


reclassificatfinal<-nh_patchdrop(rast = reclassificatfinal, min.patch = 250000, directions = 4 )
writeRaster(reclassificatfinal, "Z:/MODELS_R/ASPECTE/reclass_llindar.tif", overwrite=T)

########################################## GENERALITZACIO EN OPTIM

reclassificat<-reclassificatfinal
crs(reclassificat)<-"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

rcl1<-c(0,3.001)
rcl2<-c(2.999, 999)
rcl3<-c(0,0)
rcl<-data.frame(rcl1,rcl2,rcl3)

reclassificat<-reclassify(reclassificat, rcl)

reclassificat<-nh_patchdrop(rast = reclassificat, min.patch = 500000, directions = 4)

rcl1<-NA
rcl2<-0
rcl<-data.frame(rcl1,rcl2)

reclassificat<-reclassify(reclassificat, rcl)

writeRaster(reclassificat, "Z:/MODELS_R/ASPECTE/OPTIM_BO_PYR.tif", overwrite=T)
