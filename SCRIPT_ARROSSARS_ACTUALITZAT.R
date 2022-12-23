###############################################################################################################
###############################################################################################################
############################ DANYS CAUSATS PER LA POLLA BLAVA AL DELTA DE L'EBRE ##############################
################################################### 2022 ######################################################
###############################################################################################################
### CARREGUEM PAQUETS I POSEM DIRECTORI -------------------------------

library(raster)
library(terra)
library(rgdal)
library(foreign)
library(dplyr)
library(sp)
library(RStoolbox)
library(gridExtra)
library(ggplot2)
library(spatialEco)

setwd("C:/Users/david.munoz/OneDrive - ctfc.cat/arrossars") #Posem directori

### RETALL MONTSIA ---------------------------------------------------------------

mont<-readOGR("SIGPAC/SIGPAC_22_Montsia_2022_shp/SIGPAC_22_Montsia.shp") #Obrim els SIGPAC directament descarregats. És lent. 
#xmin, xmax,ymin,ymax
x_coord<-c(295363,295367,320614,320614,311013)
y_coord<-c(4497774,4510180,4511053,4503551,4498274)
xym <- cbind(x_coord, y_coord)

p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
crs(sps)<-crs(mont)

retall_mont<-crop(mont, sps)
retall_mont <- subset(retall_mont, US == "TA")
### RETALL BAIX EBRE -------------------------------------------------------------

baix<-readOGR("SIGPAC/SIGPAC_09_BaixEbre_2022_shp/SIGPAC_09_BaixEbre.shp") #Obrim els SIGPAC directament descarregats. És lent. 
x_coord<-c(306355,317456,318621,295412,301375)
y_coord<-c(4520267,4516103,4507170,4507170,4515210)
xym <- cbind(x_coord, y_coord)

p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
crs(sps)<-crs(baix)

retall_baix<-crop(baix, sps)
retall_baix <- subset(retall_baix, US == "TA")

#Mosaiquem i guardem. 
tot<-raster::bind(retall_baix,retall_mont)
writeOGR(tot, dsn="SIGPAC/mosaic_R" ,layer="mosaicat", driver = "ESRI Shapefile")


### AJUNTEM PARCELES AMB PROBLEMES AMB SIGPAC ------------------------------------------------
#Obrim el SHP i els problemes
tot<-readOGR("SIGPAC/mosaic_R/mosaicat.shp")  #Obrim el SHP amb tot
tot$MUNICIPI = toupper(tot$MUNICIPI)
tot$MUNICIPI[tot$MUNICIPI == "SANT CARLES DE LA RAPITA"] <- "LA RAPITA"
tot$MUNICIPI[tot$MUNICIPI == "LA RÀPITA"] <- "LA RAPITA"#De vegades es la Ràpita i de vegades Sant Carles de la Ràpita...
#Li donem un valor únic, tant aquí com a les parcel·les que han presentat problemes.
tot$COD_PAR<-1:nrow(tot)                                  #Generem una fila dins del SHP amb el COD_PAR i la omplim.
tot$COD_PAR<- with(tot, paste0(tot$POL,"_", tot$PAR,"_", tot$MUNICIPI))    

problemes<-read.csv("trameses/danys_declarats_noproblemes.csv", sep = ";", check.names = F)
problemes$TM = toupper(problemes$TM) #tot en majuscula
problemes[problemes == "SANT CARLES DE LA RAPITA"] <- "LA RAPITA"  
problemes[problemes == "LA RÀPITA"] <- "LA RAPITA"  
problemes$COD_PAR<-1:nrow(problemes)   #Generem una fila dins del SHP amb el COD_PAR i la omplim
problemes$COD_PAR<- with(problemes, paste0(problemes$Poligon,"_", problemes$Parcela,"_", problemes$TM)) 

observacions<-as.data.frame(tot) #Per a consultar mes facilment que tenen de diferent, i aleshores modificar-ho al csv de problemes
diferent<-setdiff(problemes$COD_PAR, observacions$COD_PAR) #Genera un vector amb els valors que estan a problemes i no a SIGPAC danys. 
#Assenyala errors. Mirar si cal modificar el csv, en teoria no. Potser substituir Sant Carles de la Rapita per la Rapita, etc. 

SIGPAC_DANYS <- subset(tot, COD_PAR %in% problemes$COD_PAR)        #Dins el SIGPAC, seleccionem els poligons que tinguin danys declarats. 
writeOGR(SIGPAC_DANYS, dsn="SIGPAC/parceles_amb_danys" ,layer="SIGPAC_danys", driver = "ESRI Shapefile", overwrite_layer = T) 

### SENTINEL CODI INSTRUCCIONS GEE ----------------------------------------------

#Hem de descarregar les imatges de GEE. Primer seleccionavem les fotos a traves de https://earthexplorer.usgs.gov/, 
#per trobar fotos sense nuvols, ara ja no serveix. Com més fotos menys s'exagera el dany, sembla ser. Així que millor, sense forçar. 
#Després, hem de trobar els noms de les imatges a GEE. Ho farem allà mateix, així:

//definim regió d'interès
var Delta = ee.Geometry.Rectangle([0.565066015548572,40.604362720607654,0.883669531173572,40.80009008801863]);

// Define the visualization parameters.
var vizParams = {
  bands: ['B8', 'B4', 'B3'],
  min: 294.1,
  max: 2222.9,
};


//visualitzem la quantitat d'informació
var collection = ee.ImageCollection('COPERNICUS/S2_SR')
.filterDate('2022-03-01','2022-09-30')
.filterBounds(Delta);
print(collection)

//Sample function to clip imagecollections
collection = collection.map(function(img){return img.clip(Delta)});
Map.centerObject(Delta)
Map.addLayer(collection, vizParams, 'Collection');
//Map.addLayer(Delta)

#Finalment, fem cuinetes amb la llibreta, per a eliminar tot el que no sigui TCF i TBF de les dates que ens interessen. 
#Finalment tindrem una llista a la que li hem daplicar el segünt codi:

//definim regió d'interès
var Delta = ee.Geometry.Rectangle([0.565066015548572,40.604362720607654,0.883669531173572,40.80009008801863]);

// Define the visualization parameters.
var vizParams = {
  bands: ['B8', 'B4', 'B3'],
  min: 294.1,
  max: 2222.9,
};


// una vegada sabem quines imatges volem baixar, anem pel dret a escollir cadascuna de les imatges
// repeteixo el següent procés de descàrrega afegint manualment cadascuna de les imatges 
// que he copiat a continuació:
//"20220301T105001_20220301T105446_T31TBF",
//"20220301T105001_20220301T105446_T31TCF",
//"20220425T104619_20220425T105855_T31TBF",
//"20220425T104619_20220425T105855_T31TCF",
//"20220510T104621_20220510T104859_T31TBF",
//"20220510T104621_20220510T104859_T31TCF",
//"20220515T104619_20220515T105900_T31TBF",
//"20220515T104619_20220515T105900_T31TCF",
//"20220530T104631_20220530T105902_T31TBF",
//"20220530T104631_20220530T105902_T31TCF",
//"20220609T104631_20220609T105816_T31TBF",
//"20220609T104631_20220609T105816_T31TCF",
//"20220624T104629_20220624T105906_T31TBF",
//"20220624T104629_20220624T105906_T31TCF",
//"20220704T104629_20220704T105903_T31TBF",
//"20220704T104629_20220704T105903_T31TCF",
//"20220709T105041_20220709T105923_T31TBF",
//"20220709T105041_20220709T105923_T31TCF",
//"20220823T104619_20220823T105254_T31TBF",
//"20220823T104619_20220823T105254_T31TCF",
//"20220902T104619_20220902T105826_T31TBF",
//"20220902T104619_20220902T105826_T31TCF",
//"20221002T104759_20221002T105506_T31TBF",
//"20221002T104759_20221002T105506_T31TCF"];

///**he probat fer loops però no conserva el nom de la imatge a descarregar una vegada l'ha identificat a taks
/// hi ha altres sitemes més complexes a partir de colleccions però en el cas concret del Delta
/// com que havia de fer igualment una sel·lecció de les imatges, no servien

var S2 = ee.Image('COPERNICUS/S2_SR/20220301T105001_20220301T105446_T31TBF')
.select('B8', 'B4', 'B3')
.clip(Delta);

Map.addLayer(S2, vizParams, 'Collection');


//descarreguem la imatge (les bandes de 10m)
Export.image.toDrive({
  image: S2,
  description: 'COPERNICUS/S2_SR/20220301T105001_20220301T105446_T31TBF',
  scale: 10,
  region: Delta,
  folder: "S2"
});


### PCA SENTINEL  ------------------------------------------------------
d.envs <- "SENTINEL/Imatges_crues"
BuserRas.paths<- list.files(d.envs, pattern="*TBF.tif", full.names=TRUE)
CuserRas.paths<- list.files(d.envs, pattern="*TCF.tif", full.names=TRUE)

TBF<-vector("list", 11)
TCF<-vector("list", 11)
NDVITBF<-vector("list", 11)
NDVITCF<-vector("list", 11)
NDVI<-vector("list", 11)

for (i in 1:length(TBF)) {                                #Entren les capes, amb brick perque son multibanda, 
  TBF[[i]]<-raster::brick(BuserRas.paths[[i]])            #i es calcula el NDVI per les dues dallades
  TCF[[i]]<-raster::brick(CuserRas.paths[[i]])
  NDVITBF[[i]]<-(TBF[[i]][[1]]-TBF[[i]][[2]])/(TBF[[i]][[1]]+TBF[[i]][[2]])
  NDVITCF[[i]]<-(TCF[[i]][[1]]-TCF[[i]][[2]])/(TCF[[i]][[1]]+TCF[[i]][[2]])
  NDVI[[i]]<-raster::merge(NDVITBF[[i]], NDVITCF[[i]])
  writeRaster(NDVI[[i]], paste0("sentinel/NDVI/NDVI_",i,".tif"))
}

NDVI<-stack(NDVI[[1]],NDVI[[2]],NDVI[[3]],NDVI[[4]], NDVI[[5]], NDVI[[6]], NDVI[[7]], NDVI[[8]], NDVI[[9]], NDVI[[10]], NDVI[[11]])

PCA<-rasterPCA(NDVI, nComp=3)

writeRaster(PCA$map$PC1, "SENTINEL/PCA_R/JUNTPC1.TIF", overwrite=T)
writeRaster(PCA$map$PC2, "SENTINEL/PCA_R/JUNTPC2.TIF", overwrite=T)
writeRaster(PCA$map$PC3, "SENTINEL/PCA_R/JUNTPC3.TIF", overwrite=T)

plotPCA <- lapply(1:3, function(x) ggR(PCA$map, x, geom_raster = TRUE)) #plotegem. 
grid.arrange(plotPCA[[1]],plotPCA[[2]], plotPCA[[3]], ncol=2)

### MODEL I RETALL EN PARCELES AFECTADES  ------------------------------------------------------

PC1<-raster("SENTINEL/PCA_R/JUNTPC1.TIF")
PC2<-raster("SENTINEL/PCA_R/JUNTPC2.TIF")
PC3<-raster("SENTINEL/PCA_R/JUNTPC3.TIF")
PC<-c(PC1,PC2,PC3)

danys2022 <- (1/(1+exp(-(2.5760*PC[[1]] - 0.3788*PC[[2]] + 0.8904*PC[[3]] - 4.1929))))
writeRaster(danys2022, "model/danys_2022_tot.tif", overwrite=T)

################ Mascara del model en les parceles afectades
SIGPAC_DANYS<-readOGR("SIGPAC/parceles_amb_danys/SIGPAC_DANYS.shp")
SIGPAC_DANYS<-vect(SIGPAC_DANYS)
SIGPAC_DANYS<-terra::project(SIGPAC_DANYS, crs(PC1))
SIGPAC_DANYS<- buffer(SIGPAC_DANYS,width = -7)  #Creem un buffer negatiu de 7 metres, aixi nomes ens 
#quedem amb els pixels que estiguin dins dels poligons.

retall<-rast(danys2022)
retall<-terra::mask(retall, SIGPAC_DANYS, touches=F)
writeRaster(retall, paste0("MODEL/danys2022_retall.tif"), overwrite=T)

### SELECCIO DE QUARTILS  ------------------------------------------------------
SIGPAC_DANYS<-readOGR("SIGPAC/parceles_amb_danys/SIGPAC_DANYS.shp")
SIGPAC_DANYS<-vect(SIGPAC_DANYS)
dfSIGPAC<-as.data.frame(SIGPAC_DANYS)

danys2022<-raster("model/danys_2022.tif")
danys2022<-rast(danys2022)

q25 <- function(x, p=0.25, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }
q10 <- function(x, p=0.10, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }
q05 <- function(x, p=0.05, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }

model_q25<-terra::extract(x = danys2022, y = SIGPAC_DANYS, fun = "q25")
model_q10<-terra::extract(x = danys2022, y = SIGPAC_DANYS, fun = "q10")
model_q05<-terra::extract(x = danys2022, y = SIGPAC_DANYS, fun = "q05")

#Ho ajuntem tot en una taula i canviem noms. 
quartils<-inner_join(model_q05, model_q10, by="ID")
quartils<-inner_join(quartils, model_q25, by="ID")
quartils$ID<-dfSIGPAC$COD_PAR
names(quartils)<-c("COD_PAR","Q05","Q10","Q25")
COD_PAR<-quartils[,1]
write.csv(quartils, "model/quartils_poligons_parceles.csv", sep=";", dec = ",",row.names = TRUE,col.names = TRUE)

### AREA AFECTADA PER CADA QUARTIL EN CADA PARCELA ------------------------------------------------------
quartils<-read.csv("MODEL/quartils_poligons_parceles.csv")
qquartils<-quartils[,3:5]
danys2022<-raster("model/danys_2022.tif")
SIGPAC_DANYS<-readOGR("SIGPAC/parceles_amb_danys/SIGPAC_DANYS.shp")
crs(SIGPAC_DANYS)<-crs(danys2022)

df <- data.frame(matrix(nrow = nrow(dfSIGPAC), ncol = 3))  #Generem la matriu on s'emmagatzemaran els resultats.

for (i in 1:nrow(df)){  #fem una mascara per cada poligon amb els resultats del model. 
  a <- assign(paste("pol", i, sep = ""), SIGPAC_DANYS[i,])
  mascara<-raster::mask(x=danys2022, mask=a)
  mascara<-crop(mascara, a)
  c <- vector()
  
  for(r in 1:ncol(df)) { #Els valors superiors a cadascun dels llindars es reclassifiquen a 0. Els de sota, es reclassifiquen a 
                         #1 i es compten.
    b <- mascara
    b[b[] >	qquartils[i,r]  ] = NA 
    b[!is.na(b)]<-1
    fre<-freq(b, value=1)
    c <- c(c, fre)
  }
  df[i,] <- c #Es posa tot a un dataframe. df son els pixels
}

#Dividim entre 100 per tenir el num d'Ha i fem la suma de les columnes amb el mateix COD_PAR. 
tauladanys<-df/100
tauladanys <- cbind(tauladanys, new_col = SIGPAC_DANYS$COD_PAR)     
names(tauladanys)<-c("Q05", "Q10", "Q25","COD_PAR")
tauladanys<-aggregate(cbind(Q05,Q10,Q25) ~ COD_PAR, data = tauladanys, FUN = sum, na.rm = TRUE) #Sumem els polígons de cada parcela. 

Comprovacio<-setdiff(problemes$COD_PAR, tauladanys$COD_PAR) #Així mirem que no hi hagi cap parcela en que no s'hagi calculat el dany. 

write.csv(tauladanys, "model/tauladanys_parcela.csv")

### AJUNTEM DANYS PER EXPEDIENT  ------------------------------------------------------
#A excel, li afegirem una columna amb l'expedient. Com que no coincideix el nombre d'expedients i de parceles perquè hi ha parceles 
#compartides, hem de duplicar les parceles que estiguin en 2 expedients diferents. O posar 8 vegades la parcela en 8 expedients diferents,
#si cal. I torna a entrar la taula. 
tauladanys<-read.csv("model/tauladanys_parcela.csv", sep = ";")
tauladanys<-aggregate(cbind(Q05,Q10,Q25) ~ NUM_EXPEDIENT, data = tauladanys, FUN = sum, na.rm = TRUE) #Sumem les parceles de cada expedient 
#(tenint en compte ja els expedients que tenen més d'una parcela.)
write.csv(tauladanys, "model/tauladanys_expedient.csv"  )
#Després haurem de comparar a excel mateix, que ens trii el quartil més adequat segons el dany declarat pel pagès. 
#=INDEX(C2:E2;MATCH(MIN(ABS(C2:E2-F2));ABS(C2:E2-F2))) On C2:E2 són els 3 quartils per a un expedient, i F2 serien els danys
#declarats pel mateix expedient. 

