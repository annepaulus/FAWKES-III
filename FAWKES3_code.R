############################################################################
### 00.1. set the working and temporary directories
###
### checks for nodename or username and sets directories accordingly
############################################################################

.setwdntemp <- function(){
  cu <- Sys.info()["user"]
  cn <- Sys.info()["nodename"]
  
  if (cn == "juro-MacBookPro"){
    path2wd <- "/home/juro/git/FAWKES-III/" #MB
    path2temp <- "/home/juro/git/FAWKES-III/temp/" #MB
    
  } else if (cn == "enter your name here"){
    path2wd <- "/Users/..."
    path2temp <- "/Users/...tmp/" 
  }  
  return(list(path2temp,path2wd))
}

set.list <-  .setwdntemp()
path2temp <- set.list[[1]]
path2wd <- set.list[[2]]

############################################################################
### 1. load libraries and functions
############################################################################

needed_libs <- c("ggplot2",# For plotting
                 "raster",# for adding map data
                 "rgdal", # for loading map data
                 "rgeos", # dependency for rgdal
                 "letsR",
                 "foreign",
                 #"ggplot",
                 "MASS",
                 "Hmisc",
                 "reshape2",
                 "VGAM"
)

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
sapply(needed_libs,usePackage)

rm(needed_libs)


############################################################################
### 2. load Natura2000 Species data
############################################################################

setwd(path2temp)

if (file.exists("PublicNatura2000End2016_csv.zip")==FALSE){
  download.file("https://www.dropbox.com/s/52y55t4qdjaflhi/PublicNatura2000End2016_csv.zip?dl=1", "PublicNatura2000End2016_csv.zip", mode="wb")
  unzip("PublicNatura2000End2016_csv.zip")
} else {unzip("PublicNatura2000End2016_csv.zip")}

### create table of all Bird species with Sitecodes and Conservation Status

N2000Species <- read.csv("SPECIES.csv",header=TRUE)
mySpeciesdata <- N2000Species[which(N2000Species$SPGROUP=="Birds"),c(2,3,4,16)] # only bird species
mySpeciesdata <- mySpeciesdata[which(mySpeciesdata$CONSERVATION=="A"|mySpeciesdata$CONSERVATION=="B"|mySpeciesdata$CONSERVATION=="C"),]       # remove Conservation Status = "NULL"

N2000Sites <- read.csv("NATURA2000SITES.csv",header=TRUE) 
mySPAsites <- N2000Sites[which(N2000Sites$SITETYPE=="A"|N2000Sites$SITETYPE=="C"),c(2,3)]  # take only SPA sites (where SITETYPE is A or C) and columns 2 and 3

mydata <- merge(mySpeciesdata, mySPAsites, by="SITECODE")

### load bird species lost and categories

if (file.exists("bird_categorization.csv")==FALSE){
  download.file("https://www.dropbox.com/s/bxf39frp982fe4z/bird%20categorization.csv?dl=1", "bird_categorization.csv", mode="wb")
  bird<-read.csv("bird_categorization.csv",sep=";")
} else {bird<-read.csv("bird_categorization.csv",sep=";")}


############################################################################
### 3. Load archetypes raster and Natura2000 shapefiles
############################################################################

if (file.exists("Archetypes_Levers_et_al_2015.zip")==FALSE){
  download.file("https://www.dropbox.com/s/2qybuvj1balvcwo/Archetypes_Levers_et_al_2015.zip?dl=1", "Archetypes_Levers_et_al_2015.zip", mode="wb")
  unzip("Archetypes_Levers_et_al_2015.zip")
} else {unzip("Archetypes_Levers_et_al_2015.zip")}

# load land system archetypes (15 classes)
LSA<-raster("LandSystemArchetypes_2006_Levers2015.tif")
#plot(LSA)
#hist(LSA)

# load archetypes change trajectories (17 classes)
ACT<-raster("ArchetypicalChangeTrajectories_1990_2006_Levers2015.tif")
values(ACT)[values(ACT) > 17] = NA       # there are only 17 categories: put all values > 17 to NAs
#plot(ACT)
#hist(ACT)

# load N2000 shapefile
Natura2000_shape <- readOGR(dsn = ".", layer = "Natura2000_end2016") 
sitecodes <- Natura2000_shape$SITECODE 

# subset N2000 dataset to use only SPAs and "both" (i.e. only those sites that are focusing on bird conservation)
N2000SPASiteCodes <- subset(N2000Sites$SITECODE,N2000Sites$SITETYPE=="A"|N2000Sites$SITETYPE=="C")  # 5572 sitecodes

# There are 3 Sitecodes which cannot be found in the SPA Shapefiles
overlapSPA <- sitecodes[which(as.character(sitecodes)%in%as.character(N2000SPASiteCodes)==T)]       # 5569 sitecodes
length(N2000SPASiteCodes)-length(overlapSPA)

############################################################################
### 4. overlay of SPAs with ACTs
############################################################################

plot(ACT)
Natura2000_shape_all <- Natura2000_shape[which(as.character(sitecodes)%in%as.character(N2000SPASiteCodes)==T),]

# extract the ACTs, LSAs and area of the SPAs sitecode-wise
total <- length(overlapSPA)       #5569 SPAs

ACT_ext<-list()
LSA_ext<-list()
area<-list()
perimeter<-list()

# normalizeWeights==TRUE --> der Anteil der extrahierten Pixelfläche an der Gesamtfläche des Polygones
# Problem hierbei: falls man im Nachhinein NAs rauswirft stimmen die Verhältnisse nicht mehr
# normalizeWeights==FALSE --> der Anteil des extrahierten Pixels 

# this loop will run for ca.60 minutes! ...go for lunch or for a walk ;o)

pb <- txtProgressBar(min = 0, max = total, style = 3) # produces a progress bar
for (i in 1:total){
  subset <- Natura2000_shape[as.character(Natura2000_shape$SITECODE)==as.character(overlapSPA)[i],]
  # takes each shapefile, if it is a SPA
  ACT_ext[[i]]<-extract(ACT,subset,weights=TRUE,normalizeWeights=TRUE)
  # extracts ACT rasterdata for each sitecode
  area[[i]]<-gArea(subset)/1e+06
  perimeter[[i]]<-gLength(subset)/1e+03
  # extracts areal data for each sitecode
  setTxtProgressBar(pb, i)
}
close(pb)

#ACT_ext[[1]]

###############################################################################
### Buffer stuffer
###############################################################################

# subset shapefile of 10 SPAs
subset<-Natura2000_shape_all[1:10,]
#plot(subset, col="red", add=TRUE)    # only subset for now

#fun with outer buffers
outerBuffer<-function(x, d){
  buff<-buffer(x, width = d, dissolve = F)
  e<-erase(buff,x)
  return(e)
}

buffer10<-list()
# outer10km<-outerBuffer(x=subset,d=10000)

myBuffer<-list()
for(i in 1:length(subset)){
  myBuffer[[i]]<-outerBuffer(x=subset[i,],d=10000)
}

plot(ACT)
plot(myBuffer[[1]], col="red", add=TRUE)

pb <- txtProgressBar(min = 0, max = total, style = 3) # produces a progress bar
for (i in 1:length(myBuffer)){
  subset10 <- myBuffer[[i]]
  # takes each shapefile, if it is a SPA
  buffer10[[i]]<-extract(ACT,subset10,weights=TRUE,normalizeWeights=TRUE)
  area[[i]]<-gArea(subset10)/1e+06
  perimeter[[i]]<-gLength(subset10)/1e+03
  setTxtProgressBar(pb, i)
}
close(pb)

#cleanup

ACT_red<-list()
area_red<-list()

for (i in 1:length(buffer10)){
  if(is.null(buffer10[[i]][[1]])==TRUE) {
    ACT_red[[i]]<-NA  
    area_red[[i]]<-NA
  }else{
    if(isTRUE(unique(is.na(as.data.frame(buffer10[[i]])$value)))==TRUE){
      ACT_red[[i]]<-NA  
      area_red[[i]]<-NA
    }else{
      f.correct<-1/sum(as.data.frame(buffer10[[i]])[!is.na(as.data.frame(buffer10[[i]])[,1]),2])
      reduced.na<-as.data.frame(buffer10[[i]])[!is.na(as.data.frame(buffer10[[i]])[,1]),]
      reduced.na[,2]<-reduced.na[,2]*f.correct
      ACT_red[[i]]<-reduced.na
      area_red[[i]]<-area[[i]]*sum(as.data.frame(buffer10[[i]])[!is.na(as.data.frame(buffer10[[i]])[,1]),2])
    }
  }    
}   



# create a dataframe for presenting the distribution of SPA-area per ACT
results <- as.data.frame(matrix(NA,length(ACT_red),20))
colnames(results) <- c("SITECODE","area",paste("ACT",c(1:17),sep=""),"count")

for(k in 1:length(ACT_red)){
  results[k,1]<-as.character(overlapSPA)[k]
  results[k,2]<-area_red[[k]]
  
  if(is.na(ACT_red[[k]])==TRUE){
    results[k,3:19]<-NA
  } else{
    test.sum<-cbind(aggregate(weight~value,sum,data=ACT_red[[k]]),table(ACT_red[[k]]$value))
    results[k,test.sum$value+2]<-test.sum$weight
    results[k,20]<-sum(test.sum$Freq)
    
  }
  print(k)
}



results[is.na(results)]<-0
results2 <- results[which(results$count>5),]



###############################################################################
#                   DATA CLEANING                                             #
###############################################################################

# get rid of NAs and compute class proportions

ACT_red<-list()
area_red<-list()

for (i in 1:total){
  if(is.null(ACT_ext[[i]][[1]])==TRUE) {
    ACT_red[[i]]<-NA  
    area_red[[i]]<-NA
  }else{
    if(isTRUE(unique(is.na(as.data.frame(ACT_ext[[i]])$value)))==TRUE){
      ACT_red[[i]]<-NA  
      area_red[[i]]<-NA
    }else{
      f.correct<-1/sum(as.data.frame(ACT_ext[[i]])[!is.na(as.data.frame(ACT_ext[[i]])[,1]),2])
      reduced.na<-as.data.frame(ACT_ext[[i]])[!is.na(as.data.frame(ACT_ext[[i]])[,1]),]
      reduced.na[,2]<-reduced.na[,2]*f.correct
      ACT_red[[i]]<-reduced.na
      area_red[[i]]<-area[[i]]*sum(as.data.frame(ACT_ext[[i]])[!is.na(as.data.frame(ACT_ext[[i]])[,1]),2])
    }
  }    
}   



# create a dataframe for presenting the distribution of SPA-area per ACT
results <- as.data.frame(matrix(NA,length(ACT_red),20))
colnames(results) <- c("SITECODE","area",paste("ACT",c(1:17),sep=""),"count")

for(k in 1:length(ACT_red)){
  results[k,1]<-as.character(overlapSPA)[k]
  results[k,2]<-area_red[[k]]
  
  if(is.na(ACT_red[[k]])==TRUE){
    results[k,3:19]<-NA
  } else{
    test.sum<-cbind(aggregate(weight~value,sum,data=ACT_red[[k]]),table(ACT_red[[k]]$value))
    results[k,test.sum$value+2]<-test.sum$weight
    results[k,20]<-sum(test.sum$Freq)
    
  }
  print(k)
}



results[is.na(results)]<-0
results2 <- results[which(results$count>5),]

############################################################################
###       relate conservation status with ACT via bird species           ###
############################################################################

birds<-(unique(mydata$SPECIESNAME))
length(unique(mydata$SPECIESCODE)) # 530 bird species

#for each species, link the ACts (1 to 17) with the Conservation Status (for all sites

tabFinal <- merge(mydata,results, by="SITECODE")
tabFinal<-tabFinal[!duplicated(tabFinal), ]

tabFinal[is.na(tabFinal)] <- 0

#remove rows completely 0
#test<-apply(tabFinal[,7:23], MARGIN=1,FUN=sum)
#zero<-which(test==0)
#tabFinal<-tabFinal[-zero,]

#CStatus ordered factor
CStatus<-droplevels(as.factor(tabFinal$CONSERVATION))
CStatus <- factor(CStatus,levels = c("A","B","C"),ordered = TRUE)
tabFinal$CONSERVATION<-CStatus

#tabFinal<-tabFinal[-which(tabFinal$SPECIESNAME=="--NULL--"),]
tabFinal$SPECIESNAME<-droplevels(tabFinal$SPECIESNAME)

bird<-bird[1:511,2:4]
colnames(bird)<-c("SPECIESCODE","migration","preference")
#bird <- bird[which(bird$migration=="mainly resident"),]

tabFinal <- merge(tabFinal,bird, by="SPECIESCODE")
#table(bird$SPECIESCODE)

which(table(bird$SPECIESCODE)>1)

#save.image(file = "FAWKES.RData")
#load("FAWKES.RData")





############################################################################
### 
############################################################################


for (i in 1:length(dat$SPECIESCODE)){

  dat$ACT_dom[i]<-colnames(dat[i,8:11])[max.col(dat[i,8:11])]

  }


bird2<-reshape(aggregate (dat$ACT_dom, list(dat$SPECIESCODE,dat$ACT_dom), FUN=length), v.names="x", timevar="Group.2", idvar="Group.1", direction="wide")
colnames(bird2)<-c("SPECIESCODE","Conversion", "De_intens" ,"Intens" ,"Stabil")
bird<-merge(bird,bird2, by="SPECIESCODE")

colnames(bird)<-c("SPECIESCODE", "migration"   ,"preference","Conversion", "De_intens" ,"Intens" ,"Stabil")

for (i in 1:length(bird$SPECIESCODE)){
  
  bird$ACT_dom_dom[i]<-colnames(bird[i,4:7])[max.col(bird[i,4:7])]
  
}



############################################################################
### 6. correlations between Conservation status and ACTs
############################################################################


dat <- data.frame(tabFinal[,c(1:6,24)],
                  Conversion = rowSums(tabFinal[,18:22]),
                  Intens = rowSums(tabFinal[,c(7:10,17)]),
                  De_intens = rowSums(tabFinal[,11:16]),
                  Stabil = c(tabFinal[,23]))

tab <- merge(dat,bird, by="SPECIESCODE")
View(dat)

###Describ. Stat.
boxplot(dat[,8:11],col=c("darkred","red","lightgreen","green"))
boxplot(tabFinal[,7:23],col=c("red","red","red","red","lightgreen","lightgreen",
                              "lightgreen","lightgreen","lightgreen","lightgreen","red",
                              "darkred","darkred","darkred","darkred","darkred",
                              "green"))


par(mfrow=c(3,1))
boxplot(subset(dat,dat$CONSERVATION=="A")[,c(8:11)],col="green")
boxplot(subset(dat,dat$CONSERVATION=="B")[,c(8:11)],col="yellow")
boxplot(subset(dat,dat$CONSERVATION=="C")[,c(8:11)],col="red")


##################### models for subgroup of birds #######################
prey <- subset(tab, tab$preference=="birds of prey")
open_c <- subset(tab, tab$preference=="open land carnivores")
open_h <- subset(tab, tab$preference=="open land herbivores" )
coastal <- subset(tab, tab$preference=="coastal species")
lakes <- subset(tab, tab$preference=="wetland/lakes")
wood_c <- subset(tab, tab$preference=="woodland carnivores" )
wood_h <- subset(tab, tab$preference=="woodland herbivores")

par(mfrow=c(3,1))
boxplot(subset(open_h,open_c$CONSERVATION=="A")[,c(8:11)],col="green")
boxplot(subset(open_h,open_c$CONSERVATION=="B")[,c(8:11)],col="yellow")
boxplot(subset(open_h,open_c$CONSERVATION=="C")[,c(8:11)],col="red")

par(mfrow=c(1,1))
boxplot(prey[,8:11],col=c("darkred","red","lightgreen","green"))
boxplot(open_c[,8:11],col=c("darkred","red","lightgreen","green"))
boxplot(open_h[,8:11],col=c("darkred","red","lightgreen","green"))
boxplot(wood_c[,8:11],col=c("darkred","red","lightgreen","green"))
boxplot(wood_h[,8:11],col=c("darkred","red","lightgreen","green"))
boxplot(lakes[,8:11],col=c("darkred","red","lightgreen","green"))
boxplot(coastal[,8:11],col=c("darkred","red","lightgreen","green"))


pom_prey <- vglm(factor(CONSERVATION,levels = c("A","B","C"),ordered = TRUE)~Conversion+Intens+De_intens+area,data=coastal, family = cumulative(parallel=FALSE)) 
pom.ll <- logLik(pom_prey)
pom0 <- vglm(factor(CONSERVATION,levels = c("A","B","C"),ordered = TRUE)~1,data=coastal, family = cumulative(parallel=FALSE))
p0.ll <- logLik(pom0)
# R^2 McFadden
pom.mcfad <- as.vector(1 - (pom.ll/p0.ll))
pom.mcfad

# prey: 0.00369, open_c: 0.0087, open_h: 0.004609, wood_c: 0.00709, wood_h: -5.73 ???, lakes: 0.01467, coastal: 0.00853
# counts > 5: prey: 0.0104, open_c: 0.0076, open_h: 0.0157,  wood_c: 0.0131,  wood_h: -2,09 ???, lakes: 0.00613, coastal: 0.0059

# counts > 5 & "mainly resident": prey: 0.01333, open_c: 0.0074, open_h: 0.02249,  wood_c: 0.0188,  wood_h: -2,2 ???, lakes: 0.00817, coastal: 0.00269
# "mainly resident": prey: 0.01046, open_c: 0.0076, open_h: 0.0157,  wood_c: 0.01314,  wood_h: 0.2263, lakes: 0.0061, coastal: 0.005969


a<-strsplit(as.character(dat$SPECIESNAME), " ")

name<-c()
for(i in 1:length(a)){
  name[i] <-a[[i]][1] 
}



pom0 <- vglm(factor(CONSERVATION,levels = c("A","B","C"),ordered = TRUE)~1,data=dat, family = cumulative(parallel=FALSE))
pom <- vglm(factor(CONSERVATION,levels = c("A","B","C"),ordered = TRUE)~Conversion+Intens+De_intens+area,data=dat, family = cumulative(parallel=FALSE)) 

?vglm

#plotvglm(pom)

summary(pom)


pom.ll <- logLik(pom)
pom.ll

p0.ll <- logLik(pom0)
p0.ll


# R^2 McFadden
pom.mcfad <- as.vector(1 - (pom.ll/p0.ll))
pom.mcfad



# R^2 Cox&Snell
N <- length(dat[, 1])  # Anzahl der F?lle
pom.cox <- as.vector(1 - exp((2/N) * (p0.ll - pom.ll)))
pom.cox



# R^2 Nagelkerke
pom.nagel <- as.vector((1 - exp((2/N) * (p0.ll - pom.ll)))/(1 - exp(p0.ll)^(2/N)))
pom.nagel








#Functions for PseudoR2s
RsqGLM <- function(obs = NULL, pred = NULL, model = NULL) {
  # version 1.2 (3 Jan 2015)
  
  model.provided <- ifelse(is.null(model), FALSE, TRUE)
  
  if (model.provided) {
    if (!("glm" %in% class(model))) stop ("'model' must be of class 'glm'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
    
  } else { # if model not provided
    if (is.null(obs) | is.null(pred)) stop ("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'")
    if (length(obs) != length(pred)) stop ("'obs' and 'pred' must be of the same length (and in the same order).")
    if (!(obs %in% c(0, 1)) | pred < 0 | pred > 1) stop ("Sorry, 'obs' and 'pred' options currently only implemented for binomial GLMs (binary response variable with values 0 or 1) with logit link.")
    logit <- log(pred / (1 - pred))
    model <- glm(obs ~ logit, family = "binomial")
  }
  
  null.mod <- glm(obs ~ 1, family = family(model))
  loglike.M <- as.numeric(logLik(model))
  loglike.0 <- as.numeric(logLik(null.mod))
  N <- length(obs)
  
  # based on Nagelkerke 1991:
  CoxSnell <- 1 - exp(-(2 / N) * (loglike.M - loglike.0))
  Nagelkerke <- CoxSnell / (1 - exp((2 * N ^ (-1)) * loglike.0))
  
  # based on Allison 2014:
  McFadden <- 1 - (loglike.M / loglike.0)
  Tjur <- mean(pred[obs == 1]) - mean(pred[obs == 0])
  sqPearson <- cor(obs, pred) ^ 2
  
  return(list(CoxSnell = CoxSnell, Nagelkerke = Nagelkerke, McFadden = McFadden, Tjur = Tjur, sqPearson = sqPearson))
}




