############################################################################
### 1. load libraries and functions
############################################################################

needed_libs <- c("ggplot2",# For plotting
                 "raster",# for adding map data
                 "rgdal", # for loading map data
                 "rgeos", # dependency for rgdal
                 "letsR",
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

setwd("C:/Users/hoelting/Documents/FAWKES/Leeds/tempDirectory")
unzip("PublicNatura2000End2015_csv.zip")

setwd("C:/Users/hoelting/Documents/FAWKES/Leeds")

# create table of all Bird species with Sitecodes and Conservation Status
# take only bird species
N2000Species <- read.csv("./tempDirectory/SPECIES.csv",header=TRUE)
Birds_only <- which(N2000Species$SPGROUP=="Birds")
mySpeciesdata <- N2000Species[Birds_only,c(2,3,4,16)] # 312.826 observations

# remove Conservation Status = "NULL"
Cons_only <- which(mySpeciesdata$CONSERVATION=="A"|mySpeciesdata$CONSERVATION=="B"|mySpeciesdata$CONSERVATION=="C") 
mySpeciesdata <- mySpeciesdata[Cons_only,]            # 211.807 observations

# take only SPA sites (where SITETYPE is A or C)
N2000Sites <- read.csv("./tempDirectory/NATURA2000SITES.csv",header=TRUE)
SPAs_only <- which(N2000Sites$SITETYPE=="A"|N2000Sites$SITETYPE=="C") 
mySPAsites <- N2000Sites[SPAs_only,c(2,3)]            # 5572 sites

# table of all Bird species with Sitecodes and Conservation Status
mydata <- merge(mySpeciesdata, mySPAsites, by="SITECODE") # 148.452 observations


############################################################################
### 3. Load archetypes raster and Natura2000 shapefiles
############################################################################

# archetypes raster data by Levers et al
ACT<-raster("ArchetypicalChangeTrajectories_1990_2006_Levers2015.tif")
#LSA<-raster("LandSystemArchetypes_2006_Levers2015.tif")
values(ACT)[values(ACT) > 17] = NA       # there are only 17 categories: put all values > 17 to NAs

plot(ACT)
plot(LSA)

# Natura 2000 shapefiles
shape <- readOGR(dsn = ".", layer = "Natura2000_end2016") 
sitecodes <- shape$SITECODE             # 27.510 NATURA2000 sites

# There are 3 Sitecodes which cannot be found in the SPA Shapefiles
N2000SPASiteCodes <- subset(N2000Sites$SITECODE,N2000Sites$SITETYPE=="A"|N2000Sites$SITETYPE=="C")  # 5572 sitecodes
overlapSPA <- sitecodes[which(as.character(sitecodes)%in%as.character(N2000SPASiteCodes)==T)]       # 5569 sitecodes
length(N2000SPASiteCodes)-length(overlapSPA)


############################################################################
### 4. overlay of SPAs with ACTs
############################################################################

# plot of raster and shapefiles (subset of first 50 shapefiles)
plot(ACT)
subset <- shape[which(as.character(sitecodes)%in%as.character(N2000SPASiteCodes)==T),]
plot(subset[1:50,], col="red", add=TRUE)


#fun with outer buffers
outerBuffer<-function(x, d){
  buff<-buffer(x, width = d, dissolve = F)
  e<-erase(buff,x)
  return(e)
}

outer10km<-outerBuffer(x=subset,d=10000)
outer20km<-outerBuffer(x=subset,d=20000)
outer50km<-outerBuffer(x=subset,d=50000)
outer100km<-outerBuffer(x=subset,d=100000)


plot(ACT)
plot(subset[6,], col="red", add=TRUE)
b <- buffer(subset[6,], width=100000,dissolve = T)
gb.r<-gBuffer(subset[6,], width=100000, byid = T)
plot(b, col="red", add=TRUE)


# extract the ACTs, LSAs and area of the SPAs sitecode-wise
total <- length(overlapSPA)       #5569 SPAs

ACT_ext<-list()
LSA_ext<-list()
area<-list()
perimeter<-list()
buffer10<-list()
buffer20<-list()
buffer50<-list()
buffer100<-list()


pb <- txtProgressBar(min = 0, max = total, style = 3) # produces a progress bar

# normalizeWeights==TRUE --> der Anteil der extrahierten Pixelfläche an der Gesamtfläche des Polygones
# Problem hierbei: falls man im Nachhinein NAs rauswirft stimmen die Verhältnisse nicht mehr
# normalizeWeights==FALSE --> der Anteil des extrahierten Pixels 

# this loop will run for ca.60 minutes! ...go for lunch or for a walk ;o)
for (i in 1:total){
  subset <- shape[as.character(shape$SITECODE)==as.character(overlapSPA)[i],]
  #subset10 <- outer10km[as.character(outer10km$SITECODE)==as.character(overlapSPA)[i],]
  #subset20 <- outer10km[as.character(outer20km$SITECODE)==as.character(overlapSPA)[i],]
  #subset50 <- outer10km[as.character(outer50km$SITECODE)==as.character(overlapSPA)[i],]
  #subset100 <- outer10km[as.character(outer100km$SITECODE)==as.character(overlapSPA)[i],]
  
  # takes each shapefile, if it is a SPA
  ACT_ext[[i]]<-extract(ACT,subset,weights=TRUE,normalizeWeights=TRUE)
  # extracts ACT rasterdata for each sitecode
  #buffer10[[i]]<-extract(ACT,subset10,weights=TRUE,normalizeWeights=TRUE)
  #buffer20[[i]]<-extract(ACT,subset20,weights=TRUE,normalizeWeights=TRUE)
  #buffer50[[i]]<-extract(ACT,subset50,weights=TRUE,normalizeWeights=TRUE)
  #buffer100[[i]]<-extract(ACT,subset100,weights=TRUE,normalizeWeights=TRUE)
  
  #LSA_ext[[i]]<-extract(LSA,subset,weights=TRUE,normalizeWeights=TRUE)              # extracts LSA rasterdata for each sitecode
  area[[i]]<-gArea(subset)/1e+06
  perimeter[[i]]<-gLength(subset)/1e+03
  
  #this allows us to correct for Area and also to compute 
  #Area/Perimeter relationship -> fragmentation
  
  
  # extracts areal data for each sitecode
  setTxtProgressBar(pb, i)
}
close(pb)

#ACT_ext[[1]]


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
test<-apply(tabFinal[,7:23], MARGIN=1,FUN=sum)
zero<-which(test==0)
tabFinal<-tabFinal[-zero,]

#CStatus ordered factor
CStatus<-droplevels(as.factor(tabFinal$CONSERVATION))
CStatus <- factor(CStatus,levels = c("A","B","C"),ordered = TRUE)
tabFinal$CONSERVATION<-CStatus

tabFinal<-tabFinal[-which(tabFinal$SPECIESNAME=="--NULL--"),]
tabFinal$SPECIESNAME<-droplevels(tabFinal$SPECIESNAME)



bird<-read.csv("bird categorization.csv",sep=";")
bird<-bird[1:596,2:4]
colnames(bird)<-c("SPECIESCODE","migration","preference")


tabFinal_b <- merge(tabFinal,bird, by="SPECIESCODE")

table(bird$SPECIESCODE)#sth is still strange here

which(table(bird$SPECIESCODE)>1) ###??????

#save.image(file = "FAWKES.RData")
#load("~/FAWKES/R/FAWKES.RData")


############################################################################
### 6. correlations between Conservation status and ACTs
############################################################################
require(foreign)
require(ggplot)
require(MASS)
install.packages("Hmisc")
library(Hmisc)
require(reshape2)



dat <- data.frame(tabFinal[,c(1:6,24)],
                  Conversion = rowSums(tabFinal[,18:22]),
                  Intens = rowSums(tabFinal[,c(7:10,17)]),
                  De_intens = rowSums(tabFinal[,11:16]),
                  Stabil = c(tabFinal[,23]))



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




