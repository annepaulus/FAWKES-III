############################################################################
### 1. load libraries and functions
############################################################################

needed_libs <- c("ggplot2",# For plotting
                 "raster",# for adding map data
                 "rgdal", # for loading map data
                 "rgeos", # dependency for rgdal
                 "letsR"
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

setwd("C:/Users/hoelting/Documents/FAWKES/R")
<<<<<<< HEAD
unzip("PublicNatura2000End2015_csv.zip")
=======
#setwd("~/git/FAWKES-III")

# Get Natura 2000 data
if (file.exists("PublicNatura2000End2015_csv.zip")==FALSE){
    download.file("https://www.dropbox.com/s/yaujzwuijyzluc6/PublicNatura2000End2015_csv.zip?dl=1", "PublicNatura2000End2015_csv.zip", mode="wb")
    unzip("PublicNatura2000End2015_csv.zip")
  } else {unzip("PublicNatura2000End2015_csv.zip")}

>>>>>>> f36072459a681c6773b4e55fdeef6ba9aea45b37

# create table of all Bird species with Sitecodes and Conservation Status

N2000Species <- read.csv("SPECIES.csv",header=TRUE)
Birds_only <- which(N2000Species$SPGROUP=="Birds")
mySpeciesdata <- N2000Species[Birds_only,c(2,3,4,16)] # 312.826 observations
mySpeciesdata <- mySpeciesdata[!duplicated(mySpeciesdata), ]

# remove Conservation Status = "NULL"
Cons_only <- which(mySpeciesdata$CONSERVATION=="A"|mySpeciesdata$CONSERVATION=="B"|mySpeciesdata$CONSERVATION=="C") 
mySpeciesdata <- mySpeciesdata[Cons_only,]            # 211.807 observations

# take only SPA sites (where SITETYPE is A or C)
N2000Sites <- read.csv("NATURA2000SITES.csv",header=TRUE)
SPAs_only <- which(N2000Sites$SITETYPE=="A"|N2000Sites$SITETYPE=="C") 
mySPAsites <- N2000Sites[SPAs_only,c(2,3)]            # 5572 sites

# table of all Bird species with Sitecodes and Conservation Status
mydata <- merge(mySpeciesdata, mySPAsites, by="SITECODE") # 148.452 observations


############################################################################
### 3. Load archetypes raster and Natura2000 shapefiles
############################################################################

if (file.exists("ArchetypicalChangeTrajectories_1990_2006_Levers2015.tif")==FALSE){
  download.file("https://www.dropbox.com/s/n469veo95z4lghy/ArchetypicalChangeTrajectories_1990_2006_Levers2015.tif?dl=1", "ArchetypicalChangeTrajectories_1990_2006_Levers2015.tif", mode="wb")
} else {print("file exists")}


# archetypes raster data by Levers et al
ACT<-raster("ArchetypicalChangeTrajectories_1990_2006_Levers2015.tif")
LSA<-raster("LandSystemArchetypes_2006_Levers2015.tif")
values(ACT)[values(ACT) > 17] = NA       # there are only 17 categories: put all values > 17 to NAs

plot(ACT)
plot(LSA)

if (file.exists("Natura2000_end2016_Shapefile.zip")==FALSE){
  download.file("https://www.dropbox.com/s/uxvung7v2vttqf0/Natura2000_end2016_Shapefile.zip?dl=1", "Natura2000_end2016_Shapefile.zip", mode="wb")
  unzip("Natura2000_end2016_Shapefile.zip")
} else {unzip("Natura2000_end2016_Shapefile.zip")}


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
plot(subset, col="red", add=TRUE)

# extract the ACTs, LSAs and area of the SPAs sitecode-wise
total <- length(overlapSPA)       #5569 SPAs

ACT_ext<-list()
LSA_ext<-list()
area<-list()

pb <- txtProgressBar(min = 0, max = total, style = 3) # produces a progress bar

# normalizeWeights==TRUE --> der Anteil der extrahierten Pixelfläche an der Gesamtfläche des Polygones
# Problem hierbei: falls man im Nachhinein NAs rauswirft stimmen die Verhältnisse nicht mehr
# normalizeWeights==FALSE --> der Anteil des extrahierten Pixels 

# this loop will run for ca.60 minutes! ...go for lunch or for a walk ;o)
for (i in 1:total){
  subset <- shape[as.character(shape$SITECODE)==as.character(overlapSPA)[i],]       # takes each shapefile, if it is a SPA
  ACT_ext[[i]]<-extract(ACT,subset,weights=TRUE,normalizeWeights=TRUE)              # extracts ACT rasterdata for each sitecode
  LSA_ext[[i]]<-extract(LSA,subset,weights=TRUE,normalizeWeights=TRUE)              # extracts LSA rasterdata for each sitecode
  area[[i]]<-area(subset)                                                           # extracts areal data for each sitecode
  setTxtProgressBar(pb, i)
}
close(pb)


# get rid of NAs
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

#save.image(file = "FAWKES.RData")
#load("~/FAWKES/R/FAWKES.RData")



############################################################################
### 5. create a results dataframe
############################################################################

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

View(results)

hist(results$count,breaks=100)
hist(results$area,breaks=100)
length(which(results$count<2))    # 415 sites with only one pixel (3x3km)

res_final<-data.frame(matrix(NA,nrow(results)))

#install.packages("xlsx")
#library(xlsx)
#write.xlsx(results, "C:/Users/hoelting/Documents/FAWKES/R/results3.xlsx") 

results[is.na(results)]<-0
View(results)
summary(results)


############################################################################
### 6. relate conservation status with ACT via bird species
############################################################################

#for each species, link the ACts (1 to 17) with the Conservation Status (for all sites)
specUniq <- unique(mydata$SPECIESCODE)
tabFinal <- numeric()

total_b <- length(specUniq)

for(l in 1:total_b){
  subTmp <- subset(mydata, mydata$SPECIESCODE==specUniq[l])
  subTmp <- merge(subTmp, results, by="SITECODE")
  tabFinal <- rbind(tabFinal, subTmp)
  print(l)
}


View(tabFinal)

tabFinal <- tabFinal[!duplicated(tabFinal), ]
tabFinal[is.na(tabFinal)] <- 0

#tabFinal2 <- merge(mydata,results, by="SITECODE")
#View(tabFinal2)

#save.image(file = "FAWKES.RData")
#load("~/FAWKES/R/FAWKES.RData")

############################################################################
### 6. correlations between Conservation status and ACTs
############################################################################

CStatus<-tabFinal$CONSERVATION
CStatus <- factor(CStatus,levels = c("A","B","C"))

dat <- data.frame(tabFinal[,c(1:6)],
                  CStatus = factor(CStatus,levels = c("A","B","C")),
                  Conversion = rowSums(tabFinal[,18:22]),
                  Intens = rowSums(tabFinal[,c(7:10,17)]),
                  De_intens = rowSums(tabFinal[,11:16]),
                  Stabil = c(tabFinal[,23]))

View(dat)

install.packages("ggplot2")
library("ggplot2")

ggplot(dat, aes(x=dat$CStatus, y = dat$Conversion + dat$Intens + dat$De_intens + dat$Stabil)) +
geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(dat$CStatus ~ dat$Conversion + dat$Intens + dat$De_intens + dat$Stabil, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

m <- polr(dat$CStatus ~ dat$Conversion + dat$Intens + dat$De_intens + dat$Stabil, data = dat, Hess=TRUE)
summary(m)
(ctable <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
