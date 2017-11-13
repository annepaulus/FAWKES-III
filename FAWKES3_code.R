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

# Get Natura 2000 data
unzip("PublicNatura2000End2015_csv.zip")

# create table of all Bird species with Site codes and Conservation Status
N2000Species<-read.csv("SPECIES.csv",header=TRUE)
names(N2000Species)

Birds_only<-which(N2000Species$SPGROUP=="Birds")
mySpeciesdata<-N2000Species[Birds_only,c(2,3,4,16)] # 312.826 observations and 4 variables
names(mySpeciesdata) # "SITECODE"  "SPECIESNAME"  "SPECIESCODE"  "CONSERVATION"

# Sitecodes of the SPAs
N2000Sites<-read.csv("NATURA2000SITES.csv",header=TRUE)
names(N2000Sites)
SPAs_only<-which(N2000Sites$SITETYPE=="A"|N2000Sites$SITETYPE=="C") # list of N2000 SPA Sites (where SITETYPE is A or C)
mySPAsites<-N2000Sites[SPAs_only,c(2,3)] # list of SPA sitecodes: 5572 sites
names(mySPAsites) 

#mydata<-subset(mySpeciesdata, mySpeciesdata$SITECODE==mySPAsites$SITECODE)
mydata<-merge(mySpeciesdata, mySPAsites, by="SITECODE") #205.140 observations


############################################################################
### 3. Load archetypes raster and Natura2000 shapefiles
############################################################################

# archetypes raster data by Levers et al
ACT<-raster("ArchetypicalChangeTrajectories_1990_2006_Levers2015.tif")
values(ACT)[values(ACT) > 17] = NA    #there are only 17 categories: put all values > 17 to NAs

# Natura 2000 shapefiles
shape <- readOGR(dsn = ".", layer = "Natura2000_end2016") 
sitecodes<-shape$SITECODE # get Sitecodes of shapefiles: 27.510 sites

# There are 3 Sitecodes which cannot be found in the SPA Shapefiles
N2000SPASiteCodes<-subset(N2000Sites$SITECODE,N2000Sites$SITETYPE=="A"|N2000Sites$SITETYPE=="C") # 5572 sitecodes
overlapSPA<-sitecodes[which(as.character(sitecodes)%in%as.character(N2000SPASiteCodes)==T)] # 5569 sitecodes
length(N2000SPASiteCodes)-length(overlapSPA)


# plot of raster and shapefiles (subset of first 50 shapefiles)
plot(ACT)
subset <- shape[as.character(shape$SITECODE)==as.character(overlapSPA[c(1:50)]),] # only 50 shapefiles
plot(subset, col="red", add=TRUE)


# extract the ACTs sitecode-wise
total <- length(overlapSPA) #5569 SPAs
ACT_ext<-list()

total<-1000

pb <- txtProgressBar(min = 0, max = total, style = 3)

for (i in 1:total){
  subset <- shape[as.character(shape$SITECODE)==as.character(overlapSPA)[i],] # takes one sitecode of the shapefile
  ACT_ext[[i]]<-extract(ACT,subset) # extracts rasterdata for each sitecode
  setTxtProgressBar(pb, i)
}
close(pb)



# get rid of NAs
ACT_clear<-list()

for(j in 1:length(ACT_ext)){
  LUclasses<-ACT_ext[[j]]
  ACT_clear[[j]]<-LUclasses[!is.na(LUclasses)]
}


# create a dataframe for presenting the distribution of SPA-area per ACT
results<-as.data.frame(matrix(NA,length(ACT_clear),18))
colnames(results)<-c("sitecode",paste("ACT",c(1:17),sep=""))

for(k in 1:length(ACT_clear)){
  results[k,1]<-as.character(overlapSPA)[k]
  
  if(length(ACT_clear[[k]])==0){}else{
    percent.ACT<-table(ACT_clear[[k]])/sum(table(ACT_clear[[k]]))
    results[k,as.integer(names(table(ACT_clear[[k]])))+1]<-as.numeric(percent.ACT)
  }
  print(k)
}

View(results)

