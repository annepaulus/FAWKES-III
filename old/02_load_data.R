############################################################################
### Purpose of this script module 02 is to:
###
### 02.1. Get Natura 2000 data
### 02.2. Get Levers data
###
### Should load all files needed from the web. Best to place a file 
### somewhere in your dropbox and copy the share link here (right click). 
### You need to change dl=0 to dl=1 at the end of the link to make it work.
###
### Authors: CH, MB, ...
############################################################################

# set to temp wd suitable for downloading and extracting data files
setwd(path2temp %+% "/") 

############################################################################
### 02.1. Get Natura 2000 data
############################################################################

if (file.exists("PublicNatura2000End2015_csv.zip")==FALSE){
  download.file("https://www.dropbox.com/s/yaujzwuijyzluc6/PublicNatura2000End2015_csv.zip?dl=1", "PublicNatura2000End2015_csv.zip", mode="wb")
  unzip("PublicNatura2000End2015_csv.zip")
} else {unzip("PublicNatura2000End2015_csv.zip")}

# Find list of unique N2000 SPAs (where SITETYPE is A or C)
N2000Sites<-read.csv("NATURA2000SITES.csv",header=TRUE)
N2000SPASiteCodes<-subset(N2000Sites$SITECODE,N2000Sites$SITETYPE=="A"|N2000Sites$SITETYPE=="C")

# Load list of species with presence at each site
N2000Species<-read.csv("SPECIES.csv",header=TRUE)
# Use only birds for which at least one SPA has a GLOBAL importance of at least "C" ("significant")
BirdSpecies<-unique(subset(N2000Species$SPECIESNAME,N2000Species$SPGROUP=="Birds" & N2000Species$GLOBAL %in% c("A","B","C")))

# Create a temporary object to hold the data from each web request
spData <- subset(N2000Species,N2000Species$SITECODE==N2000SPASiteCodes[x] & N2000Species$GLOBAL %in% c("A","B","C"))
spTrends<-subset(statusTable,as.vector(statusTable[,1]) %in% as.vector(spData$SPECIESNAME))







if (file.exists("Natura2000_end2016_Shapefile.zip")==FALSE){
  download.file("https://www.dropbox.com/home/FAWKES%20III/Natura2000_end2016_Shapefile.zip?dl=1", "Natura2000_end2016_Shapefile.zip", mode="wb")
  unzip("Natura2000_end2016_Shapefile.zip")
} else {unzip("Natura2000_end2016_Shapefile.zip")}



############################################################################
### 02.2. Get Levers data
############################################################################

### authorize with google docs, the first time or in a new session:
### follow the displayed url, go to browser and enter your login credentials click accept and copy key back into R
#gs_ls() # once authorized, this will list the files you have in GS



