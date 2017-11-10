############################################################################
### Purpose of this script module 08 is to:
###
### 08.1 Clean the N2000 impact data
### 08.2 Subset the data 
### 08.3 Associations of services
### 08.4 Bar plots of services by biogeographical region
### 08.5 ESS displayed by core habitat
### 08.6 Site-centred conservation and IUCN indices
### 08.7 Bird conservation (from N2000) and IUCN status by mean net ESS
### 08.8 Output services for ArcGIS plotting (Figure 1)
### 08.9 Find frequency distribution of dominance
### 08.10 Cooccurrence analysis of ESS
### 08.11 Co-occurrence analysis excluding ESS mapped to multiple threats
### 08.12 Omnibus multinomial models of habitat quality and ESS
### 08.13 Code Graveyard
### 
### Authors: CH, AC, MB, AK
###
### Run skript modules 00, 01, 02, 03 before
############################################################################

############################################################################
### 08.1 Clean the N2000 impact data
###
### Create N2000Impact data frame as a base for further data analysis, and
### fix a large number of typos and missing data
############################################################################

# set wd to path2temp where files have been downloaded and extracted
setwd(path2temp %+% "/") 

# Load data
N2000Impact<-read.csv("IMPACT.csv")

#### DATA CLEANING ####
# Change column name from "ï..SITECODE" to "SITECODE"
colnames(N2000Impact)[1] <- "SITECODE"

# Convert lower case to upper case
N2000Impact$IMPACTCODE<-gsub("j", "J", N2000Impact$IMPACTCODE)
N2000Impact$IMPACTCODE<-gsub("k", "K", N2000Impact$IMPACTCODE)

# Convert letter "O" to number "0"
N2000Impact$IMPACTCODE<-gsub("O", "0", N2000Impact$IMPACTCODE)

# Replace comma with period
N2000Impact$IMPACTCODE<-gsub(",", ".", N2000Impact$IMPACTCODE)

# Remove spaces
N2000Impact$IMPACTCODE<-gsub(" ", "", N2000Impact$IMPACTCODE)

# Some impact codes had a period as the final character, which is also invalid
for(x in 1:nrow(N2000Impact)){
  if(substr(N2000Impact$IMPACTCODE[x],nchar(N2000Impact$IMPACTCODE[x]),nchar(N2000Impact$IMPACTCODE[x]))==".")
  {N2000Impact$IMPACTCODE[x]<-substr(N2000Impact$IMPACTCODE[x],1,nchar(N2000Impact$IMPACTCODE[x])-1)}
}

# Remove codes that do not exist in definitions, i.e. beginning with 0, 6, 8, O and P (n=102)
FirstChar<-substr(N2000Impact$IMPACTCODE,1,1)
N2000Impact<-subset(N2000Impact,is.na(match(FirstChar,c("0","6", "8", "O","P"))))

# Remove NULL impact codes (n=5494)
N2000Impact<-subset(N2000Impact,N2000Impact$IMPACTCODE!="NULL")

# And some very specific mistakes
N2000Impact<-subset(N2000Impact,N2000Impact$IMPACTCODE!="D014.01") # Not possible to establish whether D01.01 or D04.01, so delete
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="D2.01")]<-"D02.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="F.03.01.01")]<-"F03.01.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="K.02.01")]<-"K02.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="G.01.04.03")]<-"G01.04.03" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="F3.01.01")]<-"F03.01.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="C3.03")]<-"C03.03"

# Remove duplicate lines
N2000Impact<-N2000Impact[!duplicated(N2000Impact),]

# Convert to factor
N2000Impact$IMPACTCODE<-as.factor(N2000Impact$IMPACTCODE)


###############################################
### 08.2 Subsetting data 
### Just working with subset of N2000Impact with the following characteristics:
###     (i) SITE_TYPE = A or C (SPA sites only)
###     (ii) INTENSITY = MEDIUM or HIGH
###     (iii) OCCURRENCE = IN or BOTH
###############################################

# First, subset N2000Impact by intensity and occurrence
N2000Impact<-subset(N2000Impact,N2000Impact$INTENSITY=="MEDIUM" | N2000Impact$INTENSITY=="HIGH")
N2000Impact<-subset(N2000Impact,N2000Impact$OCCURRENCE=="IN" | N2000Impact$OCCURRENCE=="BOTH")

# Assign site type
# Load data
N2000Sites <- read.csv("NATURA2000SITES.csv")
N2000Sites[,4] <- as.character(N2000Sites[,4])
N2000Impact$SITE_TYPE <- NA

# Add site type to N2000Impact (running time 117 seconds)
for(x in 1:nrow(N2000Impact)){
  N2000Impact$SITE_TYPE[x] <- N2000Sites[match(N2000Impact$SITECODE[x],N2000Sites$SITECODE),4]
}

# Now subset to exclude SITE_TYPE="B"
N2000Impact<-subset(N2000Impact,N2000Impact$SITE_TYPE %in% c("A","C"))


############################################################################
### 08.3 Associations of services
###
### Association the threats from the N2000Impact data with the mapped
### ecosystem services from the Google Doc
############################################################################

# Bind Guy's mapping to the threats table
N2000Impact<-cbind(N2000Impact,GuyMappingData[match(N2000Impact$IMPACTCODE,GuyMappingData$ACT_Code),])

# Create a list of services based on Guy's mapping 
ServiceList<-names(GuyMappingData[,c(3:11)])

ServiceBySite<-matrix(ncol=length(ServiceList)*4,nrow=length(unique(N2000Impact$SITECODE)))
rownames(ServiceBySite)<-unique(N2000Impact$SITECODE)
colnames(ServiceBySite)<-c(paste(ServiceList,"POS"),paste(ServiceList,"NEG"),paste(ServiceList,"BOTH"),paste(ServiceList,"NET"))

# Run through mapping and tally the positive (in the first 9 columns) and negative (second 9 columns)
# ESS associated with each site. Then calculate the difference between the two to give a net score for each
# ESS on each site
ptm <- proc.time()
for(x in 1:nrow(ServiceBySite)){
  # For each unique site ID code, extract a list of threats that have corresponding services
  SiteServices<-subset(N2000Impact,N2000Impact$SITECODE==rownames(ServiceBySite)[x])
  # For each service group (defined by Guy), sum the number of times it was positive or negative
  for(y in 1:length(ServiceList)){
    if(nrow(subset(SiteServices,SiteServices[,10+y] %in% c("c","x") & SiteServices$IMPACT_TYPE=="P"))>0) {ServiceBySite[x,y] <- 1} else {ServiceBySite[x,y] <- 0}
    if(nrow(subset(SiteServices,SiteServices[,10+y] %in% c("c","x") & SiteServices$IMPACT_TYPE=="N"))>0) {ServiceBySite[x,y+9] <- 1} else {ServiceBySite[x,y+9] <- 0}
    if("P"%in%subset(SiteServices,SiteServices[,10+y] %in% c("c","x"))$IMPACT_TYPE & "N"%in%subset(SiteServices,SiteServices[,10+y] %in% c("c","x"))$IMPACT_TYPE) {ServiceBySite[x,y+18]<-1;ServiceBySite[x,y] <- 0;ServiceBySite[x,y+9] <- 0} else {ServiceBySite[x,y+18]<-0}
    ServiceBySite[x,y+27]<-ServiceBySite[x,y]-ServiceBySite[x,y+9]
  }
  # Timer to track progress of loop
  if(x %% 100 == 0) {print(x/nrow(ServiceBySite));flush.console()}
}
proc.time() - ptm

# Final "net" value for all ESS across each site
NetESS<-rowSums(ServiceBySite[,c(28:36)])
NetESSwt<-rowSums(ServiceBySite[,c(28:34)])/7+ServiceBySite[,35]+ServiceBySite[,36]
ServiceBySite<-cbind(ServiceBySite,NetESS,NetESSwt)

# Add Bioregion (note that sometimes the BIOREGION$SITECODE field is called "i..SITECODE" which
# causes problems with matching the datasets - it may depend on operating system)
BIOREGION<-read.csv("BIOREGION.csv")
# Change column name from "ï..SITECODE" to "SITECODE"
colnames(BIOREGION)[1] <- "SITECODE"
ServiceBySite<-data.frame(ServiceBySite,Biogeog=as.factor(BIOREGION[match(rownames(ServiceBySite),BIOREGION$SITECODE),2]))


############################################################################
### 08.4 Bar plots of services by biogeographical region
###
### Creates bar plots of types of ESS by biogeographical region
############################################################################

# 1: STACKED BARS SHOWING PROPORTIONS

par(mfrow=c(3,2),mar=c(3,4,3,2))
ESLabels<-c("CR","FD","FB","LS","WF","AQ","WA","RG","RC")
# Figure 2A (all SPAs)
All_BarData<-cbind(colSums(ServiceBySite[,c(1:9)]),colSums(ServiceBySite[,c(19:27)]),colSums(ServiceBySite[,c(10:18)]))
rownames(All_BarData) <- ServiceList
colnames(All_BarData) <- c("POS","BOTH","NEG")
barplot(t(All_BarData/rowSums(All_BarData)),las=2, legend=F, main="(A) All SPAs",names.arg=ESLabels)

# Figure 2B (Atlantic)
Atlantic_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Atlantic")
Atlantic_BarData<-cbind(colSums(Atlantic_ServiceBySite[,c(1:9)]),colSums(Atlantic_ServiceBySite[,c(19:27)]),colSums(Atlantic_ServiceBySite[,c(10:18)]))
rownames(Atlantic_BarData) <- ServiceList
colnames(Atlantic_BarData) <- c("POS","BOTH","NEG")
barplot(t(Atlantic_BarData/rowSums(Atlantic_BarData)),las=2, legend=F, main="(B) Atlantic SPAs",names.arg=ESLabels)

# Figure 2C (Continental)
Continental_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Continental")
Continental_BarData<-cbind(colSums(Continental_ServiceBySite[,c(1:9)]),colSums(Continental_ServiceBySite[,c(19:27)]),colSums(Continental_ServiceBySite[,c(10:18)]))
rownames(Continental_BarData) <- ServiceList
colnames(Continental_BarData) <- c("POS","BOTH","NEG")
barplot(t(Continental_BarData/rowSums(Continental_BarData)),las=2, legend=F, main="(C) Continental SPAs",names.arg=ESLabels)

# Figure 2D (Mediterranean)
Mediterranean_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Mediterranean")
Mediterranean_BarData<-cbind(colSums(Mediterranean_ServiceBySite[,c(1:9)]),colSums(Mediterranean_ServiceBySite[,c(19:27)]),colSums(Mediterranean_ServiceBySite[,c(10:18)]))
rownames(Mediterranean_BarData) <- ServiceList
colnames(Mediterranean_BarData) <- c("POS","BOTH","NEG")
barplot(t(Mediterranean_BarData/rowSums(Mediterranean_BarData)),las=2, legend=F, main="(D) Mediterranean SPAs",names.arg=ESLabels)

# Figure 2E (Boreal)
Boreal_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Boreal")
Boreal_BarData<-cbind(colSums(Boreal_ServiceBySite[,c(1:9)]),colSums(Boreal_ServiceBySite[,c(19:27)]),colSums(Boreal_ServiceBySite[,c(10:18)]))
rownames(Boreal_BarData) <- ServiceList
colnames(Boreal_BarData) <- c("POS","BOTH","NEG")
barplot(t(Boreal_BarData/rowSums(Boreal_BarData)),las=2, legend=F, main="(E) Boreal SPAs",names.arg=ESLabels)

# Figure 2F (Alpine)
Alpine_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Alpine")
Alpine_BarData<-cbind(colSums(Alpine_ServiceBySite[,c(1:9)]),colSums(Alpine_ServiceBySite[,c(19:27)]),colSums(Alpine_ServiceBySite[,c(10:18)]))
rownames(Alpine_BarData) <- ServiceList
colnames(Alpine_BarData) <- c("POS","BOTH","NEG")
barplot(t(Alpine_BarData/rowSums(Alpine_BarData)),las=2, legend=F, main="(F) Alpine SPAs",names.arg=ESLabels)

# Export data
BiogeogOrigin<-cbind(rbind(All_BarData,Atlantic_BarData,Continental_BarData,Mediterranean_BarData,Boreal_BarData,Alpine_BarData),
      rep(c("All","Atlantic","Continental","Mediterranean","Boreal","Alpine"),each=9))
write.table(BiogeogOrigin,"BiogeogOrigin.txt")



# 2: STACKED BARS SHOWING ABSOLUTE NUMBERS

par(mfrow=c(3,2))

# Figure 2A (all SPAs)
All_BarData<-cbind(colSums(ServiceBySite[,c(1:9)]),colSums(ServiceBySite[,c(19:27)]),colSums(ServiceBySite[,c(10:18)]))
rownames(All_BarData) <- ServiceList
colnames(All_BarData) <- c("POS","BOTH","NEG")
barplot(t(All_BarData),las=2, legend=F, main="(A) All SPAs",names.arg=ESLabels)

# Figure 2B (Atlantic)
Atlantic_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Atlantic")
Atlantic_BarData<-cbind(colSums(Atlantic_ServiceBySite[,c(1:9)]),colSums(Atlantic_ServiceBySite[,c(19:27)]),colSums(Atlantic_ServiceBySite[,c(10:18)]))
rownames(Atlantic_BarData) <- ServiceList
colnames(Atlantic_BarData) <- c("POS","BOTH","NEG")
barplot(t(Atlantic_BarData),las=2, legend=F, main="(B) Atlantic SPAs",names.arg=ESLabels)

# Figure 2C (Continental)
Continental_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Continental")
Continental_BarData<-cbind(colSums(Continental_ServiceBySite[,c(1:9)]),colSums(Continental_ServiceBySite[,c(19:27)]),colSums(Continental_ServiceBySite[,c(10:18)]))
rownames(Continental_BarData) <- ServiceList
colnames(Continental_BarData) <- c("POS","BOTH","NEG")
barplot(t(Continental_BarData),las=2, legend=F, main="(C) Continental SPAs",names.arg=ESLabels)

# Figure 2D (Mediterranean)
Mediterranean_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Mediterranean")
Mediterranean_BarData<-cbind(colSums(Mediterranean_ServiceBySite[,c(1:9)]),colSums(Mediterranean_ServiceBySite[,c(19:27)]),colSums(Mediterranean_ServiceBySite[,c(10:18)]))
rownames(Mediterranean_BarData) <- ServiceList
colnames(Mediterranean_BarData) <- c("POS","BOTH","NEG")
barplot(t(Mediterranean_BarData),las=2, legend=F, main="(D) Mediterranean SPAs",names.arg=ESLabels)

# Figure 2E (Boreal)
Boreal_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Boreal")
Boreal_BarData<-cbind(colSums(Boreal_ServiceBySite[,c(1:9)]),colSums(Boreal_ServiceBySite[,c(19:27)]),colSums(Boreal_ServiceBySite[,c(10:18)]))
rownames(Boreal_BarData) <- ServiceList
colnames(Boreal_BarData) <- c("POS","BOTH","NEG")
barplot(t(Boreal_BarData),las=2, legend=F, main="(E) Boreal SPAs",names.arg=ESLabels)

# Figure 2F (Alpine)
Alpine_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Alpine")
Alpine_BarData<-cbind(colSums(Alpine_ServiceBySite[,c(1:9)]),colSums(Alpine_ServiceBySite[,c(19:27)]),colSums(Alpine_ServiceBySite[,c(10:18)]))
rownames(Alpine_BarData) <- ServiceList
colnames(Alpine_BarData) <- c("POS","BOTH","NEG")
barplot(t(Alpine_BarData),las=2, legend=F, main="(F) Alpine SPAs",names.arg=ESLabels)


# 3: STACKED BARS SHOWING Percentage of total SPAs

par(mfrow=c(3,2))

# Figure 2A (all SPAs)
All_BarData<-cbind(colSums(ServiceBySite[,c(1:9)]),colSums(ServiceBySite[,c(19:27)]),colSums(ServiceBySite[,c(10:18)]))
rownames(All_BarData) <- ServiceList
colnames(All_BarData) <- c("POS","BOTH","NEG")
barplot(t(All_BarData),las=2, legend=F, main="(A) All SPAs",names.arg=ESLabels)

# Figure 2B (Atlantic)
Atlantic_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Atlantic")
Atlantic_BarData<-cbind(colSums(Atlantic_ServiceBySite[,c(1:9)]),colSums(Atlantic_ServiceBySite[,c(19:27)]),colSums(Atlantic_ServiceBySite[,c(10:18)]))
rownames(Atlantic_BarData) <- ServiceList
colnames(Atlantic_BarData) <- c("POS","BOTH","NEG")
barplot(t(Atlantic_BarData),las=2, legend=F, main="(B) Atlantic SPAs",names.arg=ESLabels)

# Figure 2C (Continental)
Continental_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Continental")
Continental_BarData<-cbind(colSums(Continental_ServiceBySite[,c(1:9)]),colSums(Continental_ServiceBySite[,c(19:27)]),colSums(Continental_ServiceBySite[,c(10:18)]))
rownames(Continental_BarData) <- ServiceList
colnames(Continental_BarData) <- c("POS","BOTH","NEG")
barplot(t(Continental_BarData),las=2, legend=F, main="(C) Continental SPAs",names.arg=ESLabels)

# Figure 2D (Mediterranean)
Mediterranean_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Mediterranean")
Mediterranean_BarData<-cbind(colSums(Mediterranean_ServiceBySite[,c(1:9)]),colSums(Mediterranean_ServiceBySite[,c(19:27)]),colSums(Mediterranean_ServiceBySite[,c(10:18)]))
rownames(Mediterranean_BarData) <- ServiceList
colnames(Mediterranean_BarData) <- c("POS","BOTH","NEG")
barplot(t(Mediterranean_BarData),las=2, legend=F, main="(D) Mediterranean SPAs",names.arg=ESLabels)

# Figure 2E (Boreal)
Boreal_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Boreal")
Boreal_BarData<-cbind(colSums(Boreal_ServiceBySite[,c(1:9)]),colSums(Boreal_ServiceBySite[,c(19:27)]),colSums(Boreal_ServiceBySite[,c(10:18)]))
rownames(Boreal_BarData) <- ServiceList
colnames(Boreal_BarData) <- c("POS","BOTH","NEG")
barplot(t(Boreal_BarData),las=2, legend=F, main="(E) Boreal SPAs",names.arg=ESLabels)

# Figure 2F (Alpine)
Alpine_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Alpine")
Alpine_BarData<-cbind(colSums(Alpine_ServiceBySite[,c(1:9)]),colSums(Alpine_ServiceBySite[,c(19:27)]),colSums(Alpine_ServiceBySite[,c(10:18)]))
rownames(Alpine_BarData) <- ServiceList
colnames(Alpine_BarData) <- c("POS","BOTH","NEG")
barplot(t(Alpine_BarData),las=2, legend=F, main="(F) Alpine SPAs",names.arg=ESLabels)


############################################################################
### 08.5 ESS displayed by core habitat
############################################################################

# Load HABITATCLASS
HABITATCLASS<-read.csv("HABITATCLASS.csv")
# Change column name from "ï..SITECODE" to "SITECODE"
colnames(HABITATCLASS)[1] <- "SITECODE"
# Convert % cover to a numeric variable
HABITATCLASS$PERCENTAGECOVER<-as.numeric(as.vector(HABITATCLASS$PERCENTAGECOVER))
# Extract a subset where the % cover is >=50%
HABITATCLASS<-subset(HABITATCLASS,HABITATCLASS$PERCENTAGECOVER>=50)
# Add that dominant habitat to the main table
ServiceBySite<-cbind(ServiceBySite,DomHab=HABITATCLASS$HABITATCODE[match(rownames(ServiceBySite),HABITATCLASS$SITECODE)])

# Subset the data to just include those sites with a dominant class
DomHabData<-subset(ServiceBySite,ServiceBySite$DomHab%in%names(which(table(ServiceBySite$DomHab)>30)))
# Guy used "any mention" as the response
AnyMention<-matrix(nrow=nrow(DomHabData),ncol=9)
for(x in 1:nrow(DomHabData)){
  for(y in 1:9){
    if(sum(DomHabData[x,c(y,y+9,y+18)])>0) {AnyMention[x,y]<-1} else {AnyMention[x,y]<-0}
  }
}

# Find the average number of ESS per dominant habitat
aggregate(rowSums(AnyMention), list(DomHabData$DomHab), mean)

# Find the proportion of SPAs in each dominant habitat that mention each ESS
xtabs(DomHabData$DomHab~AnyMention[,1])
ESSByHab<-aggregate(AnyMention, list(DomHabData$DomHab), mean)

# Tables of each habitat, with positive, negative, and both
# First, N01
N01_Data<-subset(DomHabData,DomHabData$DomHab=="N01")
N02_Data<-subset(DomHabData,DomHabData$DomHab=="N02")
N05_Data<-subset(DomHabData,DomHabData$DomHab=="N05")
N06_Data<-subset(DomHabData,DomHabData$DomHab=="N06")
N07_Data<-subset(DomHabData,DomHabData$DomHab=="N07")
N08_Data<-subset(DomHabData,DomHabData$DomHab=="N08")
N10_Data<-subset(DomHabData,DomHabData$DomHab=="N10")
N12_Data<-subset(DomHabData,DomHabData$DomHab=="N12")
N14_Data<-subset(DomHabData,DomHabData$DomHab=="N14")
N15_Data<-subset(DomHabData,DomHabData$DomHab=="N15")
N16_Data<-subset(DomHabData,DomHabData$DomHab=="N16")
N17_Data<-subset(DomHabData,DomHabData$DomHab=="N17")
N19_Data<-subset(DomHabData,DomHabData$DomHab=="N19")
N23_Data<-subset(DomHabData,DomHabData$DomHab=="N23")

# Find proportions
N01_ESS<-data.frame(Pos=t(unname(aggregate(N01_Data[,c(1:9)], list(N01_Data$DomHab), mean))),
                   Neg=t(unname(aggregate(N01_Data[,c(10:18)], list(N01_Data$DomHab), mean))),
                   Both=t(unname(aggregate(N01_Data[,c(19:27)], list(N01_Data$DomHab), mean))))[-1,]
N02_ESS<-data.frame(Pos=t(unname(aggregate(N02_Data[,c(1:9)], list(N02_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N02_Data[,c(10:18)], list(N02_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N02_Data[,c(19:27)], list(N02_Data$DomHab), mean))))[-1,]
N05_ESS<-data.frame(Pos=t(unname(aggregate(N05_Data[,c(1:9)], list(N05_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N05_Data[,c(10:18)], list(N05_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N05_Data[,c(19:27)], list(N05_Data$DomHab), mean))))[-1,]
N06_ESS<-data.frame(Pos=t(unname(aggregate(N06_Data[,c(1:9)], list(N06_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N06_Data[,c(10:18)], list(N06_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N06_Data[,c(19:27)], list(N06_Data$DomHab), mean))))[-1,]
N07_ESS<-data.frame(Pos=t(unname(aggregate(N07_Data[,c(1:9)], list(N07_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N07_Data[,c(10:18)], list(N07_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N07_Data[,c(19:27)], list(N07_Data$DomHab), mean))))[-1,]
N08_ESS<-data.frame(Pos=t(unname(aggregate(N08_Data[,c(1:9)], list(N08_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N08_Data[,c(10:18)], list(N08_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N08_Data[,c(19:27)], list(N08_Data$DomHab), mean))))[-1,]
N10_ESS<-data.frame(Pos=t(unname(aggregate(N10_Data[,c(1:9)], list(N10_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N10_Data[,c(10:18)], list(N10_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N10_Data[,c(19:27)], list(N10_Data$DomHab), mean))))[-1,]
N12_ESS<-data.frame(Pos=t(unname(aggregate(N12_Data[,c(1:9)], list(N12_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N12_Data[,c(10:18)], list(N12_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N12_Data[,c(19:27)], list(N12_Data$DomHab), mean))))[-1,]
N14_ESS<-data.frame(Pos=t(unname(aggregate(N14_Data[,c(1:9)], list(N14_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N14_Data[,c(10:18)], list(N14_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N14_Data[,c(19:27)], list(N14_Data$DomHab), mean))))[-1,]
N15_ESS<-data.frame(Pos=t(unname(aggregate(N15_Data[,c(1:9)], list(N15_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N15_Data[,c(10:18)], list(N15_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N15_Data[,c(19:27)], list(N15_Data$DomHab), mean))))[-1,]
N16_ESS<-data.frame(Pos=t(unname(aggregate(N16_Data[,c(1:9)], list(N16_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N16_Data[,c(10:18)], list(N16_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N16_Data[,c(19:27)], list(N16_Data$DomHab), mean))))[-1,]
N17_ESS<-data.frame(Pos=t(unname(aggregate(N17_Data[,c(1:9)], list(N17_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N17_Data[,c(10:18)], list(N17_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N17_Data[,c(19:27)], list(N17_Data$DomHab), mean))))[-1,]
N19_ESS<-data.frame(Pos=t(unname(aggregate(N19_Data[,c(1:9)], list(N19_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N19_Data[,c(10:18)], list(N19_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N19_Data[,c(19:27)], list(N19_Data$DomHab), mean))))[-1,]
N23_ESS<-data.frame(Pos=t(unname(aggregate(N23_Data[,c(1:9)], list(N23_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N23_Data[,c(10:18)], list(N23_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N23_Data[,c(19:27)], list(N23_Data$DomHab), mean))))[-1,]

# Convert to numeric
N01_ESS<-transform(N01_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N02_ESS<-transform(N02_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N05_ESS<-transform(N05_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N06_ESS<-transform(N06_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N07_ESS<-transform(N07_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N08_ESS<-transform(N08_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N10_ESS<-transform(N10_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N12_ESS<-transform(N12_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N14_ESS<-transform(N14_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N15_ESS<-transform(N15_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N16_ESS<-transform(N16_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N17_ESS<-transform(N17_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N19_ESS<-transform(N19_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N23_ESS<-transform(N23_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))

# Plot barcharts
par(mfrow=c(4,4),mar=c(2,5.5,2,1))

barplot(t(as.matrix(N01_ESS)),main="N01: Marine",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N02_ESS)),main="N02: Intertidal",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N05_ESS)),main="N05: Shore",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N06_ESS)),main="N06: Inland water",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N07_ESS)),main="N07: Marshes",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N08_ESS)),main="N08: Heath",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N10_ESS)),main="N10: Grassland",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N12_ESS)),main="N12: Cropland",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N14_ESS)),main="N14: Improv grass",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N15_ESS)),main="N15: Other arable",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N16_ESS)),main="N16: Broad wood",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N17_ESS)),main="N17: Conif wood",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N19_ESS)),main="N19: Mixed wood",names.arg=ServiceList,horiz=TRUE,las=1)
barplot(t(as.matrix(N23_ESS)),main="N23: Other land",names.arg=ServiceList,horiz=TRUE,las=1)

# Now we plot the other way around: bar charts for each ESS across the different core habitats
AllESS<-as.data.frame(rbind(N01_ESS,N02_ESS,N05_ESS,N06_ESS,N07_ESS,N08_ESS,N10_ESS,N12_ESS,N14_ESS,N15_ESS,N16_ESS,N17_ESS,N19_ESS,N23_ESS))
AllESS$ESS<-rep(ServiceList,14)
Habitats<-c("N01","N02","N05","N06","N07","N08","N10","N12","N14","N15","N16","N17","N19","N23")
AllESS$Habitat<-rep(Habitats,each=9)
AllESS<-AllESS[,c(1,3,2,4,5)]


OnlyMarineESS<-OnlyWaterESS<-OnlyHeathGrassESS<-OnlyFarmESS<-OnlyWoodESS<-OnlyOtherESS<-AllESS

OnlyMarineESS[OnlyMarineESS$Habitat%in%c("N06","N07","N08","N10","N12","N14","N15","N16","N17","N19","N23"),c(1:3)]<-0
OnlyWaterESS[OnlyWaterESS$Habitat%in%c("N01","N02","N05","N08","N10","N12","N14","N15","N16","N17","N19","N23"),c(1:3)]<-0
OnlyHeathGrassESS[OnlyHeathGrassESS$Habitat%in%c("N01","N02","N05","N06","N07","N12","N14","N15","N16","N17","N19","N23"),c(1:3)]<-0
OnlyFarmESS[OnlyFarmESS$Habitat%in%c("N01","N02","N05","N06","N07","N08","N10","N16","N17","N19","N23"),c(1:3)]<-0
OnlyWoodESS[OnlyWoodESS$Habitat%in%c("N01","N02","N05","N06","N07","N08","N10","N12","N14","N15","N23"),c(1:3)]<-0
OnlyOtherESS[OnlyOtherESS$Habitat%in%c("N01","N02","N05","N06","N07","N08","N10","N12","N14","N15","N16","N17","N19"),c(1:3)]<-0


par(mfrow=c(3,3))
barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Crop")))[,rev(c(1:14))],main="Crop",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Crop")))[,rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Crop")))[,rev(c(1:14))],add=TRUE,col=c("chocolate4","chocolate3","chocolate2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Crop")))[,rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Crop")))[,rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Crop")))[,rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Fodder")))[,rev(c(1:14))],main="Fodder",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Fodder")))[,rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Fodder")))[,rev(c(1:14))],add=TRUE,col=c("chocolate4","chocolate3","chocolate2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Fodder")))[,rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Fodder")))[,rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Fodder")))[,rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Fibre")))[,rev(c(1:14))],main="Fibre",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Fibre")))[,rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Fibre")))[,rev(c(1:14))],add=TRUE,col=c("chocolate4","chocolate3","chocolate2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Fibre")))[,rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Fibre")))[,rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Fibre")))[,rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Livestock")))[,rev(c(1:14))],main="Livestock",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Livestock")))[,rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Livestock")))[,rev(c(1:14))],add=TRUE,col=c("chocolate4","chocolate3","chocolate2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Livestock")))[,rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Livestock")))[,rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Livestock")))[,rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Wild.food")))[,rev(c(1:14))],main="Wild food",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Wild.food")))[,rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Wild.food")))[,rev(c(1:14))],add=TRUE,col=c("chocolate4","chocolate3","chocolate2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Wild.food")))[,rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Wild.food")))[,rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Wild.food")))[,rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Aquaculture")))[,rev(c(1:14))],main="Aquaculture",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Aquaculture")))[,rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Aquaculture")))[,rev(c(1:14))],add=TRUE,col=c("chocolate4","chocolate3","chocolate2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Aquaculture")))[,rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Aquaculture")))[,rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Aquaculture")))[,rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Water")))[,rev(c(1:14))],main="Water",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Water")))[,rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Water")))[,rev(c(1:14))],add=TRUE,col=c("chocolate4","chocolate3","chocolate2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Water")))[,rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Water")))[,rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Water")))[,rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Regulating")))[,rev(c(1:14))],main="Regulating",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Regulating")))[,rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Regulating")))[,rev(c(1:14))],add=TRUE,col=c("chocolate4","chocolate3","chocolate2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Regulating")))[,rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Regulating")))[,rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Regulating")))[,rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Recreation")))[,rev(c(1:14))],main="Recreation",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Recreation")))[,rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Recreation")))[,rev(c(1:14))],add=TRUE,col=c("chocolate4","chocolate3","chocolate2"),horiz=TRUE,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Recreation")))[,rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Recreation")))[,rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Recreation")))[,rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,xlim=c(0,1),axisnames=FALSE)

# Write to file for plotting in Origin
write.table(AllESS,"AllESS by Dominant Habitat Origin.txt")

############################################################################
### 08.6 Site-centred conservation and IUCN indices
###
### This script takes the site as the unit of replication and calculates
### the "conservation index" (the mean habitat quality for the community
### of bird species living at the site) and the "IUCN index" (the mean
### IUCN trend for the species living at the site)
############################################################################

# Convert the A, B, C CONSERVATION code to a numeric score
SPECIES<-read.csv("SPECIES.csv")
ConvertToNumber<-data.frame(Letters=c("A","B","C"),Numbers=c(2,1,0))
SpeciesIndex<-ConvertToNumber[match(SPECIES$CONSERVATION,ConvertToNumber[,1]),2]
SPECIES$SpeciesIndex<-SpeciesIndex

BIRDSPECIES<-subset(SPECIES,SPECIES$SPGROUP=="Birds" & SPECIES$GLOBAL %in% c("A","B","C"))

# Add bird scores to the sites
IUCNIndex<-BirdIndex<-IUCNNumber<-BirdNumber<-numeric(length=nrow(ServiceBySite))
for(x in 1:nrow(ServiceBySite)){
  BirdIndex[x]<-mean(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(ServiceBySite)[x]))
  BirdNumber[x]<-length(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(ServiceBySite)[x]))
  IUCNIndex[x]<-mean(c(rep(2,subset(SiteTrends$Inc,SiteTrends$Site==rownames(ServiceBySite)[x])),
                       rep(1,subset(SiteTrends$Stable,SiteTrends$Site==rownames(ServiceBySite)[x])),
                       rep(0,subset(SiteTrends$Dec,SiteTrends$Site==rownames(ServiceBySite)[x]))))
  IUCNNumber[x]<-sum(subset(SiteTrends$Inc,SiteTrends$Site==rownames(ServiceBySite)[x]),
                     subset(SiteTrends$Stable,SiteTrends$Site==rownames(ServiceBySite)[x]),
                     subset(SiteTrends$Dec,SiteTrends$Site==rownames(ServiceBySite)[x]))
  if(x%%100==0) {print(x)}
  flush.console()
}

SiteData<-cbind(IUCNIndex,BirdIndex,IUCNNumber,BirdNumber,NetESS)
SiteData<-as.data.frame(SiteData[complete.cases(SiteData),])
par(mfrow=c(1,2),mar=c(5,4,4,2))
#plot(NetESS,BirdIndex,xlab="NetESS",ylab="Conservation index")
#mtext("A",cex=2,at=3)
#plot(NetESS,IUCNIndex,xlab="NetESS",ylab="IUCN trends index")
#mtext("B",cex=2,at=3)

SummaryBirdData<-matrix(ncol=4,nrow=13)
colnames(SummaryBirdData)<-c("NetESS","MeanBirdStatus","SE","N")
for(x in 1:13){
  SummaryBirdData[x,1]<-x-9
  SummaryBirdData[x,2]<-mean(subset(BirdIndex,NetESS==x-9),na.rm=TRUE)
  SummaryBirdData[x,3]<-sd(subset(BirdIndex,NetESS==x-9),na.rm=TRUE)/sqrt(length(subset(BirdIndex,NetESS==x-9)))
  SummaryBirdData[x,4]<-length(subset(BirdIndex,NetESS==x-9))
}
plot(SummaryBirdData[,1],SummaryBirdData[,2],ylim=c(0,2),xlim=c(-9,6),ylab="Conservation index",xlab="Net ESS")
#arrows(SummaryBirdData[,1],SummaryBirdData[,2],SummaryBirdData[,1],SummaryBirdData[,2]+SummaryBirdData[,3],length=0)
#arrows(SummaryBirdData[,1],SummaryBirdData[,2],SummaryBirdData[,1],SummaryBirdData[,2]-SummaryBirdData[,3],length=0)
cor.test(NetESS,BirdIndex,method="spearman")
#abline(lm(SummaryBirdData[,2]~SummaryBirdData[,1],weights=SummaryBirdData[,4]))
text(-7,1.75,"A",cex=2)
points(jitter(NetESS),BirdIndex,col="lightgrey",cex=0.5)
points(SummaryBirdData[,1],SummaryBirdData[,2],pch=19)
mod1<-lm(BirdIndex~NetESS,data=SiteData)
newx <- seq(min(NetESS), max(NetESS), length.out=100)
preds <- predict(mod1, newdata = data.frame(NetESS=newx), interval = 'confidence')
#plot(NetESS,BirdIndex, type = 'n',ylim=c(0.5,0.7))
# add fill
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(0.1,0.1,0.1,0.2), border = NA)
# model
lines(newx,preds[,1],type="l")
#abline(mod1)
# intervals
lines(newx, preds[ ,3], lty = 'dashed', col = 'red');lines(newx, preds[ ,2], lty = 'dashed', col = 'red')

# Net ESS versus IUCN status

SummaryIUCNData<-matrix(ncol=4,nrow=13)
colnames(SummaryIUCNData)<-c("NetESSwt","MeanBirdStatus","SE","N")
for(x in 1:13){
  SummaryIUCNData[x,1]<-x-9
  SummaryIUCNData[x,2]<-mean(subset(IUCNIndex,NetESS==x-9),na.rm=TRUE)
  SummaryIUCNData[x,3]<-sd(subset(IUCNIndex,NetESS==x-9),na.rm=TRUE)/sqrt(length(subset(IUCNIndex,NetESS==x-9)))
  SummaryIUCNData[x,4]<-length(subset(IUCNIndex,NetESS==x-9))
}
plot(SummaryIUCNData[,1],SummaryIUCNData[,2],ylim=c(0,2),xlim=c(-9,6),ylab="IUCN trends index",xlab="Net ESS")
#arrows(SummaryIUCNData[,1],SummaryIUCNData[,2],SummaryIUCNData[,1],SummaryIUCNData[,2]+SummaryIUCNData[,3],length=0)
#arrows(SummaryIUCNData[,1],SummaryIUCNData[,2],SummaryIUCNData[,1],SummaryIUCNData[,2]-SummaryIUCNData[,3],length=0)
cor.test(NetESS,IUCNIndex,method="spearman")
#abline(lm(SummaryIUCNData[,2]~SummaryIUCNData[,1],weights=SummaryIUCNData[,4]))
text(-7,1.8,"B",cex=2)
points(jitter(NetESS),IUCNIndex,col="lightgrey",cex=0.5)
points(SummaryIUCNData[,1],SummaryIUCNData[,2],pch=19)
mod2<-lm(IUCNIndex~NetESS,data=SiteData)
newx <- seq(min(NetESS), max(NetESS), length.out=100)
preds <- predict(mod2, newdata = data.frame(NetESS=newx), interval = 'confidence')
#plot(NetESS,IUCNIndex, type = 'n',ylim=c(0.5,0.7))
# add fill
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(0.1,0.1,0.1,0.2), border = NA)
# model
lines(newx,preds[,1],type="l")
# intervals
lines(newx, preds[ ,3], lty = 'dashed', col = 'red');lines(newx, preds[ ,2], lty = 'dashed', col = 'red')

# Export for plotting in Origin
NetESSvsConsIndexvsIUCNraw<-cbind(jitter(NetESS),BirdIndex,IUCNIndex)
NetESSvsConsIndexvsIUCNsummary<-cbind(SummaryBirdData,SummaryIUCNData)
write.table(NetESSvsConsIndexvsIUCNraw,"NetESSvsConsIndexvsIUCNraw.txt")
write.table(NetESSvsConsIndexvsIUCNsummary,"NetESSvsConsIndexvsIUCNsummary.txt")

############################################################################
### 08.6b Site-centred conservation and IUCN indices - by Biogeographical regions
###
### This script takes the site as the unit of replication and calculates
### the "conservation index" (the mean habitat quality for the community
### of bird species living at the site) and the "IUCN index" (the mean
### IUCN trend for the species living at the site)
### in addition, this script runs the analysis by Biogeographical regions
############################################################################

# Convert the A, B, C CONSERVATION code to a numeric score
SPECIES<-read.csv("SPECIES.csv")
ConvertToNumber<-data.frame(Letters=c("A","B","C"),Numbers=c(2,1,0))
SpeciesIndex<-ConvertToNumber[match(SPECIES$CONSERVATION,ConvertToNumber[,1]),2]
SPECIES$SpeciesIndex<-SpeciesIndex

# add Bioregions
SPECIES$Biogeog<-BIOREGION$BIOGEFRAPHICREG[match(SPECIES$SITECODE,BIOREGION$SITECODE)]
SiteTrends$Biogeog<-BIOREGION$BIOGEFRAPHICREG[match(SiteTrends$Site,BIOREGION$SITECODE)]
#regions<-unique(SPECIES$Biogeog)
#regions <- regions[!is.na(regions)]
#remove macaronesia
#regions <- regions[c(1:11,13:14)]
regions<-c("Boreal","Atlantic","Alpine","Continental","Mediterranean")

for(a in 1:length(regions)){
  sub_SPECIES<-subset(SPECIES,SPECIES$Biogeog==paste(regions[a]))
  sub_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog==paste(regions[a]))
  sub_SiteTrends<-subset(SiteTrends,SiteTrends$Biogeog==paste(regions[a]))
  sub_NetESS<-NetESS[match(rownames(sub_ServiceBySite),names(NetESS))]
  
  BIRDSPECIES<-subset(sub_SPECIES,sub_SPECIES$SPGROUP=="Birds" & sub_SPECIES$GLOBAL %in% c("A","B","C"))

  # Add bird scores to the sites
  IUCNIndex<-BirdIndex<-IUCNNumber<-BirdNumber<-numeric(length=nrow(sub_ServiceBySite))
  for(x in 1:nrow(sub_ServiceBySite)){
    BirdIndex[x]<-mean(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(sub_ServiceBySite)[x]))
    BirdNumber[x]<-length(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(sub_ServiceBySite)[x]))
    IUCNIndex[x]<-mean(c(rep(2,subset(sub_SiteTrends$Inc,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x])),
                         rep(1,subset(sub_SiteTrends$Stable,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x])),
                         rep(0,subset(sub_SiteTrends$Dec,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]))))
    IUCNNumber[x]<-sum(subset(sub_SiteTrends$Inc,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]),
                       subset(sub_SiteTrends$Stable,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]),
                       subset(sub_SiteTrends$Dec,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]))
    if(x%%100==0) {print(x)}
    flush.console()
  }

  SiteData<-cbind(IUCNIndex,BirdIndex,IUCNNumber,BirdNumber,sub_NetESS)
  # add Bioregions
  
  SiteData<-data.frame(SiteData, Biogeog=as.factor(BIOREGION[match(rownames(SiteData),BIOREGION$SITECODE),2]))
  SiteData<-as.data.frame(SiteData[complete.cases(SiteData),])
  
  par(mfrow=c(1,2),mar=c(5,4,4,2))
  
  #plot(sub_NetESS,BirdIndex,xlab="sub_NetESS",ylab="Conservation index")
  #mtext("A",cex=2,at=3)
  #plot(sub_NetESS,IUCNIndex,xlab="sub_NetESS",ylab="IUCN trends index")
  #mtext("B",cex=2,at=3)
  
  SummaryBirdData<-matrix(ncol=4,nrow=13)
  colnames(SummaryBirdData)<-c("sub_NetESS","MeanBirdStatus","SE","N")
  for(x in 1:13){
    SummaryBirdData[x,1]<-x-9
    SummaryBirdData[x,2]<-mean(subset(BirdIndex,sub_NetESS==x-9),na.rm=TRUE)
    SummaryBirdData[x,3]<-sd(subset(BirdIndex,sub_NetESS==x-9),na.rm=TRUE)/sqrt(length(subset(BirdIndex,sub_NetESS==x-9)))
    SummaryBirdData[x,4]<-length(subset(BirdIndex,sub_NetESS==x-9))
  }
  plot(SummaryBirdData[,1],SummaryBirdData[,2],ylim=c(0,2),xlim=c(-9,6),ylab="Conservation index",xlab="Net ESS", main=paste(regions[a]))
  #arrows(SummaryBirdData[,1],SummaryBirdData[,2],SummaryBirdData[,1],SummaryBirdData[,2]+SummaryBirdData[,3],length=0)
  #arrows(SummaryBirdData[,1],SummaryBirdData[,2],SummaryBirdData[,1],SummaryBirdData[,2]-SummaryBirdData[,3],length=0)
  res<-cor.test(sub_NetESS,BirdIndex,method="spearman")
  mtext(paste("p-value",round(res$p.value,digits=4)),side=3)
  #abline(lm(SummaryBirdData[,2]~SummaryBirdData[,1],weights=SummaryBirdData[,4]))
  text(-7,1.75,"A",cex=2)
  points(jitter(sub_NetESS),BirdIndex,col="lightgrey",cex=0.5)
  points(SummaryBirdData[,1],SummaryBirdData[,2],pch=19)
  mod1<-lm(BirdIndex~sub_NetESS,data=SiteData)
  newx <- seq(min(sub_NetESS), max(sub_NetESS), length.out=100)
  preds <- predict(mod1, newdata = data.frame(sub_NetESS=newx), interval = 'confidence')
  #plot(sub_NetESS,BirdIndex, type = 'n',ylim=c(0.5,0.7))
  # add fill
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(0.1,0.1,0.1,0.2), border = NA)
  # model
  lines(newx,preds[,1],type="l")
  #abline(mod1)
  # intervals
  lines(newx, preds[ ,3], lty = 'dashed', col = 'red');lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
  
  # Net ESS versus IUCN status
  
  SummaryIUCNData<-matrix(ncol=4,nrow=13)
  colnames(SummaryIUCNData)<-c("sub_NetESSwt","MeanBirdStatus","SE","N")
  for(x in 1:13){
    SummaryIUCNData[x,1]<-x-9
    SummaryIUCNData[x,2]<-mean(subset(IUCNIndex,sub_NetESS==x-9),na.rm=TRUE)
    SummaryIUCNData[x,3]<-sd(subset(IUCNIndex,sub_NetESS==x-9),na.rm=TRUE)/sqrt(length(subset(IUCNIndex,sub_NetESS==x-9)))
    SummaryIUCNData[x,4]<-length(subset(IUCNIndex,sub_NetESS==x-9))
  }
  plot(SummaryIUCNData[,1],SummaryIUCNData[,2],ylim=c(0,2),xlim=c(-9,6),ylab="IUCN trends index",xlab="Net ESS")
  #arrows(SummaryIUCNData[,1],SummaryIUCNData[,2],SummaryIUCNData[,1],SummaryIUCNData[,2]+SummaryIUCNData[,3],length=0)
  #arrows(SummaryIUCNData[,1],SummaryIUCNData[,2],SummaryIUCNData[,1],SummaryIUCNData[,2]-SummaryIUCNData[,3],length=0)
  res<-cor.test(sub_NetESS,IUCNIndex,method="spearman")
  mtext(paste("p-value",round(res$p.value,digits=4)),side=3)
  #abline(lm(SummaryIUCNData[,2]~SummaryIUCNData[,1],weights=SummaryIUCNData[,4]))
  text(-7,1.8,"B",cex=2)
  points(jitter(sub_NetESS),IUCNIndex,col="lightgrey",cex=0.5)
  points(SummaryIUCNData[,1],SummaryIUCNData[,2],pch=19)
  mod2<-lm(IUCNIndex~sub_NetESS,data=SiteData)
  newx <- seq(min(sub_NetESS), max(sub_NetESS), length.out=100)
  preds <- predict(mod2, newdata = data.frame(sub_NetESS=newx), interval = 'confidence')
  #plot(sub_NetESS,IUCNIndex, type = 'n',ylim=c(0.5,0.7))
  # add fill
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(0.1,0.1,0.1,0.2), border = NA)
  # model
  lines(newx,preds[,1],type="l")
  # intervals
  lines(newx, preds[ ,3], lty = 'dashed', col = 'red');lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
  
  # Export for plotting in Origin
  NetESSvsConsIndexvsIUCNraw<-cbind(jitter(sub_NetESS),BirdIndex,IUCNIndex)
  NetESSvsConsIndexvsIUCNsummary<-cbind(SummaryBirdData,SummaryIUCNData)
  write.table(NetESSvsConsIndexvsIUCNraw,paste(c("NetESSvsConsIndexvsIUCNraw_",regions[a]),sep="",collapse=""))
  write.table(NetESSvsConsIndexvsIUCNsummary,paste(c("NetESSvsConsIndexvsIUCNsummary_",regions[a]),sep="",collapse=""))

  }





############################################################################
### 08.6c Site-centred conservation and IUCN indices - by habitats
###
### This script takes the site as the unit of replication and calculates
### the "conservation index" (the mean habitat quality for the community
### of bird species living at the site) and the "IUCN index" (the mean
### IUCN trend for the species living at the site)
### in addition, this script runs the analysis by Biogeographical regions
############################################################################

# Convert the A, B, C CONSERVATION code to a numeric score
SPECIES<-read.csv("SPECIES.csv")
ConvertToNumber<-data.frame(Letters=c("A","B","C"),Numbers=c(2,1,0))
SpeciesIndex<-ConvertToNumber[match(SPECIES$CONSERVATION,ConvertToNumber[,1]),2]
SPECIES$SpeciesIndex<-SpeciesIndex

# add Habitats
SPECIES$domhabitats<-DomHabData$DomHab[match(SPECIES$SITECODE,rownames(DomHabData))]
SiteTrends$domhabitats<-DomHabData$DomHab[match(SiteTrends$Site,rownames(DomHabData))]

domhabi<-unique(DomHabData$DomHab)

for(a in 1:length(domhabi)){
  sub_SPECIES<-subset(SPECIES,SPECIES$domhabitats==paste(domhabi[a]))
  sub_ServiceBySite<-subset(ServiceBySite,ServiceBySite$DomHab==paste(domhabi[a]))
  sub_SiteTrends<-subset(SiteTrends,SiteTrends$domhabitats==paste(domhabi[a]))
  sub_NetESS<-NetESS[match(rownames(sub_ServiceBySite),names(NetESS))]
  
  BIRDSPECIES<-subset(sub_SPECIES,sub_SPECIES$SPGROUP=="Birds" & sub_SPECIES$GLOBAL %in% c("A","B","C"))
  
  # Add bird scores to the sites
  IUCNIndex<-BirdIndex<-IUCNNumber<-BirdNumber<-numeric(length=nrow(sub_ServiceBySite))
  for(x in 1:nrow(sub_ServiceBySite)){
    BirdIndex[x]<-mean(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(sub_ServiceBySite)[x]))
    BirdNumber[x]<-length(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(sub_ServiceBySite)[x]))
    IUCNIndex[x]<-mean(c(rep(2,subset(sub_SiteTrends$Inc,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x])),
                         rep(1,subset(sub_SiteTrends$Stable,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x])),
                         rep(0,subset(sub_SiteTrends$Dec,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]))))
    IUCNNumber[x]<-sum(subset(sub_SiteTrends$Inc,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]),
                       subset(sub_SiteTrends$Stable,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]),
                       subset(sub_SiteTrends$Dec,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]))
    if(x%%100==0) {print(x)}
    flush.console()
  }
  
  SiteData<-cbind(IUCNIndex,BirdIndex,IUCNNumber,BirdNumber,sub_NetESS)
  # add Bioregions
  
  SiteData<-data.frame(SiteData, Biogeog=as.factor(BIOREGION[match(rownames(SiteData),BIOREGION$SITECODE),2]))
  SiteData<-as.data.frame(SiteData[complete.cases(SiteData),])
  
  par(mfrow=c(1,2),mar=c(5,4,4,2))
  
  #plot(sub_NetESS,BirdIndex,xlab="sub_NetESS",ylab="Conservation index")
  #mtext("A",cex=2,at=3)
  #plot(sub_NetESS,IUCNIndex,xlab="sub_NetESS",ylab="IUCN trends index")
  #mtext("B",cex=2,at=3)
  
  SummaryBirdData<-matrix(ncol=4,nrow=13)
  colnames(SummaryBirdData)<-c("sub_NetESS","MeanBirdStatus","SE","N")
  for(x in 1:13){
    SummaryBirdData[x,1]<-x-9
    SummaryBirdData[x,2]<-mean(subset(BirdIndex,sub_NetESS==x-9),na.rm=TRUE)
    SummaryBirdData[x,3]<-sd(subset(BirdIndex,sub_NetESS==x-9),na.rm=TRUE)/sqrt(length(subset(BirdIndex,sub_NetESS==x-9)))
    SummaryBirdData[x,4]<-length(subset(BirdIndex,sub_NetESS==x-9))
  }
  plot(SummaryBirdData[,1],SummaryBirdData[,2],ylim=c(0,2),xlim=c(-9,6),ylab="Conservation index",xlab="Net ESS", main=paste(domhabi[a]))
  #arrows(SummaryBirdData[,1],SummaryBirdData[,2],SummaryBirdData[,1],SummaryBirdData[,2]+SummaryBirdData[,3],length=0)
  #arrows(SummaryBirdData[,1],SummaryBirdData[,2],SummaryBirdData[,1],SummaryBirdData[,2]-SummaryBirdData[,3],length=0)
  res<-cor.test(sub_NetESS,BirdIndex,method="spearman")
  mtext(paste("p-value",round(res$p.value,digits=4)),side=3)
  mtext(paste("n= ",length(sub_ServiceBySite$Crop.POS)),side=1)
    #abline(lm(SummaryBirdData[,2]~SummaryBirdData[,1],weights=SummaryBirdData[,4]))
  text(-7,1.75,"A",cex=2)
  points(jitter(sub_NetESS),BirdIndex,col="lightgrey",cex=0.5)
  points(SummaryBirdData[,1],SummaryBirdData[,2],pch=19)
  mod1<-lm(BirdIndex~sub_NetESS,data=SiteData)
  newx <- seq(min(sub_NetESS), max(sub_NetESS), length.out=100)
  preds <- predict(mod1, newdata = data.frame(sub_NetESS=newx), interval = 'confidence')
  #plot(sub_NetESS,BirdIndex, type = 'n',ylim=c(0.5,0.7))
  # add fill
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(0.1,0.1,0.1,0.2), border = NA)
  # model
  lines(newx,preds[,1],type="l")
  #abline(mod1)
  # intervals
  lines(newx, preds[ ,3], lty = 'dashed', col = 'red');lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
  
  # Net ESS versus IUCN status
  
  SummaryIUCNData<-matrix(ncol=4,nrow=13)
  colnames(SummaryIUCNData)<-c("sub_NetESSwt","MeanBirdStatus","SE","N")
  for(x in 1:13){
    SummaryIUCNData[x,1]<-x-9
    SummaryIUCNData[x,2]<-mean(subset(IUCNIndex,sub_NetESS==x-9),na.rm=TRUE)
    SummaryIUCNData[x,3]<-sd(subset(IUCNIndex,sub_NetESS==x-9),na.rm=TRUE)/sqrt(length(subset(IUCNIndex,sub_NetESS==x-9)))
    SummaryIUCNData[x,4]<-length(subset(IUCNIndex,sub_NetESS==x-9))
  }
  plot(SummaryIUCNData[,1],SummaryIUCNData[,2],ylim=c(0,2),xlim=c(-9,6),ylab="IUCN trends index",xlab="Net ESS")
  #arrows(SummaryIUCNData[,1],SummaryIUCNData[,2],SummaryIUCNData[,1],SummaryIUCNData[,2]+SummaryIUCNData[,3],length=0)
  #arrows(SummaryIUCNData[,1],SummaryIUCNData[,2],SummaryIUCNData[,1],SummaryIUCNData[,2]-SummaryIUCNData[,3],length=0)
  res<-cor.test(sub_NetESS,IUCNIndex,method="spearman")
  mtext(paste("p-value",round(res$p.value,digits=4)),side=3)
  mtext(paste("n= ",length(sub_ServiceBySite$Crop.POS)),side=1)
  #abline(lm(SummaryIUCNData[,2]~SummaryIUCNData[,1],weights=SummaryIUCNData[,4]))
  text(-7,1.8,"B",cex=2)
  points(jitter(sub_NetESS),IUCNIndex,col="lightgrey",cex=0.5)
  points(SummaryIUCNData[,1],SummaryIUCNData[,2],pch=19)
  mod2<-lm(IUCNIndex~sub_NetESS,data=SiteData)
  newx <- seq(min(sub_NetESS), max(sub_NetESS), length.out=100)
  preds <- predict(mod2, newdata = data.frame(sub_NetESS=newx), interval = 'confidence')
  #plot(sub_NetESS,IUCNIndex, type = 'n',ylim=c(0.5,0.7))
  # add fill
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(0.1,0.1,0.1,0.2), border = NA)
  # model
  lines(newx,preds[,1],type="l")
  # intervals
  lines(newx, preds[ ,3], lty = 'dashed', col = 'red');lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
}





############################################################################
### 08.7 Bird conservation (from N2000) and IUCN status by mean net ESS
###
### This script is similar to 08.6 but take a species-centred approach, 
### calculating the "conservation index" (the mean habitat quality for
### each species across all N2000 sites on which it is found) and the 
### "Net ESS" (the mean net services for the sites on which a species
### is found)
############################################################################

# (i) the mean net ESS that each species experiences vs the mean “Bird index” score 
# (based on the conservation status of each site)
BirdSpeciesNames<-unique(BIRDSPECIES$SPECIESNAME)
BirdSpeciesIndex<-numeric(length=length(BirdSpeciesNames))
for(x in 1:length(BirdSpeciesNames)){
  BirdSites<-subset(N2000Species$SITECODE,N2000Species$GLOBAL%in%c("A","B","C") & N2000Species$SPECIESNAME==BirdSpeciesNames[x])
  BirdSpeciesIndex[x]<-mean(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SPECIESNAME==BirdSpeciesNames[x] & BIRDSPECIES$SITECODE%in%BirdSites),na.rm = TRUE)
}

BirdNetESS<-numeric(length=length(BirdSpeciesNames))
for(x in 1:length(BirdSpeciesNames)){
  BirdSites<-subset(N2000Species$SITECODE,N2000Species$GLOBAL%in%c("A","B","C") & N2000Species$SPECIESNAME==BirdSpeciesNames[x])
  BirdNetESS[x]<-mean(subset(ServiceBySite$NetESS,rownames(ServiceBySite)%in%BirdSites),na.rm=TRUE)
}


par(mfrow=c(1,1))
plot(BirdNetESS,BirdSpeciesIndex,xlab="Net ESS",ylab="Conservation index")
abline(lm(BirdSpeciesIndex~BirdNetESS))
cor.test(BirdNetESS,BirdSpeciesIndex,method="spearman")


# (ii) the IUCN classification for each species (increasing/decreasing/stable as 
# well as VU, EN, etc) vs mean net ESS
# First, take the data from above
BirdSpeciesOutput<-data.frame(BirdSpecies=BirdSpeciesNames,ConservationIndex=BirdSpeciesIndex,NetESS=BirdNetESS)

# Next, find the species from the IUCN table
SigSpeciesIUCN<-statusTable[na.omit(match(BirdSpeciesNames,statusTable[,1])),c(1,3,5)]

# Now match the net ESS, bird index, and IUCN ratings for these species
N2000ESSdata<-BirdSpeciesOutput[na.omit(match(SigSpeciesIUCN[,1],BirdSpeciesOutput[,1])),c(1:3)]

# Bind together
SigSpeciesIUCN2<-cbind(SigSpeciesIUCN,N2000ESSdata)

# Question 1: Do species that are increasing/decreasing have different N2000 conservation indices?
par(mfrow=c(2,2),mar=c(5,4,4,2)+0.1)
SigSpeciesIUCN3<-subset(SigSpeciesIUCN2,SigSpeciesIUCN2$Population!="Unknown")
SigSpeciesIUCN3$Population<-droplevels(SigSpeciesIUCN3$Population)
SigSpeciesIUCN3$Population<-factor(SigSpeciesIUCN3$Population,levels=c("Decreasing","Stable","Increasing"))
SigSpeciesIUCN3<-SigSpeciesIUCN3[complete.cases(SigSpeciesIUCN3),]
boxplot(SigSpeciesIUCN3$ConservationIndex~SigSpeciesIUCN3$Population, ylab="Conservation index")
mtext("A",cex=2,at=1)
kruskal.test(SigSpeciesIUCN3$ConservationIndex~SigSpeciesIUCN3$Population)

test1<-clm(Population ~ ConservationIndex, data = SigSpeciesIUCN3)
summary(test1)

# Question 2: Are species that are increasing/decreasing have different Net ESS on their N2000 sites
boxplot(SigSpeciesIUCN3$NetESS~SigSpeciesIUCN3$Population,ylab="Net ESS")
kruskal.test((SigSpeciesIUCN3$NetESS~SigSpeciesIUCN3$Population))
test2<-clm(Population ~ NetESS, data = SigSpeciesIUCN3)
summary(test2)
mtext("B",cex=2,at=1)

# Question 3: Are species that are increasing/decreasing have different Net ESS on their N2000 sites
SigSpeciesIUCN4<-subset(SigSpeciesIUCN2,SigSpeciesIUCN2$Status!="NE")
SigSpeciesIUCN4$Status<-droplevels(SigSpeciesIUCN4$Status)
SigSpeciesIUCN4$Status<-factor(SigSpeciesIUCN4$Status,levels=c("CR","EN","VU","NT","LC"))
SigSpeciesIUCN4<-SigSpeciesIUCN4[complete.cases(SigSpeciesIUCN4),]

boxplot(SigSpeciesIUCN4$ConservationIndex~SigSpeciesIUCN4$Status, ylab="Conservation index")
kruskal.test((SigSpeciesIUCN4$ConservationIndex~SigSpeciesIUCN4$Status))
table(SigSpeciesIUCN4$Status)
mtext("C",cex=2,at=1)
test3<-clm(Status ~ ConservationIndex, data = SigSpeciesIUCN4)
summary(test3)

# Question 4: Are species that are increasing/decreasing have different Net ESS on their N2000 sites
boxplot(SigSpeciesIUCN4$NetESS~SigSpeciesIUCN4$Status ,ylab="Net ESS")
kruskal.test((SigSpeciesIUCN4$NetESS~SigSpeciesIUCN4$Status))
test4<-clm(Status ~ NetESS, data = SigSpeciesIUCN4)
summary(test4)
mtext("D",cex=2,at=1)

###############################################################
### 08.8 Output services for ArcGIS plotting (Figure 1)
###############################################################

# Data for map
MikaOutput<-ServiceBySite
write.table(MikaOutput,"MikaOutput v3.txt")

###############################################################
### 08.9 Basic descriptive statistics
###############################################################

# find cumulative distribution of habitat percentage cover
par(mfrow=c(1,1))
hist(subset(HABITATCLASS$PERCENTAGECOVER,HABITATCLASS$PERCENTAGECOVER>=50))
plot(density(subset(HABITATCLASS$PERCENTAGECOVER,HABITATCLASS$PERCENTAGECOVER>=50)))

# Plots of NetESS and number of ESS
par(mar=c(4,5,3,2))
hist(NetESS,col="grey",main=NULL,ylab="Freqency",xlab="Net ESS")
hist(rowSums(AnyMention),col="grey",main=NULL,ylab="Freqency",xlab="Number of ESS",breaks=c(-1,0,1,2,3,4,5,6,7,8)+0.5)

# Plots of NetESS and number of ESS
par(mar=c(4,5,3,2))
hist(NetESS,col="grey",main=NULL,ylab="Freqency",xlab="Net ESS")
hist(rowSums(AnyMention),col="grey",main=NULL,ylab="Freqency",xlab="Number of ESS",breaks=c(0,1,2,3,4,5,6,7,8))

# Spearman correlations between NetESS and bird index for different biogeographical regions
cor.test(NetESS,BirdIndex,method="spearman")
cor.test(subset(NetESS,ServiceBySite$Biogeog=="Alpine"),subset(BirdIndex,ServiceBySite$Biogeog=="Alpine"),method="spearman")
cor.test(subset(NetESS,ServiceBySite$Biogeog=="Atlantic"),subset(BirdIndex,ServiceBySite$Biogeog=="Atlantic"),method="spearman")
cor.test(subset(NetESS,ServiceBySite$Biogeog=="Boreal"),subset(BirdIndex,ServiceBySite$Biogeog=="Boreal"),method="spearman")
cor.test(subset(NetESS,ServiceBySite$Biogeog=="Continental"),subset(BirdIndex,ServiceBySite$Biogeog=="Continental"),method="spearman")
cor.test(subset(NetESS,ServiceBySite$Biogeog=="Mediterranean"),subset(BirdIndex,ServiceBySite$Biogeog=="Mediterranean"),method="spearman")

###############################################################
### 08.10 Sense check for "both" category
###############################################################

table(ServiceBySite[,1],ServiceBySite[,10],ServiceBySite[,19])
table(ServiceBySite[,2],ServiceBySite[,11],ServiceBySite[,20])
table(ServiceBySite[,3],ServiceBySite[,12],ServiceBySite[,21])
table(ServiceBySite[,4],ServiceBySite[,13],ServiceBySite[,22])
table(ServiceBySite[,5],ServiceBySite[,14],ServiceBySite[,23])
table(ServiceBySite[,6],ServiceBySite[,15],ServiceBySite[,24])
table(ServiceBySite[,7],ServiceBySite[,16],ServiceBySite[,25])
table(ServiceBySite[,8],ServiceBySite[,17],ServiceBySite[,26])
table(ServiceBySite[,9],ServiceBySite[,18],ServiceBySite[,27])

subset(ServiceBySite,ServiceBySite[,9]==1 & ServiceBySite[,18]==1)

###############################################################
### 08.10 Cooccurrence analysis of ESS
###############################################################

par(mfrow=c(2,2))

ServiceBySiteNoZeroPOS<-ServiceBySite[rowSums(ServiceBySite[,c(1:9)])>0,c(1:9)]
ServiceBySiteNoZeroNEG<-ServiceBySite[rowSums(ServiceBySite[,c(10:18)])>0,c(10:18)]
ServiceBySiteNoZeroBOTH<-ServiceBySite[rowSums(ServiceBySite[,c(19:27)])>0,c(19:27)]

ServiceBySiteNoZeroANY<-matrix(nrow=nrow(ServiceBySite),ncol=9)
for(x in 1:nrow(ServiceBySite)){
  for(y in 1:9){
    if(sum(ServiceBySite[x,c(y,y+9,y+18)])>0) {ServiceBySiteNoZeroANY[x,y]<-1} else {ServiceBySiteNoZeroANY[x,y]<-0}
  }
}
colnames(ServiceBySiteNoZeroANY)<-ServiceList

# Cooccurrence of positive ESS
cooccur.ess.pos <- cooccur(mat=t(ServiceBySiteNoZeroPOS),type="spp_site",thresh=FALSE,spp_names=TRUE)
summary(cooccur.ess.pos)
plot(cooccur.ess.pos)

# Cooccurrence of negative ESS
cooccur.ess.neg <- cooccur(mat=t(ServiceBySiteNoZeroNEG),type="spp_site",thresh=FALSE,spp_names=TRUE)
summary(cooccur.ess.neg)
plot(cooccur.ess.neg)

# Cooccurrence of both positive and negative ESS
#cooccur.ess.both <- cooccur(mat=t(ServiceBySiteNoZeroBOTH),type="spp_site",thresh=FALSE,spp_names=TRUE)
#summary(cooccur.ess.both)
#plot(cooccur.ess.both)

# Cooccurrence of any ESS
cooccur.ess.any <- cooccur(mat=t(ServiceBySiteNoZeroANY),type="spp_site",thresh=FALSE,spp_names=TRUE)
summary(cooccur.ess.any)
plot(cooccur.ess.any)

# Output the pairwise relationships to Excel to plot as pivot tables
PosCoocData<-plot(cooccur.ess.pos)$data
NegCoocData<-plot(cooccur.ess.neg)$data
#plot(cooccur.ess.both)$data
AnyCoocData<-plot(cooccur.ess.any)$data

PosCoocData[,1]<-sub(".POS","",PosCoocData[,1])
PosCoocData[,2]<-sub(".POS","",PosCoocData[,2])

NegCoocData[,1]<-sub(".NEG","",NegCoocData[,1])
NegCoocData[,2]<-sub(".NEG","",NegCoocData[,2])

NegCoocData2<-rbind(NegCoocData[c(1,2,3)],NegCoocData[,c(2,1,3)])
PosCoocData2<-rbind(PosCoocData[c(1,2,3)],PosCoocData[,c(2,1,3)])
AnyCoocData2<-rbind(AnyCoocData[c(1,2,3)],AnyCoocData[,c(2,1,3)])

NegCoocData2[,4]<-paste(NegCoocData2[,1],NegCoocData2[,2],sep="")
PosCoocData2[,4]<-paste(PosCoocData2[,1],PosCoocData2[,2],sep="")
AnyCoocData2[,4]<-paste(AnyCoocData2[,1],AnyCoocData2[,2],sep="")

CombinedCoocData<-cbind(ESS1=AnyCoocData2[,1],ESS2=AnyCoocData2[,2],ESSCombined=AnyCoocData2[,4],
                        AnyCooc=AnyCoocData2[,3],
                        NegCooc=NegCoocData2[match(AnyCoocData2[c(1:36),4],NegCoocData2[,4]),3],
                        PosCooc=PosCoocData2[match(AnyCoocData2[c(1:36),4],PosCoocData2[,4]),3])

PosCoocTable<-matrix(ncol=9,nrow=9)
rownames(PosCoocTable)<-colnames(PosCoocTable)<-c("Aquaculture","Crop","Fibre","Fodder","Livestock","Water","Wild.food","Regulating","Recreation")
AnyCoocTable<-NegCoocTable<-PosCoocTable

for(x in 1:nrow(CombinedCoocData)){
    PosCoocTable[which(rownames(PosCoocTable)==CombinedCoocData[x,1]),
                 which(colnames(PosCoocTable)==CombinedCoocData[x,2])]<-CombinedCoocData[x,6]
    PosCoocTable[which(rownames(PosCoocTable)==CombinedCoocData[x,2]),
                 which(colnames(PosCoocTable)==CombinedCoocData[x,1])]<-CombinedCoocData[x,6]
    NegCoocTable[which(rownames(NegCoocTable)==CombinedCoocData[x,1]),
                 which(colnames(NegCoocTable)==CombinedCoocData[x,2])]<-CombinedCoocData[x,5]
    NegCoocTable[which(rownames(NegCoocTable)==CombinedCoocData[x,2]),
                 which(colnames(NegCoocTable)==CombinedCoocData[x,1])]<-CombinedCoocData[x,5]
    AnyCoocTable[which(rownames(AnyCoocTable)==CombinedCoocData[x,1]),
                 which(colnames(AnyCoocTable)==CombinedCoocData[x,2])]<-CombinedCoocData[x,4]
    AnyCoocTable[which(rownames(AnyCoocTable)==CombinedCoocData[x,2]),
                 which(colnames(AnyCoocTable)==CombinedCoocData[x,1])]<-CombinedCoocData[x,4]
}



###############################################################
### 08.11 Co-occurrence analysis excluding ESS mapped to multiple threats
###############################################################

GuyMappingReduced<-subset(GuyMappingData,GuyMappingData$n<2)

nrow(GuyMappingData)
nrow(GuyMappingReduced)

# Bind Guy's mapping to the threats table
N2000Impact<-cbind(N2000Impact,GuyMappingReduced[match(N2000Impact$IMPACTCODE,GuyMappingReduced$ACT_Code),])

# Create a list of services based on Guy's mapping 
ServiceList<-names(GuyMappingReduced[,c(3:11)])

ServiceBySite<-matrix(ncol=length(ServiceList)*4,nrow=length(unique(N2000Impact$SITECODE)))
rownames(ServiceBySite)<-unique(N2000Impact$SITECODE)
colnames(ServiceBySite)<-c(paste(ServiceList,"POS"),paste(ServiceList,"NEG"),paste(ServiceList,"BOTH"),paste(ServiceList,"NET"))

# Run through Guy's mapping and tally the positive (in the first 11 columns) and negative (second 11 columns)
# ESS associated with each site. Then calculate the difference between the two to give a net score for each
# ESS on each site
ptm <- proc.time()
for(x in 1:nrow(ServiceBySite)){
  # For each unique site ID code, extract a list of threats that have corresponding services
  SiteServices<-subset(N2000Impact,N2000Impact$SITECODE==rownames(ServiceBySite)[x])
  # For each service group (defined by Guy), sum the number of times it was positive or negative
  for(y in 1:length(ServiceList)){
    if(nrow(subset(SiteServices,SiteServices[,10+y]=="x" & SiteServices$IMPACT_TYPE=="P"))>0) {ServiceBySite[x,y] <- 1} else {ServiceBySite[x,y] <- 0}
    if(nrow(subset(SiteServices,SiteServices[,10+y]=="x" & SiteServices$IMPACT_TYPE=="N"))>0) {ServiceBySite[x,y+9] <- 1} else {ServiceBySite[x,y+9] <- 0}
    if("P"%in%subset(SiteServices,SiteServices[,10+y]=="x")$IMPACT_TYPE & "N"%in%subset(SiteServices,SiteServices[,8+y]=="x")$IMPACT_TYPE) {ServiceBySite[x,y+18]<-1;ServiceBySite[x,y] <- 0;ServiceBySite[x,y+9] <- 0} else {ServiceBySite[x,y+18]<-0}
    ServiceBySite[x,y+27]<-ServiceBySite[x,y]-ServiceBySite[x,y+9]
  }
  # Timer to track progress of loop
  if(x %% 100 == 0) {print(x/nrow(ServiceBySite));flush.console()}
}
proc.time() - ptm

ServiceBySiteNoZeroPOS<-ServiceBySite[rowSums(ServiceBySite[,c(1:9)])>0,c(1:9)]
ServiceBySiteNoZeroNEG<-ServiceBySite[rowSums(ServiceBySite[,c(10:18)])>0,c(10:18)]
ServiceBySiteNoZeroBOTH<-ServiceBySite[rowSums(ServiceBySite[,c(19:27)])>0,c(19:27)]

ServiceBySiteNoZeroANY<-matrix(nrow=nrow(ServiceBySite),ncol=9)
for(x in 1:nrow(ServiceBySite)){
  for(y in 1:9){
    if(sum(ServiceBySite[x,c(y,y+9,y+18)])>0) {ServiceBySiteNoZeroANY[x,y]<-1} else {ServiceBySiteNoZeroANY[x,y]<-0}
  }
}
colnames(ServiceBySiteNoZeroANY)<-ServiceList

# Cooccurrence of positive ESS
cooccur.ess.pos <- cooccur(mat=t(ServiceBySiteNoZeroPOS),type="spp_site",thresh=FALSE,spp_names=TRUE)
summary(cooccur.ess.pos)
plot(cooccur.ess.pos)

# Cooccurrence of negative ESS
cooccur.ess.neg <- cooccur(mat=t(ServiceBySiteNoZeroNEG),type="spp_site",thresh=FALSE,spp_names=TRUE)
summary(cooccur.ess.neg)
plot(cooccur.ess.neg)

# Cooccurrence of both positive and negative ESS
cooccur.ess.both <- cooccur(mat=t(ServiceBySiteNoZeroBOTH),type="spp_site",thresh=FALSE,spp_names=TRUE)
summary(cooccur.ess.both)
plot(cooccur.ess.both)

# Cooccurrence of any ESS
cooccur.ess.any <- cooccur(mat=t(ServiceBySiteNoZeroANY),type="spp_site",thresh=FALSE,spp_names=TRUE)
summary(cooccur.ess.any)
plot(cooccur.ess.any)

# Output the pairwise relationships to Excel to plot as pivot tables
plot(cooccur.ess.pos)$data
plot(cooccur.ess.neg)$data
plot(cooccur.ess.both)$data
plot(cooccur.ess.any)$data


############################################################################################################
### 08.12 Omnibus multinomial models of habitat quality and ESS
############################################################################################################

# Create dataset to run analyses
OmnibusData<-cbind(ServiceBySite,IUCNIndex,BirdIndex)
# Exclude rows with missing data
OmnibusData<-OmnibusData[complete.cases(OmnibusData),]

# First model is explaining IUCN index with positive and negative ESS
OmniModIUCN<-glm(IUCNIndex~Crop.POS+ Fodder.POS+ Fibre.POS +Livestock.POS+ Wild.food.POS+
                   Aquaculture.POS +Water.POS +Regulating.POS+ Recreation.POS+ Crop.NEG +Fodder.NEG+
                   Fibre.NEG +Livestock.NEG+ Wild.food.NEG+Aquaculture.NEG +Water.NEG+ Regulating.NEG
                 +Recreation.NEG,data=OmnibusData)

# Second model is explaining the Bird index with positive and negative ESS
OmniModBirdIndex<-glm(BirdIndex~Crop.POS+ Fodder.POS+ Fibre.POS +Livestock.POS+ Wild.food.POS+
                        Aquaculture.POS +Water.POS +Regulating.POS+ Recreation.POS+ Crop.NEG +Fodder.NEG+
                        Fibre.NEG +Livestock.NEG+ Wild.food.NEG+Aquaculture.NEG +Water.NEG+ Regulating.NEG
                      +Recreation.NEG,data=OmnibusData)

# Added in for the dredge() function to work properly
options(na.action = "na.fail")

# Dredge all possible subsets - the "extra" argument adds R2 and F-statistics
OmniDredgeIUCN<-dredge(OmniModIUCN,trace=2, m.lim = c(NA, 1), extra = c("R^2", F = function(x)
  summary(x)$fstatistic[[1]]))
# Display all models with delta-AICc<4
subset(OmniDredgeIUCN, delta < 4)
# Get top model
summary(get.models(OmniDredgeIUCN, 1)[[1]])
# Model averaging of the parameters of models comprising the model set that make up 
# 95% of the confidence for the set (i.e. there is a 95% chance that the top model is 
# in there somewhere)
OmniDredgeIUCNModAvg<-model.avg(OmniDredgeIUCN, subset = cumsum(weight) <= .95)
summary(OmniDredgeIUCNModAvg) # can't calculate R2 for the averaged model


# Dredge all possible subsets - the "extra" argument adds R2 and F-statistics
OmniDredgeBirdIndex<-dredge(OmniModBirdIndex,trace=2)
# Display all models with delta-AICc<4
subset(OmniDredgeBirdIndex, delta < 4)
# Model averaging of the parameters of models comprising the model set that make up 
# 95% of the confidence for the set (i.e. there is a 95% chance that the top model is 
# in there somewhere)
OmniDredgeBirdIndexModAvg<-model.avg(OmniDredgeBirdIndex, subset = cumsum(weight) <= .95)
summary(OmniDredgeBirdIndexModAvg) # can't calculate R2 for the averaged model


############################################################################################################
### 08.13 Subgroup analysis of Net ESS, Bird index and IUCN
############################################################################################################

SubGroupData<-cbind(BirdIndex,NetESS,IUCNIndex)

SubBiogeog<-as.vector(BIOREGION$BIOGEFRAPHICREG[match(as.factor(rownames(SubGroupData)),BIOREGION$SITECODE,)])

SubGroupData2<-as.data.frame(cbind(SubGroupData,SubBiogeog))

SubGroupData2<-SubGroupData2[complete.cases(SubGroupData2),]

SubGroupData2<-transform(SubGroupData2,
          BirdIndex=as.numeric(BirdIndex),
          NewESS=as.numeric(NetESS),
          IUCNIndex=as.numeric(IUCNIndex),
          SubBiogeog=as.factor(SubBiogeog))

SubGroupData2<-subset(SubGroupData2,SubGroupData2$SubBiogeog%in%c("Alpine","Atlantic","Boreal","Continental","Mediterranean"))

modA<-lm(BirdIndex~as.numeric(NetESS),data=SubGroupData2)
modB<-lm(BirdIndex~as.numeric(NetESS)+SubBiogeog,data=SubGroupData2)
modC<-lm(BirdIndex~as.numeric(NetESS)*SubBiogeog,data=SubGroupData2)
modD<-lm(BirdIndex~SubBiogeog,data=SubGroupData2)

summary(modA) # Net ESS only
summary(modB) # Net ESS with additive biogeog
summary(modC) # Net ESS with interaction term
summary(modD) # Biogeog only

anova(modA,modB,modC,modD)


# TO BE COMPLETED [CH, MB]

# Data source for bird habitat types: http://www.ebcc.info/wpimages/other/SpeciesClassification2015.xls

############################################################################################################
### 08.14 Weighted means of Net ESS, Bird index and IUCN
###
### This analysis attempts to create a weighted mean of the ESS to better explain 
### variation in the Bird Index.
############################################################################################################

PosNegESS<-ServiceBySite[,c(28:36)]
ESSPresence<-matrix(nrow=nrow(ServiceBySite),ncol=9)
for(x in 1:nrow(ServiceBySite)){
  for(y in 1:9){
    if(sum(ServiceBySite[x,c(y,y+9,y+18)])>0) {ESSPresence[x,y]<-1} else {ESSPresence[x,y]<-0}
  }
}
colnames(PosNegESS)<-ServiceList

WtMeanData<-cbind(BirdIndex,NetESS,IUCNIndex,PosNegESS)
WtMeanData<-as.data.frame(WtMeanData[complete.cases(WtMeanData),])

WtMeanMod<-lm(BirdIndex~.^2,data=WtMeanData) # explains 4.4% of variance
SimpleMeanMod<-lm(BirdIndex~NetESS,data=WtMeanData) # explains 1.6% of variance

############################################################################################################
### 08.14 NetESS variation between biogeographical regions
############################################################################################################

kruskal.test(as.numeric(NetESS)~-1+SubBiogeog,data=SubGroupData2)
TukeyHSD(MODEL)
boxplot(as.numeric(as.vector(NetESS))~SubBiogeog,data=SubGroupData2)

############################################################################################################
### 08.14 GIS data source
############################################################################################################

#http://www.eea.europa.eu/ds_resolveuid/0cb5db55a14548e28344f867bc2d25c9

############################################################################################################
### 08.14 Examples of positive and negative ESS
############################################################################################################


# First, subset N2000Impact by intensity and occurrence
N2000Impact<-subset(N2000Impact, N2000Impact$INTENSITY=="HIGH")
N2000Impact<-subset(N2000Impact,N2000Impact$OCCURRENCE=="IN" | N2000Impact$OCCURRENCE=="BOTH")

# Assign site type
# Load data
N2000Sites <- read.csv("NATURA2000SITES.csv")
N2000Sites[,4] <- as.character(N2000Sites[,4])
N2000Impact$SITE_TYPE <- NA

# Add site type to N2000Impact (running time 117 seconds)
for(x in 1:nrow(N2000Impact)){
  N2000Impact$SITE_TYPE[x] <- N2000Sites[match(N2000Impact$SITECODE[x],N2000Sites$SITECODE),4]
}

# Now subset to exclude SITE_TYPE="B"
N2000Impact<-subset(N2000Impact,N2000Impact$SITE_TYPE %in% c("A"))

# Bind Guy's mapping to the threats table
N2000Impact<-cbind(N2000Impact,GuyMappingData[match(N2000Impact$IMPACTCODE,GuyMappingData$ACT_Code),])

# Create a list of services based on Guy's mapping 
ServiceList<-names(GuyMappingData[,c(3:11)])

ServiceBySite<-matrix(ncol=length(ServiceList)*4,nrow=length(unique(N2000Impact$SITECODE)))
rownames(ServiceBySite)<-unique(N2000Impact$SITECODE)
colnames(ServiceBySite)<-c(paste(ServiceList,"POS"),paste(ServiceList,"NEG"),paste(ServiceList,"BOTH"),paste(ServiceList,"NET"))

# Run through mapping and tally the positive (in the first 9 columns) and negative (second 9 columns)
# ESS associated with each site. Then calculate the difference between the two to give a net score for each
# ESS on each site
ptm <- proc.time()
for(x in 1:nrow(ServiceBySite)){
  # For each unique site ID code, extract a list of threats that have corresponding services
  SiteServices<-subset(N2000Impact,N2000Impact$SITECODE==rownames(ServiceBySite)[x])
  # For each service group (defined by Guy), sum the number of times it was positive or negative
  for(y in 1:length(ServiceList)){
    if(nrow(subset(SiteServices,SiteServices[,10+y] %in% c("c","x") & SiteServices$IMPACT_TYPE=="P"))>0) {ServiceBySite[x,y] <- 1} else {ServiceBySite[x,y] <- 0}
    if(nrow(subset(SiteServices,SiteServices[,10+y] %in% c("c","x") & SiteServices$IMPACT_TYPE=="N"))>0) {ServiceBySite[x,y+9] <- 1} else {ServiceBySite[x,y+9] <- 0}
    if("P"%in%subset(SiteServices,SiteServices[,10+y] %in% c("c","x"))$IMPACT_TYPE & "N"%in%subset(SiteServices,SiteServices[,10+y] %in% c("c","x"))$IMPACT_TYPE) {ServiceBySite[x,y+18]<-1;ServiceBySite[x,y] <- 0;ServiceBySite[x,y+9] <- 0} else {ServiceBySite[x,y+18]<-0}
    ServiceBySite[x,y+27]<-ServiceBySite[x,y]-ServiceBySite[x,y+9]
  }
  # Timer to track progress of loop
  if(x %% 100 == 0) {print(x/nrow(ServiceBySite));flush.console()}
}
proc.time() - ptm

# Final "net" value for all ESS across each site
NetESS<-rowSums(ServiceBySite[,c(28:36)])
NetESSwt<-rowSums(ServiceBySite[,c(28:34)])/7+ServiceBySite[,35]+ServiceBySite[,36]
ServiceBySite<-cbind(ServiceBySite,NetESS,NetESSwt)

# Add Bioregion (note that sometimes the BIOREGION$SITECODE field is called "i..SITECODE" which
# causes problems with matching the datasets - it may depend on operating system)
BIOREGION<-read.csv("BIOREGION.csv")
# Change column name from "ï..SITECODE" to "SITECODE"
colnames(BIOREGION)[1] <- "SITECODE"
ServiceBySite<-data.frame(ServiceBySite,Biogeog=as.factor(BIOREGION[match(rownames(ServiceBySite),BIOREGION$SITECODE),2]))

test1a<-subset(ServiceBySite,ServiceBySite[,1]==1 & 
               ServiceBySite[,10]!=1 & 
               ServiceBySite[,19]!=1 &
               substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test2a<-subset(ServiceBySite,ServiceBySite[,2]==1 & 
               ServiceBySite[,11]!=1 & 
               ServiceBySite[,20]!=1 &
               substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test3a<-subset(ServiceBySite,ServiceBySite[,3]==1 & 
               ServiceBySite[,12]!=1 & 
               ServiceBySite[,21]!=1 &
               substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test4a<-subset(ServiceBySite,ServiceBySite[,4]==1 & 
               ServiceBySite[,13]!=1 & 
               ServiceBySite[,22]!=1 &
               substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test5a<-subset(ServiceBySite,ServiceBySite[,5]==1 & 
               ServiceBySite[,14]!=1 & 
               ServiceBySite[,23]!=1 &
               substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test6a<-subset(ServiceBySite,ServiceBySite[,6]==1 & 
               ServiceBySite[,15]!=1 & 
               ServiceBySite[,24]!=1 &
               substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test7a<-subset(ServiceBySite,ServiceBySite[,7]==1 & 
               ServiceBySite[,16]!=1 & 
               ServiceBySite[,25]!=1 &
               substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test8a<-subset(ServiceBySite,ServiceBySite[,8]==1 & 
              ServiceBySite[,17]!=1 & 
               ServiceBySite[,26]!=1 &
               substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test9a<-subset(ServiceBySite,ServiceBySite[,9]==1 & 
               ServiceBySite[,18]!=1 & 
               ServiceBySite[,27]!=1 &
               substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))

test1b<-subset(ServiceBySite,ServiceBySite[,1]!=1 & 
                ServiceBySite[,10]==1 & 
                ServiceBySite[,19]!=1 &
                substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test2b<-subset(ServiceBySite,ServiceBySite[,2]!=1 & 
                ServiceBySite[,11]==1 & 
                ServiceBySite[,20]!=1 &
                substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test3b<-subset(ServiceBySite,ServiceBySite[,3]!=1 & 
                ServiceBySite[,12]==1 & 
                ServiceBySite[,21]!=1 &
                substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test4b<-subset(ServiceBySite,ServiceBySite[,4]!=1 & 
                ServiceBySite[,13]==1 & 
                ServiceBySite[,22]!=1 &
                substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test5b<-subset(ServiceBySite,ServiceBySite[,5]!=1 & 
                ServiceBySite[,14]==1 & 
                ServiceBySite[,23]!=1 &
                substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test6b<-subset(ServiceBySite,ServiceBySite[,6]!=1 & 
                ServiceBySite[,15]==1 & 
                ServiceBySite[,24]!=1 &
                substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test7b<-subset(ServiceBySite,ServiceBySite[,7]!=1 & 
                ServiceBySite[,16]==1 & 
                ServiceBySite[,25]!=1 &
                substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test8b<-subset(ServiceBySite,ServiceBySite[,8]!=1 & 
                ServiceBySite[,17]==1 & 
                ServiceBySite[,26]!=1 &
                substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))
test9b<-subset(ServiceBySite,ServiceBySite[,9]!=1 & 
                ServiceBySite[,18]==1 & 
                ServiceBySite[,27]!=1 &
                substr(rownames(ServiceBySite),1,2)%in%c("UK","PT","DE","ES","IT","CZ","AT"))

test1a$SiteName<-rownames(test1a)
test2a$SiteName<-rownames(test2a)
test3a$SiteName<-rownames(test3a)
test4a$SiteName<-rownames(test4a)
test5a$SiteName<-rownames(test5a)
test6a$SiteName<-rownames(test6a)
test7a$SiteName<-rownames(test7a)
test8a$SiteName<-rownames(test8a)
test9a$SiteName<-rownames(test9a)

test1b$SiteName<-rownames(test1b)
test2b$SiteName<-rownames(test2b)
test3b$SiteName<-rownames(test3b)
test4b$SiteName<-rownames(test4b)
test5b$SiteName<-rownames(test5b)
test6b$SiteName<-rownames(test6b)
test7b$SiteName<-rownames(test7b)
test8b$SiteName<-rownames(test8b)
test9b$SiteName<-rownames(test9b)

AllHighESS<-rbind(test1a,test2a,test3a,test4a,test5a,test6a,test7a,test8a,test9a,
      test1b,test2b,test3b,test4b,test5b,test6b,test7b,test8b,test9b)

AllHighESS$ESSType<-c(rep(paste(ServiceList[1],"positive"),nrow(test1a)),
  rep(paste(ServiceList[2],"positive"),nrow(test2a)),
  rep(paste(ServiceList[3],"positive"),nrow(test3a)),
  rep(paste(ServiceList[4],"positive"),nrow(test4a)),
  rep(paste(ServiceList[5],"positive"),nrow(test5a)),
  rep(paste(ServiceList[6],"positive"),nrow(test6a)),
  rep(paste(ServiceList[7],"positive"),nrow(test7a)),
  rep(paste(ServiceList[8],"positive"),nrow(test8a)),
  rep(paste(ServiceList[9],"positive"),nrow(test9a)),
  rep(paste(ServiceList[1],"negative"),nrow(test1b)),
  rep(paste(ServiceList[2],"negative"),nrow(test2b)),
  rep(paste(ServiceList[3],"negative"),nrow(test3b)),
  rep(paste(ServiceList[4],"negative"),nrow(test4b)),
  rep(paste(ServiceList[5],"negative"),nrow(test5b)),
  rep(paste(ServiceList[6],"negative"),nrow(test6b)),
  rep(paste(ServiceList[7],"negative"),nrow(test7b)),
  rep(paste(ServiceList[8],"negative"),nrow(test8b)),
  rep(paste(ServiceList[9],"negative"),nrow(test9b)))  

AllHighESS$SITECODE[nchar(AllHighESS$SITECODE)==10]<-substr(AllHighESS$SITECODE,1,9)
rownames(AllHighESS)<-NULL
# Find management plan URLs where they exist
# Load MANAGEMENT CSV into workspace
management<-read.csv("MANAGEMENT.csv")

# Create character vectors for site ID and PDF link
management$SITECODE<-as.character(management$ï..SITECODE)
management$MANAG_PLAN_URL<-as.character(management$MANAG_PLAN_URL)

AllHighESS$PlanURL<-management$MANAG_PLAN_URL[match(AllHighESS$SiteName,management$SITECODE)]
AllHighESS$PlanExist<-management$MANAG_STATUS[match(AllHighESS$SiteName,management$SITECODE)]
AllHighESS$SiteTitle<-N2000Sites$SITENAME[match(AllHighESS$SiteName,N2000Sites$SITECODE)]

OutputHighESS<-AllHighESS[,c(40:44)]
OutputHighESS[c(1:100),]

subset(table(OutputHighESS$SiteName),table(OutputHighESS$SiteName)>1)

subset(OutputHighESS,OutputHighESS$SiteName=="ES0000483")


# Remove duplicate rows
OutputHighESS<-OutputHighESS[!duplicated(OutputHighESS),]

write.table(OutputHighESS,"Guy High ESS table.txt")

table(OutputHighESS$ESSType,OutputHighESS$PlanExist)

############################################################################################################
### 08.14 Code Graveyard
############################################################################################################

# Cooccurrence plots using networks
NegPos<-character()
Sig<-numeric(length=nrow(cooccur.ess.pos$results))
for(x in 1:nrow(cooccur.ess.pos$results)){
  if(cooccur.ess.pos$results[x,5]-cooccur.ess.pos$results[x,7]>0) {NegPos[x]<-"red"} else {NegPos[x]<-"blue"}
  if(NegPos[x]=="red"){
    if(cooccur.ess.pos$results[x,9]<0.05) {Sig[x]<-1} else {Sig[x]<-2}
  }
  if(NegPos[x]=="blue"){
    if(cooccur.ess.pos$results[x,8]<0.05) {Sig[x]<-1} else {Sig[x]<-2}
  } 
}
CooccurPosEdge<-data.frame(ESS1=cooccur.ess.pos$results[,10],
                           ESS2=cooccur.ess.pos$results[,11],
                           NegPos=NegPos,
                           Sig=Sig,
                           weight=cooccur.ess.pos$results[,5]/cooccur.ess.pos$results[,7])
graph <- graph.data.frame(CooccurPosEdge, directed = FALSE)
Coords<-layout.circle(make_ring(9))
E(graph)$width <- (CooccurPosEdge$weight*3)+1 # Set edge width based on weight
V(graph)$size <- colSums(ServiceBySiteNoZeroPOS)/10 # Set node size based on frequency of service
plot(graph,layout=Coords,edge.lty=Sig,edge.color=NegPos)
