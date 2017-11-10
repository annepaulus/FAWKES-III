############################################################################
### Purpose of this skript module 04 is to:
###
### Create barplots of proportions of impacts in SPA and SCI sites on
### ecosystem services
### 
### 04.1 N2000Impact data frame
### 04.2 Associations of services
### 04.3 Add services to N2000Impact3 data frame
### 04.4 Add site type to N2000Impact3 data frame
### 04.5 Bar plots
### 04.6 Network plots
### 04.7 Code cemetery
###
### Authors: CH, AC, MB, AK
###
### Run skript modules 00, 01 and 02 before
############################################################################

############################################################################
### To do:
### 
###  [AK] remove duplicate lines from N2000Impact
###  [AK] check MappingData[134,"Plant.based.energy.resources"] in original
###       MappingData data frame (I defined it NA in 04.2)
### 
###
### 
############################################################################

############################################################################
### 04.1 N2000Impact data frame
###
### Create N2000Impact data frame as a base for further data analysis
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


############################################################################
### 04.2 Associations of services
###
### Create the column ThreatWithServices in the N2000Impact data frame
### which converts threats to harmonised threats that all have services
### associated (the "relation" column) in the Google Sheet.
### Aim: Get rid of threats we do not consider
############################################################################


ThreatWithService<-character(length=nrow(N2000Impact))
# if N2000Impact$IMPACTCODE[x] matches MappingData$ACT_Code in line y, then define ThreatWithService[x] as MappingData$relation[y]
for (x in 1:nrow(N2000Impact)){
  if(!is.na(match(N2000Impact$IMPACTCODE[x],MappingData$ACT_Code))) 
  {ThreatWithService[x]<-as.character(MappingData$relation[which(MappingData$ACT_Code==N2000Impact$IMPACTCODE[x])])}
  else {ThreatWithService[x]<-NA}
}
# Add that new harmonised threat to the N2000Impact table
N2000Impact2<-cbind(N2000Impact,ThreatWithService)

# Remove rows with NA from N2000Impact2 data frame
N2000Impact2 <- na.omit(N2000Impact2)

# Change data frame entry from "Review official documents..." to NA (only crosses "x" or "NA" accepted)
MappingData[134,"Plant.based.energy.resources"] <- NA


############################################################################
### 04.3 Add services to N2000Impact3 data frame
###
### Match N2000Impact2$ThreatWithService (and not N2000Impact2$IMPACTCODE)
### with MappingData$ACT_Code otherwise there will be some Impacts that are
### not related to services
############################################################################


# Add service name to N2000Impact2 data frame
# Running time: 213.87 sec

#ptm <- proc.time()
d <- as.data.frame(matrix(,nrow=nrow(N2000Impact2), ncol=4))
colnames(d) <- c("SERVICE 1", "SERVICE 2", "SERVICE 3", "SERVICE 4")
for(x in 1:nrow(N2000Impact2)){
  i = 1
  for(y in 11:29){
    if(!is.na(MappingData[match(N2000Impact2$ThreatWithService[x],MappingData$ACT_Code),y])){
      #      d[x,i] <- y
      d[x,i] <- colnames(MappingData)[y]
      i = i+1
    }
  }
}
#proc.time() - ptm
#length(which(!is.na(c[,1]))) returns 99695, i.e. no NA rows

# Add Services to N2000Impact2 data frame and create additional rows if there is more than one service related to
# the threat, i.e. one row per service

# Convert factors to characters in N2000Impact2
for(i in 1:8){
  N2000Impact2[,i] <- as.character(N2000Impact2[,i])
}
# test with str(N2000Impact2)

# Create new data frame N2000Impact3 by adding services to N2000Impact2. The original code took
# ca. 5 hrs to run due to geometrically-increasing computation times associated with elongating
# the dataframe. It has now been broken up into 10 separate loops and runs in ca. 7 minutes :-)
# Create vector of increments
from_d<-seq(1,nrow(d),by=round(nrow(d)/10))[-c(11)]
to_d<-seq(0,nrow(d),by=round(nrow(d)/10))[-1]
to_d[10]<-nrow(d)

# Time the full loop
ptm_full <- proc.time()

#  1/10 of the loop -> "N2000Impact3a"
N2000Impact3a <- as.data.frame(matrix(,ncol=9))
colnames(N2000Impact3a) <- c(colnames(N2000Impact2), "SERVICE")
i = 1
for(x in from_d[1]:to_d[1]){
  for(y in 1:ncol(d)){
    if(!is.na(d[x,y])){
      N2000Impact3a[i,1:8] <- N2000Impact2[x,]
      N2000Impact3a[i,9] <- d[x,y]
      i = i+1
    }
  }
}

#  2/10 of the loop -> "N2000Impact3b"
N2000Impact3b <- as.data.frame(matrix(,ncol=9))
colnames(N2000Impact3b) <- c(colnames(N2000Impact2), "SERVICE")
i = 1
for(x in from_d[2]:to_d[2]){
  for(y in 1:ncol(d)){
    if(!is.na(d[x,y])){
      N2000Impact3b[i,1:8] <- N2000Impact2[x,]
      N2000Impact3b[i,9] <- d[x,y]
      i = i+1
    }
  }
}

#  3/10 of the loop -> "N2000Impact3c"
N2000Impact3c <- as.data.frame(matrix(,ncol=9))
colnames(N2000Impact3c) <- c(colnames(N2000Impact2), "SERVICE")
i = 1
for(x in from_d[3]:to_d[3]){
  for(y in 1:ncol(d)){
    if(!is.na(d[x,y])){
      N2000Impact3c[i,1:8] <- N2000Impact2[x,]
      N2000Impact3c[i,9] <- d[x,y]
      i = i+1
    }
  }
}

#  4/10 of the loop -> "N2000Impact3d"
N2000Impact3d <- as.data.frame(matrix(,ncol=9))
colnames(N2000Impact3d) <- c(colnames(N2000Impact2), "SERVICE")
i = 1
for(x in from_d[4]:to_d[4]){
  for(y in 1:ncol(d)){
    if(!is.na(d[x,y])){
      N2000Impact3d[i,1:8] <- N2000Impact2[x,]
      N2000Impact3d[i,9] <- d[x,y]
      i = i+1
    }
  }
}

# 5/10 of the loop -> "N2000Impact3e"
N2000Impact3e <- as.data.frame(matrix(,ncol=9))
colnames(N2000Impact3e) <- c(colnames(N2000Impact2), "SERVICE")
i = 1
for(x in from_d[5]:to_d[5]){
  for(y in 1:ncol(d)){
    if(!is.na(d[x,y])){
      N2000Impact3e[i,1:8] <- N2000Impact2[x,]
      N2000Impact3e[i,9] <- d[x,y]
      i = i+1
    }
  }
}

# 6/10 of the loop -> "N2000Impact3f"
N2000Impact3f <- as.data.frame(matrix(,ncol=9))
colnames(N2000Impact3f) <- c(colnames(N2000Impact2), "SERVICE")
i = 1
for(x in from_d[6]:to_d[6]){
  for(y in 1:ncol(d)){
    if(!is.na(d[x,y])){
      N2000Impact3f[i,1:8] <- N2000Impact2[x,]
      N2000Impact3f[i,9] <- d[x,y]
      i = i+1
    }
  }
}

# 7/10 of the loop -> "N2000Impact3g"
N2000Impact3g <- as.data.frame(matrix(,ncol=9))
colnames(N2000Impact3g) <- c(colnames(N2000Impact2), "SERVICE")
i = 1
for(x in from_d[7]:to_d[7]){
  for(y in 1:ncol(d)){
    if(!is.na(d[x,y])){
      N2000Impact3g[i,1:8] <- N2000Impact2[x,]
      N2000Impact3g[i,9] <- d[x,y]
      i = i+1
    }
  }
}

# 8/10 of the loop -> "N2000Impact3h"
N2000Impact3h <- as.data.frame(matrix(,ncol=9))
colnames(N2000Impact3h) <- c(colnames(N2000Impact2), "SERVICE")
i = 1
for(x in from_d[8]:to_d[8]){
  for(y in 1:ncol(d)){
    if(!is.na(d[x,y])){
      N2000Impact3h[i,1:8] <- N2000Impact2[x,]
      N2000Impact3h[i,9] <- d[x,y]
      i = i+1
    }
  }
}

# 9/10 of the loop -> "N2000Impact3i"
N2000Impact3i <- as.data.frame(matrix(,ncol=9))
colnames(N2000Impact3i) <- c(colnames(N2000Impact2), "SERVICE")
i = 1
for(x in from_d[9]:to_d[9]){
  for(y in 1:ncol(d)){
    if(!is.na(d[x,y])){
      N2000Impact3i[i,1:8] <- N2000Impact2[x,]
      N2000Impact3i[i,9] <- d[x,y]
      i = i+1
    }
  }
}

# 10/10 of the loop -> "N2000Impact3j"
N2000Impact3j <- as.data.frame(matrix(,ncol=9))
colnames(N2000Impact3j) <- c(colnames(N2000Impact2), "SERVICE")
i = 1
for(x in from_d[10]:to_d[10]){
  for(y in 1:ncol(d)){
    if(!is.na(d[x,y])){
      N2000Impact3j[i,1:8] <- N2000Impact2[x,]
      N2000Impact3j[i,9] <- d[x,y]
      i = i+1
    }
  }
}

proc.time() - ptm_full



# Bind the four quarters together
N2000Impact3<-rbind(N2000Impact3a,N2000Impact3b,N2000Impact3c,N2000Impact3d,N2000Impact3e,N2000Impact3f,N2000Impact3g,N2000Impact3h,N2000Impact3i,N2000Impact3j)


#write.csv(N2000Impact3, file = "N2000Impact3.csv")


############################################################################
### 04.4 Add site type to N2000Impact3 data frame
###
### A - SPA, B - SCI, C - both
### Source: http://eur-lex.europa.eu/legal-content/EN/TXT/PDF/
### ?uri=CELEX:32011D0484&from=EN, PDF page 15
############################################################################


# Load data
N2000Sites <- read.csv("NATURA2000SITES.csv")
N2000Sites[,4] <- as.character(N2000Sites[,4])
N2000Impact3$SITE_TYPE <- NA

# Add site type to N2000Impact3
# Running time ca. 5 min
#ptm2 <- proc.time()
for(x in 1:nrow(N2000Impact3)){
  N2000Impact3$SITE_TYPE[x] <- N2000Sites[match(N2000Impact3$SITECODE[x],N2000Sites$SITECODE),4]
}
#proc.time() - ptm2


############################################################################
### 04.5 Bar plots
###
### Creates bar plots of proportions of impacts in SPA and SCI sites on
### ecosystem services
############################################################################


### SPA (Site type A+C) ###

# Sub-data-frame: select all rows from N2000Impact3 that are SPA
N2000Impact3_SPA <- subset(N2000Impact3,N2000Impact3$SITE_TYPE=="A"|N2000Impact3$SITE_TYPE=="C")
# Sub-data-frame: select all rows from SPA that are positive (and low/medium/high)
N2000Impact3_SPA_pos <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="P")
N2000Impact3_SPA_pos_low <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="P"&N2000Impact3_SPA$INTENSITY=="LOW")
N2000Impact3_SPA_pos_medium <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="P"&N2000Impact3_SPA$INTENSITY=="MEDIUM")
N2000Impact3_SPA_pos_high <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="P"&N2000Impact3_SPA$INTENSITY=="HIGH")
# Sub-data-frame: select all rows from SPA that are negative (and low/medium/high)
N2000Impact3_SPA_neg <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="N")
N2000Impact3_SPA_neg_low <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="N"&N2000Impact3_SPA$INTENSITY=="LOW")
N2000Impact3_SPA_neg_medium <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="N"&N2000Impact3_SPA$INTENSITY=="MEDIUM")
N2000Impact3_SPA_neg_high <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="N"&N2000Impact3_SPA$INTENSITY=="HIGH")
# Sub-data-frame: select all rows from SPA that are positive (and in/out/both)
N2000Impact3_SPA_pos_in <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="P"&N2000Impact3_SPA$OCCURRENCE=="IN")
N2000Impact3_SPA_pos_out <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="P"&N2000Impact3_SPA$OCCURRENCE=="OUT")
N2000Impact3_SPA_pos_both <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="P"&N2000Impact3_SPA$OCCURRENCE=="BOTH")
# Sub-data-frame: select all rows from SPA that are negative (and in/out/both)
N2000Impact3_SPA_neg_in <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="N"&N2000Impact3_SPA$OCCURRENCE=="IN")
N2000Impact3_SPA_neg_out <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="N"&N2000Impact3_SPA$OCCURRENCE=="OUT")
N2000Impact3_SPA_neg_both <- subset(N2000Impact3_SPA,N2000Impact3_SPA$IMPACT_TYPE=="N"&N2000Impact3_SPA$OCCURRENCE=="BOTH")

# Positive and negative proportion
POS_NEG <- as.data.frame(matrix(nrow=2, ncol=19))
colnames(POS_NEG) <- colnames(MappingData)[11:29]
rownames(POS_NEG) <- c("POS","NEG")

for(x in 1:ncol(POS_NEG)){
  POS_NEG[1,x] <- length(which(N2000Impact3_SPA_pos$SERVICE==colnames(POS_NEG)[x]))
  POS_NEG[2,x] <- length(which(N2000Impact3_SPA_neg$SERVICE==colnames(POS_NEG)[x]))
}
par(oma=c(1,25,1,1))
barplot(as.matrix(POS_NEG), horiz=TRUE, las=2, legend=T, main="SPA - POS_NEG")

# Low, medium, high - positive
LMH_POS <- as.data.frame(matrix(nrow=3, ncol=19))
colnames(LMH_POS) <- colnames(MappingData)[11:29]
rownames(LMH_POS) <- c("LOW","MED", "HIGH")

for(x in 1:ncol(LMH_POS)){
  LMH_POS[1,x] <- length(which(N2000Impact3_SPA_pos_low$SERVICE==colnames(LMH_POS)[x]))
  LMH_POS[2,x] <- length(which(N2000Impact3_SPA_pos_medium$SERVICE==colnames(LMH_POS)[x]))
  LMH_POS[3,x] <- length(which(N2000Impact3_SPA_pos_high$SERVICE==colnames(LMH_POS)[x]))
}
barplot(as.matrix(LMH_POS), horiz=TRUE, las=2, legend=T, main="SPA - POS_LMH")

# Low, medium, high - negative
LMH_NEG <- as.data.frame(matrix(nrow=3, ncol=19))
colnames(LMH_NEG) <- colnames(MappingData)[11:29]
rownames(LMH_NEG) <- c("LOW","MED", "HIGH")

for(x in 1:ncol(LMH_NEG)){
  LMH_NEG[1,x] <- length(which(N2000Impact3_SPA_neg_low$SERVICE==colnames(LMH_NEG)[x]))
  LMH_NEG[2,x] <- length(which(N2000Impact3_SPA_neg_medium$SERVICE==colnames(LMH_NEG)[x]))
  LMH_NEG[3,x] <- length(which(N2000Impact3_SPA_neg_high$SERVICE==colnames(LMH_NEG)[x]))
}
barplot(as.matrix(LMH_NEG), horiz=TRUE, las=2, legend=T, main="SPA - NEG_LMH")

# Inside, outside, both - positive
IOB_POS <- as.data.frame(matrix(nrow=3, ncol=19))
colnames(IOB_POS) <- colnames(MappingData)[11:29]
rownames(IOB_POS) <- c("IN","OUT", "BOTH")

for(x in 1:ncol(IOB_POS)){
  IOB_POS[1,x] <- length(which(N2000Impact3_SPA_pos_in$SERVICE==colnames(IOB_POS)[x]))
  IOB_POS[2,x] <- length(which(N2000Impact3_SPA_pos_out$SERVICE==colnames(IOB_POS)[x]))
  IOB_POS[3,x] <- length(which(N2000Impact3_SPA_pos_both$SERVICE==colnames(IOB_POS)[x]))
}
barplot(as.matrix(IOB_POS), horiz=TRUE, las=2, legend=T, main="SPA - POS_IOB")

# Inside, outside, both - negative
IOB_NEG <- as.data.frame(matrix(nrow=3, ncol=19))
colnames(IOB_NEG) <- colnames(MappingData)[11:29]
rownames(IOB_NEG) <- c("IN","OUT", "BOTH")

for(x in 1:ncol(IOB_NEG)){
  IOB_NEG[1,x] <- length(which(N2000Impact3_SPA_neg_in$SERVICE==colnames(IOB_NEG)[x]))
  IOB_NEG[2,x] <- length(which(N2000Impact3_SPA_neg_out$SERVICE==colnames(IOB_NEG)[x]))
  IOB_NEG[3,x] <- length(which(N2000Impact3_SPA_neg_both$SERVICE==colnames(IOB_NEG)[x]))
}
barplot(as.matrix(IOB_NEG), horiz=TRUE, las=2, legend=T, main="SPA - NEG_IOB")


### SCI (Site type B+C) ###

# Sub-data-frame: select all rows from N2000Impact3 that are SCI
N2000Impact3_SCI <- subset(N2000Impact3,N2000Impact3$SITE_TYPE=="B"|N2000Impact3$SITE_TYPE=="C")
# Sub-data-frame: select all rows from SCI that are positive (and low/medium/high)
N2000Impact3_SCI_pos <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="P")
N2000Impact3_SCI_pos_low <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="P"&N2000Impact3_SCI$INTENSITY=="LOW")
N2000Impact3_SCI_pos_medium <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="P"&N2000Impact3_SCI$INTENSITY=="MEDIUM")
N2000Impact3_SCI_pos_high <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="P"&N2000Impact3_SCI$INTENSITY=="HIGH")
# Sub-data-frame: select all rows from SCI that are negative (and low/medium/high)
N2000Impact3_SCI_neg <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="N")
N2000Impact3_SCI_neg_low <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="N"&N2000Impact3_SCI$INTENSITY=="LOW")
N2000Impact3_SCI_neg_medium <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="N"&N2000Impact3_SCI$INTENSITY=="MEDIUM")
N2000Impact3_SCI_neg_high <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="N"&N2000Impact3_SCI$INTENSITY=="HIGH")
# Sub-data-frame: select all rows from SCI that are positive (and in/out/both)
N2000Impact3_SCI_pos_in <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="P"&N2000Impact3_SCI$OCCURRENCE=="IN")
N2000Impact3_SCI_pos_out <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="P"&N2000Impact3_SCI$OCCURRENCE=="OUT")
N2000Impact3_SCI_pos_both <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="P"&N2000Impact3_SCI$OCCURRENCE=="BOTH")
# Sub-data-frame: select all rows from SCI that are negative (and in/out/both)
N2000Impact3_SCI_neg_in <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="N"&N2000Impact3_SCI$OCCURRENCE=="IN")
N2000Impact3_SCI_neg_out <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="N"&N2000Impact3_SCI$OCCURRENCE=="OUT")
N2000Impact3_SCI_neg_both <- subset(N2000Impact3_SCI,N2000Impact3_SCI$IMPACT_TYPE=="N"&N2000Impact3_SCI$OCCURRENCE=="BOTH")

# Positive and negative proportion
POS_NEG <- as.data.frame(matrix(nrow=2, ncol=19))
colnames(POS_NEG) <- colnames(MappingData)[11:29]
rownames(POS_NEG) <- c("POS","NEG")

for(x in 1:ncol(POS_NEG)){
  POS_NEG[1,x] <- length(which(N2000Impact3_SCI_pos$SERVICE==colnames(POS_NEG)[x]))
  POS_NEG[2,x] <- length(which(N2000Impact3_SCI_neg$SERVICE==colnames(POS_NEG)[x]))
}
barplot(as.matrix(POS_NEG), horiz=TRUE, las=2, legend=T, main="SCI - POS_NEG")

# Low, medium, high - positive
LMH_POS <- as.data.frame(matrix(nrow=3, ncol=19))
colnames(LMH_POS) <- colnames(MappingData)[11:29]
rownames(LMH_POS) <- c("LOW","MED", "HIGH")

for(x in 1:ncol(LMH_POS)){
  LMH_POS[1,x] <- length(which(N2000Impact3_SCI_pos_low$SERVICE==colnames(LMH_POS)[x]))
  LMH_POS[2,x] <- length(which(N2000Impact3_SCI_pos_medium$SERVICE==colnames(LMH_POS)[x]))
  LMH_POS[3,x] <- length(which(N2000Impact3_SCI_pos_high$SERVICE==colnames(LMH_POS)[x]))
}
barplot(as.matrix(LMH_POS), horiz=TRUE, las=2, legend=T, main="SCI - POS_LMH")

# Low, medium, high - negative
LMH_NEG <- as.data.frame(matrix(nrow=3, ncol=19))
colnames(LMH_NEG) <- colnames(MappingData)[11:29]
rownames(LMH_NEG) <- c("LOW","MED", "HIGH")

for(x in 1:ncol(LMH_NEG)){
  LMH_NEG[1,x] <- length(which(N2000Impact3_SCI_neg_low$SERVICE==colnames(LMH_NEG)[x]))
  LMH_NEG[2,x] <- length(which(N2000Impact3_SCI_neg_medium$SERVICE==colnames(LMH_NEG)[x]))
  LMH_NEG[3,x] <- length(which(N2000Impact3_SCI_neg_high$SERVICE==colnames(LMH_NEG)[x]))
}
barplot(as.matrix(LMH_NEG), horiz=TRUE, las=2, legend=T, main="SCI - NEG_LMH")

# Inside, outside, both - positive
IOB_POS <- as.data.frame(matrix(nrow=3, ncol=19))
colnames(IOB_POS) <- colnames(MappingData)[11:29]
rownames(IOB_POS) <- c("IN","OUT", "BOTH")

for(x in 1:ncol(IOB_POS)){
  IOB_POS[1,x] <- length(which(N2000Impact3_SCI_pos_in$SERVICE==colnames(IOB_POS)[x]))
  IOB_POS[2,x] <- length(which(N2000Impact3_SCI_pos_out$SERVICE==colnames(IOB_POS)[x]))
  IOB_POS[3,x] <- length(which(N2000Impact3_SCI_pos_both$SERVICE==colnames(IOB_POS)[x]))
}
barplot(as.matrix(IOB_POS), horiz=TRUE, las=2, legend=T, main="SCI - POS_IOB")

# Inside, outside, both - negative
IOB_NEG <- as.data.frame(matrix(nrow=3, ncol=19))
colnames(IOB_NEG) <- colnames(MappingData)[11:29]
rownames(IOB_NEG) <- c("IN","OUT", "BOTH")

for(x in 1:ncol(IOB_NEG)){
  IOB_NEG[1,x] <- length(which(N2000Impact3_SCI_neg_in$SERVICE==colnames(IOB_NEG)[x]))
  IOB_NEG[2,x] <- length(which(N2000Impact3_SCI_neg_out$SERVICE==colnames(IOB_NEG)[x]))
  IOB_NEG[3,x] <- length(which(N2000Impact3_SCI_neg_both$SERVICE==colnames(IOB_NEG)[x]))
}
barplot(as.matrix(IOB_NEG), horiz=TRUE, las=2, legend=T, main="SCI - NEG_IOB")


############################################################################
### 04.6 Networks showing associations between services
### Useful tutorial here: http://kateto.net/network-visualization
### relies on the function ES_Graph.r, which processes the
### input data frame to produce an edge table and igraph() object
############################################################################

# Associations of services

# Load helper script (to be copied into 01)
source(path2wd %+% "ES_Graph.r")

### SPA (Site type A) ###
ptm <- proc.time() # 24 minutes
# Sub-data-frame: select all rows from N2000Impact3 that are SPA
N2000Impact3_SPA_graph<-ES_Graph(N2000Impact3_SPA,GRAPH=FALSE)
# Sub-data-frame: select all rows from SPA that are positive (and low/medium/high)
N2000Impact3_SPA_pos_graph<-ES_Graph(N2000Impact3_SPA_pos,GRAPH=FALSE)
N2000Impact3_SPA_pos_low_graph<-ES_Graph(N2000Impact3_SPA_pos_low,GRAPH=FALSE)
N2000Impact3_SPA_pos_medium_graph<-ES_Graph(N2000Impact3_SPA_pos_medium,GRAPH=FALSE)
N2000Impact3_SPA_pos_high_graph<-ES_Graph(N2000Impact3_SPA_pos_high,GRAPH=FALSE)
# Sub-data-frame: select all rows from SPA that are negative (and low/medium/high)
N2000Impact3_SPA_neg_graph<-ES_Graph(N2000Impact3_SPA_neg,GRAPH=FALSE)
N2000Impact3_SPA_neg_low_graph<-ES_Graph(N2000Impact3_SPA_neg_low,GRAPH=FALSE)
N2000Impact3_SPA_neg_medium_graph<-ES_Graph(N2000Impact3_SPA_neg_medium,GRAPH=FALSE)
N2000Impact3_SPA_neg_high_graph<-ES_Graph(N2000Impact3_SPA_neg_high,GRAPH=FALSE)
# Sub-data-frame: select all rows from SPA that are positive (and in/out/both)
N2000Impact3_SPA_pos_in_graph<-ES_Graph(N2000Impact3_SPA_pos_in,GRAPH=FALSE)
N2000Impact3_SPA_pos_out_graph<-ES_Graph(N2000Impact3_SPA_pos_out,GRAPH=FALSE)
N2000Impact3_SPA_pos_both_graph<-ES_Graph(N2000Impact3_SPA_pos_both,GRAPH=FALSE)
# Sub-data-frame: select all rows from SPA that are negative (and in/out/both)
N2000Impact3_SPA_neg_in_graph<-ES_Graph(N2000Impact3_SPA_neg_in,GRAPH=FALSE)
N2000Impact3_SPA_neg_out_graph<-ES_Graph(N2000Impact3_SPA_neg_out,GRAPH=FALSE)
N2000Impact3_SPA_neg_both_graph<-ES_Graph(N2000Impact3_SPA_neg_both,GRAPH=FALSE)

proc.time() - ptm # 


### SCI (Site type B+C) ###
ptm <- proc.time()
# Sub-data-frame: select all rows from N2000Impact3 that are SCI
N2000Impact3_SCI_graph<-ES_Graph(N2000Impact3_SCI,GRAPH=FALSE)
# Sub-data-frame: select all rows from SCI that are positive (and low/medium/high)
N2000Impact3_SCI_pos_graph<-ES_Graph(N2000Impact3_SCI_pos,GRAPH=FALSE)
N2000Impact3_SCI_pos_low_graph<-ES_Graph(N2000Impact3_SCI_pos_low,GRAPH=FALSE)
N2000Impact3_SCI_pos_medium_graph<-ES_Graph(N2000Impact3_SCI_pos_medium,GRAPH=FALSE)
N2000Impact3_SCI_pos_high_graph<-ES_Graph(N2000Impact3_SCI_pos_high,GRAPH=FALSE)
# Sub-data-frame: select all rows from SCI that are negative (and low/medium/high)
N2000Impact3_SCI_neg_graph<-ES_Graph(N2000Impact3_SCI_neg,GRAPH=FALSE)
N2000Impact3_SCI_neg_low_graph<-ES_Graph(N2000Impact3_SCI_neg_low,GRAPH=FALSE)
N2000Impact3_SCI_neg_medium_graph<-ES_Graph(N2000Impact3_SCI_neg_medium,GRAPH=FALSE)
N2000Impact3_SCI_neg_high_graph<-ES_Graph(N2000Impact3_SCI_neg_high,GRAPH=FALSE)
# Sub-data-frame: select all rows from SCI that are positive (and in/out/both)
N2000Impact3_SCI_pos_in_graph<-ES_Graph(N2000Impact3_SCI_pos_in,GRAPH=FALSE)
N2000Impact3_SCI_pos_out_graph<-ES_Graph(N2000Impact3_SCI_pos_out,GRAPH=FALSE)
N2000Impact3_SCI_pos_both_graph<-ES_Graph(N2000Impact3_SCI_pos_both,GRAPH=FALSE)
# Sub-data-frame: select all rows from SCI that are negative (and in/out/both)
N2000Impact3_SCI_neg_in_graph<-ES_Graph(N2000Impact3_SCI_neg_in,GRAPH=FALSE)
N2000Impact3_SCI_neg_out_graph<-ES_Graph(N2000Impact3_SCI_neg_out,GRAPH=FALSE)
N2000Impact3_SCI_neg_both_graph<-ES_Graph(N2000Impact3_SCI_neg_both,GRAPH=FALSE)

proc.time() - ptm



# Plotting the resulting graphs
par(mfrow=c(2,2),mar=c(1,1,1,1))



# Sub-data-frame: select all rows from N2000Impact3 that are SPA
png("N2000Impact3_SPA_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_graph[[1]],main="N2000Impact3_SPA",layout=N2000Impact3_SPA_graph[[3]])
dev.off()
# Sub-data-frame: select all rows from SPA that are positive (and low/medium/high)
png("N2000Impact3_SPA_pos_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_pos_graph[[1]],main="N2000Impact3_SPA_pos",layout=N2000Impact3_SPA_pos_graph[[3]])
dev.off()
png("N2000Impact3_SPA_pos_low_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_pos_low_graph[[1]],main="N2000Impact3_SPA_pos_low",layout=N2000Impact3_SPA_pos_low_graph[[3]])
dev.off()
png("N2000Impact3_SPA_pos_medium_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_pos_medium_graph[[1]],main="N2000Impact3_SPA_pos_medium",layout=N2000Impact3_SPA_pos_medium_graph[[3]])
dev.off()
png("N2000Impact3_SPA_pos_high_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_pos_high_graph[[1]],main="N2000Impact3_SPA_pos_high",layout=N2000Impact3_SPA_pos_high_graph[[3]])
dev.off()
# Sub-data-frame: select all rows from SPA that are negative (and low/medium/high)
png("N2000Impact3_SPA_neg_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_neg_graph[[1]],main="N2000Impact3_SPA_neg",layout=N2000Impact3_SPA_neg_graph[[3]])
dev.off()
png("N2000Impact3_SPA_neg_low_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_neg_low_graph[[1]],main="N2000Impact3_SPA_neg_low",layout=N2000Impact3_SPA_neg_low_graph[[3]])
dev.off()
png("N2000Impact3_SPA_neg_medium_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_neg_medium_graph[[1]],main="N2000Impact3_SPA_neg_medium",layout=N2000Impact3_SPA_neg_medium_graph[[3]])
dev.off()
png("N2000Impact3_SPA_neg_high_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_neg_high_graph[[1]],main="N2000Impact3_SPA_neg_high",layout=N2000Impact3_SPA_neg_high_graph[[3]])
dev.off()
# Sub-data-frame: select all rows from SPA that are positive (and in/out/both)
png("N2000Impact3_SPA_pos_in_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_pos_in_graph[[1]],main="N2000Impact3_SPA_pos_in",layout=N2000Impact3_SPA_pos_in_graph[[3]])
dev.off()
png("N2000Impact3_SPA_pos_out_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_pos_out_graph[[1]],main="N2000Impact3_SPA_pos_out",layout=N2000Impact3_SPA_pos_out_graph[[3]])
dev.off()
png("N2000Impact3_SPA_pos_both_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_pos_both_graph[[1]],main="N2000Impact3_SPA_pos_both",layout=N2000Impact3_SPA_pos_both_graph[[3]])
dev.off()
# Sub-data-frame: select all rows from SPA that are negative (and in/out/both)
png("N2000Impact3_SPA_neg_in_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_neg_in_graph[[1]],main="N2000Impact3_SPA_neg_in",layout=N2000Impact3_SPA_neg_in_graph[[3]])
dev.off()
png("N2000Impact3_SPA_neg_out_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_neg_out_graph[[1]],main="N2000Impact3_SPA_neg_out",layout=N2000Impact3_SPA_neg_out_graph[[3]])
dev.off()
png("N2000Impact3_SPA_neg_both_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SPA_neg_both_graph[[1]],main="N2000Impact3_SPA_neg_both",layout=N2000Impact3_SPA_neg_both_graph[[3]])
dev.off()


### SCI (Site type B+C) ###

# Sub-data-frame: select all rows from N2000Impact3 that are SCI
png("N2000Impact3_SCI_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_graph[[1]],main="N2000Impact3_SCI",layout=N2000Impact3_SCI_graph[[3]])
dev.off()
# Sub-data-frame: select all rows from SCI that are positive (and low/medium/high)
png("N2000Impact3_SCI_pos_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_pos_graph[[1]],main="N2000Impact3_SCI_pos",layout=N2000Impact3_SCI_pos_graph[[3]])
dev.off()
png("N2000Impact3_SCI_pos_low_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_pos_low_graph[[1]],main="N2000Impact3_SCI_pos_low",layout=N2000Impact3_SCI_pos_low_graph[[3]])
dev.off()
png("N2000Impact3_SCI_pos_medium_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_pos_medium_graph[[1]],main="N2000Impact3_SCI_pos_medium",layout=N2000Impact3_SCI_pos_medium_graph[[3]])
dev.off()
png("N2000Impact3_SCI_pos_high_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_pos_high_graph[[1]],main="N2000Impact3_SCI_pos_high",layout=N2000Impact3_SCI_pos_high_graph[[3]])
dev.off()
# Sub-data-frame: select all rows from SCI that are negative (and low/medium/high)
png("N2000Impact3_SCI_neg_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_neg_graph[[1]],main="N2000Impact3_SCI_neg",layout=N2000Impact3_SCI_neg_graph[[3]])
dev.off()
png("N2000Impact3_SCI_neg_low_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_neg_low_graph[[1]],main="N2000Impact3_SCI_neg_low",layout=N2000Impact3_SCI_neg_low_graph[[3]])
dev.off()
png("N2000Impact3_SCI_neg_medium_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_neg_medium_graph[[1]],main="N2000Impact3_SCI_neg_medium",layout=N2000Impact3_SCI_neg_medium_graph[[3]])
dev.off()
png("N2000Impact3_SCI_neg_high_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_neg_high_graph[[1]],main="N2000Impact3_SCI_neg_high",layout=N2000Impact3_SCI_neg_high_graph[[3]])
dev.off()
# Sub-data-frame: select all rows from SCI that are positive (and in/out/both)
png("N2000Impact3_SCI_pos_in_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_pos_in_graph[[1]],main="N2000Impact3_SCI_pos_in",layout=N2000Impact3_SCI_pos_in_graph[[3]])
dev.off()
png("N2000Impact3_SCI_pos_out_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_pos_out_graph[[1]],main="N2000Impact3_SCI_pos_out",layout=N2000Impact3_SCI_pos_out_graph[[3]])
dev.off()
png("N2000Impact3_SCI_pos_both_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_pos_both_graph[[1]],main="N2000Impact3_SCI_pos_both",layout=N2000Impact3_SCI_pos_both_graph[[3]])
dev.off()
# Sub-data-frame: select all rows from SCI that are negative (and in/out/both)
png("N2000Impact3_SCI_neg_in_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_neg_in_graph[[1]],main="N2000Impact3_SCI_neg_in",layout=N2000Impact3_SCI_neg_in_graph[[3]])
dev.off()
png("N2000Impact3_SCI_neg_out_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_neg_out_graph[[1]],main="N2000Impact3_SCI_neg_out",layout=N2000Impact3_SCI_neg_out_graph[[3]])
dev.off()
png("N2000Impact3_SCI_neg_both_graph.png", width=8, height=8, units="in", res=300)
plot(N2000Impact3_SCI_neg_both_graph[[1]],main="N2000Impact3_SCI_neg_both",layout=N2000Impact3_SCI_neg_both_graph[[3]])
dev.off()




############################################################################
###
### 04.7 Service co-occurrence statistics
###
############################################################################



N2000Impact3_SPA_cooccur<-cooccur(t(N2000Impact3_SPA_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
# Sub-data-frame: select all rows from SPA that are positive (and low/medium/high)
N2000Impact3_SPA_pos_cooccur<-cooccur(t(N2000Impact3_SPA_pos_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SPA_pos_low_cooccur<-cooccur(t(N2000Impact3_SPA_pos_low_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SPA_pos_medium_cooccur<-cooccur(t(N2000Impact3_SPA_pos_medium_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SPA_pos_high_cooccur<-cooccur(t(N2000Impact3_SPA_pos_high_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
# Sub-data-frame: select all rows from SPA that are negative (and low/medium/high)
N2000Impact3_SPA_neg_cooccur<-cooccur(t(N2000Impact3_SPA_neg_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SPA_neg_low_cooccur<-cooccur(t(N2000Impact3_SPA_neg_low_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SPA_neg_medium_cooccur<-cooccur(t(N2000Impact3_SPA_neg_medium_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SPA_neg_high_cooccur<-cooccur(t(N2000Impact3_SPA_neg_high_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
# Sub-data-frame: select all rows from SPA that are positive (and in/out/both)
N2000Impact3_SPA_pos_in_cooccur<-cooccur(t(N2000Impact3_SPA_pos_in_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SPA_pos_out_cooccur<-cooccur(t(N2000Impact3_SPA_pos_out_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SPA_pos_both_cooccur<-cooccur(t(N2000Impact3_SPA_pos_both_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
# Sub-data-frame: select all rows from SPA that are negative (and in/out/both)
N2000Impact3_SPA_neg_in_cooccur<-cooccur(t(N2000Impact3_SPA_neg_in_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SPA_neg_out_cooccur<-cooccur(t(N2000Impact3_SPA_neg_out_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SPA_neg_both_cooccur<-cooccur(t(N2000Impact3_SPA_neg_both_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)


### SCI (Site type B+C) ###

# Sub-data-frame: select all rows from N2000Impact3 that are SCI
N2000Impact3_SCI_cooccur<-cooccur(t(N2000Impact3_SCI_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
# Sub-data-frame: select all rows from SCI that are positive (and low/medium/high)
N2000Impact3_SCI_pos_cooccur<-cooccur(t(N2000Impact3_SCI_pos_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SCI_pos_low_cooccur<-cooccur(t(N2000Impact3_SCI_pos_low_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SCI_pos_medium_cooccur<-cooccur(t(N2000Impact3_SCI_pos_medium_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SCI_pos_high_cooccur<-cooccur(t(N2000Impact3_SCI_pos_high_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
# Sub-data-frame: select all rows from SCI that are negative (and low/medium/high)
N2000Impact3_SCI_neg_cooccur<-cooccur(t(N2000Impact3_SCI_neg_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SCI_neg_low_cooccur<-cooccur(t(N2000Impact3_SCI_neg_low_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SCI_neg_medium_cooccur<-cooccur(t(N2000Impact3_SCI_neg_medium_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SCI_neg_high_cooccur<-cooccur(t(N2000Impact3_SCI_neg_high_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
# Sub-data-frame: select all rows from SCI that are positive (and in/out/both)
N2000Impact3_SCI_pos_in_cooccur<-cooccur(t(N2000Impact3_SCI_pos_in_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SCI_pos_out_cooccur<-cooccur(t(N2000Impact3_SCI_pos_out_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SCI_pos_both_cooccur<-cooccur(t(N2000Impact3_SCI_pos_both_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
# Sub-data-frame: select all rows from SCI that are negative (and in/out/both)
N2000Impact3_SCI_neg_in_cooccur<-cooccur(t(N2000Impact3_SCI_neg_in_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SCI_neg_out_cooccur<-cooccur(t(N2000Impact3_SCI_neg_out_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)
N2000Impact3_SCI_neg_both_cooccur<-cooccur(t(N2000Impact3_SCI_neg_both_graph[[4]]),type = "spp_site", thresh = FALSE, spp_names = TRUE)









############################################################################
### 04.8 Code cemetery
###
### Below this line you can leave everything that might be of use at a later
### point in time ;)
############################################################################
###                          ~~~~**RIP**~~~~
############################################################################



############################################################################
### Purpose of this skript module 04 is to:
### 
### 04.1. Statistics on Natura2000 pressures
### 04.2. ...
###
### Authors: CH ...
############################################################################

############################################################################
### 04.1. Statistics on Natura2000 pressures
###
###
############################################################################

# set wd to path2temp where files have been downloaded and extracted
setwd(path2temp %+% "/") 

# Load data
N2000Impact<-read.csv("IMPACT.csv")
head(N2000Impact)

## DATA CLEANING ####

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

## SUMMARY STATISTICS ####

# Create harmonised impact categories where possible at the four tiers
Tier1Impact<-substr(N2000Impact$IMPACTCODE,1,1)
Tier2Impact<-substr(N2000Impact$IMPACTCODE,1,3)
Tier3Impact<-substr(N2000Impact$IMPACTCODE,1,6)
Tier4Impact<-substr(N2000Impact$IMPACTCODE,1,9)
# Remove the value if the site does not have sufficient details
Tier2Impact[nchar(Tier2Impact)!=3]<-NA
Tier3Impact[nchar(Tier3Impact)!=6]<-NA
Tier4Impact[nchar(Tier4Impact)!=9]<-NA

# Add those new harmonised impacts to the main table
N2000Impact<-cbind(N2000Impact,Tier1Impact,Tier2Impact,Tier3Impact,Tier4Impact)

# Plot distributions of frequencies of impacts
#barplot(table(unique(N2000Impact[,c(1,8)])$Tier1Impact))
#barplot(table(unique(N2000Impact[,c(1,9)])$Tier2Impact))
#barplot(table(unique(N2000Impact[,c(1,10)])$Tier3Impact))
#barplot(table(unique(N2000Impact[,c(1,11)])$Tier4Impact))

# 10 most common impacts
# Download definitions of impact codes for simplicity
download.file("http://bd.eionet.europa.eu/activities/Natura_2000/Folder_Reference_Portal/Ref_threats_pressures_FINAL_20110330.xls", "Ref_threats_pressures_FINAL_20110330.xls", mode="wb")
ImpactDefinitions<-readWorksheetFromFile("Ref_threats_pressures_FINAL_20110330.xls",2)[,c(1,2)]

# Rank Tier1 Impacts and give definitions of codes
Tier1Ranked<-as.matrix(sort(table(unique(N2000Impact[,c(1,8)])$Tier1Impact),decreasing=TRUE))
DefineTier1<-ImpactDefinitions$Descript_EN[match(rownames(Tier1Ranked),ImpactDefinitions$ACT_Code)]
Tier1Summary<-cbind(Tier1Ranked,DefineTier1)
colnames(Tier1Summary)<-c("Number of sites","Type of impact")

# Rank Tier2 Impacts and give definitions of codes
Tier2Ranked<-as.matrix(sort(table(unique(N2000Impact[,c(1,9)])$Tier2Impact),decreasing=TRUE))
DefineTier2<-ImpactDefinitions$Descript_EN[match(rownames(Tier2Ranked),ImpactDefinitions$ACT_Code)]
Tier2Summary<-cbind(Tier2Ranked,DefineTier2)
colnames(Tier2Summary)<-c("Number of sites","Type of impact")

# Rank Tier3 Impacts and give definitions of codes
Tier3Ranked<-as.matrix(sort(table(unique(N2000Impact[,c(1,10)])$Tier3Impact),decreasing=TRUE))
DefineTier3<-ImpactDefinitions$Descript_EN[match(rownames(Tier3Ranked),ImpactDefinitions$ACT_Code)]
Tier3Summary<-cbind(Tier3Ranked,DefineTier3)
colnames(Tier3Summary)<-c("Number of sites","Type of impact")

# Rank Tier4 Impacts and give definitions of codes
Tier4Ranked<-as.matrix(sort(table(unique(N2000Impact[,c(1,11)])$Tier4Impact),decreasing=TRUE))
DefineTier4<-ImpactDefinitions$Descript_EN[match(rownames(Tier4Ranked),ImpactDefinitions$ACT_Code)]
Tier4Summary<-cbind(Tier4Ranked,DefineTier4)
colnames(Tier4Summary)<-c("Number of sites","Type of impact")

############################################################################
### 04.2. parsing database using lookup table from g.docs
### (run 04.1 to clean N2000Impact data first)
### (deprecated, based on 04.4?)
############################################################################
# 
# # Condense threats according to relation
# library(plyr)
# counttab<-count(N2000Impact$IMPACTCODE)
# names(counttab)<-c("ACT_Code","count" )
# 
# # subdata<-MappingData[which(MappingData$Cultivated.crops=='x'),1:11 ]
# # mergedata<-merge(subdata, counttab, by="ACT_Code")
# # aggdata<-aggregate(mergedata$count, by=list(mergedata$relation), FUN=sum)
# # pie(aggdata$x, labels=aggdata$Group.1, main="Cultivated crops")
# 
# 
# #subdata<-MappingData[which(MappingData$Cultivated.crops=='x'), ]
# par(mfrow=c(1,1),mar=c(1,2,2,1))
# for(i in 11:28) {
# 
#   subdata<-MappingData[which(MappingData[,i]=='x'),1:11 ]
#   mergedata<-merge(subdata, counttab, by="ACT_Code")
#   
#   #if (length(mergedata$relation)>1) { 
#   aggdata<-aggregate(mergedata$count, by=list(mergedata$relation), FUN=sum)
#   #} else {
#   #}
#   pie(aggdata$x, radius=log(sum(aggdata$x))/log(32177),labels=aggdata$Group.1, main=paste(names(data[i]), "\n", sum(aggdata$x)))
# }
# 
# 
# length(N2000Impact$IMPACTCODE[N2000Impact$IMPACTCODE=="A01"])
# 
# 
# subdata<-MappingData[which(MappingData[,i]=='x'),1:11 ]
# mergedata<-merge(subdata, counttab, by="ACT_Code")
# 
# MappingData$relation
# 
# 
# setwd(path2wd)

############################################################################
### 04.3. Networks showing associations between threats and services
### Useful tutorial here: http://kateto.net/network-visualization
###
############################################################################

### 04.3.1: Associations of threats
# Create a table of threats by sites
ThreatAssoc<-xtabs(~N2000Impact$SITECODE+N2000Impact$Tier1Impact)
ThreatAssoc<-as.data.frame.matrix(ThreatAssoc)

# Create a co-occurrence matrix of threats
ThreatCount<-numeric(length=15*15)
 for (x in 1:15){
   for(y in 1:15){
     TempDat<-subset(ThreatAssoc,ThreatAssoc[,x]!="0" & ThreatAssoc[,y]!="0")
     ThreatCount[(x-1)*15+y]<-nrow(TempDat)
   }
 }
ThreatAssocEdgeTable<-data.frame(Threat1=rep(colnames(ThreatAssoc),each=15),
                                 Threat2=rep(colnames(ThreatAssoc),15),
                                 Weight=ThreatCount)

# Remove duplicates, as well as X (no threats or pressures, or threats and pressures from outside 
# of the EU), U (unknown threats), and L (natural catastrophes)
NoDup<-t(combn(colnames(ThreatAssoc)[-c(12,14,15)],m=2))
NoDupWeight<-numeric(length=nrow(NoDup))
for(x in 1:nrow(NoDup)){
  NoDupWeight[x]<-subset(ThreatAssocEdgeTable,ThreatAssocEdgeTable$Threat1==NoDup[x,1] 
                      & ThreatAssocEdgeTable$Threat2==NoDup[x,2])[,3]
}
ThreatAssocEdgeTable<-data.frame(Threat1=NoDup[,1],Threat2=NoDup[,2],Weight=NoDupWeight)

# There are two ways to visualise the association
# ...first, a network graph
graph <- graph.data.frame(ThreatAssocEdgeTable, directed = FALSE)
E(graph)$width <- E(graph)$Weight/500 # Set edge width based on weight
plot(graph) 

# ...second, a heat matrix
netm <- as_adjacency_matrix(graph, attr="Weight", sparse=F)
colnames(netm) <- V(graph)$media
rownames(netm) <- V(graph)$media
netm[lower.tri(netm)]<-NA # Remove upper triangle
palf <- colorRampPalette(c("blue", "red")) 
heatmap(netm[,12:1], Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )


### 04.3.2: Associations of services
# Convert threats to harmonised threats that all have services associated (the "relation" column)
# in the Google Sheet (sorry that this uses some ugly loops)
ThreatWithService<-character(length=nrow(N2000Impact))
for (x in 1:nrow(N2000Impact)){
  if(!is.na(match(N2000Impact$IMPACTCODE[x],MappingData$ACT_Code))) 
  {ThreatWithService[x]<-as.character(MappingData$relation[which(MappingData$ACT_Code==N2000Impact$IMPACTCODE[x])])} else {ThreatWithService[x]<-NA}
}
# Add that new harmonised threat to the N2000Impact table
N2000Impact2<-cbind(N2000Impact,ThreatWithService)

# Create a vector of simplified service groups
ServiceGroups<-c("CultCrop","WildPlants","PlantMaterialAg","Fibre","PlantEnergy","RearedAnimal","WildAnimal","Aquaculture","SurfWaterDrink",
"SurfWaterNonDrink","GroundWaterDrink","GroundWaterNonDrink","ErosionPrevent","HydroMaintain","FloodProtect","GHGReduction",
"Experiential","Physical","Scientific")

# Create a matrix of site x service group
ServiceBySite<-matrix(ncol=length(ServiceGroups),nrow=length(unique(N2000Impact2$SITECODE)))
rownames(ServiceBySite)<-unique(N2000Impact2$SITECODE)
colnames(ServiceBySite)<-ServiceGroups
# This is the ugly bit, and takes a few minutes to run
for(x in 1:nrow(ServiceBySite)){
  # For each unique site ID code, extract a list of threats that have corresponding services
  SiteThreats<-na.omit(subset(N2000Impact2,N2000Impact2$SITECODE==rownames(ServiceBySite)[x])$ThreatWithService)
  # Remove the rows that did not have services
  SiteThreats<-SiteThreats[which(SiteThreats!="NA")]
  # Extract the rows from the mapping table that contain the services associated with those threats
  SiteThreatsMapped<-subset(MappingData,MappingData$relation %in% SiteThreats)
  # For each service group, check whether the site had a threat that indicates that service is present
  for(y in 1:ncol(ServiceBySite)){
    if("x" %in% SiteThreatsMapped[,9+y]) {ServiceBySite[x,y]<-1} else {ServiceBySite[x,y]<-0}
  }
  # Utility function to trace progress (ca. 20,000 sites total)
  if(x %% 100 == 0) {print(x);flush.console()}
}

# Find the total number of times each service occurs
ServiceFreq<-colSums(ServiceBySite)

# Create a co-occurrence matrix of threats
ServiceCount<-numeric(length=15*15)
for (x in 1:19){
  for(y in 1:19){
    TempDat<-subset(ServiceBySite,ServiceBySite[,x]!="0" & ServiceBySite[,y]!="0")
    ServiceCount[(x-1)*19+y]<-nrow(TempDat)
  }
}
ServiceBySiteEdgeTable<-data.frame(Service1=rep(colnames(ServiceBySite),each=19),
                                   Service2=rep(colnames(ServiceBySite),19),
                                   Weight=ServiceCount)

# Remove duplicates
NoDup<-t(combn(colnames(ServiceBySite),m=2))
NoDupWeight<-numeric(length=nrow(NoDup))
for(x in 1:nrow(NoDup)){
  NoDupWeight[x]<-subset(ServiceBySiteEdgeTable,ServiceBySiteEdgeTable$Service1==NoDup[x,1] 
                         & ServiceBySiteEdgeTable$Service2==NoDup[x,2])[,3]
}
ServiceBySiteEdgeTable<-data.frame(Service1=NoDup[,1],Service2=NoDup[,2],Weight=NoDupWeight)

# Plot two different forms of association
par(mfrow=c(1,2))

# ...first, a network graph
graph <- graph.data.frame(ServiceBySiteEdgeTable, directed = FALSE) # create an igraph object
E(graph)$width <- E(graph)$Weight/1000 # Set edge width based on weight
V(graph)$size <- sqrt(ServiceFreq)/3 # Set vertex size based on frequency of service
tkid <- tkplot(graph) # tkid is the id of the tkplot that will open, and allows manual rearragement of nodes
l <- tkplot.getcoords(tkid) # grab the coordinates from final tkplot after rearrangement
plot(graph, layout=l)


# ...and a second network graph excluding weak links
ServiceBySiteEdgeTable2<-subset(ServiceBySiteEdgeTable,ServiceBySiteEdgeTable$Weight>500)
graph <- graph.data.frame(ServiceBySiteEdgeTable2, directed = FALSE)
E(graph)$width <- E(graph)$Weight/1000 # Set edge width based on weight
ServiceFreq2<-ServiceFreq[names(ServiceFreq)%in%names(V(graph))] # extract frequencies from subset of services
V(graph)$size <- sqrt(ServiceFreq2)/3 # Set node size based on frequency of service
tkid <- tkplot(graph) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
plot(graph, layout=l)







############################################################################
### 04.4. Visualisation of threats by services provided
### 
###
############################################################################


# Take the N2000Impact table modified from above (which adds a column for the
# threats that are known to have services associated with them) and remove
# any threats that do not have a service
N2000Impact3<-subset(N2000Impact2,N2000Impact2$ThreatWithService!="NA")

# Calculate the number of services across the site network
SumThreats<-as.matrix(table(N2000Impact3$ThreatWithService))

# Using the mapping data, we can create a matrix that converts the number of
# each threat into the number of each service provided
ThreatByService<-matrix(ncol=19,nrow=nrow(SumThreats))
colnames(ThreatByService)<-ServiceGroups
rownames(ThreatByService)<-rownames(SumThreats)
# For each of the threats, we specify the number of times that that threat indicates
# a service, using a matrix
for(x in 1:nrow(ThreatByService)){
  # Extract the rows from the mapping table that contain the services associated with those threats
  SiteThreatsMapped<-subset(MappingData,MappingData$relation %in% rownames(SumThreats)[x])
  # For each service group, check whether the site had a threat that indicates that service is present
  for(y in 1:ncol(ThreatByService)){
    # The cells in the matrix correspond to the frequency with which each threat leads to each
    # service. Note that this will mean "double counting", but that is unavoidable due to the
    # potential multiple services indicated by each threat.
    if("x" %in% SiteThreatsMapped[,10+y]) {ThreatByService[x,y]<-SumThreats[x]} else {ThreatByService[x,y]<-0}
  }
}

# Rearrange the "wide" matrix into a "long" data frame for use in the treemap() function
LongThreatByService<-matrix(nrow=ncol(ThreatByService)*nrow(ThreatByService),ncol=3)
ThreatFreq<-as.vector(ThreatByService)
LongThreat<-rep(rownames(ThreatByService),19)
LongService<-rep(colnames(ThreatByService),each=nrow(ThreatByService))

# Modify the dataframe so that the formats are correct for treemap()
LongThreatByService<-as.data.frame(cbind(LongService,LongThreat,ThreatFreq=as.vector(as.numeric(ThreatFreq))))
LongThreatByService<-transform(LongThreatByService, ThreatFreq = as.numeric(ThreatFreq))
LongThreatByService<-LongThreatByService[which(LongThreatByService$ThreatFreq>1),]
treemap(LongThreatByService,c("LongService","LongThreat"),vSize="ThreatFreq")

############################################################################
### 04.5. Visualisation of threats by services provided by positive/negative
### 
###
############################################################################

### First: only positive impacts

N2000Impact4<-subset(N2000Impact2,N2000Impact2$ThreatWithService!="NA")
N2000Impact4<-subset(N2000Impact4,N2000Impact4$IMPACT_TYPE=="P") # P for positive
N2000Impact4_HIGH<-subset(N2000Impact4,N2000Impact4$INTENSITY=="HIGH")
N2000Impact4_MEDIUM<-subset(N2000Impact4,N2000Impact4$INTENSITY=="MEDIUM")
N2000Impact4_LOW<-subset(N2000Impact4,N2000Impact4$INTENSITY=="LOW")

# Calculate the number of services across the site network
SumThreatHIGHs<-as.matrix(table(N2000Impact4_HIGH$ThreatWithService))

# Using the mapping data, we can create a matrix that converts the number of
# each ThreatHIGH into the number of each service provided
ThreatHIGHByService<-matrix(ncol=19,nrow=nrow(SumThreatHIGHs))
colnames(ThreatHIGHByService)<-ServiceGroups
rownames(ThreatHIGHByService)<-rownames(SumThreatHIGHs)
# For each of the ThreatHIGHs, we specify the number of times that that ThreatHIGH indicates
# a service, using a matrix
for(x in 1:nrow(ThreatHIGHByService)){
  # Extract the rows from the mapping table that contain the services associated with those ThreatHIGHs
  SiteThreatHIGHsMapped<-subset(MappingData,MappingData$relation %in% rownames(SumThreatHIGHs)[x])
  # For each service group, check whether the site had a ThreatHIGH that indicates that service is present
  for(y in 1:ncol(ThreatHIGHByService)){
    # The cells in the matrix correspond to the frequency with which each ThreatHIGH leads to each
    # service. Note that this will mean "double counting", but that is unavoidable due to the
    # potential multiple services indicated by each ThreatHIGH.
    if("x" %in% SiteThreatHIGHsMapped[,10+y]) {ThreatHIGHByService[x,y]<-SumThreatHIGHs[x]} else {ThreatHIGHByService[x,y]<-0}
  }
}

# Rearrange the "wide" matrix into a "long" data frame for use in the treemap() function
LongThreatHIGHByService<-matrix(nrow=ncol(ThreatHIGHByService)*nrow(ThreatHIGHByService),ncol=3)
ThreatHIGHFreq<-as.vector(ThreatHIGHByService)
LongThreatHIGH<-rep(rownames(ThreatHIGHByService),19)
LongServiceHIGH<-rep(colnames(ThreatHIGHByService),each=nrow(ThreatHIGHByService))

# Modify the dataframe so that the formats are correct for treemap()
LongThreatHIGHByService<-as.data.frame(cbind(LongServiceHIGH,LongThreatHIGH,ThreatHIGHFreq=as.vector(as.numeric(ThreatHIGHFreq))))
LongThreatHIGHByService<-transform(LongThreatHIGHByService, ThreatHIGHFreq = as.numeric(ThreatHIGHFreq))
LongThreatHIGHByService<-LongThreatHIGHByService[which(LongThreatHIGHByService$ThreatHIGHFreq>1),]

LongThreatHIGHByService$INTENSITY<-c("HIGH")
colnames(LongThreatHIGHByService)<-c("LongService", "LongThreat", "ThreatFreq", "INTENSITY")


# Calculate the number of services across the site network
SumThreatMEDIUMs<-as.matrix(table(N2000Impact4_MEDIUM$ThreatWithService))

# Using the mapping data, we can create a matrix that converts the number of
# each ThreatMEDIUM into the number of each service provided
ThreatMEDIUMByService<-matrix(ncol=19,nrow=nrow(SumThreatMEDIUMs))
colnames(ThreatMEDIUMByService)<-ServiceGroups
rownames(ThreatMEDIUMByService)<-rownames(SumThreatMEDIUMs)
# For each of the ThreatMEDIUMs, we specify the number of times that that ThreatMEDIUM indicates
# a service, using a matrix
for(x in 1:nrow(ThreatMEDIUMByService)){
  # Extract the rows from the mapping table that contain the services associated with those ThreatMEDIUMs
  SiteThreatMEDIUMsMapped<-subset(MappingData,MappingData$relation %in% rownames(SumThreatMEDIUMs)[x])
  # For each service group, check whether the site had a ThreatMEDIUM that indicates that service is present
  for(y in 1:ncol(ThreatMEDIUMByService)){
    # The cells in the matrix correspond to the frequency with which each ThreatMEDIUM leads to each
    # service. Note that this will mean "double counting", but that is unavoidable due to the
    # potential multiple services indicated by each ThreatMEDIUM.
    if("x" %in% SiteThreatMEDIUMsMapped[,10+y]) {ThreatMEDIUMByService[x,y]<-SumThreatMEDIUMs[x]} else {ThreatMEDIUMByService[x,y]<-0}
  }
}

# Rearrange the "wide" matrix into a "long" data frame for use in the treemap() function
LongThreatMEDIUMByService<-matrix(nrow=ncol(ThreatMEDIUMByService)*nrow(ThreatMEDIUMByService),ncol=3)
ThreatMEDIUMFreq<-as.vector(ThreatMEDIUMByService)
LongThreatMEDIUM<-rep(rownames(ThreatMEDIUMByService),19)
LongServiceMEDIUM<-rep(colnames(ThreatMEDIUMByService),each=nrow(ThreatMEDIUMByService))

# Modify the dataframe so that the formats are correct for treemap()
LongThreatMEDIUMByService<-as.data.frame(cbind(LongServiceMEDIUM,LongThreatMEDIUM,ThreatMEDIUMFreq=as.vector(as.numeric(ThreatMEDIUMFreq))))
LongThreatMEDIUMByService<-transform(LongThreatMEDIUMByService, ThreatMEDIUMFreq = as.numeric(ThreatMEDIUMFreq))
LongThreatMEDIUMByService<-LongThreatMEDIUMByService[which(LongThreatMEDIUMByService$ThreatMEDIUMFreq>1),]

LongThreatMEDIUMByService$INTENSITY<-c("MEDIUM")
colnames(LongThreatMEDIUMByService)<-c("LongService", "LongThreat", "ThreatFreq", "INTENSITY")

# Calculate the number of services across the site network
SumThreatLOWs<-as.matrix(table(N2000Impact4_LOW$ThreatWithService))

# Using the mapping data, we can create a matrix that converts the number of
# each ThreatLOW into the number of each service provided
ThreatLOWByService<-matrix(ncol=19,nrow=nrow(SumThreatLOWs))
colnames(ThreatLOWByService)<-ServiceGroups
rownames(ThreatLOWByService)<-rownames(SumThreatLOWs)
# For each of the ThreatLOWs, we specify the number of times that that ThreatLOW indicates
# a service, using a matrix
for(x in 1:nrow(ThreatLOWByService)){
  # Extract the rows from the mapping table that contain the services associated with those ThreatLOWs
  SiteThreatLOWsMapped<-subset(MappingData,MappingData$relation %in% rownames(SumThreatLOWs)[x])
  # For each service group, check whether the site had a ThreatLOW that indicates that service is present
  for(y in 1:ncol(ThreatLOWByService)){
    # The cells in the matrix correspond to the frequency with which each ThreatLOW leads to each
    # service. Note that this will mean "double counting", but that is unavoidable due to the
    # potential multiple services indicated by each ThreatLOW.
    if("x" %in% SiteThreatLOWsMapped[,10+y]) {ThreatLOWByService[x,y]<-SumThreatLOWs[x]} else {ThreatLOWByService[x,y]<-0}
  }
}


# Rearrange the "wide" matrix into a "long" data frame for use in the treemap() function
LongThreatLOWByService<-matrix(nrow=ncol(ThreatLOWByService)*nrow(ThreatLOWByService),ncol=3)
ThreatLOWFreq<-as.vector(ThreatLOWByService)
LongThreatLOW<-rep(rownames(ThreatLOWByService),19)
LongServiceLOW<-rep(colnames(ThreatLOWByService),each=nrow(ThreatLOWByService))

# Modify the dataframe so that the formats are correct for treemap()
LongThreatLOWByService<-as.data.frame(cbind(LongServiceLOW,LongThreatLOW,ThreatLOWFreq=as.vector(as.numeric(ThreatLOWFreq))))
LongThreatLOWByService<-transform(LongThreatLOWByService, ThreatLOWFreq = as.numeric(ThreatLOWFreq))
LongThreatLOWByService<-LongThreatLOWByService[which(LongThreatLOWByService$ThreatLOWFreq>1),]

LongThreatLOWByService$INTENSITY<-c("LOW")
colnames(LongThreatLOWByService)<-c("LongService", "LongThreat", "ThreatFreq", "INTENSITY")


LongThreatByServiceINTENSITY<-rbind(LongThreatHIGHByService,LongThreatMEDIUMByService,LongThreatLOWByService)

png(file = path2temp %+% "ThreatByServiceINTENSITY_positive.png")
treemap(LongThreatByServiceINTENSITY,c("LongService","INTENSITY"),vSize="ThreatFreq")
dev.off()


### Second: only negative impacts

N2000Impact4<-subset(N2000Impact2,N2000Impact2$ThreatWithService!="NA")
N2000Impact4<-subset(N2000Impact4,N2000Impact4$IMPACT_TYPE=="N") # N for negative
N2000Impact4_HIGH<-subset(N2000Impact4,N2000Impact4$INTENSITY=="HIGH")
N2000Impact4_MEDIUM<-subset(N2000Impact4,N2000Impact4$INTENSITY=="MEDIUM")
N2000Impact4_LOW<-subset(N2000Impact4,N2000Impact4$INTENSITY=="LOW")

# Calculate the number of services across the site network
SumThreatHIGHs<-as.matrix(table(N2000Impact4_HIGH$ThreatWithService))

# Using the mapping data, we can create a matrix that converts the number of
# each ThreatHIGH into the number of each service provided
ThreatHIGHByService<-matrix(ncol=19,nrow=nrow(SumThreatHIGHs))
colnames(ThreatHIGHByService)<-ServiceGroups
rownames(ThreatHIGHByService)<-rownames(SumThreatHIGHs)
# For each of the ThreatHIGHs, we specify the number of times that that ThreatHIGH indicates
# a service, using a matrix
for(x in 1:nrow(ThreatHIGHByService)){
  # Extract the rows from the mapping table that contain the services associated with those ThreatHIGHs
  SiteThreatHIGHsMapped<-subset(MappingData,MappingData$relation %in% rownames(SumThreatHIGHs)[x])
  # For each service group, check whether the site had a ThreatHIGH that indicates that service is present
  for(y in 1:ncol(ThreatHIGHByService)){
    # The cells in the matrix correspond to the frequency with which each ThreatHIGH leads to each
    # service. Note that this will mean "double counting", but that is unavoidable due to the
    # potential multiple services indicated by each ThreatHIGH.
    if("x" %in% SiteThreatHIGHsMapped[,10+y]) {ThreatHIGHByService[x,y]<-SumThreatHIGHs[x]} else {ThreatHIGHByService[x,y]<-0}
  }
}

# Rearrange the "wide" matrix into a "long" data frame for use in the treemap() function
LongThreatHIGHByService<-matrix(nrow=ncol(ThreatHIGHByService)*nrow(ThreatHIGHByService),ncol=3)
ThreatHIGHFreq<-as.vector(ThreatHIGHByService)
LongThreatHIGH<-rep(rownames(ThreatHIGHByService),19)
LongServiceHIGH<-rep(colnames(ThreatHIGHByService),each=nrow(ThreatHIGHByService))

# Modify the dataframe so that the formats are correct for treemap()
LongThreatHIGHByService<-as.data.frame(cbind(LongServiceHIGH,LongThreatHIGH,ThreatHIGHFreq=as.vector(as.numeric(ThreatHIGHFreq))))
LongThreatHIGHByService<-transform(LongThreatHIGHByService, ThreatHIGHFreq = as.numeric(ThreatHIGHFreq))
LongThreatHIGHByService<-LongThreatHIGHByService[which(LongThreatHIGHByService$ThreatHIGHFreq>1),]

LongThreatHIGHByService$INTENSITY<-c("HIGH")
colnames(LongThreatHIGHByService)<-c("LongService", "LongThreat", "ThreatFreq", "INTENSITY")


# Calculate the number of services across the site network
SumThreatMEDIUMs<-as.matrix(table(N2000Impact4_MEDIUM$ThreatWithService))

# Using the mapping data, we can create a matrix that converts the number of
# each ThreatMEDIUM into the number of each service provided
ThreatMEDIUMByService<-matrix(ncol=19,nrow=nrow(SumThreatMEDIUMs))
colnames(ThreatMEDIUMByService)<-ServiceGroups
rownames(ThreatMEDIUMByService)<-rownames(SumThreatMEDIUMs)
# For each of the ThreatMEDIUMs, we specify the number of times that that ThreatMEDIUM indicates
# a service, using a matrix
for(x in 1:nrow(ThreatMEDIUMByService)){
  # Extract the rows from the mapping table that contain the services associated with those ThreatMEDIUMs
  SiteThreatMEDIUMsMapped<-subset(MappingData,MappingData$relation %in% rownames(SumThreatMEDIUMs)[x])
  # For each service group, check whether the site had a ThreatMEDIUM that indicates that service is present
  for(y in 1:ncol(ThreatMEDIUMByService)){
    # The cells in the matrix correspond to the frequency with which each ThreatMEDIUM leads to each
    # service. Note that this will mean "double counting", but that is unavoidable due to the
    # potential multiple services indicated by each ThreatMEDIUM.
    if("x" %in% SiteThreatMEDIUMsMapped[,10+y]) {ThreatMEDIUMByService[x,y]<-SumThreatMEDIUMs[x]} else {ThreatMEDIUMByService[x,y]<-0}
  }
}

# Rearrange the "wide" matrix into a "long" data frame for use in the treemap() function
LongThreatMEDIUMByService<-matrix(nrow=ncol(ThreatMEDIUMByService)*nrow(ThreatMEDIUMByService),ncol=3)
ThreatMEDIUMFreq<-as.vector(ThreatMEDIUMByService)
LongThreatMEDIUM<-rep(rownames(ThreatMEDIUMByService),19)
LongServiceMEDIUM<-rep(colnames(ThreatMEDIUMByService),each=nrow(ThreatMEDIUMByService))

# Modify the dataframe so that the formats are correct for treemap()
LongThreatMEDIUMByService<-as.data.frame(cbind(LongServiceMEDIUM,LongThreatMEDIUM,ThreatMEDIUMFreq=as.vector(as.numeric(ThreatMEDIUMFreq))))
LongThreatMEDIUMByService<-transform(LongThreatMEDIUMByService, ThreatMEDIUMFreq = as.numeric(ThreatMEDIUMFreq))
LongThreatMEDIUMByService<-LongThreatMEDIUMByService[which(LongThreatMEDIUMByService$ThreatMEDIUMFreq>1),]

LongThreatMEDIUMByService$INTENSITY<-c("MEDIUM")
colnames(LongThreatMEDIUMByService)<-c("LongService", "LongThreat", "ThreatFreq", "INTENSITY")

# Calculate the number of services across the site network
SumThreatLOWs<-as.matrix(table(N2000Impact4_LOW$ThreatWithService))

# Using the mapping data, we can create a matrix that converts the number of
# each ThreatLOW into the number of each service provided
ThreatLOWByService<-matrix(ncol=19,nrow=nrow(SumThreatLOWs))
colnames(ThreatLOWByService)<-ServiceGroups
rownames(ThreatLOWByService)<-rownames(SumThreatLOWs)
# For each of the ThreatLOWs, we specify the number of times that that ThreatLOW indicates
# a service, using a matrix
for(x in 1:nrow(ThreatLOWByService)){
  # Extract the rows from the mapping table that contain the services associated with those ThreatLOWs
  SiteThreatLOWsMapped<-subset(MappingData,MappingData$relation %in% rownames(SumThreatLOWs)[x])
  # For each service group, check whether the site had a ThreatLOW that indicates that service is present
  for(y in 1:ncol(ThreatLOWByService)){
    # The cells in the matrix correspond to the frequency with which each ThreatLOW leads to each
    # service. Note that this will mean "double counting", but that is unavoidable due to the
    # potential multiple services indicated by each ThreatLOW.
    if("x" %in% SiteThreatLOWsMapped[,10+y]) {ThreatLOWByService[x,y]<-SumThreatLOWs[x]} else {ThreatLOWByService[x,y]<-0}
  }
}


# Rearrange the "wide" matrix into a "long" data frame for use in the treemap() function
LongThreatLOWByService<-matrix(nrow=ncol(ThreatLOWByService)*nrow(ThreatLOWByService),ncol=3)
ThreatLOWFreq<-as.vector(ThreatLOWByService)
LongThreatLOW<-rep(rownames(ThreatLOWByService),19)
LongServiceLOW<-rep(colnames(ThreatLOWByService),each=nrow(ThreatLOWByService))

# Modify the dataframe so that the formats are correct for treemap()
LongThreatLOWByService<-as.data.frame(cbind(LongServiceLOW,LongThreatLOW,ThreatLOWFreq=as.vector(as.numeric(ThreatLOWFreq))))
LongThreatLOWByService<-transform(LongThreatLOWByService, ThreatLOWFreq = as.numeric(ThreatLOWFreq))
LongThreatLOWByService<-LongThreatLOWByService[which(LongThreatLOWByService$ThreatLOWFreq>1),]

LongThreatLOWByService$INTENSITY<-c("LOW")
colnames(LongThreatLOWByService)<-c("LongService", "LongThreat", "ThreatFreq", "INTENSITY")


LongThreatByServiceINTENSITY<-rbind(LongThreatHIGHByService,LongThreatMEDIUMByService,LongThreatLOWByService)

png(file = path2temp %+% "ThreatByServiceINTENSITY_negative.png")
treemap(LongThreatByServiceINTENSITY,c("LongService","INTENSITY"),vSize="ThreatFreq")
dev.off()

barplot(LongThreatByServiceINTENSITY,c("LongService","INTENSITY"),vSize="ThreatFreq")
