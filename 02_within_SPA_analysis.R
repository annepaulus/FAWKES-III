############################################################################
### 02.1. overlay of SPAs with ACTs
############################################################################

#plot(ACT)
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
### 6. correlations between Conservation status and ACTs
############################################################################


dat <- data.frame(tabFinal[,c(1:6,24)],
                  Conversion = rowSums(tabFinal[,18:22]),
                  Intens = rowSums(tabFinal[,c(7:10,17)]),
                  De_intens = rowSums(tabFinal[,11:16]),
                  Stabil = c(tabFinal[,23]))

tab <- merge(dat,bird, by="SPECIESCODE")
View(dat)



##################### Lisanne & Ron Derichlet-Regression ######################################

sites<-unique(tab$SITECODE)
mat<-as.data.frame(matrix(NA,length(sites), 9))
colnames(mat)<-c("A","B","C","int","ext","cc","stab","area", "count")
for(j in 1:length(sites)){
  sub<-subset(tab,tab$SITECODE==sites[j])
  prop.Cstat<-table(sub$CONSERVATION)/sum(table(sub$CONSERVATION))
  mat[j,1:3]<-prop.Cstat
  mat[j,4:7]<-sub[1,8:11]
  mat[j,8]<-sub[1,6]
  mat[j,9]<-sub[1,7] # count
}




species<-unique(tab$SPECIESCODE)
mat_bird<-as.data.frame(matrix(NA,length(species), 8))
colnames(mat_bird)<-c("A","B","C","int","ext","cc","stab", "SPECIESCODE")
for(j in 1:length(species)){
  sub<-subset(tab,tab$SPECIESCODE==species[j])
  
  lu_agg<-factor()
  levels(lu_agg)<-c("Conversion","Stabil","De_intens","Intens")
  
  for(k in 1:length(sub$SPECIESCODE)){
    lu_agg[k]<-names(which.max(sub[k,8:11]))
  }
  
  mat_bird[j,4:7]<-table(lu_agg)/sum(table(lu_agg))
  
  prop.Cstat<-table(sub$CONSERVATION)/sum(table(sub$CONSERVATION))
  mat_bird[j,1:3]<-prop.Cstat
  mat_bird[j,4:7]<-sub[1,8:11]
  mat_bird[j,8]<-as.character(sub[1,1])
}



mat

require("DirichletReg")



ABC_status <- DR_data(mat[, 1:3])

plot(ABC_status, cex = 0.5, a2d = list(colored = FALSE, c.grid = FALSE))

plot(rep(mat$int, 3), as.numeric(ABC_status), pch = 21,cex=0.2, bg = rep(c("#E495A5", "#86B875", "#7DB0DD"), each = 39), xlab = "intensity", ylab = "Proportion",ylim = 0:1)

first_model <- DirichReg(ABC_status ~ mat$int+mat$cc+mat$ext)

first_model <- DirichReg(ABC_status ~ mat$int)

plot(rep(mat$int, 3), as.numeric(ABC_status), pch = 21,cex=0.2, bg = rep(c("#E495A5", "#86B875", "#7DB0DD"), each = 39), xlab = "intensity", ylab = "Proportion",ylim = 0:1)

Xnew <- data.frame(int = seq(min(mat$int), max(mat$int),length.out = 100))
for (i in 1:3) lines(cbind(Xnew, predict(first_model, Xnew)[, i]), col = c("#E495A5", "#86B875", "#7DB0DD")[i], lwd = 2)
# 

pre<-predict(first_model)

plot(pre[,1]~mat$int)
plot(pre[,2]~mat$int)
plot(pre[,3]~mat$int)

boxplot(mat[,1:3])

# modA<-lm(A~int+ext+cc+stab+area,data=mat)
# summary(modA)
# 
# plot(C~ext,data=mat)







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



head(dat)

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




