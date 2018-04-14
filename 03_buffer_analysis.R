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
  subset <- Natura2000_shape_all[as.character(Natura2000_shape_all$SITECODE)==as.character(overlapSPA)[i],]
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
#subset<-Natura2000_shape_all[1:10,]
subset<-Natura2000_shape_all
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
pb <- txtProgressBar(min = 0, max = length(subset), style = 3) # produces a progress bar
for(i in 1:length(subset)){
  myBuffer[[i]]<-outerBuffer(x=subset[i,],d=10000)
  setTxtProgressBar(pb, i)
}
close(pb)

saveRDS(myBuffer, file = "myBuffer.rds")


#plot(ACT)
plot(myBuffer[[4]], col="red")

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

saveRDS(results, file = "results_buffer.rds")



###############################################################################
#                   DATA CLEANING                                             #
###############################################################################

# # get rid of NAs and compute class proportions
# 
# ACT_red<-list()
# area_red<-list()
# 
# for (i in 1:total){
#   if(is.null(ACT_ext[[i]][[1]])==TRUE) {
#     ACT_red[[i]]<-NA  
#     area_red[[i]]<-NA
#   }else{
#     if(isTRUE(unique(is.na(as.data.frame(ACT_ext[[i]])$value)))==TRUE){
#       ACT_red[[i]]<-NA  
#       area_red[[i]]<-NA
#     }else{
#       f.correct<-1/sum(as.data.frame(ACT_ext[[i]])[!is.na(as.data.frame(ACT_ext[[i]])[,1]),2])
#       reduced.na<-as.data.frame(ACT_ext[[i]])[!is.na(as.data.frame(ACT_ext[[i]])[,1]),]
#       reduced.na[,2]<-reduced.na[,2]*f.correct
#       ACT_red[[i]]<-reduced.na
#       area_red[[i]]<-area[[i]]*sum(as.data.frame(ACT_ext[[i]])[!is.na(as.data.frame(ACT_ext[[i]])[,1]),2])
#     }
#   }    
# }   
# 


# # create a dataframe for presenting the distribution of SPA-area per ACT
# results <- as.data.frame(matrix(NA,length(ACT_red),20))
# colnames(results) <- c("SITECODE","area",paste("ACT",c(1:17),sep=""),"count")
# 
# for(k in 1:length(ACT_red)){
#   results[k,1]<-as.character(overlapSPA)[k]
#   results[k,2]<-area_red[[k]]
#   
#   if(is.na(ACT_red[[k]])==TRUE){
#     results[k,3:19]<-NA
#   } else{
#     test.sum<-cbind(aggregate(weight~value,sum,data=ACT_red[[k]]),table(ACT_red[[k]]$value))
#     results[k,test.sum$value+2]<-test.sum$weight
#     results[k,20]<-sum(test.sum$Freq)
#     
#   }
#   print(k)
# }
# 
# 
# 
# results[is.na(results)]<-0
# results2 <- results[which(results$count>5),]

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

bird<-bird[1:511,1:3]
colnames(bird)<-c("SPECIESCODE","migration","preference")
#bird <- bird[which(bird$migration=="mainly resident"),]

tabFinal <- merge(tabFinal,bird, by="SPECIESCODE")
#table(bird$SPECIESCODE)

#which(table(bird$SPECIESCODE)>1)

#save.image(file = "FAWKES.RData")



saveRDS(tabFinal, file = "tabFinal_buffer.rds")


############################################################################
### 
############################################################################


dat <- data.frame(tabFinal[,c(1:6,24)],
                  Conversion = rowSums(tabFinal[,18:22]),
                  Intens = rowSums(tabFinal[,c(7:10,17)]),
                  De_intens = rowSums(tabFinal[,11:16]),
                  Stabil = c(tabFinal[,23]))

tab <- merge(dat,bird, by="SPECIESCODE")

for (i in 1:length(dat$SPECIESCODE)){
  
  dat$ACT_dom[i]<-colnames(dat[i,8:11])[max.col(dat[i,8:11])]
  
}

saveRDS(dat, file = "dat_buffer.rds")
saveRDS(tab, file = "tab_buffer.rds")

# 
# 
# bird2<-reshape(aggregate (dat$ACT_dom, list(dat$SPECIESCODE,dat$ACT_dom), FUN=length), v.names="x", timevar="Group.2", idvar="Group.1", direction="wide")
# colnames(bird2)<-c("SPECIESCODE","Conversion", "De_intens" ,"Intens" ,"Stabil")
# bird<-merge(bird,bird2, by="SPECIESCODE")
# 
# colnames(bird)<-c("SPECIESCODE", "migration"   ,"preference","Conversion", "De_intens" ,"Intens" ,"Stabil")
# 
# for (i in 1:length(bird$SPECIESCODE)){
#   
#   bird$ACT_dom_dom[i]<-colnames(bird[i,4:7])[max.col(bird[i,4:7])]
#   
# }


############################################################################
### Derichlet-Regression
############################################################################

### aggregation of data on either site or species level (both offer alternative ways to interpret the data)

### Aggregate on site level. We'll keep 1 entry per site giving us the proportion of Levers trajectories ("int","ext","cc","stab") within each site as well as the proportion of conservation classes of ALL birds within each site (ABC) + area of site and levers pixel count

sites<-unique(tab$SITECODE)
mat_sites<-as.data.frame(matrix(NA,length(sites), 9))
colnames(mat_sites)<-c("A","B","C","int","ext","cc","stab","area", "count")
for(j in 1:length(sites)){
  sub<-subset(tab,tab$SITECODE==sites[j])
  prop.Cstat<-table(sub$CONSERVATION)/sum(table(sub$CONSERVATION))
  mat_sites[j,1:3]<-prop.Cstat
  mat_sites[j,4:7]<-sub[1,8:11]
  mat_sites[j,8]<-sub[1,6]
  mat_sites[j,9]<-sub[1,7] # count
}

### Aggregate on species level. We'll keep 1 entry per species giving us the proportion of Levers trajectories ("int","ext","cc","stab") the species encounters within all sites. This is done as a two-step aggregation: first for each site the species occurs in the majority trajectory is selected and second, these majority trajectories are summarized as proportions across all sites the species occurs in. In addition we have the proportion of conservation classes of that species within all sites (ABC).

species<-unique(tab$SPECIESCODE)
mat_bird<-as.data.frame(matrix(NA,length(species), 10))
colnames(mat_bird)<-c("A","B","C","int","ext","cc","stab", "SPECIESCODE","migration","preference")
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
  mat_bird[j,9]<-as.character(sub[1,12])
  mat_bird[j,10]<-as.character(sub[1,13])
}

#remove all birds that have no levers data at all (~50)

mat_bird<-mat_bird[rowSums((mat_bird[,4:7])==0)<=0,]

### Site-based Drichlet analysis

ABC_status <- DR_data(mat_sites[, 1:3])

plot(ABC_status, cex = 0.5, a2d = list(colored = FALSE, c.grid = FALSE))

plot(rep(mat_sites$int, 3), as.numeric(ABC_status), pch = 21,cex=0.2, bg = rep(c("#E495A5", "#86B875", "#7DB0DD"), each = 39), xlab = "intensity", ylab = "Proportion",ylim = 0:1)

first_model <- DirichReg(ABC_status ~ mat_sites$int+mat_sites$cc+mat_sites$ext)
summary(first_model)

first_model <- DirichReg(ABC_status ~ mat_sites$int)

plot(rep(mat_sites$int, 3), as.numeric(ABC_status), pch = 21,cex=0.2, bg = rep(c("#E495A5", "#86B875", "#7DB0DD"), each = 39), xlab = "intensity", ylab = "Proportion",ylim = 0:1)

Xnew <- data.frame(int = seq(min(mat_sites$int), max(mat_sites$int),length.out = 100))
for (i in 1:3) lines(cbind(Xnew, predict(first_model, Xnew)[, i]), col = c("#E495A5", "#86B875", "#7DB0DD")[i], lwd = 2)
# 

pre<-predict(first_model)

plot(pre[,1]~mat_sites$int)
plot(pre[,2]~mat_sites$int)
plot(pre[,3]~mat_sites$int)

boxplot(mat_sites[,1:3])

### Species-based Drichlet analysis

for (i in 1:length(mat_bird$SPECIESCODE)){
  
  mat_bird$ABC_dom[i]<-colnames(mat_bird[i,1:3])[max.col(mat_bird[i,1:3])]
  
}

for (i in 1:length(tab$SPECIESCODE)){
  
  tab$ACT_dom[i]<-colnames(tab[i,8:11])[max.col(tab[i,8:11])]
  
}







dat$cc_area<-dat$area*dat$Conversion
dat$int_area<-dat$area*dat$Intens
dat$ext_area<-dat$area*dat$De_intens
dat$stab_area<-dat$area*dat$Stabil

one_bird<-dat[dat$SPECIESCODE=="A030",]
#head(one_bird)
one_bird$country<-factor(substr((one_bird$SITECODE),1,2))
#lapply(one_bird[, c("CONSERVATION","cc_area")], table)

one_bird<-(one_bird[(rowSums((one_bird[,8:10]))==0)<=0,])

m<-polr(formula=CONSERVATION~ACT_dom+area, data=one_bird,Hess=TRUE)
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(m)
confint.default(m)
exp(coef(m))
exp(cbind(OR = coef(m), ci))

summary(factor(one_bird$ACT_dom))

ggplot(one_bird, aes(x = CONSERVATION, y = int_area)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(ACT_dom~country, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

s <- with(one_bird, summary(as.numeric(CONSERVATION) ~ ACT_dom + area, fun=sf))

glm(I(as.numeric(CONSERVATION) >= 2) ~ ACT_dom, family="binomial", data = one_bird)
glm(I(as.numeric(CONSERVATION) >= 3) ~ ACT_dom, family="binomial", data = one_bird)

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))










farmland_birds <- c("A247","A110", "A255", "A257", "A025", "A133", "A243","A366", "A031","A348",  "A377", "A376", "A379",
                    "A382", "A096","A244","A245", "A251", "A338", "A339", "A341","A156","A242", "A383","A260","A278","A356",
                    "A112","A357", "A275","A276", "A361","A210", "A352", "A351", "A309",  "A128", "A232", "A142")
trends_farmland <- read.delim("trends_farmland.txt", header=FALSE)
trends_farmland <- cbind(farmland_birds, trends_farmland)
colnames(trends_farmland) <- c("Speciescode", "Speciesname", "Baseyear", "Trend")
farmland_trends <- as.vector(trends_farmland$Trend)


forest_birds <- c("A086", "A256", "A263", "A104", "A365", "A335", "A334", "A373", "A207", "A454", "A238", "A240",
                  "A236", "A542", "A321", "A332", "A342", "A344", "A328", "A327", "A326", "A325",
                  "A274", "A313", "A315", "A314", "A234", "A372", "A318", "A317", "A362", "A332", "A165", "A287")
trends_forest <- read.delim("trends_forest.txt", header=FALSE)

trends_forest <- cbind(forest_birds, trends_forest)
colnames(trends_forest) <- c("Speciescode", "Speciesname", "Baseyear", "Trend")
forest_trends <- as.vector(trends_forest$Trend)

trends_all <- read.delim("trends_all.txt", header=FALSE)
colnames(trends_all) <- c("SPECIESCODE", "Speciesname", "Baseyear", "Trend")

trends_farmland$type<-"farmland"
trends_forest$type<-"forest"
trends<-rbind(trends_farmland,trends_forest)

colnames(trends)[1]<-"SPECIESCODE"

mat_bird$SPECIESCODE<-factor(mat_bird$SPECIESCODE)
mat_bird_trends<-merge(mat_bird,trends,by="SPECIESCODE")

summary(lme(C~int+cc+Trend, random= ~ 1|SPECIESCODE,data=mat_bird_trends))
boxplot(A~Trend, data=mat_bird_trends)













############################################################################
### 6. correlations between Conservation status and ACTs
############################################################################


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




