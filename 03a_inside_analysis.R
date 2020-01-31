#########################################
#### Inside SPA data restructuring and Ordered logistic regression analysis for habitat Conservation status (ABC) against levers trajectories (4 summarized)
#### Using the extracted values from Levers in 10km buffers around all SPAs
#### inspiration for code found at https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
#### Authors: RR, MB, TV ...
#########################################

#########################################
### Part I: data preparation
#########################################

# clean SPA shape
Natura2000_shape_all <- Natura2000_shape[which(as.character(sitecodes)%in%as.character(N2000SPASiteCodes)==T),]

# set number of runs by length of SPA list
total <- length(overlapSPA)       #5569 SPAs

# create lists to paste extratced values in
ACT_ext<-list()
LSA_ext<-list()
area<-list()
perimeter<-list()

# do the actual extraction in a loop taking each SPA shape

# things to consider:
# normalizeWeights==TRUE --> der Anteil der extrahierten Pixelfläche an der Gesamtfläche des Polygones
# Problem hierbei: falls man im Nachhinein NAs rauswirft stimmen die Verhältnisse nicht mehr
# normalizeWeights==FALSE --> der Anteil des extrahierten Pixels 

pb <- txtProgressBar(min = 0, max = total, style = 3) # produces a progress bar
for (i in 1:total){
  subset <- Natura2000_shape_all[as.character(Natura2000_shape_all$SITECODE)==as.character(overlapSPA)[i],]
  ACT_ext[[i]]<-extract(ACT,subset,weights=TRUE,normalizeWeights=TRUE) # extracts ACT rasterdata for each sitecode
  area[[i]]<-gArea(subset)/1e+06 # extracts area data for each sitecode
  perimeter[[i]]<-gLength(subset)/1e+03  # extracts perimeter data for each sitecode
  setTxtProgressBar(pb, i)
}
close(pb)

# create lists to paste cleaned data into
ACT_red<-list()
area_red<-list()

# clean data
for (i in 1:length(ACT_ext)){
  if(is.null(ACT_ext[[i]][[1]])==TRUE) { # set empty subsets to NA
    ACT_red[[i]]<-NA  
    area_red[[i]]<-NA
  }else{
    if(isTRUE(unique(is.na(as.data.frame(ACT_ext[[i]])$value)))==TRUE){ # set exmpty extraction values to NA
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
results_inside <- as.data.frame(matrix(NA,length(ACT_red),20))
colnames(results_inside) <- c("SITECODE","area",paste("ACT",c(1:17),sep=""),"count")

for(k in 1:length(ACT_red)){
  results_inside[k,1]<-as.character(overlapSPA)[k]
  results_inside[k,2]<-area_red[[k]]
  
  if(is.na(ACT_red[[k]])==TRUE){
    results_inside[k,3:19]<-NA
  } else{
    test.sum<-cbind(aggregate(weight~value,sum,data=ACT_red[[k]]),table(ACT_red[[k]]$value))
    results_inside[k,test.sum$value+2]<-test.sum$weight
    results_inside[k,20]<-sum(test.sum$Freq)
    
  }
  print(k)
}

results_inside[is.na(results_inside)]<-0

saveRDS(results_inside, file = "results_inside.rds")

#ACT_ext[[1]]

############################################################################
###       relate conservation status with ACT via bird species           ###
############################################################################

birds<-(unique(mydata$SPECIESNAME))
length(unique(mydata$SPECIESCODE)) # 530 bird species

#for each species, link the ACTs (1 to 17) with the Conservation Status (for all sites

tabFinal_inside <- merge(mydata,results_inside, by="SITECODE")
tabFinal_inside<-tabFinal_inside[!duplicated(tabFinal_inside), ]

tabFinal_inside[is.na(tabFinal_inside)] <- 0

#remove rows completely 0
#test<-apply(tabFinal_inside[,7:23], MARGIN=1,FUN=sum)
#zero<-which(test==0)
#tabFinal_inside<-tabFinal_inside[-zero,]

#CStatus ordered factor
CStatus<-droplevels(as.factor(tabFinal_inside$CONSERVATION))
CStatus <- factor(CStatus,levels = c("A","B","C"),ordered = TRUE)
tabFinal_inside$CONSERVATION<-CStatus

#tabFinal_inside<-tabFinal_inside[-which(tabFinal_inside$SPECIESNAME=="--NULL--"),]
tabFinal_inside$SPECIESNAME<-droplevels(tabFinal_inside$SPECIESNAME)

# bird<-bird[1:511,1:3]
# colnames(bird)<-c("SPECIESCODE","migration","preference")
#bird <- bird[which(bird$migration=="mainly resident"),]

tabFinal_inside <- merge(tabFinal_inside,bird, by="SPECIESCODE")
#table(bird$SPECIESCODE)

#which(table(bird$SPECIESCODE)>1)

#save.image(file = "FAWKES.RData")



saveRDS(tabFinal_inside, file = "tabFinal_inside.rds")

############################################################################
### 
############################################################################


dat_inside <- data.frame(tabFinal_inside[,c(1:6,24)],
                  Conversion = rowSums(tabFinal_inside[,18:22]),
                  Intens = rowSums(tabFinal_inside[,c(7:10,17)]),
                  De_intens = rowSums(tabFinal_inside[,11:16]),
                  Stabil = c(tabFinal_inside[,23]),
                  Int_crop = rowSums(tabFinal_inside[,7:10]),
                  Ext_crop = rowSums(tabFinal_inside[,11:13]),
                  Ext_pasture = rowSums(tabFinal_inside[,14:16]),
                  Int_wood = c(tabFinal_inside[,17]),
                  Cropland_loss = rowSums(tabFinal_inside[,20:21]),
                  Forest_gain = c(tabFinal_inside[,18]),
                  Forest_loss = c(tabFinal_inside[,19]),
                  Urban = c(tabFinal_inside[,22])
                  )

tab_inside <- merge(dat_inside,bird, by="SPECIESCODE")

for (i in 1:length(dat_inside$SPECIESCODE)){
  
  dat_inside$ACT_dom4[i]<-colnames(dat_inside[i,8:11])[max.col(dat_inside[i,8:11])]
  
}

for (i in 1:length(dat_inside$SPECIESCODE)){
  
  dat_inside$ACT_dom9[i]<-colnames(dat_inside[i,11:19])[max.col(dat_inside[i,11:19])]
  
}

saveRDS(dat_inside, file = "dat_inside.rds")
saveRDS(tab_inside, file = "tab_inside.rds")

#########################################
#### Ordered logistic regression analysis for habitat Conservation status (ABC) against levers trajectories (4 summarized)
#### This is using the extracted values from Levers in 10km buffers around all SPAs
#### inspiration for code found at https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
#### Authors: MB, TV ...
#########################################



#### re-calculate actual area, unit unknown ...
dat_inside$cc_area<-dat_inside$area*dat_inside$Conversion
dat_inside$int_area<-dat_inside$area*dat_inside$Intens
dat_inside$ext_area<-dat_inside$area*dat_inside$De_intens
dat_inside$stab_area<-dat_inside$area*dat_inside$Stabil

#### create new dataframe, each species in one row
bird_OR_results <- data.frame(unique(droplevels(dat_inside$SPECIESCODE)),
                              OR_int=seq(1:486),
                              p_int=seq(1:486),
                              LCI_OR_int=seq(1:486),
                              UCI_OR_int=seq(1:486),
                              OR_ext=seq(1:486),
                              p_ext=seq(1:486),
                              LCI_OR_ext=seq(1:486),
                              UCI_OR_ext=seq(1:486),
                              OR_cc=seq(1:486),
                              p_cc=seq(1:486),
                              LCI_OR_cc=seq(1:486),
                              UCI_OR_cc=seq(1:486),
                              OR_stab=seq(1:486),
                              p_stab=seq(1:486),
                              LCI_OR_stab=seq(1:486),
                              UCI_OR_stab=seq(1:486),
                              
                              OR_Int_crop=seq(1:486),
                              p_Int_crop=seq(1:486),
                              LCI_OR_Int_crop=seq(1:486),
                              UCI_OR_Int_crop=seq(1:486),
                              
                              OR_Ext_crop=seq(1:486),
                              p_Ext_crop=seq(1:486),
                              LCI_OR_Ext_crop=seq(1:486),
                              UCI_OR_Ext_crop=seq(1:486),
                              
                              OR_Ext_pasture=seq(1:486),
                              p_Ext_pasture=seq(1:486),
                              LCI_OR_Ext_pasture=seq(1:486),
                              UCI_OR_Ext_pasture=seq(1:486),
                              
                              OR_Int_wood=seq(1:486),
                              p_Int_wood=seq(1:486),
                              LCI_OR_Int_wood=seq(1:486),
                              UCI_OR_Int_wood=seq(1:486),
                              
                              OR_Cropland_loss=seq(1:486),
                              p_Cropland_loss=seq(1:486),
                              LCI_OR_Cropland_loss=seq(1:486),
                              UCI_OR_Cropland_loss=seq(1:486),
                              
                              OR_Forest_gain=seq(1:486),
                              p_Forest_gain=seq(1:486),
                              LCI_OR_Forest_gain=seq(1:486),
                              UCI_OR_Forest_gain=seq(1:486),
                              
                              OR_Forest_loss=seq(1:486),
                              p_Forest_loss=seq(1:486),
                              LCI_OR_Forest_loss=seq(1:486),
                              UCI_OR_Forest_loss=seq(1:486),
                              
                              OR_Urban=seq(1:486),
                              p_Urban=seq(1:486),
                              LCI_OR_Urban=seq(1:486),
                              UCI_OR_Urban=seq(1:486),
                          
                              stringsAsFactors=FALSE)
names(bird_OR_results)[1]<-"SPECIESCODE"

#### loop running individual models for all bird species

for (i in 1:length(bird_OR_results$SPECIESCODE)){
  one_bird<-dat_inside[dat_inside$SPECIESCODE==paste(bird_OR_results$SPECIESCODE[i]),]
  one_bird<-(one_bird[(rowSums((one_bird[,11:19]))==0)<=0,])
  
  # skip all species that occur in less than 5 SPAs
  if (nrow(one_bird) < 5){
    bird_OR_results[i,2:9]<-NA
  } else {
    
    #### Intens models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Intens+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
    ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,2]<-res[1,1]
    bird_OR_results[i,4]<-res[1,2]
    bird_OR_results[i,5]<-res[1,3]
    bird_OR_results[i,3]<-ctable[1,4]
    
    #### De_intens models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~De_intens+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
    ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,6]<-res[1,1]
    bird_OR_results[i,8]<-res[1,2]
    bird_OR_results[i,9]<-res[1,3]
    bird_OR_results[i,7]<-ctable[1,4]
    
    #### Conversion models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Conversion+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
      ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,10]<-res[1,1]
    bird_OR_results[i,12]<-res[1,2]
    bird_OR_results[i,13]<-res[1,3]
    bird_OR_results[i,11]<-ctable[1,4]
    
    #### Stabil models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Stabil+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
    ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,14]<-res[1,1]
    bird_OR_results[i,16]<-res[1,2]
    bird_OR_results[i,17]<-res[1,3]
    bird_OR_results[i,15]<-ctable[1,4]
    
    #### Int_crop models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Int_crop+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
    ctable <- coef(summary(m))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,18]<-res[1,1]
    bird_OR_results[i,20]<-res[1,2]
    bird_OR_results[i,21]<-res[1,3]
    bird_OR_results[i,19]<-ctable[1,4]
    
    #### Ext_crop models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Ext_crop+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
    ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,22]<-res[1,1]
    bird_OR_results[i,24]<-res[1,2]
    bird_OR_results[i,25]<-res[1,3]
    bird_OR_results[i,23]<-ctable[1,4]
    
    #### Ext_pasture models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Ext_pasture+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
          ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,26]<-res[1,1]
    bird_OR_results[i,28]<-res[1,2]
    bird_OR_results[i,29]<-res[1,3]
    bird_OR_results[i,27]<-ctable[1,4]
    
    #### Int_wood models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Int_wood+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
    ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,30]<-res[1,1]
    bird_OR_results[i,32]<-res[1,2]
    bird_OR_results[i,33]<-res[1,3]
    bird_OR_results[i,31]<-ctable[1,4]
    
    #### Cropland_loss models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Cropland_loss+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
      ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,34]<-res[1,1]
    bird_OR_results[i,36]<-res[1,2]
    bird_OR_results[i,37]<-res[1,3]
    bird_OR_results[i,35]<-ctable[1,4]
    
    #### Forest_gain models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Forest_gain+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
      ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,38]<-res[1,1]
    bird_OR_results[i,40]<-res[1,2]
    bird_OR_results[i,41]<-res[1,3]
    bird_OR_results[i,39]<-ctable[1,4]
    
    #### Forest_loss models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Forest_loss+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
      ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,42]<-res[1,1]
    bird_OR_results[i,44]<-res[1,2]
    bird_OR_results[i,45]<-res[1,3]
    bird_OR_results[i,43]<-ctable[1,4]
    
    #### Urban models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Urban+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
      ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    res<-exp(cbind(OR = coef(m), ci))
    bird_OR_results[i,46]<-res[1,1]
    bird_OR_results[i,48]<-res[1,2]
    bird_OR_results[i,49]<-res[1,3]
    bird_OR_results[i,47]<-ctable[1,4]
    
  }
  print(i)
}

saveRDS(bird_OR_results, file = "bird_OR_results_inside.rds")

bird_OR_results<-readRDS(file = "bird_OR_results_inside.rds")

#### results summary & plot

int_neg<-summary((bird_OR_results$OR_int<1 & bird_OR_results$p_int<0.05))
int_pos<-summary((bird_OR_results$OR_int>1 & bird_OR_results$p_int<0.05))

ext_neg<-summary((bird_OR_results$OR_ext<1 & bird_OR_results$p_ext<0.05))
ext_pos<-summary((bird_OR_results$OR_ext>1 & bird_OR_results$p_ext<0.05))

cc_neg<-summary((bird_OR_results$OR_cc<1 & bird_OR_results$p_cc<0.05))
cc_pos<-summary((bird_OR_results$OR_cc>1 & bird_OR_results$p_cc<0.05))

stab_neg<-summary((bird_OR_results$OR_stab<1 & bird_OR_results$p_stab<0.05))
stab_pos<-summary((bird_OR_results$OR_stab>1 & bird_OR_results$p_stab<0.05))

Int_crop_neg<-summary((bird_OR_results$OR_Int_crop<1 & bird_OR_results$p_Int_crop<0.05))
Int_crop_pos<-summary((bird_OR_results$OR_Int_crop>1 & bird_OR_results$p_Int_crop<0.05))

Ext_crop_neg<-summary((bird_OR_results$OR_Ext_crop<1 & bird_OR_results$p_Ext_crop<0.05))
Ext_crop_pos<-summary((bird_OR_results$OR_Ext_crop>1 & bird_OR_results$p_Ext_crop<0.05))

Ext_pasture_neg<-summary((bird_OR_results$OR_Ext_pasture<1 & bird_OR_results$p_Ext_pasture<0.05))
Ext_pasture_pos<-summary((bird_OR_results$OR_Ext_pasture>1 & bird_OR_results$p_Ext_pasture<0.05))

Int_wood_neg<-summary((bird_OR_results$OR_Int_wood<1 & bird_OR_results$p_Int_wood<0.05))
Int_wood_pos<-summary((bird_OR_results$OR_Int_wood>1 & bird_OR_results$p_Int_wood<0.05))

Cropland_loss_neg<-summary((bird_OR_results$OR_Cropland_loss<1 & bird_OR_results$p_Cropland_loss<0.05))
Cropland_loss_pos<-summary((bird_OR_results$OR_Cropland_loss>1 & bird_OR_results$p_Cropland_loss<0.05))

Forest_gain_neg<-summary((bird_OR_results$OR_Forest_gain<1 & bird_OR_results$p_Forest_gain<0.05))
Forest_gain_pos<-summary((bird_OR_results$OR_Forest_gain>1 & bird_OR_results$p_Forest_gain<0.05))

Forest_loss_neg<-summary((bird_OR_results$OR_Forest_loss<1 & bird_OR_results$p_Forest_loss<0.05))
Forest_loss_pos<-summary((bird_OR_results$OR_Forest_loss>1 & bird_OR_results$p_Forest_loss<0.05))

Urban_neg<-summary((bird_OR_results$OR_Urban<1 & bird_OR_results$p_Urban<0.05))
Urban_pos<-summary((bird_OR_results$OR_Urban>1 & bird_OR_results$p_Urban<0.05))

res_summary<-rbind(int_neg,int_pos,ext_neg,ext_pos,cc_neg,cc_pos,stab_neg,stab_pos)
res_summary<-rbind(Int_crop_neg,Int_crop_pos,Ext_crop_neg,Ext_crop_pos,Ext_pasture_neg,Ext_pasture_pos,Int_wood_neg,Int_wood_pos, Cropland_loss_neg,Cropland_loss_pos,Forest_gain_neg,Forest_gain_pos,Forest_loss_neg,Forest_loss_pos,Urban_neg,Urban_pos,stab_neg,stab_pos)

res_summary<-res_summary[,-1]

barplot(as.numeric(res_summary[,2]),names.arg=c(rownames(res_summary)), col=c("red","green"), las=2)




bird_OR_results<-merge(bird_OR_results,bird, by = "SPECIESCODE")
bird_pref<-unique(bird_OR_results$preference)

for (i in 1:length(bird_pref)){
sub_bird<-bird_OR_results[bird_OR_results$preference==bird_pref[i],]

ext_neg<-summary((sub_bird$OR_ext<1 & sub_bird$p_ext<0.05))
ext_pos<-summary((sub_bird$OR_ext>1 & sub_bird$p_ext<0.05))

cc_neg<-summary((sub_bird$OR_cc<1 & sub_bird$p_cc<0.05))
cc_pos<-summary((sub_bird$OR_cc>1 & sub_bird$p_cc<0.05))

stab_neg<-summary((sub_bird$OR_stab<1 & sub_bird$p_stab<0.05))
stab_pos<-summary((sub_bird$OR_stab>1 & sub_bird$p_stab<0.05))

Int_crop_neg<-summary((sub_bird$OR_Int_crop<1 & sub_bird$p_Int_crop<0.05))
Int_crop_pos<-summary((sub_bird$OR_Int_crop>1 & sub_bird$p_Int_crop<0.05))

Ext_crop_neg<-summary((sub_bird$OR_Ext_crop<1 & sub_bird$p_Ext_crop<0.05))
Ext_crop_pos<-summary((sub_bird$OR_Ext_crop>1 & sub_bird$p_Ext_crop<0.05))

Ext_pasture_neg<-summary((sub_bird$OR_Ext_pasture<1 & sub_bird$p_Ext_pasture<0.05))
Ext_pasture_pos<-summary((sub_bird$OR_Ext_pasture>1 & sub_bird$p_Ext_pasture<0.05))

Int_wood_neg<-summary((sub_bird$OR_Int_wood<1 & sub_bird$p_Int_wood<0.05))
Int_wood_pos<-summary((sub_bird$OR_Int_wood>1 & sub_bird$p_Int_wood<0.05))

Cropland_loss_neg<-summary((sub_bird$OR_Cropland_loss<1 & sub_bird$p_Cropland_loss<0.05))
Cropland_loss_pos<-summary((sub_bird$OR_Cropland_loss>1 & sub_bird$p_Cropland_loss<0.05))

Forest_gain_neg<-summary((sub_bird$OR_Forest_gain<1 & sub_bird$p_Forest_gain<0.05))
Forest_gain_pos<-summary((sub_bird$OR_Forest_gain>1 & sub_bird$p_Forest_gain<0.05))

Forest_loss_neg<-summary((sub_bird$OR_Forest_loss<1 & sub_bird$p_Forest_loss<0.05))
Forest_loss_pos<-summary((sub_bird$OR_Forest_loss>1 & sub_bird$p_Forest_loss<0.05))

Urban_neg<-summary((sub_bird$OR_Urban<1 & sub_bird$p_Urban<0.05))
Urban_pos<-summary((sub_bird$OR_Urban>1 & sub_bird$p_Urban<0.05))

res_summary<-rbind(int_neg,int_pos,ext_neg,ext_pos,cc_neg,cc_pos,stab_neg,stab_pos)
res_summary<-rbind(Int_crop_neg,Int_crop_pos,Ext_crop_neg,Ext_crop_pos,Ext_pasture_neg,Ext_pasture_pos,Int_wood_neg,Int_wood_pos, Cropland_loss_neg,Cropland_loss_pos,Forest_gain_neg,Forest_gain_pos,Forest_loss_neg,Forest_loss_pos,Urban_neg,Urban_pos,stab_neg,stab_pos)

res_summary<-res_summary[,-1]

png(gsub("[[:punct:]]", " ", paste(bird_pref[i],"inside SPAs.png")))
par(mar=c(10,3,3,3))
barplot(as.numeric(res_summary[,2]),names.arg=c(rownames(res_summary)), col=c("red","green"), las=2, main=paste(bird_pref[i],"inside SPAs"))
dev.off()

}


##### Guy-inspired plot

pcols<-c(15,19,23,27,31,35,39,43,47)
plot_list = list()

for (i in 1:length(pcols)){

  sub<-bird_OR_results[bird_OR_results[pcols[i]]<0.05,]
  #sub<-sub[sub[pcols[i]+2]<10000,]
  #sub<-sub[sub[pcols[i]-1]>0.00001,]
  
  sub<-sub[order(sub[pcols[i]-1]),]
  sub$SPECIESCODE<-factor(sub$SPECIESCODE,levels=sub$SPECIESCODE)
  sub$SPECIESCODE<-droplevels(sub$SPECIESCODE)
  
  p = ggplot(sub[sub[pcols[i]-1]<10,], aes_string(x = colnames(sub[1]), y = colnames(sub[pcols[i]-1]))) +
    geom_point(size = 4) +
    geom_hline(yintercept=1)+
    ylim(0,25)+
    geom_errorbar(aes_string(ymax = colnames(sub[pcols[i]+1]), ymin = colnames(sub[pcols[i]+2])))
  
  # plot_list[[i]] = p
  ggsave(p, file=paste0("plot_", colnames(sub[pcols[i]-1]),".png"))
  
    }


### intensification
sub<-bird_OR_results[bird_OR_results$p_int<0.05,]
sub<-sub[sub$UCI_OR_int<10000,]
sub<-sub[sub$OR_int>0.00001,]

sub<-sub[order(sub$OR_int),]
sub$SPECIESCODE<-factor(sub$SPECIESCODE,levels=sub$SPECIESCODE)
sub$SPECIESCODE<-droplevels(sub$SPECIESCODE)

ggplot(sub[sub$OR_int<5,], aes(x = SPECIESCODE, y = OR_int)) +
  geom_point(size = 4) +
  geom_hline(yintercept=1)+
  geom_errorbar(aes(ymax = UCI_OR_int, ymin = LCI_OR_int))

### extensification

sub<-bird_OR_results[bird_OR_results$p_ext<0.05,]
sub<-sub[sub$UCI_OR_int<10000,]
sub<-sub[sub$OR_ext>0.00001,]

sub<-sub[order(sub$OR_ext),]
sub$SPECIESCODE<-factor(sub$SPECIESCODE,levels=sub$SPECIESCODE)
sub$SPECIESCODE<-droplevels(sub$SPECIESCODE)

ggplot(sub[sub$OR_ext<10,], aes(x = SPECIESCODE, y = OR_ext)) +
  geom_point(size = 4) +
  geom_hline(yintercept=1)+
  geom_errorbar(aes(ymax = UCI_OR_ext, ymin = LCI_OR_ext))


### conversion

sub<-bird_OR_results[bird_OR_results$p_cc<0.05,]
sub<-sub[sub$UCI_OR_int<10000,]
sub<-sub[sub$OR_cc>0.00001,]

sub<-sub[order(sub$OR_cc),]
sub$SPECIESCODE<-factor(sub$SPECIESCODE,levels=sub$SPECIESCODE)
sub$SPECIESCODE<-droplevels(sub$SPECIESCODE)

ggplot(sub[sub$OR_cc<10,], aes(x = SPECIESCODE, y = OR_cc)) +
  geom_point(size = 4) +
  geom_hline(yintercept=1)+
  geom_errorbar(aes(ymax = UCI_OR_cc, ymin = LCI_OR_cc))

### stable

sub<-bird_OR_results[bird_OR_results$p_stab<0.05,]
sub<-sub[sub$UCI_OR_int<10000,]
sub<-sub[sub$OR_stab>0.00001,]

sub<-sub[order(sub$OR_stab),]
sub$SPECIESCODE<-factor(sub$SPECIESCODE,levels=sub$SPECIESCODE)
sub$SPECIESCODE<-droplevels(sub$SPECIESCODE)

ggplot(sub[sub$OR_stab<10,], aes(x = SPECIESCODE, y = OR_stab)) +
  geom_point(size = 4) +
  geom_hline(yintercept=1)+
  geom_errorbar(aes(ymax = UCI_OR_stab, ymin = LCI_OR_stab))

