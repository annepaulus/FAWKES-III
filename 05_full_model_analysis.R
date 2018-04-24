############################################################################
### 5. full ordered logistic regression model
############################################################################


dat_buffer<-readRDS(file = "dat_inner_buffer.rds")
tab_buffer<-readRDS(file = "tab_inner_buffer.rds")

#########################################
#### Ordered logistic regression analysis for habitat Conservation status (ABC) against levers trajectories (4 summarized)
#### This is using the extracted values from Levers in 10km buffers around all SPAs
#### inspiration for code found at https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
#### Authors: MB, TV ...
#########################################



#### re-calculate actual area, unit unknown ...
dat_buffer$cc_area<-dat_buffer$area*dat_buffer$Conversion
dat_buffer$int_area<-dat_buffer$area*dat_buffer$Intens
dat_buffer$ext_area<-dat_buffer$area*dat_buffer$De_intens
dat_buffer$stab_area<-dat_buffer$area*dat_buffer$Stabil

#### create new dataframe, each species in one row
bird_OR_results <- data.frame(unique(droplevels(dat_buffer$SPECIESCODE)),
                              
                              OR_Int_crop=seq(1:486),
                              p_Int_crop=seq(1:486),
                              
                              OR_Ext_crop=seq(1:486),
                              p_Ext_crop=seq(1:486),
                              
                              OR_Ext_pasture=seq(1:486),
                              p_Ext_pasture=seq(1:486),
                              
                              OR_Int_wood=seq(1:486),
                              p_Int_wood=seq(1:486),
                              
                              OR_Cropland_loss=seq(1:486),
                              p_Cropland_loss=seq(1:486),
                              
                              OR_Forest_gain=seq(1:486),
                              p_Forest_gain=seq(1:486),
                              
                              OR_Forest_loss=seq(1:486),
                              p_Forest_loss=seq(1:486),
                              
                              OR_Urban=seq(1:486),
                              p_Urban=seq(1:486),
                              
                              stringsAsFactors=FALSE)
names(bird_OR_results)[1]<-"SPECIESCODE"
bird_OR_results[1:486,2:17]<-NA

#### loop running individual models for all bird species
for (i in 1:length(bird_OR_results$SPECIESCODE)){
  one_bird<-dat_buffer[dat_buffer$SPECIESCODE==paste(bird_OR_results$SPECIESCODE[i]),]
  one_bird<-(one_bird[(rowSums((one_bird[,11:19]))==0)<=0,])
  
  # skip all species that occur in less than 5 SPAs
  if (nrow(one_bird) < 5){
    bird_OR_results[i,2:9]<-NA
  } else {
    
    #### models
    
    tryCatch({
      m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Int_crop+Ext_crop+Ext_pasture+Int_wood+Cropland_loss+Forest_gain+Forest_loss+Urban+area, data=one_bird,Hess=TRUE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    tryCatch({
      ctable <- coef(summary(m))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    if(length(rownames(ctable))<11) next # skip 3rd iteration and go to next iteration
    
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)
    
    tryCatch({
      ci <- confint(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #confint.default(m)
    #exp(coef(m))
    tryCatch({
    res<-exp(cbind(OR = coef(m)))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    bird_OR_results[i,2]<-res[1,1]
    bird_OR_results[i,3]<-ctable[1,4]
    
    bird_OR_results[i,4]<-res[2,1]
    bird_OR_results[i,5]<-ctable[2,4]
    
    bird_OR_results[i,6]<-res[3,1]
    bird_OR_results[i,7]<-ctable[3,4]
    
    bird_OR_results[i,8]<-res[4,1]
    bird_OR_results[i,9]<-ctable[4,4]
    
    bird_OR_results[i,10]<-res[5,1]
    bird_OR_results[i,11]<-ctable[5,4]
    
    bird_OR_results[i,12]<-res[6,1]
    bird_OR_results[i,13]<-ctable[6,4]
    
    bird_OR_results[i,14]<-res[7,1]
    bird_OR_results[i,15]<-ctable[7,4]
    
    bird_OR_results[i,16]<-res[8,1]
    bird_OR_results[i,17]<-ctable[8,4]
    
  }
  print(i)
}

saveRDS(bird_OR_results, file = "bird_OR_results_inner_buffer_full_model.rds")

bird_OR_results<-readRDS(file = "bird_OR_results_inner_buffer_full_model.rds")

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

par(mar=c(10,3,3,3))
barplot(as.numeric(res_summary[,2]),names.arg=c(rownames(res_summary)), col=c("red","green"), las=2, main="full model: inside and buffer")

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
  png(gsub("[[:punct:]]", " ", paste(bird_pref[i],"full model inside and buffer SPAs.png")))
  par(mar=c(10,3,3,3))
  barplot(as.numeric(res_summary[,2]),names.arg=c(rownames(res_summary)), col=c("red","green"), las=2, main=paste(bird_pref[i],"full model: inside and outside SPAs"))
  dev.off()
  
}
