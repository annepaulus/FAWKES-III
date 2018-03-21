#########################################
#### Ordered logistic regression analysis for habitat Conservation status (ABC) against levers trajectories (4 summarized)
#### This is using the extracted values from Levers in 10km buffers around all SPAs
#### inspiration for code found at https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
#### Authors: MB, TV ...
#########################################



#### re-calculate actual area, unit unknown ...
dat$cc_area<-dat$area*dat$Conversion
dat$int_area<-dat$area*dat$Intens
dat$ext_area<-dat$area*dat$De_intens
dat$stab_area<-dat$area*dat$Stabil

#### create new dataframe, each species  in one row
bird_OR_results <- data.frame(unique(droplevels(dat$SPECIESCODE)),
OR_int=seq(1:486),
p_int=seq(1:486),
OR_ext=seq(1:486),
p_ext=seq(1:486),
OR_cc=seq(1:486),
p_cc=seq(1:486),
OR_stab=seq(1:486),
p_stab=seq(1:486),
stringsAsFactors=FALSE)
names(bird_OR_results)[1]<-"SPECIESCODE"

#### loop running individual models for all bird species

for (i in 1:length(bird_OR_results$SPECIESCODE)){
one_bird<-dat[dat$SPECIESCODE==paste(bird_OR_results$SPECIESCODE[i]),]
one_bird<-(one_bird[(rowSums((one_bird[,8:10]))==0)<=0,])

# skip all species that occur in less than 5 SPAs
if (nrow(one_bird) < 5){
bird_OR_results[i,2:9]<-NA
} else {

#### Intens models
  
tryCatch({
m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Intens+area, data=one_bird,Hess=TRUE)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

tryCatch({
ci <- confint(m)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#confint.default(m)
#exp(coef(m))
res<-exp(cbind(OR = coef(m), ci))
bird_OR_results[i,2]<-res[1,1]
bird_OR_results[i,3]<-ctable[1,4]

#### De_intens models

tryCatch({
  m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~De_intens+area, data=one_bird,Hess=TRUE)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

tryCatch({
  ci <- confint(m)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#confint.default(m)
#exp(coef(m))
res<-exp(cbind(OR = coef(m), ci))
bird_OR_results[i,4]<-res[1,1]
bird_OR_results[i,5]<-ctable[1,4]

#### Conversion models

tryCatch({
  m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Conversion+area, data=one_bird,Hess=TRUE)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

tryCatch({
  ci <- confint(m)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#confint.default(m)
#exp(coef(m))
res<-exp(cbind(OR = coef(m), ci))
bird_OR_results[i,6]<-res[1,1]
bird_OR_results[i,7]<-ctable[1,4]

#### Stabil models

tryCatch({
  m<-polr(formula=factor(one_bird$CONSERVATION,levels = c("C","B","A"),ordered = TRUE)~Stabil+area, data=one_bird,Hess=TRUE)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

tryCatch({
  ci <- confint(m)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#confint.default(m)
#exp(coef(m))
res<-exp(cbind(OR = coef(m), ci))
bird_OR_results[i,8]<-res[1,1]
bird_OR_results[i,9]<-ctable[1,4]

}
print(i)
}

#### results summary & plot

int_neg<-summary((bird_OR_results$OR_int<1 & bird_OR_results$p_int<0.05))
int_pos<-summary((bird_OR_results$OR_int>1 & bird_OR_results$p_int<0.05))

ext_neg<-summary((bird_OR_results$OR_ext<1 & bird_OR_results$p_ext<0.05))
ext_pos<-summary((bird_OR_results$OR_ext>1 & bird_OR_results$p_ext<0.05))

cc_neg<-summary((bird_OR_results$OR_cc<1 & bird_OR_results$p_cc<0.05))
cc_pos<-summary((bird_OR_results$OR_cc>1 & bird_OR_results$p_cc<0.05))

stab_neg<-summary((bird_OR_results$OR_stab<1 & bird_OR_results$p_stab<0.05))
stab_pos<-summary((bird_OR_results$OR_stab>1 & bird_OR_results$p_stab<0.05))

res_summary<-rbind(int_neg,int_pos,ext_neg,ext_pos,cc_neg,cc_pos,stab_neg,stab_pos)
res_summary<-res_summary[,-1]

barplot(as.numeric(res_summary[,2]),names.arg=c(rownames(res_summary)), col=c("red","green"))




########### RESTERAMPE ##############


# one_bird_plot<-ggplot(one_bird, aes(x = CONSERVATION, y = area)) +
#   geom_boxplot(size = .75) +
#   geom_jitter(alpha = .5) +
#   facet_grid(. ~ ACT_dom, margins = TRUE) +
#   ggtitle(paste(one_bird[1,3]))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# ggsave(paste(i,".png"))


# one_bird<-dat[dat$SPECIESCODE=="A030",]
# #head(one_bird)
# one_bird$country<-factor(substr((one_bird$SITECODE),1,2))
# #lapply(one_bird[, c("CONSERVATION","cc_area")], table)
# 
# one_bird<-(one_bird[(rowSums((one_bird[,8:10]))==0)<=0,])
# 
# m<-polr(formula=CONSERVATION~ACT_dom+area, data=one_bird,Hess=TRUE)
# ctable <- coef(summary(m))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# ctable <- cbind(ctable, "p value" = p)
# ci <- confint(m)
# confint.default(m)
# exp(coef(m))
# exp(cbind(OR = coef(m), ci))
# 
# summary(factor(one_bird$ACT_dom))
# 
# ggplot(one_bird, aes(x = CONSERVATION, y = int_area)) +
#   geom_boxplot(size = .75) +
#   geom_jitter(alpha = .5) +
#   facet_grid(ACT_dom~country, margins = TRUE) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# 
# 
# sf <- function(y) {
#   c('Y>=1' = qlogis(mean(y >= 1)),
#     'Y>=2' = qlogis(mean(y >= 2)),
#     'Y>=3' = qlogis(mean(y >= 3)))
# }
# 
# s <- with(one_bird, summary(as.numeric(CONSERVATION) ~ ACT_dom + area, fun=sf))
# 
# glm(I(as.numeric(CONSERVATION) >= 2) ~ ACT_dom, family="binomial", data = one_bird)
# glm(I(as.numeric(CONSERVATION) >= 3) ~ ACT_dom, family="binomial", data = one_bird)
# 
# s[, 4] <- s[, 4] - s[, 3]
# s[, 3] <- s[, 3] - s[, 3]
# plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))