# ---------------------------------------------------------------------------------------------- #
#   Generate Table E.2 Test of Squared Differences from Mean                                     #
#                                                                                                # 
#   R-Version: 4.04                                                                              #                                                             #
#   Date Last Modification: 12/01/2021                                                           #
# -----------------------------------------------------------------------------------------------#

# Clear workspace
rm(list = ls()) 

# Set working directory
setwd("C:/Users/genin/OneDrive/Documents/Git/Discrimination/data")

# Define function for loading packages
rm(list = ls()) 

#Load Packages
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

#Load Packages
packages <- c("readxl", "readstata13", "lfe", "Synth","data.table", "plm", "ggplot2", "MatchIt", "experiment", "stargazer")
lapply(packages, pkgTest)

# Output
out <- "C:/Users/genin/OneDrive/Documents/Git/Discrimination/output/"


## Housing Search  ######################################################################################################

#Preamble

# Unless Otherwise Specified, Model Sequence is as follows:
# Model 1: treatment effect conditional on advertisement price
# Model 2: treatment effect also conditional on level of treatment outcome in advertised listing
# Model 3: treatment effect also conditional on racial composition of block group of advertised listing
# Model 4: treatment effect also conditional on racial composition of block group of recommended listing
# Model 5: treatment effect also conditional on price of recommended home

# All models cluster standard errors by trial 

recs_trial_final <- readRDS("HUDprocessed_JPE_census_042021.rds")

#construct separate indicators for market and control
recs_trial_final$market <- as.factor(sapply(strsplit(recs_trial_final$CONTROL, "-"), `[`, 1))
recs_trial_final$CONTROL <- as.factor(recs_trial_final$CONTROL)


#construct indicators for race groups
recs_trial_final$ofcolor <- 0
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==2] <- 1
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==3] <- 1
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==4] <- 1
recs_trial_final$ofcolor <- as.factor(recs_trial_final$ofcolor)

#subset experimental data to mothers
recs_trial_final_mothers <- subset(recs_trial_final, kids.x==1 & TSEX.x.x==0)
recs_trial_final_mothers <- within(recs_trial_final_mothers, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})

#subset experimental data to families
recs_trial_final_families <- subset(recs_trial_final, kids.x==1)
recs_trial_final_families <- within(recs_trial_final_families, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})



###Test Scores
#############################################################################################################################################################################
recs_trial_final_2 <- readRDS("HUDprocessed_JPE_testscores_042021.rds")

#construct separate indicators for market and control
recs_trial_final_2$market <- as.factor(sapply(strsplit(as.character(recs_trial_final_2$CONTROL), "-"), `[`, 1))
recs_trial_final_2$CONTROL <- as.factor(recs_trial_final_2$CONTROL)


#construct indicators for race groups
recs_trial_final_2$ofcolor <- 0
recs_trial_final_2$ofcolor[recs_trial_final_2$APRACE.x==2] <- 1
recs_trial_final_2$ofcolor[recs_trial_final_2$APRACE.x==3] <- 1
recs_trial_final_2$ofcolor[recs_trial_final_2$APRACE.x==4] <- 1
recs_trial_final_2$ofcolor <- as.factor(recs_trial_final_2$ofcolor)

#subset experimental data to mothers
recs_trial_final_mothers_2 <- subset(recs_trial_final_2, kids.x==1 & TSEX.x.x==0)
#recs_trial_final_mothers <- within(recs_trial_final_mothers, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})

##################Difference#####################

#####Creation of max by variable####

recs_trial_final$mean.skill <- ave(recs_trial_final$skill_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)
recs_trial_final$mean.college <- ave(recs_trial_final$college_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)
recs_trial_final$mean.elementary <- ave(recs_trial_final$Elementary_School_Score_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)

recs_trial_final_2$mean.elem_score <- ave(recs_trial_final_2$mn_avg_ol_elem_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)
recs_trial_final_2$mean.midd_score <- ave(recs_trial_final_2$mn_avg_ol_middle_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)



#####Creation of min by variable####

recs_trial_final$mean.povrate <- ave(recs_trial_final$povrate_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)
recs_trial_final$mean.assault <- ave(recs_trial_final$Assault_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)
recs_trial_final$mean.sfcount <- ave(recs_trial_final$SFcount_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)
recs_trial_final$mean.RSEI <- ave(recs_trial_final$RSEI_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)
recs_trial_final$mean.PM25 <- ave(recs_trial_final$PM25_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)
recs_trial_final$mean.singlefamily <- ave(recs_trial_final$singlefamily_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)
recs_trial_final$mean.ownerocc <- ave(recs_trial_final$ownerocc_Rec ,recs_trial_final$TESTERID,recs_trial_final$CONTROL, FUN=mean)

difference_mean_sq=function(a,b){
  di=(a-b)^2
  return(di)
}

recs_trial_final_2$dif_ED_elem=difference_mean_sq(recs_trial_final_2$mn_avg_ol_elem_Rec,recs_trial_final_2$mean.elem_score)
recs_trial_final_2$dif_ED_middle4=difference_mean_sq(recs_trial_final_2$mn_avg_ol_middle_Rec,recs_trial_final_2$mean.midd_score)

recs_trial_final$dif_ASadrace=difference_mean_sq(recs_trial_final$Assault_Rec,recs_trial_final$mean.assault)
recs_trial_final$dif_ESadrace=difference_mean_sq(recs_trial_final$Elementary_School_Score_Rec,recs_trial_final$mean.elementary)                                     
recs_trial_final$dif_PRadrace=difference_mean_sq(recs_trial_final$povrate_Rec,recs_trial_final$mean.povrate)
recs_trial_final$dif_SKadrace=difference_mean_sq(recs_trial_final$skill_Rec,recs_trial_final$mean.skill)
recs_trial_final$dif_COLadrace=difference_mean_sq(recs_trial_final$college_Rec,recs_trial_final$mean.college)
recs_trial_final$dif_SF4=difference_mean_sq(recs_trial_final$singlefamily_Rec,recs_trial_final$mean.singlefamily)
recs_trial_final$dif_own4=difference_mean_sq(recs_trial_final$ownerocc_Rec,recs_trial_final$mean.ownerocc)
recs_trial_final$dif_SPadrace=difference_mean_sq(recs_trial_final$SFcount_Rec,recs_trial_final$mean.sfcount)
recs_trial_final$dif_RSEIadrace =difference_mean_sq(recs_trial_final$RSEI_Rec,recs_trial_final$mean.RSEI)
recs_trial_final$dif_PMadrace=difference_mean_sq(recs_trial_final$PM25_Rec,recs_trial_final$mean.PM25)



########################################################################################################################################################################
############  Ofcolor Regression  #####################################################################################################################################                                   
#######################################################################################################################################################################



###########    Panel A         #####################


#############################################################################################################################################################################
# STEERING and ELEMENTARY School Test Scores
## Stanford School Data

recs_trial_final_2$Outcome <- recs_trial_final_2$mn_avg_ol_elem_Ad 
ED_elem <- felm(dif_ED_elem ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  |  0 | 0, data = recs_trial_final_2)

summary(ED_elem)


#########################################################################################################################################################################

# STEERING and MIDDLE School Test Scores
## Stanford School Data

recs_trial_final_2$Outcome <- recs_trial_final_2$mn_avg_ol_middle_Ad
ED_middle4 <- felm(dif_ED_middle4 ~ ofcolor  + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  |  0 | 0, data = recs_trial_final_2)

summary(ED_middle4)


# STEERING INTO HIGH Assault NEIGHBORHOODS 
## Number of nearby Assaults in 2017

# Group Specific vs. White
recs_trial_final$Outcome <- recs_trial_final$Assault_Ad
ASadrace <- felm(dif_ASadrace ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice |  SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(ASadrace)

# STEERING and ELEMENTARY School Quality
## Great School Rankings in 2017

# Group Specific vs. White
recs_trial_final$Outcome <- recs_trial_final$Elementary_School_Score_Ad
ESadrace <- felm(dif_ESadrace ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice |  SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(ESadrace)


##################################################
########Panel B################################ 
##############################################

#NEIGHBORHOOD POVERTY RATES
## Poverty rates are based on American Community Survey definitions by Census block group

# Group Specific vs. White

recs_trial_final$Outcome <- recs_trial_final$povrate_Ad
PRadrace <- felm(dif_PRadrace~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(PRadrace)


# STEERING AND NEIGHBORHOOD HIGH Skill  
## Skill is share of census block group employed in ACS defined Management, business, science, and arts occupations

# Group Specific vs. White

recs_trial_final$Outcome <- recs_trial_final$skill_Ad
SKadrace <- felm(dif_SKadrace ~ ofcolor  + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(SKadrace)


# STEERING INTO HIGHLY EDUCATED NEIGHBORHOODS 
## College is share of census block group with at least a college education

# Group Specific vs. White

recs_trial_final$Outcome <- recs_trial_final$college_Ad
COLadrace <- felm(dif_COLadrace ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(COLadrace)


#NEIGHBORHOOD Single Family Households
## Household Composition is based on American Community Survey definitions by Census block group

recs_trial_final$Outcome <- recs_trial_final$singlefamily_Ad
SF4 <- felm(dif_SF4 ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(SF4)

#NEIGHBORHOOD Home Ownership
## Home Ownership by Race is based on American Community Survey definitions by Census block group

recs_trial_final$Outcome <- recs_trial_final$ownerocc_Ad
own4 <- felm(dif_own4 ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(own4)


########################################################################################################################################################################
############ APRACE Regressions  #####################################################################################################################################                                   
#######################################################################################################################################################################


#########Panel A#####################

# STEERING and ELEMENTARY School Test Scores
## Stanford School Data

recs_trial_final_2$Outcome <- recs_trial_final_2$mn_avg_ol_elem_Ad 
ED_elem_ <- felm(dif_ED_elem ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  |  0 | 0, data = recs_trial_final_2)

summary(ED_elem_)


# STEERING and MIDDLE School Test Scores
## Stanford School Data

recs_trial_final_2$Outcome <- recs_trial_final_2$mn_avg_ol_middle_Ad
ED_middle4_ <- felm(dif_ED_middle4 ~ APRACE.x  + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  |  0 | 0, data = recs_trial_final_2)

summary(ED_middle4_)


# STEERING INTO HIGH Assault NEIGHBORHOODS 
## Number of nearby Assaults in 2017

# Group Specific vs. White
recs_trial_final$Outcome <- recs_trial_final$Assault_Ad
ASadrace_ <- felm(dif_ASadrace ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice |  SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(ASadrace_)

# STEERING and ELEMENTARY School Quality
## Great School Rankings in 2017

# Group Specific vs. White
recs_trial_final$Outcome <- recs_trial_final$Elementary_School_Score_Ad
ESadrace_ <- felm(dif_ESadrace ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice |  SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(ESadrace_)


##################################################
########Panel B################################ 

#NEIGHBORHOOD POVERTY RATES
## Poverty rates are based on American Community Survey definitions by Census block group
##Group Specific vs. White

recs_trial_final$Outcome <- recs_trial_final$povrate_Ad
PRadrace_ <- felm(dif_PRadrace~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(PRadrace_)

# STEERING AND NEIGHBORHOOD HIGH Skill  
## Skill is share of census block group employed in ACS defined Management, business, science, and arts occupations
# Group Specific vs. White

recs_trial_final$Outcome <- recs_trial_final$skill_Ad
SKadrace_ <- felm(dif_SKadrace ~ APRACE.x  + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(SKadrace_)


# STEERING INTO HIGHLY EDUCATED NEIGHBORHOODS 
## College is share of census block group with at least a college education
# Group Specific vs. White

recs_trial_final$Outcome <- recs_trial_final$college_Ad
COLadrace_ <- felm(dif_COLadrace ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(COLadrace_)


#NEIGHBORHOOD Single Family Households
## Household Composition is based on American Community Survey definitions by Census block group
# Group Specific vs. White

recs_trial_final$Outcome <- recs_trial_final$singlefamily_Ad
SF4_ <- felm(dif_SF4 ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(SF4_)

#NEIGHBORHOOD Home Ownership
## Home Ownership by Race is based on American Community Survey definitions by Census block group
# Group Specific vs. White

recs_trial_final$Outcome <- recs_trial_final$ownerocc_Ad
own4_ <- felm(dif_own4 ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x  | 0 | 0, data = recs_trial_final)

summary(own4_)



### GENERATE TABLES

mean1=as.vector(c(mean(recs_trial_final_2[recs_trial_final_2$white.x==1,"dif_ED_elem"],na.rm=TRUE),
                  mean(recs_trial_final_2[recs_trial_final_2$white.x==1,"dif_ED_middle4"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"dif_ASadrace"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"dif_ESadrace"],na.rm=TRUE)))

mean2=as.vector(c(mean(recs_trial_final[recs_trial_final$white.x==1,"dif_PRadrace"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"dif_SKadrace"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"dif_COLadrace"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"dif_SF4"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"dif_own4"],na.rm=TRUE)))



### Impled Preferences for Neighborhood Attributes

stargazer(ED_elem, ED_middle4, ASadrace, ESadrace,
          type = "latex",
          out = paste0(out, "tabE2_A.tex"),
          column.labels = c("Elem School","Middle School", "Assaults", "Elem School"),
          keep = c( "ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=NULL,notes.append=T,object.names=FALSE)



stargazer(PRadrace, SKadrace, COLadrace, SF4, own4,
          type = "latex",
          out = paste0(out, "tabE2_B.tex"),
          column.labels = c("Elem School","Middle School", "Assaults", "Elem School"),
          keep = c( "ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=NULL,notes.append=T,object.names=FALSE)



### Impled Preferences for Neighborhood Attributes

stargazer(ED_elem_, ED_middle4_, ASadrace_, ESadrace_,
          type = "latex",
          out = paste0(out, "tabE2_A_.tex"),
          column.labels = c("Elem School","Middle School", "Assaults", "Elem School"),
          keep = c( "APRACE.x2", "APRACE.x3", "APRACE.x4", "APRACE.x5"),
          order = c( "APRACE.x2", "APRACE.x3", "APRACE.x4", "APRACE.x5"),
          covariate.labels = c("African American ",
                               "Hispanic ",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("Comparison Mean (White)",signif(mean1, digits = 2))),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=NULL,notes.append=T,object.names=FALSE)



stargazer(PRadrace_, SKadrace_, COLadrace_, SF4_, own4_,
          type = "latex",
          out = paste0(out, "tabE2_B_.tex"),
          column.labels = c("Elem School","Middle School", "Assaults", "Elem School"),
          keep = c( "APRACE.x2", "APRACE.x3", "APRACE.x4", "APRACE.x5"),
          order = c( "APRACE.x2", "APRACE.x3", "APRACE.x4", "APRACE.x5"),
          covariate.labels = c("African American ",
                               "Hispanic ",
                               "Asian",
                               "Other"),
          add.lines = list(c("Comparison Mean (White)",signif(mean2, digits = 2))),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=NULL,notes.append=T,object.names=FALSE)



