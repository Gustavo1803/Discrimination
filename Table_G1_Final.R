# ---------------------------------------------------------------------------------------------- #
#   Generate Table E.1 Discriminatory Steering and Neighborhood Effects                          #
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
## These lines load the required packages
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

recs_trial_final$whitebg_Ad <- 0
recs_trial_final$whitebg_Ad[recs_trial_final$w2012pc_Ad > .7405] <- 1
recs_trial_final = subset(recs_trial_final, w2012pc_Ad > .7405)

####################################################
#################Panel A############################
####################################################

###Test Scores
#############################################################################################################################################################################
recs_trial_final_2 <- readRDS("HUDprocessed_JPE_testscores_042021.rds")

recs_trial_final_2$whitebg_Ad <- 0
recs_trial_final_2$whitebg_Ad[recs_trial_final_2$w2012pc_Ad > .7405] <- 1
recs_trial_final_2 = subset(recs_trial_final_2, w2012pc_Ad > .7405)

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



#############################################################################################################################################################################
# STEERING and ELEMENTARY School Test Scores
## Stanford School Data

ED_elem4 <- felm(mn_avg_ol_elem_Rec ~ APRACE.x + mn_avg_ol_elem_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x +ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)

summary(ED_elem4)


#########################################################################################################################################################################

# STEERING and MIDDLE School Test Scores
## Stanford School Data

ED_middle4 <- felm(mn_avg_ol_middle_Rec ~ APRACE.x + mn_avg_ol_middle_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x +ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)

summary(ED_middle4)


#############################################################################################################################################################################
# STEERING INTO HIGH Assault NEIGHBORHOODS 
## Number of nearby Assaults in 2017

AS4 <- felm(Assault_Rec ~ APRACE.x + Assault_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(AS4)
 

###SCHOOL QUALITY
#############################################################################################################################################################################
# STEERING and ELEMENTARY School Quality
## Great School Rankings in 2017


ES4 <- felm(Elementary_School_Score_Rec ~ APRACE.x + Elementary_School_Score_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(ES4)


###########################################
############Panel B#######################
#########################################

#NEIGHBORHOOD POVERTY RATES
## Poverty rates are based on American Community Survey definitions by Census block group

PR4 <- felm(povrate_Rec ~ APRACE.x + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(PR4)

##########################################################################################################################################################################

# STEERING AND NEIGHBORHOOD HIGH Skill  
## Skill is share of census block group employed in ACS defined Management, business, science, and arts occupations


SK4 <- felm(skill_Rec ~ APRACE.x + skill_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(SK4)

#############################################################################################################################################################################

# STEERING INTO HIGHLY EDUCATED NEIGHBORHOODS 
## College is share of census block group with at least a college education

COL4<- felm(college_Rec ~ APRACE.x + college_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(COL4)

#############################################################################################################################################################################

#NEIGHBORHOOD Single Family Households
## Household Composition is based on American Community Survey definitions by Census block group

SF4 <- felm(singlefamily_Rec ~ APRACE.x + singlefamily_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(SF4)


#############################################################################################################################################################################
#NEIGHBORHOOD Home Ownership
## Home Ownership by Race is based on American Community Survey definitions by Census block group


own4 <- felm(ownerocc_Rec ~ APRACE.x + ownerocc_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(own4)


##############################
#####Panel C###############
############################


# STEERING AND Superfund Proximity
# Group Specific vs. White

SPadrace <- felm(SFcount_Rec ~ APRACE.x + SFcount_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(SPadrace)


# STEERING INTO RSEI 
## RSEI data are based on 2012 measurements from EPA

# Group Specific vs. White

RSEIadrace <- felm(RSEI_Rec ~ APRACE.x + RSEI_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(RSEIadrace)


# STEERING INTO PM2.5

# Group Specific vs. White

PMadrace <- felm(PM25_Rec ~ APRACE.x + PM25_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(PMadrace)


### GENERATE TABLES

mean1=as.vector(c(mean(recs_trial_final_2[recs_trial_final_2$white.x==1,"mn_avg_ol_elem_Rec"],na.rm=TRUE),
                  mean(recs_trial_final_2[recs_trial_final_2$white.x==1,"mn_avg_ol_middle_Rec"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"Assault_Rec"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"Elementary_School_Score_Rec"],na.rm=TRUE))) 

mean2=as.vector(c(mean(recs_trial_final[recs_trial_final$white.x==1,"povrate_Rec"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"skill_Rec"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"college_Rec"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"singlefamily_Rec"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"ownerocc_Rec"],na.rm=TRUE))) 

mean3=as.vector(c(mean(recs_trial_final[recs_trial_final$white.x==1,"SFcount_Rec"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"RSEI_Rec"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"PM25_Rec"],na.rm=TRUE))) 

stargazer( ED_elem4, ED_middle4, AS4, ES4,
          type = "latex",
          out = paste0(out, "HUM_G3_A_JPE.tex"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels  = c("African American",
                   "Hispanic",
                   "Asian",
                   "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Comparison Mean (White)",signif(mean1, digits = 2))))

stargazer(PR4, SK4, COL4, SF4, own4,
          type = "latex",
          out = paste0(out, "HUM_tabG3_B_JPE.tex"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels  = c("African American",
                                "Hispanic",
                                "Asian",
                                "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Comparison Mean (White)",signif(mean2, digits = 2))))

stargazer(SPadrace, RSEIadrace, PMadrace, 
          type = "latex",
          out = paste0(out, "HUM_tabG3_C_JPE.tex"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels  = c("African American",
                                "Hispanic",
                                "Asian",
                                "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Comparison Mean (White)",signif(mean3, digits = 2))))



