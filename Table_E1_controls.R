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
recs_trial_final$RecPriceBins <- cut(as.numeric(recs_trial_final$RecPrice), seq(from = 0, to = 10000000, by = 20000))

#subset experimental data to mothers
recs_trial_final_mothers <- subset(recs_trial_final, kids.x==1 & TSEX.x.x==0)
recs_trial_final_mothers <- within(recs_trial_final_mothers, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})

#subset experimental data to families
recs_trial_final_families <- subset(recs_trial_final, kids.x==1)
recs_trial_final_families <- within(recs_trial_final_families, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})



####################################################
#################Panel A############################
####################################################

###Test Scores
#############################################################################################################################################################################
recs_trial_final_2 <- readRDS("HUDprocessed_JPE_testscores_042021.rds")

#construct separate indicators for market and control
recs_trial_final_2$market <- as.factor(sapply(strsplit(as.character(recs_trial_final_2$CONTROL), "-"), `[`, 1))
recs_trial_final_2$CONTROL <- as.factor(recs_trial_final_2$CONTROL)
recs_trial_final_2$RecPriceBins <- cut(as.numeric(recs_trial_final_2$RecPrice), seq(from = 0, to = 10000000, by = 20000))

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

### Temporary (Switch to full models below when new data come in)
# Of Color vs. White
ED_elem41 <- felm(mn_avg_ol_elem_Rec ~ ofcolor | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)
ED_elem42 <- felm(mn_avg_ol_elem_Rec ~ ofcolor +  mn_avg_ol_elem_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)
ED_elem43 <- felm(mn_avg_ol_elem_Rec ~ ofcolor +  mn_avg_ol_elem_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)


# Group Specific vs. White
ED_elem41_ <- felm(mn_avg_ol_elem_Rec ~ APRACE.x | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)
ED_elem42_ <- felm(mn_avg_ol_elem_Rec ~ APRACE.x +  mn_avg_ol_elem_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)
ED_elem43_ <- felm(mn_avg_ol_elem_Rec ~ APRACE.x +  mn_avg_ol_elem_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)

summary(ED_elem4)
summary(ED_elem4_)

#########################################################################################################################################################################

# STEERING and MIDDLE School Test Scores
## Stanford School Data

#Temporary
# Of Color vs. White
ED_middle41 <- felm(mn_avg_ol_middle_Rec ~ ofcolor| CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)
ED_middle42 <- felm(mn_avg_ol_middle_Rec ~ ofcolor + mn_avg_ol_middle_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)
ED_middle43 <- felm(mn_avg_ol_middle_Rec ~ ofcolor + mn_avg_ol_middle_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)



# Group Specific vs. White
ED_middle41_ <- felm(mn_avg_ol_middle_Rec ~ APRACE.x| CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)
ED_middle42_ <- felm(mn_avg_ol_middle_Rec ~ APRACE.x + mn_avg_ol_middle_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)
ED_middle43_ <- felm(mn_avg_ol_middle_Rec ~ APRACE.x + mn_avg_ol_middle_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)

summary(ED_middle4)
summary(ED_middle4_)


#############################################################################################################################################################################
# STEERING INTO HIGH Assault NEIGHBORHOODS 
## Number of nearby Assaults in 2017

# Of Color vs. White
AS41 <- felm(Assault_Rec ~ ofcolor  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
AS42 <- felm(Assault_Rec ~ ofcolor + Assault_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
AS43 <- felm(Assault_Rec ~ ofcolor + Assault_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


# Group Specific vs. White
AS41_ <- felm(Assault_Rec ~ APRACE.x  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
AS42_ <- felm(Assault_Rec ~ APRACE.x + Assault_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
AS43_ <- felm(Assault_Rec ~ APRACE.x + Assault_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(AS4)
summary(AS4_)


###SCHOOL QUALITY
#############################################################################################################################################################################
# STEERING and ELEMENTARY School Quality
## Great School Rankings in 2017

# Of Color vs. White
ES41 <- felm(Elementary_School_Score_Rec ~ ofcolor  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
ES42 <- felm(Elementary_School_Score_Rec ~ ofcolor + Elementary_School_Score_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
ES43 <- felm(Elementary_School_Score_Rec ~ ofcolor + Elementary_School_Score_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


# Group Specific vs. White
ES41_ <- felm(Elementary_School_Score_Rec ~ APRACE.x  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
ES42_ <- felm(Elementary_School_Score_Rec ~ APRACE.x + Elementary_School_Score_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
ES43_ <- felm(Elementary_School_Score_Rec ~ APRACE.x + Elementary_School_Score_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(ES4)
summary(ES4_)


###########################################
############Panel B#######################
#########################################

#NEIGHBORHOOD POVERTY RATES
## Poverty rates are based on American Community Survey definitions by Census block group

# Of Color vs. White
PR41 <- felm(povrate_Rec ~ ofcolor  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
PR42 <- felm(povrate_Rec ~ ofcolor + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
PR43 <- felm(povrate_Rec ~ ofcolor + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


# Group Specific vs. White
PR41_ <- felm(povrate_Rec ~ APRACE.x  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
PR42_ <- felm(povrate_Rec ~ APRACE.x + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
PR43_ <- felm(povrate_Rec ~ APRACE.x + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(PR4)
summary(PR4_)


##########################################################################################################################################################################

# STEERING AND NEIGHBORHOOD HIGH Skill  
## Skill is share of census block group employed in ACS defined Management, business, science, and arts occupations

# Of Color vs. White
SK41 <- felm(skill_Rec ~ ofcolor  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SK42 <- felm(skill_Rec ~ ofcolor + skill_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SK43 <- felm(skill_Rec ~ ofcolor + skill_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


# Group Specific vs. White
SK41_ <- felm(skill_Rec ~ APRACE.x  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SK42_ <- felm(skill_Rec ~ APRACE.x + skill_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SK43_ <- felm(skill_Rec ~ APRACE.x + skill_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(SK4)
summary(SK4_)


#############################################################################################################################################################################

# STEERING INTO HIGHLY EDUCATED NEIGHBORHOODS 
## College is share of census block group with at least a college education

# Of Color vs. White
COL41 <- felm(college_Rec ~ ofcolor | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
COL42 <- felm(college_Rec ~ ofcolor + college_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
COL43 <- felm(college_Rec ~ ofcolor + college_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

# Group Specific vs. White
COL41_ <- felm(college_Rec ~ APRACE.x | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
COL42_ <- felm(college_Rec ~ APRACE.x + college_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
COL43_ <- felm(college_Rec ~ APRACE.x + college_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(COL4)
summary(COL4_)


#############################################################################################################################################################################

#NEIGHBORHOOD Single Family Households
## Household Composition is based on American Community Survey definitions by Census block group

# Of Color vs. White
SF41 <- felm(singlefamily_Rec ~ ofcolor  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SF42 <- felm(singlefamily_Rec ~ ofcolor + singlefamily_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SF43 <- felm(singlefamily_Rec ~ ofcolor + singlefamily_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


# Group Specific vs. White
SF41_ <- felm(singlefamily_Rec ~ APRACE.x  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SF42_ <- felm(singlefamily_Rec ~ APRACE.x + singlefamily_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SF43_ <- felm(singlefamily_Rec ~ APRACE.x + singlefamily_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(SF4)
summary(SF4_)


#############################################################################################################################################################################
#NEIGHBORHOOD Home Ownership
## Home Ownership by Race is based on American Community Survey definitions by Census block group

# Of Color vs. White
own41 <- felm(ownerocc_Rec ~ ofcolor  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
own42 <- felm(ownerocc_Rec ~ ofcolor + ownerocc_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
own43 <- felm(ownerocc_Rec ~ ofcolor + ownerocc_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


# Group Specific vs. White
own41_ <- felm(ownerocc_Rec ~ APRACE.x  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
own42_ <- felm(ownerocc_Rec ~ APRACE.x + ownerocc_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
own43_ <- felm(ownerocc_Rec ~ APRACE.x + ownerocc_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


summary(own4)
summary(own4_)

#########Panel C#############
# STEERING AND Superfund Proximity
# Of Color vs. White
SP41 <- felm(SFcount_Rec ~ ofcolor  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SP42 <- felm(SFcount_Rec ~ ofcolor + SFcount_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SP43 <- felm(SFcount_Rec ~ ofcolor + SFcount_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


# Group Specific vs. White
SP41_ <- felm(SFcount_Rec ~ APRACE.x | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SP42_ <- felm(SFcount_Rec ~ APRACE.x + SFcount_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
SP43_ <- felm(SFcount_Rec ~ APRACE.x + SFcount_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


summary(SP4)
summary(SP4_)

# STEERING INTO RSEI 
## RSEI data are based on 2012 measurements from EPA

# Of Color vs. White
RSEI41 <- felm(RSEI_Rec ~ ofcolor | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
RSEI42 <- felm(RSEI_Rec ~ ofcolor + RSEI_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
RSEI43 <- felm(RSEI_Rec ~ ofcolor + RSEI_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


# Group Specific vs. White
RSEI41_ <- felm(RSEI_Rec ~ APRACE.x | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
RSEI42_ <- felm(RSEI_Rec ~ APRACE.x + RSEI_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
RSEI43_ <- felm(RSEI_Rec ~ APRACE.x + RSEI_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(RSEI4)
summary(RSEI4_)

# STEERING INTO PM2.5

# Of Color vs. White
PM41 <- felm(PM25_Rec ~ ofcolor  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
PM42 <- felm(PM25_Rec ~ ofcolor + PM25_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
PM43 <- felm(PM25_Rec ~ ofcolor + PM25_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


# Group Specific vs. White
PM41_ <- felm(PM25_Rec ~ APRACE.x  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
PM42_ <- felm(PM25_Rec ~ APRACE.x + PM25_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
PM43_ <- felm(PM25_Rec ~ APRACE.x + PM25_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + RecPriceBins  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)



### GENERATE TABLES


###########Control Group Mean####################



stargazer( ED_elem41, ED_elem42, ED_elem43, ED_middle41, ED_middle42, ED_middle43, AS41, AS42,AS43,
           type = "latex",
           out = paste0(out, "HUM_E1_A_JPE.tex"),
           model.numbers = F,
           keep = c("ofcolor"),
           covariate.labels = c("Racial Minority"),
           keep.stat=c("n","adj.rsq"),
           digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
           add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y"),
                            c("Racial Comp Advert Home","Y","Y","Y","Y"),
                            c("Outcome Advertised Home","Y","Y","Y","Y")))

stargazer(  ES41,  ES42,  ES43, PR41, PR42, PR43 ,SK41,SK42,SK43, 
           type = "latex",
           out = paste0(out, "HUM_E1_B_JPE.tex"),
           model.numbers = F,
           keep = c("ofcolor"),
           covariate.labels = c("Racial Minority"),
           keep.stat=c("n","adj.rsq"),
           digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
           add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y"),
                            c("Racial Comp Advert Home","Y","Y","Y","Y"),
                            c("Outcome Advertised Home","Y","Y","Y","Y")))

stargazer( COL41, COL42, COL43, SF41,  SF42,  SF43, own41,own42,own43,
            type = "latex",
            out = paste0(out, "HUM_E1_C_JPE.tex"),
            model.numbers = F,
            keep = c("ofcolor"),
            covariate.labels = c("Racial Minority"),
            keep.stat=c("n","adj.rsq"),
            digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
            add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y"),
                             c("Racial Comp Advert Home","Y","Y","Y","Y"),
                             c("Outcome Advertised Home","Y","Y","Y","Y")))

stargazer( SP41, SP42,SP43,RSEI41,RSEI42,RSEI43, PM41,PM42,PM43,
           type = "latex",
           out = paste0(out, "HUM_E1_D_JPE.tex"),
           model.numbers = F,
           keep = c("ofcolor"),
           covariate.labels = c("Racial Minority"),
           keep.stat=c("n","adj.rsq"),
           digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
           add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y"),
                            c("Racial Comp Advert Home","Y","Y","Y","Y"),
                            c("Outcome Advertised Home","Y","Y","Y","Y")))


stargazer( ED_elem41_, ED_elem42_, ED_elem43_, ED_middle41_, ED_middle42_, ED_middle43_, AS41_, AS42_,AS43_,
           type = "latex",
           out = paste0(out, "HUM_E1_A_JPE_.tex"),
           dep.var.labels.include = F,
           column.labels = c("Assaults", "Elem School",  "Elem School", "Middle School"),
           model.numbers = F,
           keep = c("APRACE.x"),
           covariate.labels = c("African American",
                                "Hispanic",
                                "Asian",
                                "Other"),
           keep.stat=c("n","adj.rsq"),
           digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
           add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y"),
                            c("Racial Comp Advert Home","Y","Y","Y","Y"),
                            c("Outcome Advertised Home","Y","Y","Y","Y")))

stargazer(  ES41_,  ES42_,  ES43_,PR41_, PR42_, PR43_,SK41_,SK42_,SK43_,  
            type = "latex",
            out = paste0(out, "HUM_E1_B_JPE_.tex"),
            dep.var.labels.include = F,
            column.labels = c("Assaults", "Elem School",  "Elem School", "Middle School"),
            model.numbers = F,
            keep = c("APRACE.x"),
            covariate.labels = c("African American",
                                 "Hispanic",
                                 "Asian",
                                 "Other"),
            keep.stat=c("n","adj.rsq"),
            digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
            add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y"),
                             c("Racial Comp Advert Home","Y","Y","Y","Y"),
                             c("Outcome Advertised Home","Y","Y","Y","Y")))

stargazer( COL41_, COL42_, COL43_, SF41_,  SF42_,  SF43_, own41_,own42_,own43_,
           type = "latex",
           out = paste0(out, "HUM_E1_C_JPE_.tex"),
           dep.var.labels.include = F,
           column.labels = c("Assaults", "Elem School",  "Elem School", "Middle School"),
           model.numbers = F,
           keep = c("APRACE.x"),
           covariate.labels = c("African American",
                                "Hispanic",
                                "Asian",
                                "Other"),
           keep.stat=c("n","adj.rsq"),
           digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
           add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y"),
                            c("Racial Comp Advert Home","Y","Y","Y","Y"),
                            c("Outcome Advertised Home","Y","Y","Y","Y")))

stargazer( SP41_, SP42_,SP43_,RSEI41_,RSEI42_,RSEI43_, PM41_,PM42_,PM43_,
           type = "latex",
           out = paste0(out, "HUM_E1_D_JPE_.tex"),
           dep.var.labels.include = F,
           column.labels = c("Assaults", "Elem School",  "Elem School", "Middle School"),
           model.numbers = F,
           keep = c("APRACE.x"),
           covariate.labels = c("African American",
                                "Hispanic",
                                "Asian",
                                "Other"),
           keep.stat=c("n","adj.rsq"),
           digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
           add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y"),
                            c("Racial Comp Advert Home","Y","Y","Y","Y"),
                            c("Outcome Advertised Home","Y","Y","Y","Y")))
