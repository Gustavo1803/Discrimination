# ---------------------------------------------------------------------------------------------- #
#   Generate Table F.1 Discriminatory Steering and Home Price                                    #
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



####################################################
#################Panel A############################
####################################################

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


#############################################################################################################################################################################
# STEERING and ELEMENTARY School Test Scores
## Stanford School Data

### Temporary (Switch to full models below when new data come in)
# Of Color vs. White
p1 <- felm(logRecPrice  ~ ofcolor | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
p2 <- felm(logRecPrice ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
p3 <- felm(logRecPrice ~ ofcolor + b2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad  + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


p1_ <- felm(logRecPrice  ~ APRACE.x | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
p2_ <- felm(logRecPrice ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
p3_ <- felm(logRecPrice ~ APRACE.x + b2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad  + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

mean(recs_trial_final[recs_trial_final$white.x==1,"logRecPrice"],na.rm=TRUE)

### GENERATE TABLES


stargazer( p1,p2,p3,
           type = "latex",
           out = paste0(out, "HUM_tab_F1.tex"),
           model.numbers = F,
           keep = c("ofcolor"),
           covariate.labels = c("Racial Minority"),
           keep.stat=c("n","adj.rsq"),
           digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
           add.lines = list(c("Racial Comp Advert Home","Y","Y","Y","Y"),
                            c("ln(Price) Advert Home","Y","Y","Y","Y")))

stargazer(p1_,p2_,p3_,
          type = "latex",
          out = paste0(out, "HUM_tab_F1_.tex"),
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels = c("African American",
                               "Hispanic",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Racial Comp Advert Home","Y","Y","Y","Y"),
                           c("ln(Price) Advert Home","Y","Y","Y","Y")))


