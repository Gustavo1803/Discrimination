# ---------------------------------------------------------------------------------------------- #
#   Generate Table C.4 Differential Steering and Neighborhood Racial Composition by Income       #
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


###Income Estimates####


# STEERING AND white NEIGHBORHOOD 
W_hi <- felm(HispHI_Rec ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + povrate_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x| 0 | CONTROL, data = recs_trial_final)
W_mi <- felm(HispMI_Rec ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + povrate_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
W_li <- felm(HispLI_Rec ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + povrate_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

# STEERING AND white NEIGHBORHOOD
W_hi_ <- felm(HispHI_Rec ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + povrate_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x| 0 | CONTROL, data = recs_trial_final)
W_mi_ <- felm(HispMI_Rec ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + povrate_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
W_li_ <- felm(HispLI_Rec ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + povrate_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(W_hi)
summary(W_mi)
summary(W_li)
summary(W_hi_)
summary(W_mi_)
summary(W_li_)

mean1=as.vector(c(mean(recs_trial_final[recs_trial_final$white.x==1,"HispHI_Rec"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"HispMI_Rec"],na.rm=TRUE),
                  mean(recs_trial_final[recs_trial_final$white.x==1,"HispLI_Rec"],na.rm=TRUE))) 

### GENERATE TABLES
out <- "C:/Users/genin/OneDrive/Documents/Git/Discrimination/output/"

### Racial Steering Tables
stargazer(W_hi, W_mi, W_li,
          type = "latex",
          out = paste0(out, "tabC4.tex"),
          dep.var.caption = ("Dep. Variable: African American Household Share by Income"),
          dep.var.labels.include = FALSE,
          column.labels = c("High Inc", "Mid Inc", "Low Inc"),
          model.numbers = FALSE,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Share White Advert Home","N","Y","Y","Y","Y"),
                           c("ln(Price) Advert Home","N","N","Y","Y","Y"),
                           c("Racial Comp Advert Home","N","N","N","Y","Y"),
                           c("Poverty Share Advert Home","N","N","N","N","Y")))


stargazer(W_hi_, W_mi_, W_li_,
          type = "latex",
          out = paste0(out, "tabC4_.tex"),
          dep.var.caption = ("Dep. Variable: Hispanic Household Share by Income"),
          title="Differential Steering and White Household Share",
          dep.var.labels.include = F,
          column.labels = c("High Inc", "Mid Inc", "Low Inc"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels = c("African American",
                               "Hispanic",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Share White Advert Home","N","Y","Y","Y","Y"),
                           c("ln(Price) Advert Home","N","N","Y","Y","Y"),
                           c("Racial Comp Advert Home","N","N","N","Y","Y"),
                           c("Poverty Share Advert Home","N","N","N","N","Y")))
