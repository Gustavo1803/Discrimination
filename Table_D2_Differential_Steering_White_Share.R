# ---------------------------------------------------------------------------------------------- #
#   Generate Table D.2 Differential Steering and White Share                                     #
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

#Load Data
recs_trial_final_tract <- readRDS("HUDprocessed_tract.rds")


#Create New Variables
recs_trial_final_tract$market <- as.factor(sapply(strsplit(as.character(recs_trial_final_tract$CONTROL), "-"), `[`, 1))

recs_trial_final_tract$tractpovrate_Rec= recs_trial_final_tract$tractpovrate_Rec/100
recs_trial_final_tract$tractpovrate_Ad= recs_trial_final_tract$tractpovrate_Ad/100

#Main Regressions

Wtract4_ <- felm(tractwhite_Rec ~ APRACE.x | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x| 0 | CONTROL, data = recs_trial_final_tract)
Wtract5_ <- felm(tractwhite_Rec ~ APRACE.x + logAdPrice + tractwhite_Ad + Minority_Population_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final_tract)


PRtract4_ <- felm(tractpovrate_Rec ~ APRACE.x  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final_tract)
PRtract5_ <- felm(tractpovrate_Rec ~ APRACE.x + tractpovrate_Ad + tractwhite_Ad + Minority_Population_Ad + logAdPrice  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final_tract)



recs_trial_final_tract$CONTROLTESTER <- paste(as.character(recs_trial_final_tract$TESTERID),as.character(recs_trial_final_tract$CONTROL), sep = "")


testermean <- aggregate(recs_trial_final_tract, by=list(recs_trial_final_tract$CONTROL, recs_trial_final_tract$TESTER), FUN=mean, na.rm=TRUE)
testermean$CONTROL <- testermean$Group.1
testermean_black <- subset(testermean, white.x==1 | black.x==1)
testermean_black <- within(testermean_black, { pairtest <- ave(black.x, CONTROL, FUN=function(x) mean(x))})
testermean_black <- subset(testermean_black, pairtest>0 & pairtest<1)

Wtract4mean_ <- felm(tractwhite_Rec ~ black.x |CONTROL | 0 | 0, data = testermean_black)
Wtract5mean_ <- felm(tractwhite_Rec ~ black.x + tractwhite_Ad + Minority_Population_Ad + logAdPrice|CONTROL | 0 | CONTROL, data = testermean_black)

PRtract4mean_ <- felm(tractpovrate_Rec ~ black.x |CONTROL | 0 | 0, data = testermean_black)
PRtract5mean_ <- felm(tractpovrate_Rec ~ black.x + tractwhite_Ad + Minority_Population_Ad + logAdPrice |CONTROL | 0 | 0, data = testermean_black)


## Generate Tables

stargazer(Wtract4_, Wtract5_, Wtract4mean_, Wtract5mean_,
          type = "latex",
          out = paste0(out, "Tab_D2_A.tex"),
          dep.var.caption = ("Dep. Variable: White Share"),
          title="Differential Steering and White Share",
          dep.var.labels.include = F,
          column.labels = c("Tract", "Tract", "Tester Mean", "Tester Mean"),
          model.numbers = F,
          keep = c("APRACE.x", "black.x"),
          covariate.labels = c("African American"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Poverty Rate Advert Home","Y","N","Y","N","Y"),
                           c("Racial Comp Advert Home","Y","N","Y","N","Y"),
                           c("ln(Price) Advert Home","Y","N","Y","N","Y")))

stargazer(PRtract4_, PRtract5_, PRtract4mean_, PRtract5mean_,
          type = "latex",
          out = paste0(out, "Tab_D2_B.tex"),
          dep.var.caption = ("Dep. Variable: Poverty Rate"),
          title="Differential Steering and Share Below Poverty Level",
          dep.var.labels.include = F,
          column.labels = c("Tract", "Tract", "Tester Mean", "Tester Mean"),
          model.numbers = F,
          keep = c("APRACE.x", "black.x"),
          covariate.labels = c("African American"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Poverty Rate Advert Home","Y","N","Y","N","Y"),
                           c("Racial Comp Advert Home","Y","N","Y","N","Y"),
                           c("ln(Price) Advert Home","Y","N","Y","N","Y")))

