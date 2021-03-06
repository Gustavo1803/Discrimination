#######1.Clearing old workspace#########
rm(list = ls()) 

#######2.Load Packages and Set Environment##################
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

##Set WD local
setwd("C:/Users/genin/OneDrive/Documents/Git/Discrimination/data")

## Housing Search  ######################################################################################################

#Preamble

# Unless Otherwise Specified, Model Sequence is as follows:
# Model 1: treatment effect conditional on advertisement price
# Model 2: treatment effect also conditional on level of treatment outcome in advertised listing
# Model 3: treatment effect also conditional on racial composition of block group of advertised listing
# Model 4: treatment effect also conditional on racial composition of block group of recommended listing
# Model 5: treatment effect also conditional on price of recommended home

######3.Load Data Sets#################

# All models cluster standard errors by trial 
#recs_trial_final <- readRDS("HUDprocessed_JPE.rds")
recs_trial_final <- readRDS("HUDprocessed.rds")

######4. Clean Data####################

#construct separate indicators for market and control
recs_trial_final$market <- as.factor(sapply(strsplit(recs_trial_final$CONTROL, "-"), `[`, 1))
recs_trial_final$CONTROL <- as.factor(recs_trial_final$CONTROL)

#construct indicators for race groups
recs_trial_final$ofcolor <- 0
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==2] <- 1
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==3] <- 1
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==4] <- 1
recs_trial_final$ofcolor <- as.factor(recs_trial_final$ofcolor)

#construct indicators for agent race
recs_trial_final$race <- recs_trial_final$APRACE.x
recs_trial_final$race[recs_trial_final$race==5] <- NA
recs_trial_final_hp <- subset(recs_trial_final, AdPrice > 245000)

# outcomes at mean price and racial composition
recs_trial_final <- subset(recs_trial_final, RecPrice<10000000)
recs_trial_final_men <- subset(recs_trial_final, TSEX.x.x==1)
recs_trial_final_kids <- subset(recs_trial_final, kids.x==1)
recs_trial_final_sqft <- subset(recs_trial_final, Sqft_Ad > 1872)

# Advertised home in neighborhood above 50th percentile white = w2012pc_Ad > .7405
recs_trial_final$white <- 0
recs_trial_final$white[recs_trial_final$APRACE.x==1] <- 1

######5. Estimates ##############################

#NEIGHBORHOOD POVERTY RATES
## Poverty rates are based on American Community Survey definitions by Census block group

# Of Color vs. White
PR4 <- felm(povrate_Rec ~ ofcolor + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
PR4_ <- felm(povrate_Rec ~ APRACE.x + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(PR4)
summary(PR4_)


# STEERING AND NEIGHBORHOOD HIGH Skill  
## Skill is share of census block group employed in ACS defined Management, business, science, and arts occupations

# Of Color vs. White
SK4 <- felm(skill_Rec ~ ofcolor + skill_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
SK4_ <- felm(skill_Rec ~ APRACE.x + skill_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(SK4)
summary(SK4_)


# STEERING INTO HIGHLY EDUCATED NEIGHBORHOODS 
## College is share of census block group with at least a college education

# Of Color vs. White
COL4 <- felm(college_Rec ~ ofcolor + college_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
COL4_ <- felm(college_Rec ~ APRACE.x + college_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(COL4)
summary(COL4_)


# STEERING and ELEMENTARY School Quality
## Great School Rankings in 2017

# Of Color vs. White
ES4 <- felm(Elementary_School_Score_Rec ~ ofcolor + Elementary_School_Score_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
ES4_ <- felm(Elementary_School_Score_Rec ~ APRACE.x + Elementary_School_Score_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


summary(ES4)
summary(ES4_)

# STEERING INTO HIGH Assault NEIGHBORHOODS 
## Number of nearby Assaults in 2017

# Of Color vs. White
AS4 <- felm(Assault_Rec ~ ofcolor + Assault_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
AS4_ <- felm(Assault_Rec ~ APRACE.x + Assault_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(AS4)
summary(AS4_)


# STEERING AND Superfund Proximity
#########Is this Superfund_Proximity_Rec??######
# Of Color vs. White
SP4 <- felm(SFcount_Rec ~ ofcolor + SFcount_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
SP4_ <- felm(SFcount_Rec ~ APRACE.x + SFcount_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(SP4)
summary(SP4_)

# STEERING INTO RSEI 
## RSEI data are based on 2012 measurements from EPA
#########Is this RespiratoryHazardIndex_Ad??######
# Of Color vs. White
RSEI4 <- felm(RSEI_Rec ~ ofcolor + RSEI_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
RSEI4_ <- felm(RSEI_Rec ~ APRACE.x + RSEI_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(RSEI4)
summary(RSEI4_)

# STEERING INTO PM2.5
#########Is this Diesel_PM_Rec??######
# Of Color vs. White
PM4 <- felm(PM25_Rec ~ ofcolor + PM25_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
PM4_ <- felm(PM25_Rec ~ APRACE.x + PM25_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(PM4)
summary(PM4_)
#######Table 8 Estimates################

out <- "projects/HUD_Discrimination/views/tables/"
out <- "C:/Users/pchrist/Desktop/"

p1 <- as.vector(PR4$cpval[1])
p2 <- as.vector(SK4$cpval[1])
p3 <- as.vector(COL4$cpval[1])
p4 <- as.vector(ES4$cpval[1])


p_1 <- sort( c(p1, p2, p3), decreasing = FALSE)
q1 <- p.adjust(p_1, method = "hochberg", n = length(p_1))

p5 <- as.vector(AS4$cpval[1])
p6 <- as.vector(SP4$cpval[1])
p7 <- as.vector( RSEI4$cpval[1])
p8 <- as.vector( PM4$cpval[1])

p_2 <- sort( c(p6, p7, p8), decreasing = FALSE)
q2 <- p.adjust(p_2, method = "hochberg", n = length(p_2))



stargazer(PR4, SK4, COL4, ES4, AS4, SP4, RSEI4, PM4,
          type = "latex",
          out = paste0(out, "HUM_Cen2.tex"),
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("p-values",signif(p_1, digits = 2),signif(p_2, digits = 2)),
                           c("q-values",signif(q1, digits = 2),signif(q2, digits = 2)),
                           c("ln(Price) Advert Home","Y","Y","Y","Y","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y","Y","Y","Y","Y")))


stargazer(PR4_, SK4_, COL4_, ES4_, AS4_, SP4_, RSEI4_, PM4_,
          type = "latex",
          out = paste0(out, "HUM_Cen_.tex"),
          title="Steering and Neighborhood Effects",
          dep.var.labels.include = F,
          column.labels = c("Poverty Rate","High Skill", "College", "Elem School", "Assaults", "Superfund", "Toxics", "PM"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels = c("African American",
                               "Hispanic",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y","Y","Y","Y","Y")))



stargazer(PR4, SK4, COL4, ES4,
          type = "latex",
          out = paste0(out, "HUM_Cen1.tex"),
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("p-values",signif(p_1, digits = 2), signif(p4, digits = 2)),
                           c("q-values",signif(q1, digits = 2), signif(p4, digits = 2)),
                           c("ln(Price) Advert Home","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y")))

stargazer(AS4, SP4, RSEI4, PM4,
          type = "latex",
          out = paste0(out, "HUM_Cen2.tex"),
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("p-values", signif(p5, digits = 2), signif(p_2, digits = 2)),
                           c("q-values", signif(p5, digits = 2),signif(q2, digits = 2)),
                           c("ln(Price) Advert Home","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y")))


stargazer(PR4_, SK4_, COL4_, ES4_,
          type = "latex",
          out = paste0(out, "HUM_Cen1_.tex"),
          title="Steering and Neighborhood Effects",
          dep.var.labels.include = F,
          column.labels = c("Poverty Rate","High Skill", "College", "Elem School"),
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


stargazer(AS4_, SP4_, RSEI4_, PM4_,
          type = "latex",
          out = paste0(out, "HUM_Cen2_.tex"),
          title="Steering and Neighborhood Effects",
          dep.var.labels.include = F,
          column.labels = c("Assaults", "Superfund", "Toxics", "PM"),
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

