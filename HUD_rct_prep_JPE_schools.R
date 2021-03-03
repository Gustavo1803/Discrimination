#Clearing old workspace
rm(list = ls()) 

#Load Packages
## \Roaming\Microsoft\Windows\Network Shortcuts\share (141.142.208.117 (netshare-backup server (Samba, Ubuntu)))
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

#Set WD local
setwd("//141.142.208.117/share")

#Set WD netshare
#setwd("~/share")

#Load Data Shared Drive
rechomes <- read.csv("projects/HUD_Discrimination/stores/HDS_rawdata/rechomes.csv")
rhgeo <- read.csv("projects/HUD_Discrimination/stores/rhgeo_final.csv")
sales <- read.csv("projects/HUD_Discrimination/stores/HDS_rawdata/sales.csv")
tester <- read.csv("projects/HUD_Discrimination/stores/tester_1.csv")
assignment <- read.csv("projects/HUD_Discrimination/stores/HDS_rawdata/assignment.csv")
rhgeo_rechomes_merged_full1 <- readRDS("projects/discrimination_school/outputs/combined_test_scores.rds")

#rhgeo_rechomes_merged_full <- read.csv("projects/HUD_Discrimination/stores/rhgeo_rechomes_merged_full.csv")
rechomes <- rhgeo_rechomes_merged_full


# preprocess rechomes

rechomes$white <- 0
rechomes$white[rechomes$RACEID == 11 | rechomes$RACEID == 21 | rechomes$RACEID == 31] <- 1

rechomes$black <- 0
rechomes$black[rechomes$RACEID == 12 | rechomes$RACEID == 22 | rechomes$RACEID == 32] <- 1

rechomes$hispanic <- 0
rechomes$hispanic[rechomes$RACEID == 13 | rechomes$RACEID == 23 | rechomes$RACEID == 33] <- 1

rechomes$asian <- 0
rechomes$asian[rechomes$RACEID == 14 | rechomes$RACEID == 24 | rechomes$RACEID == 34] <- 1

rechomes$natamer <- 0
rechomes$natamer[rechomes$RACEID == 15 | rechomes$RACEID == 25 | rechomes$RACEID == 35] <- 1

rechomes$other <- 0
rechomes$other[rechomes$RACEID == 16 | rechomes$RACEID == 26 | rechomes$RACEID == 36] <- 1


rechomes$minority <- 0
rechomes$minority[rechomes$RACEID == 12 | rechomes$RACEID == 22 | rechomes$RACEID == 32] <- 1
rechomes$minority[rechomes$RACEID == 13 | rechomes$RACEID == 23 | rechomes$RACEID == 33] <- 2
rechomes$minority[rechomes$RACEID == 14 | rechomes$RACEID == 24 | rechomes$RACEID == 34] <- 3
rechomes$minority[rechomes$RACEID == 15 | rechomes$RACEID == 25 | rechomes$RACEID == 35] <- 4
rechomes$minority[rechomes$RACEID == 16 | rechomes$RACEID == 26 | rechomes$RACEID == 36] <- 5
rechomes$minority <- as.factor(rechomes$minority)

# preprocess rhgeo

rhgeo$CONTROL <- rhgeo$ï..CONTROL

# preprocess tester
tester$TESTERID <- tester$TCID

# preprocess sales
sales$date <- as.Date(sales$SAPPTD, format="%m/%d/%Y")
sales$datepos <- as.POSIXlt(sales$date)
sales$month <- as.numeric(sales$datepos$mon+1)

# preprocess tester
tester$TESTERID <- tester$TesterID
tester$date <- as.Date(tester$TDOB, format="%m/%d/%Y")
tester$datepos <- as.POSIXlt(tester$date)
tester$year <- as.numeric(tester$datepos$year+1900)
tester$age <- as.numeric(2012-tester$year)

tester$SubAsian <- as.numeric(tester$TASIANG)
tester$SubHispanic <- as.numeric(tester$THISPUBG)
tester$APRACE[tester$SubAsian ==1 | tester$SubAsian == 2 | tester$SubAsian == 3| tester$SubAsian == 4 | tester$SubAsian == 5 | tester$SubAsian == 6] <- 4
tester$APRACE[tester$SubHispanic ==1 | tester$SubHispanic == 2 | tester$SubHispanic == 3| tester$SubHispanic == 4 | tester$SubHispanic == 5 | tester$SubHispanic == 6 | tester$SubHispanic == 7 | tester$SubHispanic == 8] <- 3

tester$APRACE <- as.factor(tester$APRACE)


tester$TASIANG <- as.factor(tester$TASIANG)
tester$THISPUBG[tester$THISPUBG ==8] <- -1
tester$THISPUBG <- as.factor(tester$THISPUBG)
tester$THHEGAI <- as.factor(tester$THHEGAI)
tester$TPEGAI <- as.factor(tester$TPEGAI)
tester$THIGHEDU <- as.factor(tester$THIGHEDU)


# Merge with addresses on trial
 #rechomes_rhgeo <- merge(rechomes, rhgeo, by = c("CONTROL", "TESTERID", "SEQRH"))
rechomes_rhgeo_sales <- merge(rechomes,sales, by = c("CONTROL", "TESTERID"))
rechomes_rhgeo_sales_tester <- merge(rechomes_rhgeo_sales, tester, by = c("TESTERID"))
rechomes_rhgeo_sales_assignment <- merge(rechomes_rhgeo_sales_tester, assignment, by = c("CONTROL", "TESTERID"))


#split data into advertised and recommended
ads <- subset(rechomes_rhgeo_sales_assignment, HADHOME==1)
recs <- subset(rechomes_rhgeo_sales_assignment, HADHOME==0)

# preprocess recs 
recs$RACE_Rec <- as.factor(recs$RACEID.x)
recs$logRecPrice <- as.numeric(log(recs$HPRICE + 1))
recs$Superfund_Proximity_Rec <- as.numeric(recs$Superfund_Proximity)
recs$Diesel_PM_Rec <- as.numeric(recs$NATA._Diesel_PM)
recs$Air_Toxics_Cancer_Risk_Rec <- as.numeric(recs$NATA._Air_Toxics_Cancer_Risk)
recs$RespiratoryHazardIndex_Rec <- as.numeric(recs$NATA._Respiratory_Hazard_Index)
recs$pctwhite_Rec <- as.numeric(recs$pctwhite)
recs$Assault_Rec <- as.factor(recs$Assault)
recs$High_School_Score_Rec <- as.factor(recs$High_Schol_Score)
recs$STOTUNIT_Rec <- as.numeric(recs$STOTUNIT)
recs$HINT1_Rec <- as.factor(recs$HINT1)
recs$Minority_Population_Rec <- as.numeric(recs$Minority_Population.)
recs$Low_Income_Population_Rec <- as.numeric(recs$Low_Income_Population.)
recs$HSITEAD_Rec <- as.character(recs$HSITEAD)
recs$HCITY_Rec <- as.character(recs$HCITY)
recs$HSTATE_Rec <- as.character(recs$HSTATE)
recs$HZIP_Rec <- as.character(recs$HZIP)


# preprocess ads 
ads$RACE_Ad <- as.factor(ads$RACEID.x)
ads$logAdPrice <- as.numeric(log(ads$HPRICE + 1))
ads$Superfund_Proximity_Ad <- as.numeric(ads$Superfund_Proximity)
ads$Diesel_PM_Ad <- as.numeric(ads$NATA._Diesel_PM)
ads$Air_Toxics_Cancer_Risk_Ad <- as.numeric(ads$NATA._Air_Toxics_Cancer_Risk)
ads$RespiratoryHazardIndex_Ad <- as.numeric(ads$NATA._Respiratory_Hazard_Index)
ads$pctwhite_Ad <- as.numeric(ads$pctwhite)
ads$zip_Ad <- as.factor(ads$Zip_Code)
ads$stfid_Ad <- as.factor(ads$stfid)
ads$Assault_Ad <- as.factor(ads$Assault)
ads$High_School_Score_Ad <- as.factor(ads$High_Schol_Score)
ads$HINT1_Ad <- as.factor(ads$HINT1)
ads$Minority_Population_Ad <- as.numeric(ads$Minority_Population.)
ads$Low_Income_Population_Ad <- as.numeric(ads$Low_Income_Population.)

# Merge on trial
recs_trial <- merge(recs,ads, by = c("CONTROL", "TESTERID"))


# Final dataset

#recs_trial_nomiss <- recs_trial[c("CONTROL", "TESTERID", "white.x", "black.x", "hispanic.x", "asian.x", "natamer.x", "RACE_Rec", "pctwhite_Ad", "pctwhite_Rec", "logAdPrice", "logRecPrice", "Superfund_Proximity_Rec", "Diesel_PM_Rec", "Air_Toxics_Cancer_Risk_Rec", "RespiratoryHazardIndex_Rec", "Superfund_Proximity_Ad", "Diesel_PM_Ad", "Air_Toxics_Cancer_Risk_Ad", "RespiratoryHazardIndex_Ad", "HCITY.x.x", "SEQUENCE.x.x", "month.x", "zip_Ad", "stfid_Ad", "High_School_Score_Rec", "Assault_Rec", "High_School_Score_Ad", "Assault_Ad", "ARELATE2.x", "HHMTYPE.x", "SAVLBAD.x", "STOTUNIT_Rec", "SAPPTAM.x", "AAGE1.x", "TSEX.x")]
#recs_trial_nomiss <- recs_trial[c("CONTROL", "TESTERID", "white.x", "black.x", "hispanic.x", "asian.x", "natamer.x", "RACE_Rec", "pctwhite_Ad", "pctwhite_Rec", "logAdPrice", "logRecPrice", "Superfund_Proximity_Rec", "Diesel_PM_Rec", "Air_Toxics_Cancer_Risk_Rec", "RespiratoryHazardIndex_Rec", "Superfund_Proximity_Ad", "Diesel_PM_Ad", "Air_Toxics_Cancer_Risk_Ad", "RespiratoryHazardIndex_Ad", "HCITY.x.x", "SEQUENCE.x.x", "month.x", "zip_Ad", "stfid_Ad", "High_School_Score_Rec", "Assault_Rec", "High_School_Score_Ad", "Assault_Ad", "ARELATE2.x", "HHMTYPE.x", "SAVLBAD.x", "STOTUNIT_Rec", "SAPPTAM.x")]
#recs_trial_nomiss <- recs_trial[c("CONTROL", "TESTERID", "white.x", "minority.x", "black.x", "hispanic.x", "asian.x", "natamer.x", "other.x", "RACE_Rec", "pctwhite_Ad", "pctwhite_Rec", "logAdPrice", "logRecPrice", "Superfund_Proximity_Rec", "Diesel_PM_Rec", "Air_Toxics_Cancer_Risk_Rec", "RespiratoryHazardIndex_Rec", "Superfund_Proximity_Ad", "Diesel_PM_Ad", "Air_Toxics_Cancer_Risk_Ad", "RespiratoryHazardIndex_Ad", "HCITY.x.x", "SEQUENCE.x.x", "month.x", "zip_Ad", "stfid_Ad", "High_School_Score_Rec", "Assault_Rec", "High_School_Score_Ad", "Assault_Ad", "ARELATE2.x", "HHMTYPE.x", "SAVLBAD.x", "STOTUNIT_Rec", "SAPPTAM.x", "HINT1_Ad", "HINT1_Rec")]
recs_trial_final <- recs_trial[c("CONTROL", "TESTERID", "white.x", "minority.x", "black.x", "hispanic.x", "asian.x", "natamer.x", "other.x", "APRACE.x", "TASIANG.x", "THISPUBG.x", "RACE_Rec", "pctwhite_Ad", "pctwhite_Rec", "Minority_Population_Rec", "Low_Income_Population_Rec", "Low_Income_Population_Ad", "Minority_Population_Ad",  "logAdPrice", "logRecPrice", "Superfund_Proximity_Rec", "Diesel_PM_Rec", "Air_Toxics_Cancer_Risk_Rec", "RespiratoryHazardIndex_Rec", "Superfund_Proximity_Ad", "Diesel_PM_Ad", "Air_Toxics_Cancer_Risk_Ad", "RespiratoryHazardIndex_Ad", "HCITY.x", "SEQUENCE.x.x", "month.x", "zip_Ad", "stfid_Ad", "High_School_Score_Rec", "Assault_Rec", "High_School_Score_Ad", "Assault_Ad", "ARELATE2.x", "HHMTYPE.x", "SAVLBAD.x", "STOTUNIT_Rec", "SAPPTAM.x", "HINT1_Ad", "HINT1_Rec", "TSEX.x.x", "age.x", "THHEGAI.x", "TPEGAI.x", "THIGHEDU.x", "TCURTENR.x", "HSITEAD_Rec", "HCITY_Rec", "HSTATE_Rec", "HZIP_Rec")]

recs_trial_final <-subset(recs_trial_final, other.x==0)
saveRDS(recs_trial_final, "projects/HUD_Discrimination/stores/HUDprocessed_JPE.rds")
#saveRDS(ads, "projects/HUD_Discrimination/stores/adsprocessed_JPE.rds")
#saveRDS(recs, "projects/HUD_Discrimination/stores/recsprocessed_JPE.rds")
