################################################################################
# PROGRAM: cigarette.R
# PURPOSE: Examining pain across those who quit smoking in POINT cohort
# WRITTEN BY: Phillip Hungerford
# DATE: 26 May 2019
################################################################################
# 0. Load dependencies
library(dplyr)
################################################################################
# 1. Load data
dir <- "J:/Projects/Pharmaceutical Opioid Cohort/xPhillipH/ndarc-point-cleaning/0_data/processed/core.csv"
df <- read.csv(dir)

################################################################################
# EDA
smokers <- df %>% 
  select(participant_id, wave, cig_12m)

bs_smokers <- subset(smokers, wave == "Baseline")
y1_smokers <- subset(smokers, wave == "Year1")
y2_smokers <- subset(smokers, wave == "Year2")
y3_smokers <- subset(smokers, wave == "Year3")
y4_smokers <- subset(smokers, wave == "Year4")
y5_smokers <- subset(smokers, wave == "Year5")

sum(bs_smokers$cig_12m == "Yes", na.rm = T)
sum(y1_smokers$cig_12m == "Yes", na.rm = T)
sum(y2_smokers$cig_12m == "Yes", na.rm = T)
sum(y3_smokers$cig_12m == "Yes", na.rm = T)
sum(y4_smokers$cig_12m == "Yes", na.rm = T)
sum(y5_smokers$cig_12m == "Yes", na.rm = T)


################################################################################
# 2. Create smoker flag
smokers <- df %>% 
  select(participant_id, wave, cig_12m)

# find people who smoke in baseline
tmp <- subset(smokers, wave=="Baseline")
tmp <- subset(tmp, cig_12m == "Yes")
bs_smokers <- tmp$participant_id
# Create data frame of people who smoked from baseline onwards
smokers <- subset(smokers, participant_id %in% bs_smokers)
# filter through to have participants who have data for all years
keep <- NULL
for (i in 1:length(tmp$participant_id)){
  tmp2 <- subset(smokers, participant_id == tmp$participant_id[i])
  if (sum(tmp2[,1], na.rm=T) / 6 == tmp$participant_id[i]){
    keep[i] <- tmp$participant_id[i]
    }
}

keep <- unique(keep) # 323 participants who stay full term 5 years
ft_smokers <- subset(smokers, participant_id %in% keep)
################################################################################
# 3. Find smokers who quit at each interval 

# Create function that can detect who quit at what year 
quit_detector <- function(ft_smokers, year){
  # Create placeholder to flag
  ft_smokers$quit <- 0
  # Get list of participants who smoke into a list
  participants <- unique(ft_smokers$participant_id)
  # Iterate through each participant
  
  for (participant in participants){
    tmp <- subset(ft_smokers, participant_id == participant)
    smoking <- tmp[,3]
    status <- unique(smoking[(year+1):6])
    
    if ( length(status) < 2){
      if (status == "No"){
        ft_smokers$quit[ft_smokers$participant_id == participant] <- "Yes"}
    }}
  ft_smokers$quit[ft_smokers$quit == 0] <- "No"
  ft_smokers$quit <- as.factor(ft_smokers$quit)
return(ft_smokers)
}

# People who quit at Year1
y1_quitters <- quit_detector(ft_smokers, year=1)

candidates <- unique(y1_quitters$participant_id) #322 people quit smoking at t1
################################################################################
# 4. EDA

################################################################################
# 5. Analysis

################################################################################
##################################### END  #####################################
################################################################################