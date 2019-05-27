################################################################################
# PROGRAM: cigarette.R
# PURPOSE: Examining pain across those who quit smoking in POINT cohort
# WRITTEN BY: Phillip Hungerford
# DATE: 26 May 2019
################################################################################
# 0. Load dependencies
library(dplyr)
library(ggplot2)
################################################################################
# 1. Load data
dir <- "J:/Projects/Pharmaceutical Opioid Cohort/xPhillipH/ndarc-point-cleaning/0_data/processed/core.csv"
df <- read.csv(dir)

################################################################################
# EDA
smokers <- df %>% 
  select(participant_id, wave, cig_12m)

for (i in 0:5){
  tmp <- subset(smokers, wave== i)
  cat("\nWave: ", i, sum(tmp$cig_12m == "Yes", na.rm=T))
}

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
ft_smokers <- subset(df, participant_id %in% keep)
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
    
    smoking <- tmp[["cig_12m"]]
    status1 <- unique(smoking[1:year])
    status2 <- unique(smoking[(year+1):6])
    
    if (length(status1) < 2){
      if (length(status2) <2){
        if (status1 == "Yes" & status2 == "No"){
          ft_smokers$quit[ft_smokers$participant_id == participant] <- "Yes"}
    }}}
  
  ft_smokers$quit[ft_smokers$quit == 0] <- "No"
  ft_smokers$quit <- as.factor(ft_smokers$quit)
return(ft_smokers)
}

# People who quit at Year1
quitters <- quit_detector(ft_smokers, year=1)
candidates <- subset(quitters, quit == "Yes")
num_cand <- unique(candidates$participant_id)

################################################################################
# 4. EDA
candidates$wave <- as.numeric(candidates$wave)
ggplot(candidates, aes(x=wave, y=bpi_pscore)) +
 geom_point() + geom_smooth(method ='lm')

ggplot(candidates, aes(x=wave, y=bpi_interference)) +
  geom_point() + geom_smooth(method="lm")

ggplot(candidates, aes(x=wave, y=sf12_mcs)) +
  geom_point() + geom_smooth(method="lm")

ggplot(candidates, aes(x=wave, y=sf12_pcs)) +
  geom_point() + geom_smooth(method="lm")

ggplot(candidates, aes(x=wave, y=pseq_score)) +
  geom_point() + geom_smooth(method="lm")

ggplot(candidates, aes(x=wave, y=slp9)) +
  geom_point() + geom_smooth(method="lm")
################################################################################
# 5. Analysis

################################################################################
##################################### END  #####################################
################################################################################