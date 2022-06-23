# Introduction ----

# Author Information 
# Adam Grottoli
# Email: grottoli4@gmail.com

# Description: This code merges the old and new polar bear observation data into
# dataframes that are easy to perform analysis on. The code used is specific to 
# data collected at APC. Other organizations using this code as reference will need
# to modify variable names to be specific to their data.

# Libraries ----
library(tidyverse)
library(lubridate)

# Setup ----
# Change to appropriate workspace
setwd(paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/Polar Bear/",
             "Stereotypies/pb-stereotypy"))

#Load data ----
AdataNew <- read.csv("20220622 Aurora_report_study.csv", header=T)
SdataNew <- read.csv("20220622 Star_report_study.csv", header=T)
J2CdataOld <- read.csv("20210212 J2C_report_study.csv", header=T)
LIPBCCdataOld <- read.csv("20210212 LIPBCC_report_study.csv", header=T)

#Functions ---- 
###function to make binary data easier to work with
replaceBinary <- function(x) {
  ifelse(x == 'Yes', 1, 0)
}

# Data wrangle ----
## This makes the behaviour column have the specific type of stereotypy. 
## Specific stereotypies were not recorded for new data
J2CdataOld[which(J2CdataOld[,29]=="Other repetitive behaviour"),26] <- "Other repetitive behaviour"
J2CdataOld[which(J2CdataOld[,28]=="Swim pattern"),26] <- "Swim pattern"
J2CdataOld[which(J2CdataOld[,27]=="Pacing"),26] <- "Pacing"
LIPBCCdataOld[which(LIPBCCdataOld[,24]=="Other repetitive behaviour"),21] <- "Other repetitive behaviour"
LIPBCCdataOld[which(LIPBCCdataOld[,22]=="Pacing"),21] <- "Pacing"


## need to create a new dataframe with session details
## create meaningful names for each bear, select important variables for each dataframe. 
#  Unfortunately this needs to be done seperately for each file due to the different column names

AAdata <- AdataNew %>%
  rename(bear = Focal.Name, start_time = Session.Start.Time, end_time = Session.End.Time, 
         observation_area = Continuous.Channel.1.Name, animal_location = Continuous.Channel.1.Value, 
         duration_in_location = Continuous.Channel.1.Duration..sec., 
         behaviour = Continuous.Channel.2.Value, behaviour_duration = Continuous.Channel.2.Duration..sec., 
         care_behaviour = Continuous.Channel.3.Value, 
         care_behaviour_duration = Continuous.Channel.3.Duration..sec.,
         observation_type = Question..Type.of.Observation_94, 
         pacing_intensity = Question..Intensity.of.pacing_92, 
         sociality = Continuous.Channel.4.Value, 
         sociality_duration = Continuous.Channel.4.Duration..sec.,
         access_holding = Question..Holding_88, access_ocean = Question..Ocean_89, 
         access_churchill = Question..Churchill_90, 
         access_wapusk = Question..Wapusk_91, 
         access_maternity = Question..Maternity.Yard_93) %>%
  
  select(SessionID, bear, Observer, start_time,	end_time,	DateTime,
         Date, Time,	Year,	Month,	Hour, Duration, observation_area,	
         animal_location,	duration_in_location, behaviour, behaviour_duration, 
         care_behaviour, care_behaviour_duration,
         observation_type, pacing_intensity, sociality, sociality_duration, 
         access_holding, access_ocean, access_churchill, access_wapusk, 
         access_maternity) %>%
  mutate(pacing_intensity = as.character(pacing_intensity))


SAdata <- SdataNew %>%
  rename(bear = Focal.Name, start_time = Session.Start.Time, end_time = Session.End.Time, 
         observation_area = Continuous.Channel.1.Name,
        animal_location = Continuous.Channel.1.Value, 
        duration_in_location = Continuous.Channel.1.Duration..sec., 
        behaviour = Continuous.Channel.2.Value, 
        behaviour_duration = Continuous.Channel.2.Duration..sec., 
        care_behaviour = Continuous.Channel.3.Value, 
        care_behaviour_duration = Continuous.Channel.3.Duration..sec.,
        observation_type = Question..Type.of.Observation_95, 
        pacing_intensity = Question..Intensity.of.pacing_86, 
        access_holding = Question..Holding_82, access_north = Question..North_83, 
        access_middle = Question..Middle_84, 
        access_south = Question..South_85) %>%
  
select(SessionID, bear, Observer, start_time,	end_time,	DateTime,
       Date, Time,	Year,	Month,	Hour, Duration, observation_area,	
       animal_location,	duration_in_location, behaviour, behaviour_duration, 
       care_behaviour, care_behaviour_duration,
       observation_type, pacing_intensity, access_holding, access_north, 
       access_middle, access_south) 


J2CdataRe <- J2CdataOld %>%
  rename(bear = Focal.Name, start_time = Session.Start.Time, end_time = Session.End.Time, 
         observation_area = Continuous.Channel.3.Name,
         animal_location = Continuous.Channel.3.Value, 
         duration_in_location = Continuous.Channel.3.Duration.s, 
         behaviour = Continuous.Channel.1.Value, 
         behaviour_duration = Continuous.Channel.1.Duration.s, 
         sociality = Continuous.Channel.2.Value, 
         sociality_duration = Continuous.Channel.2.Duration.s,
         access_holding = Question..Holding_31, access_ocean = Question..Ocean_32, 
         access_churchill = Question..Churchill_33, 
         access_wapusk = Question..Wapusk_34, access_maternity = Question..Maternity.Yard_35) %>%
  
  select(SessionID, bear, Observer, start_time,	end_time,	DateTime,
         Date, Time,	Year,	Month,	Hour, Duration, observation_area,	
         animal_location,	duration_in_location, behaviour, behaviour_duration, 
         sociality, sociality_duration,access_holding, access_ocean, access_churchill,
         access_wapusk, access_maternity)

LIPBCCdataRe <- LIPBCCdataOld %>%
  rename(bear = Focal.Name, start_time = Session.Start.Time, end_time = Session.End.Time, 
         observation_area = Continuous.Channel.3.Name,
         animal_location = Continuous.Channel.3.Value, 
         duration_in_location = Continuous.Channel.3.Duration.s, 
         behaviour = Continuous.Channel.1.Value, 
         behaviour_duration = Continuous.Channel.1.Duration.s,
         sociality = Continuous.Channel.2.Value, 
         sociality_duration = Continuous.Channel.2.Duration.s,
         access_holding = Question..Holding_41, access_north = Question..North_42, 
         access_middle = Question..Middle_43, 
         access_south = Question..South_44) %>%
  
  select(SessionID, bear, Observer, start_time,	end_time,	DateTime,
         Date, Time,	Year,	Month,	Hour, Duration, observation_area,	
         animal_location,	duration_in_location, sociality, sociality_duration, 
         behaviour, behaviour_duration, access_holding, access_north, access_middle, 
         access_south)

oldJoin <- bind_rows(LIPBCCdataRe, J2CdataRe) ### join the old data together so next bit 
#only needs to be run once

###Need to simplify old data to align with new data
###This is just awful
oldJoin$behaviour <- str_replace(oldJoin$behaviour, 'Other locomotion','Active')
oldJoin$behaviour <- str_replace(oldJoin$behaviour, 'Eating/Drinking','Active')
oldJoin$behaviour <- str_replace(oldJoin$behaviour, 'Play','Active')
oldJoin$behaviour <- str_replace(oldJoin$behaviour, 'Asleep / At rest','Rest')
oldJoin$behaviour <- str_replace(oldJoin$behaviour, 'Rest/Asleep','Rest')
oldJoin$behaviour <- str_replace(oldJoin$behaviour, 'Locomoting','Active')
oldJoin$behaviour <- str_replace(oldJoin$behaviour, 'Other active','Active')
oldJoin$behaviour <- str_replace(oldJoin$behaviour, 'Self-directed','Rest')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Holding- Ocean','Holding')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Holding- Wapusk','Holding')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Wapusk- Other','Waspusk')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Wapusk- Pool','Waspusk')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Wapusk- Holding','Holding')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Ocean- Pool','Ocean')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Ocean- Other','Ocean')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Ocean- Holding','Holding')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Churchill- Tundra Grill','Churchill')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Churchill- Seal Beach','Churchill')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Churchill- Research','Churchill')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Churchill- PRT','Churchill')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Churchill- Pool','Churchill')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Churchill- Other','Churchill')
oldJoin$animal_location <- str_replace(oldJoin$animal_location, 'Porch','Holding')


## Bind the old and new dataframes

allBears <- bind_rows(oldJoin, SAdata, AAdata )

## this will make the different types of behaviour in the new and old data align 
## in the correct columns
allBears <- allBears %>%
  mutate(care_behaviour = ifelse(behaviour == "Interacting with Animal Care Specialist", 
                                 "Interacting with Animal Care Specialist", care_behaviour)) %>%
  mutate(behaviour = ifelse(behaviour == "Interacting with Animal Care Specialist", 
                            NA, behaviour)) %>%
  mutate(care_behaviour_duration = 
           ifelse(care_behaviour == "Interacting with Animal Care Specialist", 
                  behaviour_duration, care_behaviour_duration)) %>%
  mutate(behaviour_duration = 
           ifelse(care_behaviour == "Interacting with Animal Care Specialist", NA, 
                  behaviour_duration))

# Creating analysis dataframes ----
## Location summary dataframe----
locationSum <- allBears %>% 
  mutate(across(starts_with("access"), replaceBinary)) %>% 
  group_by(bear, SessionID) %>%
  drop_na(duration_in_location) %>%
  mutate(area_used = n_distinct(animal_location), na.rm = TRUE) %>% 
  ## counts the number of locations used in the session
  group_by(bear, SessionID, animal_location) %>%
  mutate(sum_duration = sum(duration_in_location)) %>% 
  ## counts the total number of seconds spent in each location
  ungroup() %>%
  mutate(prop_used = (area_used / (select(., access_holding:access_maternity) %>% 
                                     rowSums(na.rm = TRUE)))) %>% 
  ##A proportion of the number of locations used to the number of locations available. 
  distinct(bear, SessionID, Date, Time, animal_location, sum_duration, prop_used)

write.csv(all_survival, paste0("P:/Conservation_Research/Restricted/CRD/Research",
                               "Projects/Polar Bear/Stereotypies/pb-stereotypy"

##NOTE: Errors in data entry caused some proportions to be higher than 1.00 and 
# will need to be corrected

## Behaviour summary dataframe----
behaviourSum <- allBears %>%
  drop_na(behaviour_duration) %>%
  filter(!grepl('Change of behaviour', behaviour)) %>% 
  ##remove social behaviour changes
  group_by(bear, SessionID) %>%
  mutate(total_behaviour = sum(behaviour_duration)) %>% 
  group_by(bear, SessionID, behaviour) %>%
  mutate(sum_behaviour = sum(behaviour_duration)) %>% 
  ##gives the total time spent in each behaviour
  mutate(prop_behaviour = sum_behaviour/total_behaviour) %>% 
  ## gives proportion of time spent in behaviour relative to time spent in other behaviours
  distinct(bear, SessionID, Date, Time, animal_location, sum_behaviour,
           prop_behaviour, pacing_intensity)

## Interaction with keepers dataframe ----
interactionSum <- allBears %>% 
  drop_na(care_behaviour_duration) %>%
  group_by(bear, SessionID, care_behaviour) %>%
  mutate(sum_care_behaviour = sum(care_behaviour_duration)) %>%
  distinct(bear, SessionID, Date, Time, animal_location, care_behaviour, 
           sum_care_behaviour)

## Sociality dataframe ----
socialitySum <- allBears %>%
  drop_na(sociality_duration) %>%
  group_by(bear, SessionID) %>%
  mutate(total_sociality = sum(sociality_duration)) %>% 
  group_by(bear, SessionID, sociality) %>%
  mutate(sum_sociality = sum(sociality_duration)) %>% 
  ##gives the total time per session spent either in group or solo 
  mutate(prop_sociality = sum_sociality/total_sociality) %>%
  ## give proportion of time in each session spent alone and in a group
  distinct(bear, SessionID, Date, Time, animal_location, sociality, 
           sociality_duration, prop_sociality)
