###goals for this data set
##behaviours related to temperature and humidity
##Star's behaviour related to wind direction/speed (external stim)
##

#library(weathercan)
library(tidyverse)
library(lubridate)
library(corrplot)
library(wesanderson)

# Palette -----------------------------------------------------------------
pal <- wes_palette(name = "Zissou1", type = "continuous")
pal5 <- wes_palette(5, name = "Zissou1", type = "discrete")

color1<-pal[1] #blue
color2<-pal[2] #light blue
color3<-pal[3] #yellow
color4<-pal[4] #orange
color5<-pal[5] #red
new_pal <- c(color1, color3, color4, color5)

scale_color_manual(values=wes_palette(n=3, name="Zissou1"))
scale_fill_manual(values=wes_palette(n=3, name="Zissou1"))

# Weather data ------------------------------------------------------------
##load weather data from 2019-07-01 to present
#stations_search("Winnipeg", interval = "hour")

#Station 51097 WINNIPEG INTL A
W51097 <-weather_dl(station_ids = 51097, start = "2019-01-01", 
                    end = "2020-12-31", interval = "hour", format=F)
names(W51097)<-str_replace_all(names(W51097), " ", "_") 
write.csv(W51097,"W51097hourlydata.csv")

#if redoing this edit the csv to remove odd characters from var names
weather <- read.csv("W51097hourlydata.csv", header=T)

weather <- mutate(weather, DateTime = ymd_hm(Date_Time_LST)) 

weather <- weather %>%
  select(DateTime, Year,	Month, Day,	Time_LST,	Temp_C, 
         Rel_Hum,	Wind_Dir_10s_deg,	Wind_Spd_kmh)


# Merge behaviour and weather ----------------------------------------------------------------
#Load behaviour if needed
ZM_AS <- read.csv("20210212_AuroraStarZM.csv", header=T)
ZM_AS <- mutate(ZM_AS, Date = ymd(Date), 
                SessionTime = ymd_hms(SessionTime), hour=hour(SessionTime),
                DateTime= round_date(SessionTime, unit="hour")) 

#Total <- merge(ZM_AS, weather, by="DateTime") #1152 obs why?
total <- left_join(ZM_AS, weather, by="DateTime") #1861 obs

write.csv(total,"20210212_ZooMonitor_AS_ZM_weather.csv")


# START HERE --------------------------------------------------------------

total <- read.csv("20210212_ZooMonitor_AS_ZM_weather.csv", header=T)
#needs a fix so variable characterisic are correct 
total <- mutate(total, Date = ymd(Date), 
                SessionTime = ymd_hms(SessionTime)) 

#these are both suposed to do the same thing but give 1152 and 1861 obs???

# Exploring with graphs ----------------------------------------------------------------

#Temp both bears
ggplot (total, aes(x=Temp_C, y=behave, color=Continuous_Channel_1_Value))+
  scale_color_manual(values=new_pal) + 
  geom_smooth()

#Humidity both bears
ggplot (total, aes(x=Rel_Hum, y=behave, color=Continuous_Channel_1_Value))+
  scale_color_manual(values=new_pal) + 
  geom_smooth()

#Wind speed both bears
ggplot (total, aes(x=Wind_Spd_kmh, y=behave, color=Continuous_Channel_1_Value))+
  scale_color_manual(values=new_pal) + 
  geom_smooth()

#Wind direction both bears
ggplot (total, aes(x=Wind_Dir_10s_deg, y=behave, color=Continuous_Channel_1_Value))+
  scale_color_manual(values=new_pal) + 
  geom_smooth()
##ST peak around 24 - what does that mean?

# Just Stereo -------------------------------------------------------------
SteroAS <- total %>%
  filter (Continuous_Channel_1_Value == "Stereotypies")
SteroA <- total %>%
  filter (Continuous_Channel_1_Value == "Stereotypies", Bear_Name=="Aurora")
SteroS <- total %>%
  filter (Continuous_Channel_1_Value == "Stereotypies", Bear_Name=="Star")

ggplot (SteroAS, aes(x=Date, y=behave, color=Bear_Name))+
  scale_color_manual(values=new_pal) + 
  geom_smooth()

ggplot (SteroS, aes(x=Wind_Dir_10s_deg, y=behave))+
  scale_color_manual(values=new_pal) + 
  geom_smooth()



