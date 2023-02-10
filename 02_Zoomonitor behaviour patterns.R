# Introduction ----

###goals for this data set
##behaviours related to time of year; increase during breeding
##are Aurora and Star in syncrony?
##are SB increasing over time?

# Libraries ----

library(tidyverse)
library(lubridate)
library(corrplot)
library(ggplot2)
library(weathercan)

# Setup ----
# Change to appropriate workspace!!
setwd(paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/Polar Bear/",
             "Stereotypies/pb-stereotypy"))

# Create date frame with immobilization information
bear <- c("Star", "Aurora", "Aurora")
month <- c(8, 11, 10)
day <- c(20, 6, 19)
year <- c(2019, 2019, 2021)
date <- as.Date(c("2019-08-20", "2019-11-06", "2021-10-19"))
immobilization <- data.frame(bear, date, month, day, year)

# Palette
library(wesanderson)
pal <- wes_palette(name = "Zissou1", type = "continuous")
pal5 <- wes_palette(5, name = "Zissou1", type = "discrete")

color1<-pal[1] #blue
color2<-pal[2] #light blue
color3<-pal[3] #yellow
color4<-pal[4] #orange
color5<-pal[5] #red
new_pal <- c(color1, color3, color5)

scale_color_manual(values=wes_palette(n=3, name="Zissou1"))
scale_fill_manual(values=wes_palette(n=3, name="Zissou1"))

# Load data from 01_Zoomonitor data wrangle
behaviour_sum <- read.csv("./all_sessions.csv")

## Add day of year and week number 
behaviour_sum$doy <- as.numeric(strftime(behaviour_sum$Date, format = "%j"))
behaviour_sum$week <- lubridate::week(ymd(behaviour_sum$Date))
behaviour_sum$month <- lubridate::month(ymd(behaviour_sum$Date))

# Exploring behaviours of bears ----
#To visualize the effort put into each bear over time

unique_sessions <- behaviour_sum %>%
  group_by(bear, Year, month) %>%
  summarize(days_observed = n_distinct(doy)) %>%
  filter(bear %in% c("Aurora", "Star")) %>%
  unite(date, c(Year, month), sep = "-", remove = FALSE) %>%
  mutate(date2 = as.Date(paste(date,"-01",sep="")))

# Barplot: number of session per bear per month
ggplot(unique_sessions, aes(x = month, y = days_observed, fill = bear)) +
  stat_summary(geom = "bar", position = "dodge2") +
  facet_wrap(~Year) +
  labs(y = "Days Observed", x = "Month") +
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  scale_x_continuous(breaks = c(1:12))

# Split bears into unique dataframes
behaviour_sum_aurora <- behaviour_sum %>%
  filter(bear == "Aurora")

behaviour_sum_agee <- behaviour_sum %>%
  filter(bear == "Agee")

behaviour_sum_star <- behaviour_sum %>%
  filter(bear == "Star")

# Look at a proportion of time spent pacing time series using different span 
# parameters for Aurora and star - Vline represent immobilizations
ggplot(behaviour_sum_aurora, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_point() +
  geom_smooth(span = 0.75) +
  ylim(0, 1) +
  labs(y = "Proportion spent pacing", x = "Day of year")+
  geom_vline(xintercept = c(292, 310))

ggplot(behaviour_sum_aurora, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_point() +
  geom_smooth(span = 0.5) +
  ylim(0, 1) +
  labs(y = "Proportion spent pacing", x = "Day of year")

ggplot(behaviour_sum_aurora, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_point() +
  geom_smooth(span = 0.25) +
  ylim(0, 1) +
  labs(y = "Proportion spent pacing", x = "Day of year")

ggplot(behaviour_sum_star, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_point() +
  geom_smooth(span = 0.75) +
  ylim(0, 1) +
  labs(y = "Proportion spent pacing", x = "Day of year")+
  geom_vline(xintercept = 220)

ggplot(behaviour_sum_star, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_point() +
  geom_smooth(span = 0.5) +
  ylim(0, 1) +
  labs(y = "Proportion spent pacing", x = "Day of year") +
  geom_vline(xintercept = 220)

ggplot(behaviour_sum_star, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_point() +
  geom_smooth(span = 0.25) +
  ylim(0, 1) +
  labs(y = "Proportion spent pacing", x = "Day of year") +
  geom_vline(xintercept = 220)


ggplot(behaviour_sum_star, aes(x = agee_arrival, y = Pacing_prop_behaviour, group = agee_arrival)) +
  geom_boxplot()

ggplot(behaviour_sum_star, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_point() +
  geom_smooth(span = 0.5) +
  ylim(0, 1)

ggplot(behaviour_sum_star, aes(x = doy, y = sum_behaviour, group = Year, colour = Year)) +
  geom_point() +
  geom_smooth() +
  ylim(0, 1800) +
  scale_x_continuous(limits = c(0, 365), breaks = seq(0, 365, by = 20))

ggplot(behaviour_sum_aurora, aes(x = prop_available, y = Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_boxplot() +
  ylim(0, 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .1))



ggplot(behaviour_sum_agee, aes(x = doy, y = Pacing_prop_behaviour)) +
  stat_summary(geom = "bar") +
  ylim(0, 1) +
  scale_x_discrete(guide = guide_axis(angle = 25)) +
  labs(y = "Proportion of Observation Spent Pacing") 

ggplot(behaviour_sum_agee, aes(x = doy, y = Pacing_prop_behaviour)) +
  geom_point() +
  geom_smooth(method  = "loess") +
  labs(y = "Proportion spent pacing", x = "Day of year") +
  theme_bw()

# Combine all years and brek down the amount of time spent pacing each month
ggplot(behaviour_sum_aurora, aes(x = month, y = Pacing_prop_behaviour, group = month)) +
  geom_boxplot() +
  labs(y = "Proportion spent pacing", x = "Month") +
  scale_x_continuous(breaks = c(1:12)) +
  annotate("text",
           x = 1:length(table(behaviour_sum_aurora$month)),
           y = aggregate(Pacing_prop_behaviour ~ month, behaviour_sum_aurora, median)[ , 2],
           label = table(behaviour_sum_aurora$month),
           col = "red",
           vjust = - 1) 

ggplot(behaviour_sum_star, aes(x = month, y = Pacing_prop_behaviour, group = month)) +
  geom_boxplot() +
  labs(y = "Proportion spent pacing", x = "Month") +
  scale_x_continuous(breaks = c(1:12)) +
  annotate("text",
           x = 1:length(table(behaviour_sum_aurora$month)),
           y = aggregate(Pacing_prop_behaviour ~ month, behaviour_sum_aurora, median)[ , 2],
           label = table(behaviour_sum_aurora$month),
           col = "red",
           vjust = - 1)

ggplot(behaviour_sum_aurora, aes(x = prop_available, y = Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_point() +
  geom_smooth() +
  ylim(0, 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .1)))


# Visualize time spent resting and pacing vs. max temperature 
ggplot(behaviour_sum_aurora, aes(x = max_temp, y = Pacing_prop_behaviour)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 1) +
  labs(y = "Proportion spent pacing", x = "Max Daily Temp") +
  theme_bw()

ggplot(behaviour_sum_star, aes(x = max_temp, y = Pacing_prop_behaviour)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 1) +
  labs(y = "Proportion spent pacing", x = "Max Daily Temp") +
  theme_bw() 

ggplot(behaviour_sum_aurora, aes(x = max_temp, y = Rest_prop_behaviour)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 1) +
  labs(y = "Proportion spent resting", x = "Max Daily Temp") +
  theme_bw()

ggplot(behaviour_sum_star, aes(x = max_temp, y = Rest_prop_behaviour)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 1) +
  labs(y = "Proportion spent resting", x = "Max Daily Temp") +
  theme_bw() 

# Data removal validation ----
# How much data can we sacrifice before losing trends
ggplot(behaviour_sum_aurora, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_smooth(span = 0.25) +
  ylim(0, 1) +
  labs(y = "", x = "") + 
  theme(legend.position = "none")


aurora_remove20 <-  behaviour_sum_aurora %>% 
  group_by(Year, month) %>% 
  sample_frac(size = 1-0.20)

aurora_remove40 <-  behaviour_sum_aurora %>% 
  group_by(Year, month) %>% 
  sample_frac(size = 1-0.40)

aurora_remove60 <-  behaviour_sum_aurora %>% 
  group_by(Year, month) %>% 
  sample_frac(size = 1-0.60)

star_remove20 <-  behaviour_sum_star %>% 
  group_by(Year, month) %>% 
  sample_frac(size = 1-0.20)

star_remove40 <-  behaviour_sum_star %>% 
  group_by(Year, month) %>% 
  sample_frac(size = 1-0.40)

star_remove60 <-  behaviour_sum_star %>% 
  group_by(bear, Year, month) %>% 
  sample_frac(size = 1-0.60)

ggplot(aurora_remove20, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_smooth(span = 0.25) +
  ylim(0, 1) +
  labs(y = "", x = "") + 
  theme(legend.position = "none")

ggplot(aurora_remove40, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_smooth(span = 0.25) +
  ylim(0, 1) +
  labs(y = "", x = "") + 
  theme(legend.position = "none")

ggplot(aurora_remove60, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_smooth(span = 0.25) +
  ylim(0, 1) +
  labs(y = "", x = "") + 
  theme(legend.position = "none")

ggplot(star_remove20, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_smooth(span = 0.25) +
  ylim(0, 1) +
  labs(y = "", x = "") + 
  theme(legend.position = "none")

ggplot(star_remove40, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_smooth(span = 0.25) +
  ylim(0, 1) +
  labs(y = "", x = "") + 
  theme(legend.position = "none")

ggplot(star_remove60, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_smooth(span = 0.25) +
  ylim(0, 1) +
  labs(y = "", x = "") + 
  theme(legend.position = "none")

ggplot(behaviour_sum_star, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_smooth(span = 0.25) +
  ylim(0, 1) +
  labs(y = "", x = "") + 
  theme(legend.position = "none")

ggplot(behaviour_sum_star, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_point() +
  geom_smooth(span = 0.5) +
  ylim(0, 1) +
  labs(y = "Proportion spent pacing", x = "Day of year") +
  geom_vline(xintercept = 220)

ggplot(behaviour_sum_star, aes(x=doy, y=Pacing_prop_behaviour, group = Year, colour = Year)) +
  geom_point() +
  geom_smooth(span = 0.25) +
  ylim(0, 1) +
  labs(y = "Proportion spent pacing", x = "Day of year") +
  geom_vline(xintercept = 220)

ggplot(star_remove20, aes(x = month, y = Pacing_prop_behaviour, group = month)) +
  geom_boxplot() +
  labs(y = "Proportion spent pacing", x = "Month") +
  scale_x_continuous(breaks = c(1:12)) +
  annotate("text",
           x = 1:length(table(star_remove20$month)),
           y = aggregate(Pacing_prop_behaviour ~ month, star_remove20, median)[ , 2],
           label = table(star_remove20$month),
           col = "red",
           vjust = - 1)

# Correlation (haven't worked on this much)-------------------------------------------------------------
# pivot data to do a correlation analysis (thanks to LB for code)
corr <- ZM_AS %>%
  mutate(Continuous_Channel_1_Value = str_replace_all(Continuous_Channel_1_Value, " ","_"))%>%
  pivot_wider(names_from = Continuous_Channel_1_Value, values_from = behave)%>%
  mutate(Active = replace_na(Active,0),
         Out_of_sight = replace_na(Out_of_sight,0),
         Rest = replace_na(Rest,0),
         Stereotypies = replace_na(Stereotypies,0))%>%
  select(Active, Out_of_sight, Stereotypies, Rest)

ggcorr(corr) #no correlation beccause proportional data?

##all 4 behaviours add up to 100% of the session. will this affect analysis?
##seperate out just SB

# Would like to see if we remove mating season, what 
# happens to relationship with behaviour. We don't observe bears as often when it
# cold

aurora_remove_lowtemp <- behaviour_sum_aurora %>%
  filter(month %in% c(5,6,7,8,9,10,11,12))

ggplot(aurora_remove_lowtemp, aes(x = max_temp, y = Pacing_prop_behaviour)) +
  geom_point() +
  geom_smooth(method = "glm") +
  ylim(0, 1) +
  labs(y = "Proportion spent pacing", x = "Max Daily Temp") +
  theme_bw()

# Stereotypic Behaviours --------------------------------------------------
Stero_AS <- ZM_AS %>%
  filter (Continuous_Channel_1_Value == "Stereotypies")

#scatter plot is very messy but smoothed provides a pattern
ggplot(Stero_AS, aes(x=Date, y=behave, color = Bear_Name))+ 
  scale_color_manual(values=new_pal) +
  geom_scatter()

ggplot(Stero_AS, aes(x=as.factor(month(Date)), y=behave, color = Bear_Name))+ 
  scale_color_manual(values=new_pal) +
  geom_boxplot()