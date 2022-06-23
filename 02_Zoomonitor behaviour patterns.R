###goals for this data set
##behaviours related to time of year; increase during breeding
##are Aurora and Star in syncrony
##are SB increasing over time

library(tidyverse)
library(lubridate)
library(corrplot)

library(GGally)

# Palette -----------------------------------------------------------------
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

 

# Load data from 01_Zoomonitor data wrangle ----------------------------------------------------
ZM_AS <- read.csv("20210212_AuroraStarZM.csv", header=T)

ZM_AS <- mutate(ZM_AS, Date = ymd(Date), 
                SessionTime = ymd_hms(SessionTime)) 
                
# Exploring All behaviours both bears -------------------------------------------------------------------
#box plot both bears all behaviours
ggplot(ZM_AS, aes(x=Continuous_Channel_1_Value, y=behave, color=Bear_Name))+ 
  scale_color_manual(values=new_pal) +
  geom_boxplot()

ggplot(ZM_AS, aes(x=Date, y=behave, color=Continuous_Channel_1_Value))+ 
  scale_color_manual(values=pal) +
  geom_smooth()


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

# Stereotypic Behaviours --------------------------------------------------
Stero_AS <- ZM_AS %>%
  filter (Continuous_Channel_1_Value == "Stereotypies")

#scatter plot is very messy but smoothed provides a pattern
ggplot(Stero_AS, aes(x=Date, y=behave, color = Bear_Name))+ 
  scale_color_manual(values=new_pal) +
  geom_smooth()

ggplot(Stero_AS, aes(x=as.factor(month(Date)), y=behave, color = Bear_Name))+ 
  scale_color_manual(values=new_pal) +
  geom_boxplot()


