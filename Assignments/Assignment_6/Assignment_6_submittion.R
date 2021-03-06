df <- read.csv("./Data/BioLog_Plate_Data.csv")
####### Q1 and Q2 are fulfilled below

df010 <- df %>%
  pivot_longer(starts_with("Hr_"),names_to = "Time",
               values_to = "Absorbance", names_prefix = "Hr_") %>% 
  mutate(Time = as.numeric(Time)) %>% 
  mutate(SampleType = case_when(`Sample.ID` == "Clear_Creek" ~ "Water",
                                `Sample.ID` == "Waste_Water" ~ "Water",
                                TRUE ~ "Soil")) %>% 
  filter(Dilution == 0.1) 

# this is the graphs for Q3

df010 %>% 
  ggplot(aes(x=Time,y=Absorbance,color = SampleType)) +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(Substrate))

# I then started again in order to remove the filter Dilution 0.1 function

df1 <- df %>% 
  pivot_longer(starts_with("Hr_"),names_to = "Time",
               values_to = "Absorbance", names_prefix = "Hr_") %>% 
  mutate(Time = as.numeric(Time)) %>% 
  mutate(SampleType = case_when(`Sample.ID` == "Clear_Creek" ~ "Water",
                                `Sample.ID` == "Waste_Water" ~ "Water",
                                TRUE ~ "Soil"))

# and then started to organize the data that I was going to use for Q4

# I was unable to isolate the mean of the 3 sample runs based on the 
# Sample.ID. Then when I would use the line graph it would generate vertical
# lines at the hours(I am assuming that is due to the several observed absorbance
# readings). Below is what I had.

a <- df1 %>% 
  filter(Substrate == "Itaconic Acid") %>% 
  group_by(Sample.ID) %>% 
  arrange(desc(Dilution)) %>% 
  summarize(Dilution,Sample.ID,Absorbance,Time,Substrate)

library(gganimate)

a %>% 
  ggplot(aes(x=Time,y=Absorbance,color=Sample.ID,group=Dilution)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(Dilution)) +
  gganimate::transition_reveal(Time)

# I had even tried to select the rows individually for the mean after arranging
# the data by the dilutions but I wasn't able to figure that out either.


# note from class
df %>% 
  pivot_longer(starts_with("Hr_"),names_to = "time",values_to = "absorbance",
               names_prefix = "Hr_") %>% 
  mutate(time = as.numeric(time)) %>% 
  filter(Substrate == "Itaconic Acid") %>% 
  ggplot(aes(x=time,y=absorbance)) +
  geom_smooth() +
  facet_wrap(vars(Dilution)) +
  gganimate::transition_reveal(time)

library(tidyverse)
library(gganimate)
library(janitor)
BioLog <- read_csv("./Data/BioLog_Plate_Data.csv")
b <- BioLog %>% 
  pivot_longer(starts_with("Hr_"),
               names_to = "Time",
               values_to = "Absorbance",
               names_prefix = "Hr_") %>% 
  mutate(Time = as.numeric(Time)) %>% 
  filter(Substrate == "Itaconic Acid") %>% 
  select(`Sample ID`,Time,Dilution,Substrate,Absorbance) %>% 
  group_by(Time,Dilution) 

%>% 
  summarise(Dilution) 

%>% 
  ggplot(aes(x=Time,y=Absorbance,color=`Sample ID`,group=Dilution)) +
  geom_point()+
  geom_line()+
  facet_wrap(vars(Dilution)) +
  transition_reveal(Time)
