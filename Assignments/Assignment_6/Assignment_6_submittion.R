df <- read.csv("./Data/BioLog_Plate_Data.csv")

df %>% 
  pivot_longer(starts_with("Hr_"),names_to = "time",values_to = "absorbance",
               names_prefix = "Hr_") %>% 
  mutate(time = as.numeric(time)) %>% 
  filter(Substrate == "L-Arginine") %>% 
  ggplot(aes(x=time,y=absorbance)) +
  geom_point() +
  facet_wrap(~Rep) +
  gganimate::transition_reveal(time)


df010 <- df %>% 
  pivot_longer(starts_with("Hr_"),names_to = "Time",
               values_to = "Absorbance", names_prefix = "Hr_") %>% 
  mutate(Time = as.numeric(Time)) %>% 
  mutate(SampleType = case_when(`Sample.ID` == "Clear_Creek" ~ "Water",
                                `Sample.ID` == "Waste_Water" ~ "Water",
                                TRUE ~ "Soil")) %>% 
  filter(Dilution == 0.1) 

df010 %>% 
  ggplot(aes(x=Time,y=Absorbance,color = SampleType)) +
  geom_line() +
  facet_wrap(vars(Substrate))

?select
