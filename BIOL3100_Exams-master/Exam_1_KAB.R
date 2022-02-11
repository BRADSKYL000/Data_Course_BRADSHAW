#1

ds <- read_csv("BIOL3100_Exams-master/Exam_1/cleaned_covid_data.csv")
ds

#2

library(tidyverse)

A_states <- ds[grepl("^A", ds$Province_State),]
A_states

#3

A_states %>% 
  ggplot(aes(x=Last_Update,y=Deaths,color=Province_State)) +
  geom_point(alpha=.5) +
  geom_smooth(method="lm",aes(group=Province_State),color="black",se=FALSE) +
  facet_wrap(vars(Province_State),scales = "free")

#4

df <-  ds['Province_State', 'Case_Fatality_Ratio']
state_max_fatality_rate <- ds %>% group_by(Province_State) %>% 
  summaraize(max= max(Case_Fatality_Ratio))

state_max_fatality_rate

ds %>% group_by(Province_State) %>% top_n(1,x)

max(ds$Case_Fatality_Ratio)

state_max_fatality_rate <- ds %>% 
  group_by(Province_State) %>% 
  arrange(desc(Case_Fatality_Ratio))

df
?data.frame
f