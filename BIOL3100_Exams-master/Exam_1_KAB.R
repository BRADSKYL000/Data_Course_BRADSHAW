#1

ds <- read_csv("BIOL3100_Exams-master/Exam_1/cleaned_covid_data.csv")
ds

#2

library(tidyverse)
library(tidyr)
library(dplyr)

A_states <- ds[grepl("^A", ds$Province_State),]
A_states

#3

A_states %>% 
  ggplot(aes(x=Last_Update,y=Deaths,color=Province_State)) +
  geom_point(alpha=.5) +
  geom_smooth(method="lm",aes(group=Province_State),color="black",se=FALSE) +
  facet_wrap(vars(Province_State),scales = "free")

#4

state_max_fatality_rate <- select(ds,Province_State,Case_Fatality_Ratio,Last_Update) %>% 
  group_by(Province_State) %>% 
  arrange(desc(Case_Fatality_Ratio))

state_max_fatality_rate %>% 
pivot_wider(names_from = Province_State, 
            values_from = Case_Fatality_Ratio,
            values_fill = 0,
            id_cols = Last_Update)

# I was trying to use the dates in order to be able to maintain the data.
# It was the closest that I could get

#5

state_max_fatality_rate %>% 
  ggplot(aes(x=Province_State,y=Case_Fatality_Ratio)) +
  geom_bar()
