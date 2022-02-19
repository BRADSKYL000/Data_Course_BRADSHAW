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

state_max_fatality_rate <- ds %>% 
  group_by(Province_State) %>% 
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio,na.rm = TRUE)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))


# I was trying to use the dates in order to be able to maintain the data.
# It was the closest that I could get

#5

state_max_fatality_rate %>% 
  mutate(orderedstate = factor(Province_State,levels = Province_State)) %>% 
  ggplot(aes(x = orderedstate, y = Maximum_Fatality_Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))


#Extra

ds %>% 
  group_by(Last_Update) %>% 
  summarize(cumulativedeath = sum(Deaths)) %>% 
  ggplot(aes(x = Last_Update, y = cumulativedeath)) +
  geom_point()

#the rest of the material went by too quick on 2/17 or Week6_Day2
# it should be good now 2/19