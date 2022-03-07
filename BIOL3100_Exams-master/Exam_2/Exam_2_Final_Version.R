library(tidyverse)
#1
ds <- read_csv("BIOL3100_Exams-master/Exam_2/unicef-u5mr.csv")
#2
ds1 <- ds %>% 
  pivot_longer(starts_with("U5MR."),names_to = "Year",
               values_to = "U5MR", names_prefix = "U5MR.") %>% 
  mutate(Year = as.numeric(Year))
#3
ds1 %>% 
  ggplot(aes(x=Year,y=U5MR)) +
  geom_path() + 
  facet_wrap(~Continent)
#4
# it is saved as BRADSHAW_plot_1.png in the Exam_2 folder.
#5
ds1 %>%  
  group_by(Continent,Year) %>% 
  summarise(U5MR_Mean = mean(U5MR,na.rm = T))%>% 
  ggplot(aes(x = Year,y = U5MR_Mean,color=Continent)) +
  geom_line()
#6
# it is saved as BRADSHAW_plot_2.png in the Exam_2 folder.
#7
#Create 3 models of U5MR
#mod1 should account for only Year
#- mod2 should account for Year and Continent
#- mod3 should account for Year, Continent, and their interaction term
mod1 <- glm(data=ds1,
    formula = U5MR ~ Year)

mod2 <- glm(data=ds1,
            formula = U5MR ~ Year + Continent)

mod3 <- glm(data=ds1,
            formula = U5MR ~ Year * Continent)

#8
# Compare the three models with respect to their performance

summary(mod1)
summary(mod2)
summary(mod3)

compare_models(mods1,mod2,mod3,style = "se_p")
compare_performance(mod1,mod2,mod3) %>% plot()

# from the model comparisons I would assume that the third model is the best
# due to the model comparisons graph.

#9

add_predictions(ds1,mod1) %>% 
  ggplot(aes(x=Year)) +
  geom_smooth(method = "lm",aes(y=U5MR),se=FALSE) +
  geom_smooth(aes(y=pred)) +
  ylab("U5MR Prediction")

add_predictions(ds1,mod3) %>% 
  ggplot(aes(x=Year,color=Continent)) +
  geom_smooth(method = "lm",aes(y=U5MR),se=FALSE) +
  ylab("U5MR Prediction")

# I was unable to figure out the model 2 graph as I ran out of time before I 
# left for break.

#10

