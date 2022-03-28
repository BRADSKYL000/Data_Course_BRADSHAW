library(tidyverse)
library(modelr)
library(easystats)
library(GGally)

df <- read_csv("Data/GradSchool_Admissions.csv")

# we could have also used
# df$admit %>% as.logical()

df <- df %>% 
  mutate(admit=case_when(admit == 1 ~ TRUE,
                         admit == 0 ~ FALSE),
         rank=factor(rank))

df %>% glimpse()

ggpairs(df)

# This dataset is a logistic regresion where there are only two variables
mod1 <- glm(data=df,
            formula = admit ~ gre + gpa + rank,
            family = "binomial")

mod2 <- glm(data=df,
            formula = admit ~ gre + gpa,
            family = "binomial")

mod3 <- glm(data=df,
            formula = admit ~ gre,
            family = "binomial")

mod4 <- glm(data=df,
            formula = admit ~ gpa + rank,
            family = "binomial")

mod5 <- glm(data=df,
            formula = admit ~ rank,
            family = "binomial")

mod6 <- glm(data=df,
            formula = admit ~ gre + rank,
            family = "binomial")

mod7 <- glm(data=df,
            formula = admit ~ gpa,
            family = "binomial")

comps <- compare_performance(mod1,mod2,mod3,mod4,mod5,mod6,mod7,
                             rank=TRUE)
comps

comps %>% plot()

add_predictions(df,mod1,type = "response") %>% 
  ggplot(aes(x=gpa,y=pred,color=rank)) +
  geom_smooth() +
  labs(title = "acceptance prediction vs gpa score",
       subtitle = "separated based on univerity ranckings")

add_predictions(df,mod1,type = "response") %>% 
  ggplot(aes(x=gre,y=pred,color=rank)) +
  geom_smooth() +
  labs(title = "acceptance prediction vs gre score",
       subtitle = "separated based on univerity ranckings")
