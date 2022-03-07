library(tidyverse)
library(modelr)
library(easystats)
file.choose()
df <- read_csv("Data/GradSchool_Admissions.csv")

# we could have also used
# df$admit %>% as.logical()

df <- df %>% 
  mutate(admit=case_when(admit == 1 ~ TRUE,
                   admit == 0 ~ FALSE),
                   rank=factor(rank))

library(GGally)
ggpairs(df)
# This dataset is a logistic regresion where there are only two variables
mod1 <- glm(data=df,
            formula = admit ~ gre + gpa + rank,
            family = "binomial")

add_predictions(df,mod1,type = "response") %>% 
  ggplot(aes(x=gpa,y=pred,color=rank)) +
  geom_smooth()

library(palmerpenguins)

penguins %>% glimpse()

pen <- penguins %>% 
  mutate(Fatty = case_when(body_mass_g > 5000 ~ TRUE,
                           body_mass_g <= 5000 ~ FALSE))

mod2 <- glm(data=pen,
            formula = Fatty ~ bill_length_mm * bill_depth_mm,
            family = "binomial")

add_predictions(penguins,mod2,type="response") %>% 
  ggplot(aes(x=bill_length_mm,y=pred)) +
  geom_smooth()

#cross validation

training_set <- penguins %>% 
  slice_sample(prop = .75)
#to get the other 25% we will use the following
testing_set <- anti_join(penguins,training_set)
#this is a check to be sure that you have all of the rows accounted for
nrow(training_set) + nrow(testing_set) == nrow(penguins)

mod3 <- glm(data=training_set,
            formula = bill_length_mm ~ sex*species*body_mass_g)

add_predictions(testing_set,mod3) %>% 
  ggplot(aes(x=body_mass_g)) +
  geom_point(aes(y=bill_length_mm,color=sex)) +
  geom_point(aes(y=pred))
# the colored points are the actual data points and the black is the prediction

