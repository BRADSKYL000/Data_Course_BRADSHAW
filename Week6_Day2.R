library(tidyverse)
library(palmerpenguins)

# We are going over case_when() functtion todya

penguins %>% names()

penguins %>% 
  ggplot(aes(x = species,y = body_mass_g)) +
  geom_jitter()

# we are wanting to issolate the penguins above 5000g in a different color than
#those less than 5000g

# We use case_when for this specific instant

penguins %>% 
  mutate(opinion=case_when(body_mass_g > 5000 ~ "chonky",
                           body_mass_g <= 5000 ~ "not chonky")) %>%
  ggplot(aes(x = species, y = body_mass_g,color= opinion)) +
  geom_jitter()

read.csv("./")

df <- read.csv("./Data/BioLog_Plate_Data.csv")
df %>% 
  pivot_longer(starts_with("Hr_"),names_to = "Time",
               values_to = "Absorbance", names_prefix = "Hr_")

#Timez is being viewed as a character so we muttate it to make it numeric

df %>% 
  pivot_longer(starts_with("Hr_"),names_to = "Time",
               values_to = "Absorbance", names_prefix = "Hr_") %>% 
  mutate(Timez = as.numeric(Time))

# we are wanting to look at the sample id in paticular

df1 <- df %>% 
  pivot_longer(starts_with("Hr_"),names_to = "Time",
               values_to = "Absorbance", names_prefix = "Hr_") %>% 
  mutate(Time = as.numeric(Time)) %>% 
  mutate(SampleType = case_when(`Sample.ID` == "Clear_Creek" ~ "Water",
                                `Sample.ID` == "Waste_Water" ~ "Water",
                                TRUE ~ "Soil"))

df1 %>% 
  ggplot(aes(x = Time,y = Absorbance, color = Sample.ID)) +
  geom_jitter() +
  facet_wrap(vars(Sample.ID), scales = "free")

#these are the things that I should never have to google about

pivot_wider()
pivot_longer()
mutate()
arrange()
filter()
select()

# to filter out data from just the year 2000 and where cases are greater
# than 3000

table1 %>% 
  filter(year == 2000 & cases > 3000) %>% 
  select(-year) %>% 
  mutate(rate = (cases/population)*100) %>% 
  arrange(rate,population)

# in arrange() the first statement is for what to arrange. To add another
# sorting varibale you separate the columns with a ,

# id_col() in pivot_wider acts as a preservative to the columns

table2 %>% 
  pivot_wider(id_cols = c(country,year), # what to leave behind
              names_from = type, # which col has the new variable
              values_from = count) # which col has the values

table3 %>% 
  separate(rate, into = c("cases","population"))

table4a
table4b
# to combine them as is but making things weird use
full_join(table4a,table4b)
#instead to will issolate the different variables using pivot_longer to stack
t4a <- table4a %>% 
  pivot_longer(-country,names_to = "year",values_to = "cases")

t4b <- table4b %>% 
  pivot_longer(-country,names_to = "year",values_to = "population")

full_join(t4a,t4b)

# you can also do the join with the lengthened code

full_join(table4a %>% 
            pivot_longer(-country,names_to = "year",values_to = "cases"),
          table4b %>% 
            pivot_longer(-country,names_to = "year",values_to = "population"))


library(janitor)

# you can change excel date numerics into a chaaracter value using...
janitor::excel_numeric_to_date()

#you can  look at the names directly using this
janitor::clean_names(iris) %>% names

library(gganimate)

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
  
  
?case_when()



iris %>% 
  ggplot(aes(x=Sepal.Length,y=Sepal.Width)) +
  geom_point() +
  gganimate::transition_states(Species)
library(gganimate)
