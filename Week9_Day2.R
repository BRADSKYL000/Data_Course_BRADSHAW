stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')


library(tidyverse) # all the things
library(ggExtra)   # marginal plots
library(ggtext)    # color your text
library(patchwork) # combine multiple plots
library(paletteer) # get all the color palettes
library(scales)    # helper functions from ggplot2
souce_url <- "https://raw.githubusercontent.com/ArrowheadAnalytics/next-gen-scrapy-2.0/master/pass_and_game_data.csv"

pass_map_df <- read_csv(souce_url) %>% 
  na.omit() 

  select(-X1)

glimpse(pass_map_df)


# class stuff

# Best practices for making a "clean" R script

# Load packages ####
library(tidyverse)
library(modelr)
library(janitor)
library(easystats)
# for big statistical analysis for a summary in a summary use the below
library(broom)
tidy(mod)
report(mod)
# Set themes, etc. ####

theme_set(theme_minimal())
# you can find your colors from color picker online and input the code
pal <- c("#c4a113","#c1593c","#611d78")

# Load functions ####

# Import Data ####
df <- read_csv("Data/Bird_Measurements.csv")

# Clean the data ####
glimpse(df)
# clean the column names
df <- janitor::clean_names(df)


# Separate out the sexes
male <- df %>% 
  select(-ends_with("_n")) %>% 
  select(family,species_number,species_name,english_name,clutch_size,egg_mass,
         mating_system,starts_with("m_")) %>% 
  pivot_longer(starts_with("m_"),
               names_to = "measurment",
               values_to = "value",
               names_prefix = "m_") %>% 
  mutate(sex="male")

female <- df %>% 
  select(-ends_with("_n")) %>% 
  select(family,species_number,species_name,english_name,clutch_size,egg_mass,
         mating_system,starts_with("f_"))%>% 
  pivot_longer(starts_with("f_"),
               names_to = "measurment",
               values_to = "value",
               names_prefix = "f_") %>% 
  mutate(sex="female")

unsexed <- df %>% 
  select(-ends_with("_n")) %>% 
  select(family,species_number,species_name,english_name,clutch_size,egg_mass,
         mating_system,starts_with("unsexed_"))%>% 
  pivot_longer(starts_with("unsexed_"),
               names_to = "measurment",
               values_to = "value",
               names_prefix = "unsexed_") %>% 
  mutate(sex="unsexed")
# join them back together
full <- full_join(male,female) %>% 
  full_join(unsexed)

# clean up the enviornment
rm(male)
rm(female)
rm(unsexed)
rm(df)

# double check what you got
full %>% glimpse

# we dont like the measurements column so we are gonn pivot wider
full <- pivot_wider(full,id_cols =-measurment,
            names_from = measurment,
            values_from = value)


# visualize the data ####

full %>% 
  ggplot(aes(x=egg_mass,y=mass,color=sex)) +
  geom_point(alpha=.5,size=3) +
  facet_wrap(~sex) +
  scale_color_manual(values=pal)
ggsave("./output/myplot.jpeg",height = 4,width = 4,dpi=300)

# the ostrich is making the data act strange
full %>% 
  ggplot(aes(x=log10(mass))) +
  geom_density()


# Model and test the hypothesis ####

mod <- glm(data = full,
           formula = log10(mass)~egg_mass + sex)
summary(mod)

# in order to save the information from the summary

sink("./W9D2_bird_data.txt")
