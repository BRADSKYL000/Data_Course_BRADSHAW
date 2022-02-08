iris
class(iris)
iris$Species
class(iris$Species)
# distinguishes the different factors
unique(iris$Species)
# table gives you a table of how many data points are in all factors
table(iris$Species)

install.packages("easystats", repos = "https://easystats.r-universe.dev")

#the command to be able to pull up packages are below

library(tidyverse)

#the () is for the name of the package

# it basically drops a prompt into a parenthesis
iris %>% class()
iris$Species %>% class()

ggplot(iris, aes(x=Sepal.Length,y=Sepal.Width,color=Species)) +
  geom_point() +
  geom_smooth()

# we are wanting to be able to issolate the data from the different 
#species

iris$Species == "setosa"

setosa <- iris[iris$Species == "setosa",]
setosa$Sepal.Length %>%  mean()

versicolor <- iris[iris$Species == "versicolor",]
versicolor$Sepal.Length %>%  mean()

virginica <- iris[iris$Species == "virginica",]
virginica$Sepal.Length %>%  mean()

# you can highlight a section and press ctrl+F and get a find
# that can replace specific words

# You can make it a little easier by doing the following

iris %>% 
  group_by(Species) %>% 
  summarize(MeanSepLength = mean(Sepal.Length), 
            SdSepLength = sd(Sepal.Length),
            MinSepLength = min(Sepal.Length),
            MaxSepLength = max(Sepal.Length))

iris$Sepal.Length %>% summary()

iris %>% 
  ggplot(aes(x = Sepal.Length,color = Species)) +
  geom_density()


library(palmerpenguins)
# the data set is about penguins
 penguins
 # the names function gives you the names of each variable
 names(penguins)
 # you can find the basic data when you use the summary about all of
 # the seperate columns
 summary(penguins)
# you can then make a table in order to know some more information
 table(penguins$species,penguins$island)

 library(GGally)
# ggpairs is a easy and fast way to be able to make a variety of
 # graphs using the variables in contrast to make every style of graph
 # that use 2 variables
 ggpairs(penguins) 
 # for corr the range is -1:1 and those most positive are at the ends
 # the positive and negative relate to the direction of the graph
 # The dots on the box plots are outliers
 
 ggplot(penguins,aes(x=body_mass_g,
                     y=flipper_length_mm,
                     color=species)) +
   geom_point() +
   geom_smooth()
 