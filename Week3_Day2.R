library(tidyverse)
library(palmerpenguins)



p <- penguins %>% 
ggplot(aes(x=bill_length_mm,y=body_mass_g,color=species))
p

p + geom_point()

p + geom_point(alpha=.25) + 
  geom_smooth(method="lm")

#this section is about facets

p + geom_point(alpha=.25) + 
  geom_smooth(method="lm") +
  facet_wrap(~species)

# now we are looking at statistics

p + geom_point(alpha=.25) + 
  geom_smooth(method="lm") +
  facet_wrap(~species) +
  coord_flip()

# you can make the scales variable to their facets

p + geom_point(alpha=.25) + 
  geom_smooth(method="lm") +
  facet_wrap(~species,scales = "free") +
  coord_flip()

p + geom_point(alpha=.25) + 
  geom_smooth(method="lm") +
  facet_wrap(~island) +
  coord_cartesian(xlim=c(40:50),ylim=c(3000,4000)) + 
  theme_minimal()

p + geom_point(alpha=.25) + 
  geom_smooth(method="lm") +
  facet_wrap(~species) +
  coord_flip() +
  theme_minimal()

p + geom_point(alpha=.25) + 
  geom_smooth(method="lm") +
  facet_wrap(~island) +
  theme(axis.title.x = element_text(face = "bold",size = 12,color = "blue")) +
  labs(x = "Bill length (mm)",
       y = "Body mass (g)",
       title = "Ugly plot thingy",
       subtitle = "Where I make music",
       caption = "Data about where I make music",
       color = "species")

#my own plot

Me <- penguins %>% 
  ggplot(aes(x=sex,y=flipper_length_mm,fill=species))

Me + geom_boxplot() +
  theme(axis.title.x = element_blank(), title = element_text(face = "bold",size = 17,color = blue)) +
  labs(y = "Flipper length (mm)",
       title = "Flipper length vs sex",
       subtitle = "When the flippers come marching in")

