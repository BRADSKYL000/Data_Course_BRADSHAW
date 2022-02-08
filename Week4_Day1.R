library(tidyverse)
library(palmerpenguins)
penguins %>%  glimpse

#look at a single variable
ggplot(penguins,aes(x=bill_length_mm,fill=species)) +
  geom_histogram(alpha=.5)

#not all chart types need (or can even accept) both x and y aesthetics

# body mass vs bill length (basic scatterplot)

ggplot(penguins,aes(x=body_mass_g,y=bill_length_mm)) +
  geom_point(aes(color=species),alpha=.5) +
  geom_smooth(method="lm",aes(group=species),color="black",se=FALSE) +
  labs(x="Body mass (g)",
       y="Bill length (mm)",
       color="species",
       title="Body mass vs bill length\nin three penguin species") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=.5,angle=180,color="Yellow",size=19),
        plot.background = element_rect(fill="yellow3")) +
  scale_color_viridis_d()

ggsave("myplot.jpg",width = 6,height = 5,dpi =300)