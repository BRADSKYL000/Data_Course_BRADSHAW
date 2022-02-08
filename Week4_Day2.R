library(tidyverse)

mpg %>%  glimpse()

mpg %>% 
  ggplot(aes(y=cty,x=displ)) +
  geom_point() +
  geom_smooth(method="lm",formula=y~poly(x,2)) +
  theme_bw() +
  labs(x="Engine displacementn (L)",
       y="City miles per gallon",
       color="Year") +
# scale_color_viridis_d()
  scale_color_manual(values = c("Orange","Purple")) +
  facet_wrap(~drv) +
  theme(strip.text = element_text(face="bold",color="Yellow"),
        strip.background = element_rect(fill="Blue",color="Red",
                                        linetype=2),
        axis.title.x= element_text(angle=180),
        plot.background= element_rect(fill="Yellow"))

mpg %>% 
  group_by(class) +
  summarize(MedHwy = median(hwy),
            MaxHwy = max(hwy)) %>% 
  arrange(desc(MedHwy))

mpr$class %>% class
factor(mpg$class,levels = c("suv","midsize"))

mpg %>% 
  mutate(ordered_class = factor(class,levels = ordered_by_med$class)) %>% 
  View()

mpg %>% 
  ggplot(aes(x=class,y=hwy)) +
  geom_point() +
  geom_violin(fill="DarkGreen") +
  geom_boxplot(alpha=.5) +
  coord_flip()
