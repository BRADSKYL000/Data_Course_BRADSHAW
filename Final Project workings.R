library(usmap)
library(ggplot2)

plot_usmap(regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

plot_usmap(data = countypov, values = "pct_pov_2014", include = c("UT", "NV", "CO", "ID", "NM","AZ", "WY"), color = "blue") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 
  labs(title = "New England Region", subtitle = "Poverty Percentage Estimates for New England Counties in 2014") +
  theme(legend.position = "right")
