
library(ggplot2)
library(jpeg)
library(patchwork)
library(ggpubr)

nic1 <- readJPEG('nic1.jpg')

ggplot(iris, aes(y=Sepal.Width, x=Sepal.Length, image(nic1)))+
  background_image(nic1)+
  geom_point(colour = "black", size = 4.5) +
  geom_point(colour = "pink", size = 4) +
  geom_point(aes(fill = Species,)) +
  fill_palette("jco")
