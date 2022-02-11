data <- data.frame(x = 1:6,
                   y = 10:5)

library(ggplot2)

ggp <- ggplot(data, aes(x,y)) +
  geom_point()

ggp

library(jpeg)



nic1 <- readJPEG('nic1.jpg')

library(patchwork)

nic1.1 <- ggp + 
  inset_element(p = nic1,
                left = 0.5,
                bottom = 0.55,
                right = 0.95,
                top = 0.95)
nic1.1
library(ggpubr)

# Import the image
img.file <- system.file(file.path("images", "background-image.png"),
                        package = "ggpubr")
img <- png::readPNG(img.file)

# Plot with background image

ggplot(iris, aes(y=Sepal.Width, x=Sepal.Length, image(nic1)))+
  background_image(nic1)+
  geom_point(aes(fill = Species), color = "Yellow")+
  fill_palette("jco")

?background_image
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point(aes(size = qsec, shape = factor(cyl)))



jpeg(nic1,width=300,height=300,units="px",bg = "transparent")
boxplot(d)
dev.off()
p
d <- rnorm(100) #generating random data
df <- data.frame(y=d,x=1)
p <- ggplot(df) + stat_boxplot(aes(x = x,y=y)) + opts(
  panel.background = theme_rect(p = nic1, fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = theme_blank(), 
  panel.grid.major = theme_blank()