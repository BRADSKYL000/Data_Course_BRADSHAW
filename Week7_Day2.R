





















# try a couple of models
mod1 <- glm(data = df,
            formula = cty ~ displ)
mod2 <- glm(data = df,
            formula = cty ~ displ + drv)
mod3 <- glm(data = df,
            formula = cty ~ displ * drv)

summary(mod1)
summary(mod2)
summary(mod3)


compare_models(mods1,mod2,mod3,style = "se_p")
compare_performance(mod1,mod2,mod3) %>% plot()
                    
                    
# look at model predictions for a hypothetical car
hyp_preds <- data.frame(displ=6,drv="r") %>% 
  gather_predictions(mod1,mod2,mod3)
hyp_preds

# look at plors of model predictionas all tofether
gather_predictions(data = df,mod1,mod2,mod3)


geom_smooth(method = "lm",aes(y=pred),se = FALSE) +
  face_wrap(~model) +
  geom_point(data = hyp_preds,aes(x=displ,y=pred),color = "Black")

# revisit our model formulee
mod1$formula
mod2$formula
mod3$formula


# closer look at intersections
# interactions ####
library(tidyverse)
library(modelr)
library(easystats)
sim3
# y is response, x1 & x2 are predictions
mod1 <- glm(data=sim3,
    formula = y ~ x1 +x2) # without interaction

mod2 <- glm(data=sim3,
            formula = y ~ x1 * x2) #with interaction

compare_models(mod1,mod2)
compare_performance(mod1,mod2) %>% plot()

sim3 %>% 
  ggplot(aes(x=x1,y=y,color=x2)) +
  geom_point()

sim3 %>% 
  gather_predictions(mod1,mod2) %>% 
  ggplot(aes(x=x1,y=pred,color=x2)) +
  geom_smooth(method = "lm") +
  facet_wrap(~model)

# this is a package that is useful when looking at stats
library(MASS)
mpg %>% names
mod3 <- glm(data=mpg,
            formula = cty~displ *year*cyl*drv*model)

step <- stepIAC(mod3)
step$formula

mod4 <- glm(data = mpg,
            formula = step$formula)

add_predictions(mpg,mod4) %>% 
  ggplot(aes(x=displ,y=pred,color=factor(cyl))) +
  geom_smooth(method = "lm")

mpg %>% 
  ggplot(aes(x=displ,y=cty)) +
  geom_point()

mod5 <- mpg %>% glm(data=mpg,
            formula = cty~poly(displ,4))
add_predictions(mpg,mod5) %>% 
  ggplot(aes(x=displ)) +
  geom_point(aes(y=cty)) +
  geom_smooth(aes(y=pred))

# the + in the variable moves the intersept and * moves all intersepts individually
           





