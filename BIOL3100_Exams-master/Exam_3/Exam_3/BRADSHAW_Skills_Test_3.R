library(tidyverse)
library(modelr)
library(easystats)
library(broom)

ds <- read_csv("BIOL3100_Exams-master/Exam_3/FacultySalaries_1995.csv")

# clean up ####
ds <- ds %>% 
  select(State,Tier,AvgFullProfSalary,AvgAssocProfSalary,AvgAssistProfSalary)

ds <- ds[c(1:1154,1156:1161),]

ds <- ds %>% 
  pivot_longer(starts_with("Avg"),names_to = "rank",
             values_to = "Salary", names_prefix = "Avg")

ds <- ds %>% mutate(Rank = case_when(`rank` == "FullProfSalary" ~ "Full",
                              `rank` == "AssocProfSalary" ~ "Assoc",
                              `rank` == "AssistProfSalary" ~ "Assist")) %>% 
  select(State,Tier,Rank,Salary)

# question 1 graph ####

ds %>% 
  ggplot(aes(y=Salary,x=Tier,fill=Rank)) +
  geom_boxplot()

# question 2 anova analysis ####

#all of the different models that are going to be compared
dsa <- lm(Salary~State+Tier+Rank,data=ds)

 anova(dsa)
 summary(dsa)
 
# opening and cleaning second ds ####
 
ds2 <- read_csv("BIOL3100_Exams-master/Exam_3/Juniper_Oils.csv")
 
ds2 <- ds2 %>% 
  pivot_longer(cols = c("alpha-pinene","para-cymene","alpha-terpineol",
                        "cedr-9-ene","alpha-cedrene","beta-cedrene",
                        "cis-thujopsene","alpha-himachalene","beta-chamigrene",
                        "cuparene","compound 1","alpha-chamigrene","widdrol",
                        "cedrol","beta-acorenol","alpha-acorenol",
                        "gamma-eudesmol","beta-eudesmol","alpha-eudesmol",
                        "cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2",
                        "thujopsenal"),names_to = "ChemicalID",values_to = "Concentration") %>% 
  select(YearsSinceBurn,ChemicalID,Concentration)

# question 4 graph ####

ds2 %>% 
  ggplot(aes(x=YearsSinceBurn,y=Concentration,)) +
  geom_smooth() +
  facet_wrap(~ChemicalID, scales = "free")

# question 5 model impacts ####

ds2a <- glm(Concentration~ChemicalID,data = ds2)

ds2b <- tidy(ds2a)

ds2c <- ds2b[c(which(ds2b$p.value <= 0.05)),] # this gets you the p-value applicable chemicals

