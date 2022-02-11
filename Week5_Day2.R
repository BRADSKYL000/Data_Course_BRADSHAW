library(tidyverse)

iris %>% 
  filter(Species != "setosa") %>% 
  filter(Sepal.Length < 6.5 & Sepal.Width < 2.75) %>% 
  ggplot(aes(x=Sepal.Length,y=Sepal.Width,color=Species)) +
  geom_point()

# filter() is a way to filter out data
# chooses rows based on expressions like COL == "ABC"
# group_by() / summarize() 
# mutate() is a way to add a new column
# arrange() it just arranges rows based on columns
  # desc() for descending order
# select() works on columns directly
  # starts_with() 
  # ends_with()
  # contains()

iris %>% 
  select(starts_with("P"))

#in data file wide_income_rent.csv
df <- read.csv(choose.files())
df
#income vs rent colored by state

df %>% 
  ggplot(aes(x=NAME,y=estimate_rent)) +
  geom_col()
names(df)

# in class there were 52 columns and 2 rows, we were pivoting the variables
# DONT RUN BELOW STUFF

pivot_longer(df,!variable,names_to = "State",values_to = "USD") %>% 
  ggplot(aes(x=State,y=USD,color=variable)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle=90,hjust=1))

df %>% 
  select(starts_with("Pue"))

utah <- read_csv(choose.files())
utah
utah %>% 
  select(Religious, `Non-Religious`) %>% 
  rowSums()
utah %>% names
utah[,5:17] %>% rowSums()

utah_long <- utah %>% 
  pivot_longer(-c('County',"Pop_2010","Religious"),
               names_to = "Religion",
               values_to = "Proportion")
Evan_order <- utah_long %>% 
  filter(Religion == "Evangelical") %>% 
  arrange(desc(Proportion))

Evan_order

utah_long %>% 
  mutate(COunty = factor(County,levels = evang_order$County)) %>% 
  ggplot(aes(x=County,y=Proportion,fill=Religion)) +
           geom_col()
