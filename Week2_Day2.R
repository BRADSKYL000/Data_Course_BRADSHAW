for(i in 1:3){print(5+i)}

df <- read.csv("../../Data/wingspan_vs_mass.csv")
class(df)
dim(df)

as.numeric(c(1,2,3,"Bob"))

df[5,3]
df[5,]
df[,1]
df[,"variety"]
df[,c("variety","mass")]
df$variety

Momentum_de_birds <- df$mass *df$velocity
df$mass *df$velocity
sum(df[,3])
df$Momentum_de_birds <- df$mass *df$velocity

#NULL is able to remove or null a data set

# expressions (logical)
1>3
3>1
"bob" == "bob"
"billy" != "bob"
1 > FALSE
1 > TRUE
FALSE > TRUE

%in%
  # %in% is for finding values in the range
  
1 %in% 1:10

# & is to compare two conditions
# | is the or condition

momentus <- df$Momentum_de_birds > 2000
which(momentus == TRUE)

df[momentus,]
dim(df[momentus,])

# base plotting
plot(x=df$wingspan,y=df$mass)
