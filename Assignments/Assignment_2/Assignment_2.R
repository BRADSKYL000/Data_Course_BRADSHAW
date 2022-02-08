csv_files <- list.files(path = "../../Data",
           pattern = ".csv",
           full.names = TRUE)
# open the wingspan_vs_mass.csv file and store the contents
# as an R project names "df" using the read.csv() function
wvm <- list.files(path = "../../Data",
           pattern = "wingspan_vs_mass.csv",
           full.names = TRUE)

df <- read.csv(wvm)
class(df)
str(df)
head(df,n = 5)
# for these steps I was able to locate the files starting with b
# the use the for feature to display the first line of each file
bstrt <- list.files(path = "../../Data",
           pattern = "^b",
           recursive = TRUE,
           full.name = TRUE
          )

for(i in 1:3){print(readLines(bstrt[i])[1])}

# The same line of code will be used with changed perameters to 
# work with the larger data set

csvstrt <- list.files(path = "../../Data",
                    pattern = ".csv",
                    recursive = TRUE,
                    full.name = TRUE
)

for(i in 1:145){print(readLines(csvstrt[i])[1])}
