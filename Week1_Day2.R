csvfiles <- list.files(path = "Data",pattern = ".csv",full.names = TRUE)
readLines("Data/Fake_grade_data.csv")
list.files(path = "Data",pattern = ".csv",full.names = TRUE)
# is a way to be able to write notes without having it show up to the right
# <- is a way to put the code into the files to the right
csvfiles[1]
#vector
# "list" of things that are all the same type

nums <- 1:10
nums + 1
nums/2
nums2 <- nums/2
nums + nums

#task- find all the file in data that begin with "m" (recursively)

mfiles <-list.files(path = "Data",pattern = "^m",recursive = TRUE,full.names = TRUE)

mushroom_growth <- readLines(mfiles[11])

# read.csv() is a file that is used to read csv files explicitely

read.csv(mfiles[11])
