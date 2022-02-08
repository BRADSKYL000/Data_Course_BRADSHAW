txt_files <- list.files(path = "Data",
                        pattern = ".txt",
                        full.names = TRUE,
                        recursive = TRUE)
first3 <- txt_files[1:3]
# the objects inside of the txt_files is called a character vector
# the first3 was called an integer vector
c(1,3,5) # C() makes vectors, it can be used to select specific files
txt_files[c(1:10,15)]


for(i in 1:3){print(readLines(first3[i])[3])}

thestuffiwant <- c(1:10,15)
txt_files[thestuffiwant] #take the first 10 and the 15th elements

#read the first 3 lines together
readLines("Data/2114.txt")
# an easier way is to do the following
readLines(first3[1])
#then to be able to read the first line of the element
readLines(first3[1])[3]

readLines(first3[2])[3]

readLines(first3[3])[3]

# fore-loop, a way to be able to apply the same instructions 
#to a wide array of files
x <- c("Billy","Bob","Jane")
paste0("My name is"," Bob",".")

for(i in x){print(paste0("My name is ",i,"."))}
 