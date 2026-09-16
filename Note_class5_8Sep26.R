#1. Read 'wingspan_vs_mass.csv' using relative path and ## save as an object
#2. what type of obj is this?
#3. how many rows and columns in the file 

read.csv(file = "/Users/michaelwarnick/Desktop/Data_Course/Data/wingspan_vs_mass.csv")
class("wingspan_vs_mass.csv")
wingspan_vs_mass.csv <- read.csv("wingspan_vs_mass.csv")
obj <- read.csv("wingspan_vs_mass.csv")
obj <- read.csv("Data/wingspan_vs_mass.csv")
class(obj)
dim(obj)
nrow(obj)
ncol(obj)
str(obj)

##
head(obj, n=3)
tail(obj, n=4)
list.files('Data')
list.files('Data/', recursive = F)
list.files('Data/', recursive = T)
length(obj)
list.files('Data/', pattern = 'csv')

my_fun = function(x,y){
  out = x + y 
  print(out)
}

my_fun(1,2)

for (i in 1:3) {
  out = i + 1 
  print(i)
}
### try to write a for loop 
for (i in 1:nrow(obj)) {
  print(obj$wingspan[i])
}

## 1. create a data frame contains your favoirtie fruits (at least 3) and their calories
## 2. after creating the df, add a new col called 'calories_100' the value = original cal + 100
## 3. Write a loop to print out 'Calories_100'
## 4. save the data frame to local

fruits_df <- data.frame(
  fruit = c("Banana", "Apple", "Mango"),
  calories = c(105, 95, 150)
)

fruits_df$calories_100 <- fruits_df$calories + 100

for (i in 1:nrow(fruits_df)) {
  print(fruits_df$calories_100[i])
}
  
write.csv(fruits_df, "fruits_df.csv", row.names = F)
