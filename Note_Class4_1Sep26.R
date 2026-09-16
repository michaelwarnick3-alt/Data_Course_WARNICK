## 1. Create R project
## read a .csv file using relative path 

read.csv('Data/1620_scores.csv')
getwd()
csv_file = read.csv(file = 'Data/1620_scores.csv')
obj1 = 1
obj2 = '1'
obj + 1
is.character(obj1)
is.character(obj2)
is.numeric(obj1)
is.numeric(obj2)
is.logical(obj1)
is.logical(obj2)

as.numeric(obj2)
as.character(obj1)

obj2 = as.numeric(obj2)
vec = c(1, 2, 3)
vec
vec + 1 
vec2 = c(1, "2", 3)
vec2 = c(TRUE, FALSE, FALSE)
vec2 = c("TRUE", FALSE, FALSE)
length(vec2)

#matrix
mat = matrix(
?matrix()
mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2", "C.3")))
mdat[2,2]

#array similar with matrix, multiple dimention, same type
arr =array(c(1,2,3,4,5,6))
arr = array(1:12, ncol = 2, nrow = 2)
?array
arr = array(1:12, dim = c(2,2,6))
arr[2,1,6]

## data frame : 2 array
dat = data.frame(
  id = c(1,2,3), 
  weight = c(4,5,6),
  color = c('red', 'blue', 'green')
)
dat
dat$id
dat$animal = c(17, 'giraffe', 'tiger')
dat

#list
lis = list(id = c(1,2,3), 
           weight = c(4,5,6),
           color = c('red', 'blue', 'green'),
           mdat-mdat
           )
?list
dat 
