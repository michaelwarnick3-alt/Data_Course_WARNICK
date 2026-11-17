## save cars with mpg > average and 
## cyl equal to 4 to new obj 

data (mtcars)
df_car <- mtcars

mean(df_car$mpg)

df_mpg_avg <- df_car[df_car$mpg > mean(df_car$mpg), ]
df_new = df_mpg_avg[df_mpg_avg$cyl == 4, ]
df_new

df_car = mtcars
dim(df_car)
df_car[,-1]
dim(df_car[,-c(1, 3, 5)])
dim(df_car[,c(1:5)])
dim(df_car[-1,])

names(mtcars)
## remove 'hp', 'wt' and save car with mpg > 20
## and cyl not 4
df_car <- mtcars
df_car$hp <- NULL
df_car$wt <- NULL
df_new <- df_car[df_car$mpg > 20, ]
df_new <- df_new[df_new$cyl != 4, ]
df_new


df_new = df_car[df_car$mpg > 20,] 
rows = df_car$mpg > 20 & df_car$cyl != 4
cols = -c(4, 6)
df_new = df_car[rows, cols]
View(df_new)

## install package
install.packages()
install.packages('qrcode')
library(qrcode)
url <- 'https:www.bioconductor.org/install/'
qr <- qr_code(url)
qr
plot(qr)
package::function

## 1. install 'tidyverse' package 
## 2. load 'tidyverse' package in your environment

install.packages('tidyverse')
library(tidyverse)

df_car %>%
  name()

mean(df_car$mpg)
df_car$mpg %>%
  mean() %>%
  view()

## save cars with mpg > 22, cyl = 4, wt < 3 
## and hp > 90 in a new obj
## use tidyverse 

library(tidyverse)
df_new <- df_car %>%
  filter(mpg > 22, cyl == 4, wt < 3, hp > 90)

df_new


newobj

new_obj %>%
  pluck('mpg') %>%
  mean()

new_obj %>%
  select('mpg') %>%
  mean()

##remove car wih cyl = 4
## and calulate average mpg 

library(tidyverse)
df_car |> 
  filter(cyl != 4) |> 
  pull(mpg) |> 
  mean()
