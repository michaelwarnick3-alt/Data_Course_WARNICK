#read 'cleaned_bird_data.csv' using relative path 
#calculate average of egg size
##save birds with egg sizes greater than average to new obj
## save to csv and open it on laptop
getwd()
library(tidyverse)
bird_data <- read.csv('Data/cleaned_bird_data.csv')
avg_egg = bird_data %>%
   filter(Egg_mass != 'NA')
pull(avg_egg)
str(avg_egg)
names(bird_data)

avg_egg %>%
  pluck('Egg_mass') %>%
  mean()

big_eggs = avg_egg %>%
  filter(Egg_mass > 21.784)
view(big_eggs)

write_csv(big_eggs, "big_eggs.csv")
