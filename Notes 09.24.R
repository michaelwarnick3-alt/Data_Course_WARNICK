## install 'palmerpenuins' package
## calculate average bill length
## (bonus) calculate average bill length by species 

install.packages('palmerpenguins')
?palmerpenguins
library(palmerpenguins)
penguins
view(penguins)

mean(penguins$bill_length_mm, na.rm = TRUE)

library(tidyverse)
penguins %>%
  group_by(species) %>%
  summarize(ovg_bill_length = mean(bill_length_mm, na.rm = TRUE))

## practice group_by() & summarize 
## how to remove NA (empty col)

penguins %>%
  group_by(sex,year,flipper_length_mm) %>%
  summarize(avg_length = mean(flipper_length_mm, na.rm = TRUE))

penguins %>%
  drop_na() %>%
  view('penguins_without_na')
penguins %>%
  filter(!is.na(bill_length_mm)) %>%
  View()

## practice using arrange() & relocate ()
## create a new col using mutate()
penguins %>%
  mutate(bill_ratio = bill_length_mm / bill_depth_mm)%>%
  View()

case_when( condition ~ if T, then )
