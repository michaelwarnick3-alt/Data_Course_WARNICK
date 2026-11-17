install.packages("datasauRus")
library(datasauRus)
install.packages("datasauRus", repos = "https://cloud.r-project.org")
library(datasauRus)
library(tidyverse)

dat = datasaurus_dozen
dat
install.packages("GGally")
library(GGally)
ggpairs(penguins)
ggpairs(Dinosaurs)


## Copy Graph 
install.packages("palmerpenguins")
install.packages("tidyverse")
library(palmerpenguins)
library(tidyverse)

penguins %>%
  drop_na() %>%
  ggplot(aes(x = bill_depth_mm, 
             y = body_mass_g,
             color = sex)) +
  facet_wrap(~species) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("female" = "purple", "male" = "green")) +
  labs(x = "Bill depth (mm)",
       y = "Body mass (g)",
       color = "Sex")
  



