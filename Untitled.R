## Ugly Graph Contest 
install.packages("ggimage")
library(tidyverse)
library(ggimage)
install.packages("jpeg")
library(jpeg)

face_bg = readJPEG("face_background.jpg")
## make up some data
set.seed(8)
stinky_data = tibble(
  stinky = runif(30, 0, 100),
  poopy  = runif(30, 0, 100),
  image  = "hunter head.png"            # same head for every point
)

## plot
stinky_plot = stinky_data %>%
  ggplot(aes(x = stinky,
             y = poopy)) +
  geom_image(aes(image = image), size = 0.08) +
  labs(title = "Stinky vs Poopy",
       x = "Stinky",
       y = "Poopy")
stinky_plot

ggsave("stinky_plot.png", plot = stinky_plot, width = 8, height = 6)

library(tidyverse)
library(ggimage)
library(jpeg)

## read in the background photo
face_bg = readJPEG("face_background.jpg")

set.seed(8)
stinky_data = tibble(
  stinky = runif(30, 0, 100),
  poopy  = runif(30, 0, 100),
  image  = "head.png"
)

stinky_plot = stinky_data %>%
  ggplot(aes(x = stinky,
             y = poopy)) +
  annotation_raster(face_bg,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +   # his face fills the whole background
  geom_image(aes(image = image), size = 0.08) +  # his head as every point
  labs(title = "Stinky vs Poopy",
       x = "Stinky",
       y = "Poopy")
stinky_plot

ggsave("stinky_plot.png", plot = stinky_plot, width = 6, height = 8)