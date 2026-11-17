## Ugly Graph Contest
## install.packages("ggimage")   # only run once
## install.packages("jpeg")      # only run once
library(tidyverse)
library(ggimage)
library(jpeg)

## file names - change these if your files are named differently
head_file = "hunter head.png"
face_bg = readJPEG("face_background.jpg")

## LOTS of heads
set.seed(8)
stinky_data = tibble(
  stinky    = runif(80, 0, 100),
  poopy     = runif(80, 0, 100),
  image     = head_file,
  head_size = runif(80, 0.04, 0.12),
  smell     = sample(c("mild", "rank", "TOXIC", "call 911"), 80, replace = TRUE)
)

## a few GIANT heads
giant_heads = tibble(
  stinky = c(15, 50, 85),
  poopy  = c(80, 45, 15),
  image  = head_file
)

## the plot
stinky_plot = stinky_data %>%
  ggplot(aes(x = stinky,
             y = poopy)) +
  annotation_raster(face_bg,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +                   # his face as the background
  geom_smooth(method = 'lm', se = TRUE,
              color = "yellow", fill = "magenta",
              linewidth = 4, alpha = 0.4) +                     # pointless trend line
  geom_point(aes(color = smell), size = 18, alpha = 0.8) +      # neon circles behind each head
  geom_image(data = giant_heads,
             aes(image = image),
             size = 0.3) +                                      # 3 giant heads
  geom_image(aes(image = image, size = I(head_size))) +         # all the heads, on top
  scale_color_manual(values = c("chartreuse", "hotpink", "orange", "cyan")) +
  labs(title = "STINKY vs POOPY!!!!!!!!",
       subtitle = "peer reviewed (by nobody)",
       x = "how stinky",
       y = "poopy level (units unknown)",
       color = "smell??",
       caption = "data collected with my nose") +
  theme(plot.background = element_rect(fill = "purple", color = "limegreen", linewidth = 15),
        panel.grid.major = element_line(color = "red", linewidth = 2, linetype = "dashed"),
        panel.grid.minor = element_line(color = "yellow", linewidth = 1),
        plot.title = element_text(size = 40, color = "yellow", face = "bold.italic",
                                  angle = -3, hjust = 0.5),
        plot.subtitle = element_text(color = "cyan", size = 14, hjust = 1),
        plot.caption = element_text(color = "orange", size = 8),
        axis.title.x = element_text(color = "hotpink", size = 25, angle = 180),
        axis.title.y = element_text(color = "chartreuse", size = 10),
        axis.text.x = element_text(color = "cyan", size = 20, angle = 45),
        axis.text.y = element_text(color = "orange", size = 6, angle = 200),
        legend.position = "top",
        legend.background = element_rect(fill = "brown"),
        legend.text = element_text(color = "white", size = 12),
        legend.title = element_text(color = "yellow", face = "bold"))
stinky_plot

ggsave("stinky_plot_crazy.png", plot = stinky_plot, width = 10, height = 8)
dev.off()
geom_smooth(method = 'lm', se = TRUE,
            color = "yellow", fill = "magenta",
            linewidth = 4, alpha = 0.4,
            show.legend = FALSE) +
  geom_point(aes(color = smell), size = 18, alpha = 0.8) +
  geom_image(data = giant_heads,
             aes(image = image),
             size = 0.3,
             show.legend = FALSE) +
  geom_image(aes(image = image, size = I(head_size)),
             show.legend = FALSE) 
ggsave("stinky_plot_crazy.png", plot = stinky_plot, width = 10, height = 8)        
