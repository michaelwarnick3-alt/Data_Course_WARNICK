## Assignment 6 

library(tidyverse)
library(gganimate)
dat = read.csv("../../Data/BioLog_Plate_Data.csv")
dat
names(dat)

## 1. Clean data into tidy, (note to self) Hr_24, Hr_48, Hr_144 hold same value at different times, makes data too wide
## 2. Create a new column for soil or water
dat_tidy = dat %>%
  pivot_longer(cols = c(Hr_24, Hr_48, Hr_144),
               names_to = "Time",
               values_to = "Absorbance") %>%
  separate(Time, into = c("hr", "Time"), sep = "_", convert = TRUE) %>%
  select(-hr) %>%
  mutate(Type = ifelse(grepl("Soil", Sample.ID, ignore.case = TRUE), "Soil", "Water"))
dat_tidy
view(dat_tidy)
dat_tidy %>% count(Sample.ID, Type)   # check: 2 soil samples and 2 water samples

## 3. Plot that matches example, (note to self) use filter to keep only 0.1 dilution
plot1 = dat_tidy %>% 
  filter(Dilution == 0.1) %>%
  ggplot(aes(x = Time,
             y = Absorbance,
             color = Type)) + 
  geom_smooth(se = F) +
  facet_wrap(~ Substrate) +
  labs(title = "Just dilution 0.1") +
  theme_minimal()
plot1
ggsave("dilution_0.1_plot.png", plot = plot1, width = 14, height = 10)

## 4. Animated plot of mean absorbance for Itaconic Acid, (note to self) group_by + summarise averages 3 replicates for each sample
## transition_reveal makes the lines draw themselves like the example
itaconic = dat_tidy %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(Sample.ID, Dilution, Time) %>%
  summarise(Mean_absorbance = mean(Absorbance))
itaconic

plot2 = itaconic %>%
  ggplot(aes(x = Time,
             y = Mean_absorbance,
             color = Sample.ID)) +
  geom_line() +
  facet_wrap(~ Dilution) + 
  theme_minimal() +
  transition_reveal(Time)
animate(plot2, renderer = gifski_renderer())
anim_save("itaconic_acid_animation.gif")
