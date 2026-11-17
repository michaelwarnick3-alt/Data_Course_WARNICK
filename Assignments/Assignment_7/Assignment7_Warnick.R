## Assignment 7 - Utah Religions by County
library(tidyverse)

## import data
religion = read_csv("Utah_Religions_by_County.csv")
glimpse(religion)

## data is too wide - each religion is its own column
## rename Non-Religious so it has no dash
religion = religion %>%
  rename(Non_Religious = `Non-Religious`)

## make it tidy: one row per county per religion
religion_tidy = religion %>%
  pivot_longer(cols = -c(County, Pop_2010, Religious, Non_Religious),
               names_to = "Religion",
               values_to = "Proportion")
religion_tidy
write_csv(religion_tidy, "Utah_Religions_tidy.csv")

## explore: average proportion of each religion
plot1 = religion_tidy %>%
  group_by(Religion) %>%
  summarise(avg = mean(Proportion)) %>%
  ggplot(aes(x = reorder(Religion, -avg), y = avg)) +
  geom_col() +
  labs(x = "Religion", y = "Average Proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot1
ggsave("plot1_avg_religion.png", plot = plot1)
## LDS is by far the largest

## explore: non-religious by county
plot2 = religion %>%
  ggplot(aes(x = reorder(County, Non_Religious), y = Non_Religious)) +
  geom_col() +
  labs(x = "County", y = "Proportion Non-Religious") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot2
ggsave("plot2_nonreligious.png", plot = plot2)
## San Juan, Grand, Summit have the most non-religious

## Q1: does county population correlate with any religion?
plot3 = religion_tidy %>%
  ggplot(aes(x = Pop_2010, y = Proportion)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  scale_x_log10() +
  facet_wrap(~ Religion, scales = "free_y")
plot3
ggsave("plot3_population.png", plot = plot3, width = 12, height = 8)

religion_tidy %>%
  group_by(Religion) %>%
  summarise(cor_pop = cor(Pop_2010, Proportion)) %>%
  arrange(desc(cor_pop))
## Q1: Most religions have only a weak correlation with county population, and LDS has
## basically none (r = -0.003), so it makes up about the same share in big and small counties.
## Muslim has the highest correlation (r = 0.76), but that is mostly driven by Salt Lake County alone.
## Smaller religions like Pentecostal and Assemblies of God tend to show up more in bigger
## counties, since many small rural counties have 0 of them.

## Q2: does any religion correlate with non-religious?
plot4 = religion_tidy %>%
  ggplot(aes(x = Non_Religious, y = Proportion)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(~ Religion, scales = "free_y")
plot4
ggsave("plot4_nonreligious.png", plot = plot4, width = 12, height = 8)

religion_tidy %>%
  group_by(Religion) %>%
  summarise(cor_nonrel = cor(Non_Religious, Proportion)) %>%
  arrange(cor_nonrel)
## Q2: LDS has a strong negative correlation with non-religious people (r = -0.87), so counties
## with more LDS members have fewer non-religious people. This makes sense because LDS makes up
## most of the religious population in Utah. Smaller groups like Episcopal (r = 0.60) and
## Evangelical (r = 0.39) are positively correlated, meaning they are more common in less LDS,
## more non-religious counties. These are only correlations from 29 counties, so they don't show cause and effect.
