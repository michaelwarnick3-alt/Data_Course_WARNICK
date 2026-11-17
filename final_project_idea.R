## Assignment 4 

## Assignment 4 - final project idea
library(tidyverse)

## create fake data that simulates what my final project data might look like
fake_fluoride = tibble(
  state           = c("Utah", "Idaho", "Colorado", "Arizona", "California", "Oregon",
                      "Texas", "Florida", "Georgia", "Ohio", "Illinois", "Minnesota",
                      "New York", "Maine", "New Jersey"),
  region          = c("West", "West", "West", "West", "West", "West",
                      "South", "South", "South", "Midwest", "Midwest", "Midwest",
                      "Northeast", "Northeast", "Northeast"),
  pct_fluoridated = c(55, 35, 70, 55, 60, 25,
                      75, 75, 95, 90, 98, 98,
                      70, 80, 15),
  pct_teeth_lost  = c(38, 43, 34, 39, 37, 44,
                      40, 41, 35, 33, 32, 30,
                      36, 35, 45)
)
fake_fluoride
view(fake_fluoride)

## add a column with mutate()
fake_fluoride = fake_fluoride %>%
  mutate(high_fluoride = ifelse(pct_fluoridated > 50, "yes", "no"))

## save the fake data as a csv
write_csv(fake_fluoride, "fake_fluoride_data.csv")

## average tooth loss by region with group_by() + summarise()
region_avg = fake_fluoride %>%
  group_by(region) %>%
  summarise(avg_fluoridated = mean(pct_fluoridated),
            avg_teeth_lost = mean(pct_teeth_lost))
region_avg

## plot the expected result: more fluoridation = less tooth loss
fluoride_plot = fake_fluoride %>%
  ggplot(aes(x = pct_fluoridated,
             y = pct_teeth_lost,
             color = region)) + #aesthetic
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), method = 'lm', se = F, color = "black") +
  labs(title = "Expected Result: More Fluoridation, Less Tooth Loss",
       subtitle = "Fake data",
       x = "Population on fluoridated water (%)",
       y = "Adults 65+ who lost 6+ teeth (%)",
       color = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
fluoride_plot

ggsave("fluoride_expected_result.png", plot = fluoride_plot)
