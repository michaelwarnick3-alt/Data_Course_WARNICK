## Assignment 7 - Utah Religions by County
## Michael Warnick

library(tidyverse)

## Import the data (relative path from the Assignment_7 folder)
religion = read_csv("Utah_Religions_by_County.csv")
religion
names(religion)
glimpse(religion)

## (note to self) first look at the data:
## - each row is a county, 29 counties
## - Pop_2010 = county population, Religious + Non-Religious add up to 1
## - every religion is its own column, so the data is too WIDE
## - religion should be one column and the proportion should be another
## - "Non-Religious" has a dash in the name which is annoying, so rename it first

religion = religion %>%
  rename(Non_Religious = `Non-Religious`)

## check that Religious + Non_Religious = 1 for every county
religion %>%
  mutate(total = Religious + Non_Religious) %>%
  select(County, total)
## they all add up to 1, so these are proportions not counts

## Clean it up into tidy shape
## keep County, Pop_2010, Religious, Non_Religious as they are (one value per county)
## stack all the religion columns into Religion and Proportion
religion_tidy = religion %>%
  pivot_longer(cols = -c(County, Pop_2010, Religious, Non_Religious),
               names_to = "Religion",
               values_to = "Proportion")
religion_tidy
view(religion_tidy)

## save the cleaned data
write_csv(religion_tidy, "Utah_Religions_tidy.csv")


## ---------------- Exploring the data ----------------

## which religions are the biggest overall?
religion_tidy %>%
  group_by(Religion) %>%
  summarise(avg_proportion = mean(Proportion)) %>%
  arrange(desc(avg_proportion))
## LDS is by far the biggest (about 69% on average)
## every other group is under 5%

## plot 1: average proportion of each religion across all counties
plot1 = religion_tidy %>%
  group_by(Religion) %>%
  summarise(avg_proportion = mean(Proportion)) %>%
  ggplot(aes(x = reorder(Religion, -avg_proportion),
             y = avg_proportion)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Proportion of Each Religion in Utah Counties",
       x = "Religion",
       y = "Average Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
plot1
ggsave("plot1_avg_religion.png", plot = plot1, width = 8, height = 5, bg = "white")
## LDS is so big it squishes everything else, so I'll look at it with and without LDS

## plot 2: religious vs non-religious in each county
plot2 = religion %>%
  ggplot(aes(x = reorder(County, Non_Religious),
             y = Non_Religious)) +
  geom_col(fill = "darkorange") +
  labs(title = "Proportion of Non-Religious People by County",
       x = "County",
       y = "Proportion Non-Religious") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
plot2
ggsave("plot2_nonreligious_by_county.png", plot = plot2, width = 9, height = 5, bg = "white")
## San Juan, Grand, and Summit counties have the most non-religious people (over 40%)
## Utah County has the least (about 9%)

## plot 3: religion makeup of each county, without LDS so the smaller groups show up
plot3 = religion_tidy %>%
  filter(Religion != "LDS") %>%
  ggplot(aes(x = County,
             y = Proportion,
             fill = Religion)) +
  geom_col() +
  labs(title = "Non-LDS Religions by County",
       x = "County",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
plot3
ggsave("plot3_nonLDS_by_county.png", plot = plot3, width = 10, height = 6, bg = "white")
## Catholic is the second biggest group in most counties
## Carbon County has a lot of Catholics compared to the others (about 19%)

## plot 4: how spread out is each religion? (boxplot)
plot4 = religion_tidy %>%
  ggplot(aes(x = Religion,
             y = Proportion)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Spread of Each Religion Across Counties (log scale)",
       x = "Religion",
       y = "Proportion (log scale)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
plot4
ggsave("plot4_religion_spread.png", plot = plot4, width = 8, height = 5, bg = "white")
## used log scale because LDS is so much bigger than everything else
## (counties with 0 get dropped on the log scale, that's ok for just looking)


## ---------------- Question 1 ----------------
## "Does population of a county correlate with the proportion
##  of any specific religious group in that county?"

## population is really skewed (Salt Lake has over 1 million, Daggett has about 1,000)
## so I used a log scale for population

plot5 = religion_tidy %>%
  ggplot(aes(x = Pop_2010,
             y = Proportion)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  scale_x_log10() +
  facet_wrap(~ Religion, scales = "free_y") +
  labs(title = "County Population vs Proportion of Each Religion",
       x = "County Population 2010 (log scale)",
       y = "Proportion") +
  theme_minimal()
plot5
ggsave("plot5_population_vs_religion.png", plot = plot5, width = 12, height = 8, bg = "white")

## correlation of population with each religion
pop_cor = religion_tidy %>%
  group_by(Religion) %>%
  summarise(cor_with_pop = cor(Pop_2010, Proportion)) %>%
  arrange(desc(cor_with_pop))
pop_cor

## Answer to question 1:
## - LDS has basically NO correlation with population (about 0), so LDS proportion
##   is about the same in big and small counties
## - Muslim has the strongest correlation (about 0.76), but looking at the plot that is
##   mostly because of Salt Lake County, the biggest county, so one point is driving it
## - Pentecostal, Assemblies of God, and Orthodox have weak positive correlations
##   (about 0.25), so these groups tend to be a little bigger in bigger counties
## - overall, most religions don't really change much with population, except smaller
##   groups tend to show up more in the big counties (most are 0 in small counties)


## ---------------- Question 2 ----------------
## "Does proportion of any specific religion in a given county correlate
##  with the proportion of non-religious people?"

plot6 = religion_tidy %>%
  ggplot(aes(x = Non_Religious,
             y = Proportion)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(~ Religion, scales = "free_y") +
  labs(title = "Proportion Non-Religious vs Proportion of Each Religion",
       x = "Proportion Non-Religious",
       y = "Proportion") +
  theme_minimal()
plot6
ggsave("plot6_nonreligious_vs_religion.png", plot = plot6, width = 12, height = 8, bg = "white")

## correlation of non-religious with each religion
nonrel_cor = religion_tidy %>%
  group_by(Religion) %>%
  summarise(cor_with_nonreligious = cor(Non_Religious, Proportion)) %>%
  arrange(cor_with_nonreligious)
nonrel_cor

## closer look at LDS since it had the strongest correlation
plot7 = religion_tidy %>%
  filter(Religion == "LDS") %>%
  ggplot(aes(x = Non_Religious,
             y = Proportion)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', se = F) +
  labs(title = "LDS vs Non-Religious by County",
       x = "Proportion Non-Religious",
       y = "Proportion LDS") +
  theme_minimal()
plot7
ggsave("plot7_LDS_vs_nonreligious.png", plot = plot7, width = 7, height = 5, bg = "white")

## Answer to question 2:
## - LDS has a strong NEGATIVE correlation with non-religious (about -0.87)
##   counties with more LDS members have fewer non-religious people
##   this makes sense because LDS is most of the religious people in Utah
## - Episcopal Church has a moderate POSITIVE correlation (about 0.60)
## - Evangelical and Southern Baptist have weak positive correlations (about 0.39)
##   so these smaller groups are a little bigger in counties with more non-religious people
## - the rest are close to 0, so there isn't really a relationship
## - these are just correlations from 29 counties, so it doesn't mean one causes the other
