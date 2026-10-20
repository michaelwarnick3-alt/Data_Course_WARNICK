## 1. Read the cleaned_covid_data.csv file into R data frame
library(tidyverse)
CleanedCovid = read_csv("cleaned_covid_data.csv") %>%
  mutate(Last_Update = as.Date(Last_Update))
CleanedCovid
view(CleanedCovid)

## 2. Subset the data set to just show states that begin with "A"
## and save this as an object called A_states
A_states = CleanedCovid %>%
  filter(startsWith(Province_State, "A"))
A_states
unique(A_states$Province_State)

## 3. Create a plot of that subset showing Deaths over time
## with a separate facet for each state
A_states %>% 
  ggplot(aes(x = Last_Update,
             y = Deaths)) +
  geom_point() + 
  geom_smooth(method = 'loess', se = F) + 
  facet_wrap(~ Province_State, scales = "free") +
  labs(title = "COVID-19 Deaths Over Time in 'A' States",
       x = "Date",
       y = "Cumulative Deaths")

## 4. Find the "peak" of Case_Fatality_Ratio
## for each state and save this as state_max_fatality_rate
state_max_fatality_rate = CleanedCovid %>%
  group_by(Province_State) %>%
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

state_max_fatality_rate

## 5. Use that new data frame from 4. to create another plot 
state_max_fatality_rate %>%
  mutate(Province_State = factor(Province_State,
                                 levels = Province_State)) %>%
  ggplot(aes(x = Province_State,
             y = Maximum_Fatality_Ratio)) + 
  geom_col() +
  labs(title = "Peak Case Fatality Ratio by State",
       x = "State",
       y = "Maximum Fatality Ratio") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

## 6. (BONUS) Using the FULL data set, plot cumulative deaths
## for the entire US over time
us_deaths = CleanedCovid %>%
  group_by(Last_Update) %>%
  summarise(Total_Deaths = sum(Deaths, na.rm = TRUE))
us_deaths

us_deaths %>%
  ggplot(aes(x = Last_Update,
             y = Total_Deaths)) +
  geom_line(linewidth = 1, color = "navyblue") + 
  labs(title = "Cumulative COVID-19 Deaths in the US",
       x = "Date",
       y = "Total Deaths")





