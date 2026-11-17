View(penguins)

##read 'wide_income_rent' using relative path
## compare data structure with penguins data
## make a graph 


library(tidyverse)
library(palmerpenguins)
getwd()
incomedat = read_csv("Data/wide_income_rent.csv") 
incomedat
names(incomedat)
view(penguins)
view(incomedat)

income_rent = incomedat %>%
  pivot_longer(-variable,
               names_to = "state",
               values_to = "amount") %>% 
  pivot_wider(names_from = variable,
              values_from = amount)
income_rent
view(income_rent)

income_rent %>%
  ggplot(aes(x = income,
             y = rent)) + 
  geom_point(size = 3, alpha = 0.7) + 
  geom_smooth(method = 'lm') +
  labs(title = "Median Income vs Rent by State",
       x = "Median Income ($)",
       y = "Median Rent ($)") 

table1
table2
table2_cleanup = table2 %>%
  pivot_wider(names_from = type,
              values_from = count)
table2_cleanup

table2_cleanup %>%
  mutate(rate = cases / population * 10000) %>%
  ggplot(aes(x = year,
             y = rate,
             color = country)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(title = "TB Cases per 10,000 People",
       x = "Year",
       y = "Cases per 10,000 people",
       color = "Country") 

table3
table_3cleanup = table3 %>%
  separate(rate, 
           into = c("cases", "population"),
           sep = "/",
           convert = TRUE)
table_3cleanup

## make table4a pivot longer 
table4a
cases_long = table4a %>%
  pivot_longer(cols = c('1999', '2000'),
               names_to = "year",
               values_to = "population")
cases_long

## make table 4b pivot longer 
table4b
pop_long = table4b %>%
  pivot_longer(cols = c(`1999`, `2000`),
               names_to = "year",
               values_to = "population")
pop_long

## join them together 
table4_cleanup = cases_long %>%
  full_join(pop_long, by = c("country", "year")) %>%
  mutate(year = as.numeric(year))
table4_cleanup  
