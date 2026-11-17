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

table5
table5_tidy = table5 %>%
  unite(year, century, year, sep = "") %>%
  separate(rate,
           into = c("cases", "population"),
           sep = "/",
           convert = TRUE)
table5_tidy
  