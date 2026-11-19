## Exam 2 - good luck Michael 
library(tidyverse)
library(easystats)

## 1. read in the unice data
dat = read_csv("unicef-u5mr.csv")
glimpse(dat)

## 2. tidy up, each year is its own column 
dat_tidy = dat %>%
  pivot_longer(cols = starts_with("U5MR"),
               names_to = "Year",
               values_to = "U5MR") %>%
  separate(Year, into = c("label", "Year"), sep = "\\.", convert = TRUE) %>%
  select(-label) %>% 
  drop_na(U5MR)
dat_tidy

## 3. Plot each country over time
plot1 = dat_tidy %>%
  ggplot(aes(x = Year,
             y = U5MR,
             group = CountryName)) +
  geom_line() + 
  facet_wrap(~ Continent) + 
  theme_bw()
plot1
ggsave("WARNICK_Plot_1.png", plot = plot1, width = 8, height = 6)

## 4. mean U5MR for each continent at each year
plot2 = dat_tidy %>%
  group_by(Continent, Year) %>%
  summarise(Mean_U5MR = mean(U5MR)) %>%
  ggplot(aes(x = Year,
             y = Mean_U5MR,
             color = Continent)) +
  geom_line(linewidth = 2) +
  theme_bw()
plot2
ggsave("WARNICK_Plot_2.png", plot = plot2, width = 8, height = 6)

## 5. Three Models 
mod1 = glm(U5MR ~ Year, data = dat_tidy)
mod2 = glm(U5MR ~ Year + Continent, data = dat_tidy)
mod3 = glm(U5MR ~ Year * Continent, data = dat_tidy)

## 6. Compare the models
compare_performance(mod1, mod2, mod3, rank = TRUE)
# mod3 was the best model with the lowest AIC and the highest R2,
# because each continent's U5MR dropped at a different rate over time

## 7. Plot model's predictions
dat_pred = dat_tidy %>%
  mutate(mod1 = predict(mod1, dat_tidy),
         mod2 = predict(mod2, dat_tidy),
         mod3 = predict(mod3, dat_tidy)) %>%
  pivot_longer(cols = c(mod1, mod2, mod3),
               names_to = "model",
               values_to = "pred")
dat_pred %>%
  ggplot(aes(x = Year,
             y = pred,
             color = Continent)) +
  geom_line(linewidth = 1) + 
  facet_wrap(~ model) + 
  labs(title = "Model predictions",
       y = "Predicted U5MR") +
  theme_bw()

## 8. BONUSSS Ecuador in 2020 13 under-5 deaths per 1000 live births
ecuador = data.frame(CountryName = "Ecuador",
                     Continent = "Americas",
                     Year = 2020)
## mod3 prediction 
ecuador_mod3 = predict(mod3, ecuador)
ecuador_mod3
ecuador_mod3 - 13

## mod4 
mod4 = glm(log(U5MR) ~ Year * Continent, data = dat_tidy)
ecuador_mod4 = exp(predict(mod4, ecuador))
ecuador_mod4
ecuador_mod4 - 13
