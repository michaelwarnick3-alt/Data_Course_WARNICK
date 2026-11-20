## Assignment 8 Mushroom Growth Models
library(tidyverse)
library(modelr)
library(easystats)
mush = read_csv("../../Data/mushroom_growth.csv")
glimpse(mush)

## explore response vs the predictors
## growth vs light
mush %>%
  ggplot(aes(x = Light, y = GrowthRate, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~ Humidity) +
  theme_minimal()
## more light = more growth, especially P.cornucopiae at high humidity

## growth vs nitrogen
mush %>%
  ggplot(aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()
## NOT linear - goes up until about 25 then back down (hump shape)

## growth vs humidity
mush %>%
  ggplot(aes(x = Humidity, y = GrowthRate, fill = Species)) +
  geom_boxplot() +
  theme_minimal()
## high humidity grows more, mostly for P.cornucopiae

## growth vs temperature
mush %>%
  ggplot(aes(x = factor(Temperature), y = GrowthRate, fill = Species)) +
  geom_boxplot() +
  labs(x = "Temperature") +
  theme_minimal()
## temperature doesn't change much on its own

mod1 = glm(GrowthRate ~ Light, data = mush)
mod2 = glm(GrowthRate ~ Light + Humidity, data = mush)
mod3 = glm(GrowthRate ~ Light * Humidity * Species, data = mush)
mod4 = glm(GrowthRate ~ Light * Humidity * Species + Temperature + Nitrogen, data = mush)
## nitrogen is curved so use poly(Nitrogen, 2) to fit a curve
mod5 = glm(GrowthRate ~ Light * Humidity * Species + Temperature + poly(Nitrogen, 2), data = mush)

## mean squared error on each model (had to find on internet)
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2)
mean(mod5$residuals^2)
## mod1 = 7703, mod2 = 5829, mod3 = 3727, mod4 = 3629, mod5 = 2839

## best model
compare_performance(mod1, mod2, mod3, mod4, mod5, rank = TRUE)
## mod5 is best: lowest MSE, highest R2 (about 0.71)
## adding nitrogen as a curve helped a lot
report(mod5)

## real data with predictions
df = mush %>%
  add_predictions(mod5)
df %>% dplyr::select("GrowthRate", "pred")

## new values outside the real data (real light max = 20, nitrogen max = 45)
newdf = expand.grid(Light = c(30, 40),
                    Nitrogen = c(50, 60),
                    Humidity = c("Low", "High"),
                    Species = c("P.ostreotus", "P.cornucopiae"),
                    Temperature = 20)

pred = predict(mod5, newdata = newdf)

hyp_preds = newdf %>%
  mutate(pred = pred)

## label real vs hypothetical
df$PredictionType = "Real"
hyp_preds$PredictionType = "Hypothetical"

fullpreds = full_join(df, hyp_preds)
view(hyp_preds)
## some predictions at Nitrogen = 60 are NEGATIVE

## plot predictions 
ggplot(fullpreds, aes(x = Nitrogen, y = pred, color = PredictionType)) +
  geom_point() +
  geom_point(aes(y = GrowthRate), color = "black") +
  facet_wrap(~ Species) +
  labs(y = "Growth Rate") +
  theme_minimal()
