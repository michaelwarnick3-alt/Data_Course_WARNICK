## read 'messy_bp.xlsx'
## how would you make it tidy?


library(tidyverse)
library(readxl)

Messybp = read_excel("Data/messy_bp.xlsx", skip = 3)
names(Messybp)

names(Messybp) = c("pat_id", "birth_month", "birth_day", "birth_year",
                   "race", "sex", "hispanic",
                   "bp_1", "hr_1", "bp_2", "hr_2", "bp_3", "hr_3")

Messybp = Messybp %>%
  unite(birthdate, birth_year, birth_month, birth_day, sep = "-") %>%
  mutate(race = ifelse(race %in% c("Caucasian", "WHITE"), "White", race))

## 3. blood pressure: make it long, then split 167/92
bp_long = Messybp %>%
  select(-c(hr_1, hr_2, hr_3)) %>%
  pivot_longer(cols = c(bp_1, bp_2, bp_3),
               names_to = "visit",
               values_to = "bp") %>%
  separate(visit, into = c("type", "visit"), sep = "_") %>%
  separate(bp, into = c("systolic", "diastolic"), sep = "/", convert = TRUE) %>%
  select(-type)
bp_long
view(bp_long)



## 4. heart rate: make it long
hr_long = Messybp %>%
  select(pat_id, birthdate, hr_1, hr_2, hr_3) %>%
  pivot_longer(cols = c(hr_1, hr_2, hr_3),
               names_to = "visit",
               values_to = "hr") %>%
  separate(visit, into = c("type", "visit"), sep = "_") %>%
  select(-type)
hr_long
view(hr_long)

## combine bp & hr 

Messybptidy = full_join(hr_long, bp_long,
                        by = join_by(pat_id, birthdate, visit))
view(Messybptidy)

## heart rate by visit
Messybptidy %>%
  group_by(visit, sex) %>%
  summarise(avg_hr = mean(hr)) %>%
  ggplot(aes(x = visit,
             y = avg_hr,
             fill = sex)) +
  geom_col(alpha = 0.6, position = 'dodge') +
  labs(title = "Average Heart Rate by Visit",
       x = "Visit (1,2,3)",
       y = "Average Heart Rate (bpm)",
       fill = "Sex (M & F)")

## bp by visit 
Messybptidy %>% 
  group_by(visit) %>%
  summarise(systolic = mean(systolic),
            diastolic = mean(diastolic)) %>%
  pivot_longer(cols = c(systolic, diastolic),
               names_to = "bp_type",
               values_to = "avg_bp") %>%
  ggplot(aes(x = visit,
             y = avg_bp,
             fill = bp_type)) + 
  geom_col(alpha = 0.6, position = 'dodge') +
  labs(title = "Average BP by Visit",
       x = "Visit",
       y = "Average BP",
       fill = "Type")

## examine the data and make it tidy 
data_tidy = read.csv('Data/Bird_Measurements.csv')
view(data_tidy)
install.packages("skimr")
library(skimr)
skim(data_tidy)
names(data_tidy)

birdy_tidy = data_tidy %>%
  select(-ends_with("_N")) %>%
  pivot_longer(cols = starts_with(c("M_", "F_","Unsexed")),
               names_to = c("sex", "measurement"),
               names_sep = "_",
               values_to = "value") %>%
  pivot_wider(names_from = measurement,
              values_from = value)
birdy_tidy
view(birdy_tidy)
skim(birdy_tidy)
birdy_tidy %>% count(sex)

malebirdy = birdy_tidy %>%
  select(Family, Species_name, English_name, starts_with("M_")) %>%
  select(-ends_with("_N")) %>%
  mutate(sex = "male") %>%
  rename_with(~ str_remove(.x, "M_"))
malebirdy
view(malebirdy)

femalebirdy = birdy_tidy %>% 
  select(Family, Species_name, English_name, starts_with("F_")) %>%
  select(-ends_with("_N")) %>%
  mutate(sex = "female") %>%
  rename_with(~str_remove(.x, "F_"))
femalebirdy
view(femalebirdy)

birdy_combined = full_join(malebirdy, femalebirdy) %>% view()

heavy_birds = birdy_combined %>%
  filter(mass > 1000)
heavy_birds

library(tidyverse)
library(palmerpenguins)
penguins %>% 
  drop_na() %>% 
  ggplot(aes(x = body_mass_g,
             fill = sex)) + 
  geom_density(alpha = 0.5)

t.test(body_mass_g ~ sex, data = penguins)
aov1 = aov(formula = body_mass_g ~ species, 
    data = penguins)
summary(aov1)
str(aov1)
glm()
lm()

mod1 = glm(formula = body_mass_g ~ species, 
           data = penguins)
summary(mod1)
str(mod1)

# test run the model (t-test and glm) and calculate avg body_mass_g

penguins %>% 
  drop_na(sex) %>% 
  group_by(sex) %>% 
  summarise(avg_mass = mean(body_mass_g))

penguins %>% 
  group_by(species) %>%
  summarise(avg_mass = mean(body_mass_g, na.rm = TRUE))


mod2 = glm(body_mass_g ~ species, data = penguins)
summary(mod2)

install.packages("easystats")
library(easystats)
report(mod2)
performance(mod2)
check_model(mod2)


mod2$fitted.values
mod2$coefficients
mod2$effects
mod2$model$body_mass_g

plot(mod2$model$body_mass_g,
     mod2$fitted.values)

dat_pen = penguins
dat_pen = data.frame(species = c(rep("Gentoo", 3),
                                 rep("Adelie", 5),
                                 rep("Chinstrap", 3)))

dat_pen$pred = predict(mod2, dat_pen)
mod2$coefficients
summary(mod2)

library()

# using 'mpg'
#does engine displacement (displ) affect cty? how much? 

library(tidyverse)
library(easystats)

mpg
glimpse(mpg)

mpg %>% 
  ggplot(aes(x = displ,
             y = cty)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) + 
  labs(title = "Engine Displacement vs City MPG",
       x = "Engine Displacement (liters)",
       y = "City MPG")
mod3 = glm(formula = cty ~ displ,
           data = mpg)
summary(mod3)

report(mod3)
performance(mod3)
check_model(mod3)
mod3$coefficients
mod3$fitted.values

plot(mod3$model$cty,
     mod3$fitted.values)

mpg %>%
  ggplot(aes(x = displ,
             y = cty)) +
  geom_point() +
  geom_smooth(method = "lm") +
  annotate(geom = 'text',
           x = 6, y = 30,
           label = 'R^2 = 0.64') +
  scale_x_continuous(limits = c(0,8), expand = c(0, 0))


## city mpg explained by engine size + drive type + number of cylinders + car class
library(tidyverse)
library(ggplot2)
library(easystats)
mod_mike = glm(cty ~ displ + drv + cyl + class, data = mpg) 
## This shows the coefficients, which tell you how much each predictor changes city mpg
summary(mod_mike) 
## This gives fit statistics, like R², which says how much of the variation in mpg the model explains.
performance(mod_mike) 
## This makes diagnostic plots that check whether the model’s assumptions hold, meaning whether you can trust the results.
check_model(mod_mike)
## you built a model showing that city mpg depends mostly on cylinder count, drive type, and vehicle class, and that engine size doesn’t add much once you already know the number of cylinders.

library(tidyverse)
library(easystats)

## Model 1: engine size only
mod1 = glm(cty ~ displ,
           data = mpg)

## Model 2: engine size + drive type
mod2 = glm(cty ~ displ + drv,
           data = mpg)

## Model 3: cylinders + drive type + car class (displ dropped because it overlaps with cyl)
mod3 = glm(cty ~ cyl + drv + class,
           data = mpg)

summary(mod1)
summary(mod2)
summary(mod3)
## compare side by side 
compare_performance(mod1, mod2, mod3)
## ranked which is best 
compare_performance(mod1, mod2, mod3, rank = TRUE)
## as a plot 
plot(compare_performance(mod1, mod2, mod3, rank = TRUE))
## stat test 
test_performance(mod1, mod2, mod3)

## cty = dispsl, cyl 
## build these three model (simple, additive, interaction)
## predict the cty using each model 

## simple only one preictor 
mod_simple = glm(cty ~ displ,
                 data = mpg)
## additive two predictors 
mod_add = glm(cty ~ displ + cyl,
              data = mpg)
## interactive displ depends on cyl 
mod_int = glm(cty ~ displ * cyl,
              data = mpg)
summary(mod_simple)
summary(mod_add)
summary(mod_int)

mpg2 <- mpg
mpg2$pred1 = predict(mod_simple, mpg2)
plot(mpg2$cty, mpg2$pred1)

cor.test(mpg$displ, mpg$cty)
mpg2$pred2 = predict(mod_add, mpg2)
mpg2$pred3 = predict(mod_int, mpg2)

mpg2 %>%
  ggplot(aes(x = displ, y = pred1, color = factor(cyl))) +
  geom_point()

mpg2 %>%
  ggplot(aes(x = displ, y = pred2, color = factor(cyl))) +
  geom_point()

mpg2 %>%
  ggplot(aes(x = displ, y = pred3, color = factor(cyl))) +
  geom_point()


## plot three model together 
view(mpg2)

## plot three models together
mpg2 = mpg %>%
  mutate(pred_simple = predict(mod_simple, mpg),
         pred_add    = predict(mod_add, mpg),
         pred_int    = predict(mod_int, mpg))
view(mpg2)

mpg2_long = mpg2 %>%
  pivot_longer(cols = c(pred_simple, pred_add, pred_int),
               names_to = "model_type",
               values_to = "pred_cty")

mpg2 %>% 
  pivot_longer(starts_with('pred')) %>%
  ggplot(aes(x = displ, y = cty, color = factor(cyl))) + 
  geom_point() + 
  geom_point(aes(y = value), color = "black") + 
  facet_wrap(~ name)

## build your BEST model to predict cty 
mod_best = glm(cty ~ displ * cyl + drv + class + fl + year, data = mpg)

summary(mod_best)
performance(mod_best)
check_model(mod_best)



         
         