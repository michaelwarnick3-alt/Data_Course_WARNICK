## install gapminder, gganimate, gifski, av 
## take a look of gappminder and make some cool graph 

install.packages(c("gapminder", "gganimate", "gifski", "av"))   # only run once

library(tidyverse)
library(gapminder)
library(gganimate)
gapminder
view(gapminder)
col(gapminder)
 
gapminder %>%
  group_by(country) %>% 
  summarise(avg_lifeExp = mean(lifeExp),
            avg_gdp = mean(gdpPercap))

gapminder %>%
  ggplot(aes(x = lifeExp,
             y = gdpPercap,
             size = pop,
             color = continent)) + 
  geom_point(alpha = 0.7) +
  geom_text(aes(label = my_label),
            size = 2.5, vjust = -1.2, show.legend = FALSE) +
  facet_wrap(~ continent) +
  transition_time(time = year)
my_country = c('Kuwait', 'Saudi Arabia','Gambia', 'Haiti', 'Mexico', 'Dominican Republic', 'France', 'Egypt', 'Switzerland')
unique('my_country')
gapminder%
'Saudi Arabia' %in%

##save your animate and view it 
##try and label the country you are interested in
anim_save('my_anim.gif')


ggsave("../../Downloads/GDP_Lifeexpectancy.png")
  
getwd()
