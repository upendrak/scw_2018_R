# Reading in the data
setwd("~/scw_2018/intro_R/")
gapminder <- read.csv("gapminder-FiveYearData.csv")

head(gapminder)

str(gapminder)

summary(gapminder)

# group_by with counts
gapminder %>% group_by(country) %>% tally()

# group_by with summarise
gapminder %>% group_by(country) %>% summarise(avg = mean(pop), std = sd(pop), total = n())

# Use of arrange function in dplyr
gapminder %>% group_by(country) %>% 
  summarise(avg = mean(pop), std = sd(pop), total = n()) %>% 
  arrange(desc(avg))

# Mutate
gapminder_mod <- gapminder
gapminder_mod <- gapminder_mod %>% mutate(gdp = pop * gdpPercap)

# Calculate the average life expectancy per country. Which nation has the longest average life expectancy and which has the shortest average life expectancy? 
gapminder_mod %>% group_by(country) %>% 
  summarise(avg = mean(lifeExp)) %>% 
  arrange(avg) %>% head(1)

gapminder_mod %>% group_by(country) %>% 
  summarise(avg = mean(lifeExp)) %>% 
  arrange(desc(avg)) %>% head(1)

# oneliner
gapminder_mod %>% group_by(country) %>% 
  summarise(avg = mean(lifeExp)) %>% 
  filter(avg == max(avg) | avg == min(avg))

# Plotting
# base R plotting
plot(x = gapminder_mod$gdpPercap, y = gapminder_mod$lifeExp)

# ggplot2
library(ggplot2)
ggplot(gapminder_mod, aes(x = gdpPercap, y = lifeExp)) + geom_point()

# log10 conversion
ggplot(gapminder_mod, aes(x = log10(gdpPercap), y = lifeExp)) + 
  geom_point()

# transperancy
ggplot(gapminder_mod, aes(x = log10(gdpPercap), y = lifeExp)) + 
  geom_point(alpha = 1/3, size = 3)

# color 
p <- ggplot(gapminder_mod, aes(x = log10(gdpPercap), y = lifeExp, size = continent)) + 
  geom_point()
p <- p + facet_wrap(~ continent)
p2 <- p + geom_smooth(color = "orange")
p2

# Combine dplyr with ggplot2
gapminder %>% mutate(gdp = pop * gdpPercap) %>% 
  ggplot(aes(gdp, lifeExp)) + geom_point()

# Histogram
p3 <- ggplot(gapminder_mod, aes(lifeExp, fill = continent)) + 
  geom_histogram(binwidth = 1) + 
  ggtitle("Histogram_gapminder")
p3

# saving plots
ggsave(p3, file = "~/scw_2018/advanced_R/histogram_lifeExp.png")

# line plot
gapminder_mod %>% filter(country == "Afghanistan") %>% 
  ggplot(aes(x = year, y = lifeExp)) +
  geom_line(color = "blue")

# 2. Plot lifeExp against year and facet by continent and fit a smooth and/or linear regression, w/ or w/o facetting (Hint: Look at geom_smooth)
 p5 <- ggplot(gapminder_mod, aes(lifeExp, year)) + geom_point() + 
  facet_wrap(~ continent) + 
  geom_smooth(color = "orange", lwd = 2, se = FALSE)
p6 <- p5 + geom_smooth(color = "blue", lwd = 2, se = FALSE, method = "lm")
p6
ggsave(p6, file = "geom_smooth_type.png")

# density plot
P7 <- ggplot(gapminder_mod, aes(gdpPercap, lifeExp)) + 
  geom_point(size = 0.25) +
  geom_density_2d() + scale_x_log10()
P7

# Combine plots
install.packages("gridExtra")
library(gridExtra)
gridExtra::grid.arrange(
  p5 <- ggplot(gapminder_mod, aes(lifeExp, year)) + geom_point() + 
    facet_wrap(~ continent) + 
    geom_smooth(color = "orange", lwd = 2, se = FALSE),
  P7 <- ggplot(gapminder_mod, aes(gdpPercap, lifeExp)) + 
    geom_point(size = 0.25) +
    geom_density_2d() + scale_x_log10()
)

# loops

gapminder_mod %>% filter(continent == "Asia") %>% 
  summarise(avg = mean(lifeExp))

contin <- unique(gapminder_mod$continent)
contin

# for (variable in list) {
#   do something
# }

for (c in contin) {
  for (y in unique(gapminder_mod$year))
  #print(c)
  res <- gapminder_mod %>% filter(continent == c) %>% 
    summarise(avg = mean(lifeExp))
  print(paste0("The avg life expectancy of ", c, "for the year", y "is:", res))
  #print(res)
}

gapminder_mod %>% group_by(continent, year) %>% 
  summarise(avg = mean(lifeExp))

# Functions
mean(2,3)

adder <- function(x, y){
  print(paste0("The sum of ", x, " and ", y, " is: ", x+y))
  #return(x + y)
}
adder(2,3)

# This is the end of the workshop
