# read a data frame from a CSV file
# stringsAsFactors = TRUE because all strings in the file should be treated as
# categories
gapminder <- read.csv(file = "data/gapminder_data.csv", stringsAsFactors = TRUE)

# create a data frame from scratch
cats <- data.frame(coat = c("calico","black","tabby"),
                   weight = c(2.1, 5.0, 3.2),
                   likes_string = c(1,0,1))

# indexing (also extracting, subsetting)
cats$weight

# column-wide operation; this assigns new values to the column
# to create a variable and see its output at the same time, put paren around it
(cats$weight <- cats$weight * 2)

# Paste the things together
paste("My cat is ", cats$coat)

typeof(cats$weight)

(cats$likes_string <- as.logical(cats$likes_string))

str(cats)
str(gapminder)

(cats$coat <- as.factor(cats$coat))

str(cats)
as.factor(cats$weight)

combine_vector = c(2,6,3)

quiz_vector = c(2,6,'myname')

ab_vector = c('a', 'b')
combine_example = c(ab_vector, 'blah')

seq(1:100)

big_vector <- 1:1000
head(big_vector)
head(big_vector, n = 4)

length(big_vector)
length(cats$weight)

pizza_price <- c(pizzasubito = 5.64, pizzafresh = 6.60, callapizza = 4.50)
names(pizza_price)

pizza_price["pizzasubito"]
names(pizza_price)[3] <- "call-a-pizza"
names(pizza_price)

list_example = list(1, "a", TRUE)
list_example

complex_list = list("a", 1:100, FALSE)
complex_list

lol = list("a", list("b", 234))
lol


str(complex_list)

complex_list[[2]][30]

complex_list[[1]][3]

complex_list[[2]]



cats$coat
cats[1]

gapminder[2]
str(gapminder)

cats[2,3]
cats[1,2]

cats$coat[2]

cats[1:2,3]

cats[3, "coat"]

cats[cats$coat != "tabby", ]

cats[,"weight"]

test_run <- gapminder[gapminder$continent %in% c("Europe", "Oceania"), ]
str(test_run)









str(gapminder)
nrow(gapminder)
ncol(gapminder)
dim(gapminder)
summary(gapminder)

mean(gapminder[gapminder$continent == "Africa", "gdpPercap"])

library(dplyr)
gapminder %>%
  group_by(continent) %>%
  summarize(mean = mean(gdpPercap))



year_country_gdp <- gapminder%>%
  select(country,year,gdpPercap)

small_dataset <- select(gapminder, -continent, -country, -year)

tidy_gdp <- gapminder %>%
  rename(gdp_per_cap = gdpPercap)
head(tidy_gdp)

gapminder_Africa <- gapminder %>%
  filter(continent == "Africa")
tail(gapminder_Africa)

gapminder %>%
  filter(year == 2007) %>%
  select(-year)

(lifeExp_bycountry <- gapminder %>%
  group_by(country))

# calculate mean life expectancy for each group in the data
lifeExp_bycountry %>%
  summarise(mean_lifeExp = mean(lifeExp))

# Print *all* of the rows
gapminder %>%
  group_by(country) %>%
  summarise(mean_lifeExp = mean(lifeExp)) %>%
  print(n = Inf)


mean_lifeExp_by_country <- gapminder %>%
  group_by(country) %>%
  summarise(mean_lifeExp = mean(lifeExp)) %>%
  arrange(desc(mean_lifeExp)) %>%
  print(n = Inf)
mean_lifeExp_by_country

gapminder %>%
  filter(year == 2002) %>%
  count(continent, sort = TRUE)

# standard error; n() = nunmber of rows
gapminder %>%
  group_by(continent) %>% 
  summarize(se_le = sd(lifeExp) / sqrt(n()), n = n())

gapminder %>%
  group_by(continent) %>% 
  summarize(mean = mean(lifeExp),
            sd = sd(lifeExp),
            se_le = sd(lifeExp) / sqrt(n()),
            n = n())

gdp_total <- gapminder %>%
  mutate(gdp = gdpPercap * pop)
head(gdp_total)



library(ggplot2)

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap, y = lifeExp))
# --> only creates coordinate space

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha=0.1)
# alpha is transparency level of points

ggplot(data = gapminder,
       mapping = aes(x = year, y = lifeExp, color = continent)) +
  geom_line()

ggplot(data = gapminder,
       mapping = aes(x = year, y = lifeExp, group = continent, color = continent)) +
  geom_line()

gapminder <- gapminder %>%
  summarise(gdp = gdpPercap * pop)

ggplot(data = gapminder,
       mapping = aes(x = gdp, y = pop)) +
  geom_point(color = "aquamarine3")

ggplot(data = gapminder,
       mapping = aes(x = year, y = lifeExp, color = continent)) +
  geom_smooth()

# use a log scale to better visualize
ggplot(data = gapminder,
       mapping = aes(x = gdpPercap, y=lifeExp)) +
  geom_point(alpha = 0.5) +
  scale_x_log10()

# Change aesthetics of the regression line (turn off error bars, thicken line, and change color):
ggplot(data = gapminder,
       mapping = aes(x = gdpPercap, y=lifeExp)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "orange")

# Display results with each country in a separate plot:
gapminder %>%
  filter(continent == "Americas") %>%
  ggplot(mapping = aes(x = year, y = lifeExp)) + 
  geom_smooth() +
  facet_wrap(~ country) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = "Year", y = "Life Expectancy", title = "Figure 1: Life Expectancy in the Americas")

