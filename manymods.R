install.packages("gapminder")
library(gapminder)

nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")


###here i need to group_by()
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}
###use purrr:map to map the model to each dataframe
models <- map(by_country$data, country_model)

by_country <- by_country %>% 
  mutate(model = map(data, country_model))

by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

by_country
