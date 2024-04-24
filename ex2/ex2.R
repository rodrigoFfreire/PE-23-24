library(ggplot2)
theme_set(theme_dark())

url <- "https://web.tecnico.ulisboa.pt/~paulo.soares/pe/projeto/master.csv"
data <- read.csv(url, check.names = FALSE)
target_year <- 2002
age_group <- "55-74 years"

data |>
  subset(year == target_year & age == age_group) |>
  ggplot(aes(color = `sex`)) +
  geom_boxplot(aes(x = `sex`, y = `suicides/100k pop`)) +
  coord_flip() +
  labs(title = paste("Suicides/100k Inhabitants - Males vs Females (", target_year, ",", age_group, ")"),
       x = paste("Sex"),
       y = paste("Suicides/100k Inhabitants"))