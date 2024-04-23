library(ggplot2)
theme_set(theme_dark())

url <- "https://web.tecnico.ulisboa.pt/~paulo.soares/pe/projeto/master.csv"
data <- read.csv(url, check.names = FALSE)
target_year <- 2002
age_group <- "55-74 years"

filtered_data <- subset(data, year == target_year & age == age_group)

male_data <- subset(filtered_data, sex == "male")

female_data <- subset(filtered_data, sex == "female")

male_plot <- ggplot(male_data, aes(x = `country`, y = `suicides/100k pop`)) +
  geom_boxplot() +
  labs(title = paste("Male suicides / 100k Inhabitants (", target_year, ", ", age_group, ")"))

female_plot <- ggplot(female_data, aes(x = `country`, y = `suicides/100k pop`)) +
  geom_boxplot() +
  labs(title = paste("Female suicides / 100k Inhabitants (", target_year, ",", age_group, ")"))

print(male_plot)
print(female_plot)