library(ggplot2)
theme_set(theme_dark())

url <- 'https://web.tecnico.ulisboa.pt/~paulo.soares/pe/projeto/Paises_PIB_ICH.csv'
data <- read.csv(url, check.names = FALSE)
continents <- c("Asia", "Africa")
countries <- c("United Arab Emirates", "Nepal", "Comoros", "Namibia")

data |>
  subset(Continent %in% continents) |>
    ggplot(aes(x = GDP, y = HCI, color = Continent)) +
    geom_point() +
    geom_text(aes(label = ifelse(Country %in% countries, Country, "")),
              hjust = 1,
              vjust = -0.2) +
    scale_x_log10() +
    labs(title = paste("HCI vs GDP in", paste(continents, collapse = " and ")))
