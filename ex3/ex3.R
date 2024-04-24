library(ggplot2)
theme_set(theme_dark())


data <- readxl::read_excel("./electricity.xlsx")
product <- "Renewables"
min_year <- 2015
countries <- c("Italy", "Latvia", "IEA Total")

filtered_data <- subset(data, PRODUCT == product & YEAR >= min_year & COUNTRY %in% countries)

filtered_data$TIME <- as.Date(paste(filtered_data$TIME, "01"), format = "%B %Y %d")

filtered_data$share <- as.numeric(filtered_data$share)

filtered_data |>
  ggplot(aes(x = TIME, y = share * 100, color = COUNTRY)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_date(date_breaks = "5 months", date_labels = "%B %Y") +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = paste("Monthly evolution of Renewable energies market share of Italy vs Latvia vs IEA Total"),
       x = paste("Date"),
       y = paste("Market Share Percentage (%)"))
