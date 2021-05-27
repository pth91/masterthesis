library(ggplot2)
library(plotly)

plot_year_frequency <- function(country = "all") {
  print(country)
  ggplotly(ggplot(get_age_count_by_country(country), aes(AGEYEARS, n, color = DCOUNTRY)) +
    geom_point())
}
