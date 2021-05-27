# TODO: man hat hier als land auch immernoch den `.`, das sollte man auf jeden
# fall nochmal erwaehnen
source("../../helper/package_check.R")
source("../../helper/data.R")
load_idl_complete()

library(ggplot2)
library(dplyr)

# idl_complete$DCOUNTRY is the column which holds the country of deatch of the
# person
observations_by_dcountry <- idl_complete %>% count(DCOUNTRY)
observations_by_dcountry_sex <- idl_complete %>% count(DCOUNTRY, SEX)
observations_by_dcountry_sex <- observations_by_dcountry_sex[
  -which(observations_by_dcountry_sex$DCOUNTRY == "."),
]

# there is the country `.`, with one female

# note that FIN and ITA have observations for Females only. For plotting reasons
# we add a 0 to the count for FIN and ITA for Males.
# ita_males <- c("ITA", "M", 0)
# fin_males <- c("FIN", "M", 0)
#
# observations_by_dcountry_sex <- rbind(observations_by_dcountry_sex, ita_males)
# observations_by_dcountry_sex <- rbind(observations_by_dcountry_sex, fin_males)

plot_obs_by_dcountry_sex <- ggplot(
  observations_by_dcountry_sex,
  aes(x = DCOUNTRY, y = n, group = SEX, colour = SEX, fill = SEX)
) +
  theme(
    legend.position = c(0.9, 0.85),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "grey"),
    panel.background = element_blank(),
    panel.grid.major.x =  element_blank()
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(trans = "log10") +
  labs(x = "Country of Death", y = "Number of Observations") +
  #ggtitle(
  #  label = "Distribution of Observations by Country of Death and Sex",
  #  subtitle = "FIN, ITA and LBN have data for females only"
  #) +
  scale_colour_discrete(
    name = "SEX",
    breaks = c("F", "M"),
    labels = c("female", "male")
  ) +
  scale_fill_discrete(
    name = "SEX",
    breaks = c("F", "M"),
    labels = c("female", "male")
  )

plot_obs_by_dcountry_sex <- annotate_figure(
  plot_obs_by_dcountry_sex,
  top = "Distribution of Observations by Country of Death and Sex"
)

ggsave(
  "./figures/obs_by_dcountry_sex.png",
  plot = plot_obs_by_dcountry_sex,
  width = 7,
  height = 4.3
)

print(
  xtable(observations_by_dcountry_sex),
  file = "./tables/table_observationsByDCOUNTRYandSEX.tex"
)

# occurences per death year in the cases of DEU and FRA
get_year_occurences <- function(country = "DEU", sex = "F") {
  df_tmp <-
    idl_complete[
      which(
        idl_complete$DCOUNTRY == country &
          idl_complete$SEX == sex
      ),
    ]
  death_years <- df_tmp$DDATE
  # have to do some string transformations
  death_years <- as.Date(death_years, "%d/%m/%Y")
  death_years <- format(death_years, "%Y")
  occurences <- table(as.integer(death_years))
  return(occurences)
}

death_occurrences_deu_female <- get_year_occurences(country = "DEU", sex = "F")
print(
  xtable(death_occurrences_deu_female),
  file = "./tables/table_death_occurences_deu_female.tex"
)
death_occurrences_deu_male <- get_year_occurences(country = "DEU", sex = "M")
print(
  xtable(death_occurrences_deu_male),
  file = "./tables/table_death_occurences_deu_male.tex"
)
death_occurrences_fra_female <- get_year_occurences(country = "FRA", sex = "F")
print(
  xtable(death_occurrences_fra_female),
  file = "./tables/table_death_occurences_fra_female.tex"
)
death_occurrences_fra_male <- get_year_occurences(country = "FRA", sex = "M")
print(
  xtable(death_occurrences_fra_male),
  file = "./tables/table_death_occurences_fra_male.tex"
)
