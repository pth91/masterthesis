source("../../helper/package_check.R")
source("../../helper/data.R")

load_idl_complete()

#-------------------------------------------------------------------------------
data_ <- idl_complete[idl_complete$DCOUNTRY == "DEU", ]
data_$DDATE <- substr(data_$DDATE, 7, 10)
#-----------------------------------------------------------------------------

women <- data_[which(data_$SEX == "F" & data_$DDATE == "2001"), ]
if (dim(women)[1] > 1) {
  agedays_women <- as.numeric(women$AGEDAYS)
  fit_women <- fpot(agedays_women, threshold = min(agedays_women))
  fitted_params_women <- fit_women$param

  #-----------------------------------------------------------------------------
  country_qq_women <- ggplot(
    data = women,
    mapping = aes(sample = AGEDAYS)
  ) +
    stat_qq(
      distribution = evd::qgpd,
      dparams = list(
        loc = min(agedays_women),
        scale = fitted_params_women["scale"],
        shape = fitted_params_women["shape"]
      )
    ) +
    stat_qq_line(
      distribution = evd::qgpd,
      dparams = list(
        loc = min(agedays_women),
        scale = fitted_params_women["scale"],
        shape = fitted_params_women["shape"]
      )
    ) +
    labs(
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggtitle(
      label = "Females, year 2003"
    )
}

#-----------------------------------------------------------------------------

men <- data_[which(data_$SEX == "M" & data_$DDATE == "2000"),]
if (dim(men)[1] > 1) {
  agedays_men <- as.numeric(men$AGEDAYS)
  fit_men <- fpot(agedays_men, threshold = min(agedays_men))
  fitted_params_men <- fit_men$param

  #-----------------------------------------------------------------------------

  country_qq_men <- ggplot(
    data = men,
    mapping = aes(sample = AGEDAYS)
  ) +
    stat_qq(
      distribution = evd::qgpd,
      dparams = list(
        loc = min(agedays_men),
        scale = fitted_params_men["scale"],
        shape = fitted_params_men["shape"]
      )
    ) +
    stat_qq_line(
      distribution = evd::qgpd,
      dparams = list(
        loc = min(agedays_women),
        scale = fitted_params_men["scale"],
        shape = fitted_params_men["shape"]
      )
    ) +
    labs(
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggtitle(
      label = "Males, year 2000"
    ) +
    theme(
      axis.title.y = element_blank()
    )
}

#-----------------------------------------------------------------------------

arranged_qq_plot <- ggarrange(
  country_qq_women,
  country_qq_men
)

arranged_qq_plot <- annotate_figure(
  arranged_qq_plot,
  top = text_grob(
    paste0(
      "Germany"
    )
  ),
  # bottom = text_grob(
  #    "Theoretical against empirical quantiles plotted."
  # )
)

#-----------------------------------------------------------------------------
ggsave(
  plot = arranged_qq_plot,
  file = paste0(
    "./figures/fra_ger/qq_",
    "germany",
    "_arranged.png"
  ),
  # TODO: das muss man iwie noch besser anpassen, goldener schnitt als
  # verhaeltnis zwischen breie und hoehe macht sich ganz gut
  width = 7,
  height = 4.3
)

data_ <- idl_complete[idl_complete$DCOUNTRY == "FRA", ]
data_$DDATE <- substr(data_$DDATE, 7, 10)

women <- data_[which(data_$SEX == "F" & data_$DDATE == "2017"), ]
if (dim(women)[1] > 1) {
  agedays_women <- as.numeric(women$AGEDAYS)
  fit_women <- fpot(agedays_women, threshold = min(agedays_women))
  fitted_params_women <- fit_women$param

  #-----------------------------------------------------------------------------
  country_qq_women <- ggplot(
    data = women,
    mapping = aes(sample = AGEDAYS)
  ) +
    stat_qq(
      distribution = evd::qgpd,
      dparams = list(
        loc = min(agedays_women),
        scale = fitted_params_women["scale"],
        shape = fitted_params_women["shape"]
      )
    ) +
    stat_qq_line(
      distribution = evd::qgpd,
      dparams = list(
        loc = min(agedays_women),
        scale = fitted_params_women["scale"],
        shape = fitted_params_women["shape"]
      )
    ) +
    labs(
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggtitle(
      label = "Females, year 2017"
    )
}

#-----------------------------------------------------------------------------

men <- data_[which(data_$SEX == "M" & data_$DDATE == "2015"),]
if (dim(men)[1] > 1) {
  agedays_men <- as.numeric(men$AGEDAYS)
  fit_men <- fpot(agedays_men, threshold = min(agedays_men))
  fitted_params_men <- fit_men$param

  #-----------------------------------------------------------------------------

  country_qq_men <- ggplot(
    data = data_[which(data_$SEX == "M" & data_$DDATE == "2015"), ],
    mapping = aes(sample = AGEDAYS)
  ) +
    stat_qq(
      distribution = evd::qgpd,
      dparams = list(
        loc = min(agedays_men),
        scale = fitted_params_men["scale"],
        shape = fitted_params_men["shape"]
      )
    ) +
    stat_qq_line(
      distribution = evd::qgpd,
      dparams = list(
        loc = min(agedays_men),
        scale = fitted_params_men["scale"],
        shape = fitted_params_men["shape"]
      )
    ) +
    labs(
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggtitle(
      label = "Males, year 2015"
    ) +
    theme(
      axis.title.y = element_blank()
    )
}

#-----------------------------------------------------------------------------

arranged_qq_plot <- ggarrange(
  country_qq_women,
  country_qq_men
)

arranged_qq_plot <- annotate_figure(
  arranged_qq_plot,
  top = text_grob(
    paste0(
      "France"
    )
  ),
  # bottom = text_grob(
  #    "Theoretical against empirical quantiles plotted."
  # )
)

#-----------------------------------------------------------------------------
ggsave(
  plot = arranged_qq_plot,
  file = paste0(
    "./figures/fra_ger/qq_",
    "france",
    "_arranged.png"
  ),
  # TODO: das muss man iwie noch besser anpassen, goldener schnitt als
  # verhaeltnis zwischen breie und hoehe macht sich ganz gut
  width = 7,
  height = 4.3
)
