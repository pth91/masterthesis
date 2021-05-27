
source("../../helper/package_check.R")
source("../../helper/data.R")
load_idl_complete()

#-------------------------------------------------------------------------------

countries <- unique(idl_complete$DCOUNTRY)

# has to be done to delete the `.` death country
countries <- countries[nchar(countries) > 1]

# TODO: auf jeden fall muss ich noch irgendwo schreiben, wieso ich gerade diese
# vier rausggenommmen habe...
# es gibt nur frauen und davon zu wenige beobachtungen
# TODO: EW ist das dcountry bei leuten die in UK geboren sind... wofür soll das
# stehen?
countries <- countries[
  countries != "ITA" & countries != "LBN" & countries != "FIN" & countries != "SWE"
]

#-------------------------------------------------------------------------------

for (country in countries) {
  print(country)
  data_ <- idl_complete[idl_complete$DCOUNTRY == country, ]
  #-----------------------------------------------------------------------------

  women <- data_[data_$SEX == "F", ]
  if(dim(women)[1] > 1){
    agedays_women <- as.numeric(women$AGEDAYS)
    fit_women <- fpot(agedays_women, threshold = min(agedays_women))
    fitted_params_women <- fit_women$param

    #-----------------------------------------------------------------------------
    country_qq_women <- ggplot(
      data = data_[data_$SEX == "F", ],
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
      labs(
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      ggtitle(
        label = "Females"
      )
  }

  #-----------------------------------------------------------------------------

  men <- data_[data_$SEX == "M", ]
  if(dim(men)[1] > 1){
    agedays_men <- as.numeric(men$AGEDAYS)
    fit_men <- fpot(agedays_men, threshold = min(agedays_men))
    fitted_params_men <- fit_men$param

    #-----------------------------------------------------------------------------

    country_qq_men <- ggplot(
      data = data_[data_$SEX == "M", ],
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
      labs(
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      ggtitle(
        label = "Males"
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
        country,
        " QQ Plot"
      )
    ),
    bottom = text_grob(
      "Theoretical against empirical quantiles plotted."
    )
  )

  #-----------------------------------------------------------------------------
  ggsave(
    plot = arranged_qq_plot,
    file = paste0(
      "./figures/no_gender/qq_",
      country,
      "_arranged.png"
    ),
    # TODO: das muss man iwie noch besser anpassen, goldener schnitt als
    # verhaeltnis zwischen breie und hoehe macht sich ganz gut
    width = 7,
    height = 4.3
  )
}
