source("../../helper/package_check.R")
source("../../helper/data.R")
source('./estimators.R')
source('./parametric_gpd.R')
source('./necessary_variables.R')

df_mle_women <- data.frame(
  x = as.numeric(estimators_german_women$DDATE),
  y = as.numeric(estimators_german_women$mle_shape)
)
df_mle_men <- data.frame(
  x = as.numeric(estimators_german_men$DDATE),
  y = as.numeric(estimators_german_men$mle_shape)
)

plot_mle_ger <- ggplot(
  NULL,
  aes(x, y),
  colour = y
) +
geom_line(data = df_mle_women, aes(x, y, colour = "Women")) +
geom_line(data = df_mle_men, aes(x, y, colour = "Men")) +
geom_hline(yintercept = -0.5, col = "orange") +
labs(
  x = "Year of Death",
  y = unname(TeX("$\\hat{\\gamma}"))
) +
scale_color_manual(name = c('Women', 'Men'), values=c(Women = "red", Men = "blue"))+
theme(
  legend.position = c(0.15, 0.15),
  legend.key.size = unit(0.5, "cm"),
  legend.key.height = unit(0.5, "cm"),
  legend.key.width = unit(0.5, "cm"),
  legend.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  # panel.border = element_blank(),
  panel.grid.major = element_line(color = "grey"),
  # panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(color = "black"),
  legend.text.align = 0
)

ggsave(
  "./plots/neg_evi/mle_shape_germany.png",
  plot = plot_mle_ger,
  width = 7,
  height = 4.3
)

df_mle_women <- data.frame(
  x = as.numeric(estimators_france_women$DDATE),
  y = as.numeric(estimators_france_women$mle_shape)
)
df_mle_men <- data.frame(
  x = as.numeric(estimators_france_men$DDATE),
  y = as.numeric(estimators_france_men$mle_shape)
)

plot_mle_france <- ggplot(
  NULL,
  aes(x, y),
  colour = y
) +
geom_line(data = df_mle_women, aes(x, y, colour = "Women")) +
geom_line(data = df_mle_men, aes(x, y, colour = "Men")) +
geom_hline(yintercept = -0.5, col = "orange") +
labs(
  x = "Year of Death",
  y = unname(TeX("$\\hat{\\gamma}"))
) +
scale_color_manual(name = c('Women', 'Men'), values=c(Women = "red", Men = "blue"))+
theme(
  legend.position = c(0.15, 0.15),
  legend.key.size = unit(0.5, "cm"),
  legend.key.height = unit(0.5, "cm"),
  legend.key.width = unit(0.5, "cm"),
  legend.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  # panel.border = element_blank(),
  panel.grid.major = element_line(color = "grey"),
  # panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(color = "black"),
  legend.text.align = 0
)

ggsave(
  "./plots/neg_evi/mle_shape_france.png",
  plot = plot_mle_france,
  width = 7,
  height = 4.3
)

