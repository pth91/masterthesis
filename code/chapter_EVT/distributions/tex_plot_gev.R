# TODO: man kann hier prinzipiell auf der x achse noch vertikal einzeichnen, wo
# TODO: wieso sind gev und gpd dasselbe bild?
# die .95 bzw. 1 quantile liegen fuer den fall dass die endlich sind
#source("code/package_check.R")
support_pos <- seq(-1.5, 6, length.out = 1e6)
df_support_pos <- data.frame(
  x = support_pos,
  gev0 = pgev(support_pos, loc = 0, scale = 1, shape = 0),
  gev1 = pgev(support_pos, loc = 0, scale = 1, shape = 1),
  gev2 = pgev(support_pos, loc = 0, scale = 1, shape = 2),
  gev4 = pgev(support_pos, loc = 0, scale = 1, shape = 4)
)

df_pos <- gather(
  df_support_pos,
  "gev0",
  "gev1",
  "gev2",
  "gev4",
  key = "gev", value = "y"
)

plot_gev_pos <- ggplot(
  df_pos,
  aes(x, y, colour = gev)
) +
  geom_line() +
  labs(
    x = "x",
    y = unname(TeX("$ G_{1, 0, \\gamma}(x)"))
  ) +
  scale_colour_discrete(
    name = "Extreme Value Index",
    breaks = c("gev0", "gev1", "gev2", "gev4"),
    labels = c(
      expression(paste(gamma, " = 0")),
      expression(paste(gamma, " = 1")),
      expression(paste(gamma, " = 2")),
      expression(paste(gamma, " = 4"))
    )
  ) +
  theme(
    legend.position = c(0.85, 0.15),
    legend.key.size = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "grey"),
    # panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  ) #+
# scale_x_continuous(trans = "log10")

#-------------------------------------------------------------------------------

support_neg <- seq(-5, 2, length.out = 1e6)
df_support_neg <- data.frame(
  x = support_neg,
  gev0 = pgev(support_neg, loc = 0, scale = 1, shape = -1),
  gev1 = pgev(support_neg, loc = 0, scale = 1, shape = -2),
  gev2 = pgev(support_neg, loc = 0, scale = 1, shape = -4)
)

df_neg <- gather(
  df_support_neg,
  "gev0",
  "gev1",
  "gev2",
  key = "gev", value = "y"
)

plot_gev_neg <- ggplot(
  df_neg,
  aes(x, y, colour = gev)
) +
  geom_line() +
  labs(x = "x", y = "GEV_dist") +
  scale_colour_discrete(
    name = "Extreme Value Index",
    breaks = c("gev0", "gev1", "gev2"),
    labels = c(
      expression(paste(gamma, " = -1")),
      expression(paste(gamma, " = -2")),
      expression(paste(gamma, " = -4"))
    )
  ) +
  theme(
    legend.position = c(0.85, 0.15),
    legend.key.size = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "grey"),
    # panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    legend.text.align = 0
  ) #+

#-------------------------------------------------------------------------------

plot_gev_arranged <- ggarrange(
  plot_gev_pos,
  plot_gev_neg
)

#plot_gev_arranged <- annotate_figure(
#  plot_gev_arranged,
#  top = "Generalized Extreme Value Distributions"
#)

ggsave(
  plot_gev_arranged,
  file = "./figures/gev_arranged.png",
  width = 7,
  height = 4.3
)

#-------------------------------------------------------------------------------

quantile_gev_pos <- c(0.25, 0.5, 0.75, 0.9, 0.95)
df_quantiles_pos <- data.frame(
  q = quantile_gev_pos,
  qgeqv0 = qgev(quantile_gev_pos, loc = 0, scale = 1, shape = 1),
  qgeqv1 = qgev(quantile_gev_pos, loc = 0, scale = 1, shape = 2),
  qgeqv2 = qgev(quantile_gev_pos, loc = 0, scale = 1, shape = 4)
)
colnames(df_quantiles_pos) <- c("quantiles", "$gamma=1$", "$gamma=2$", "$gamma=4$")

print(
  xtable(df_quantiles_pos, type = "tex"),
  file = "./tables/table_gev_pos_quantiles.tex"
)

#-------------------------------------------------------------------------------

quantile_gev_neg <- c(0.25, 0.5, 0.75, 0.9, 0.95)
df_quantiles_neg <- data.frame(
  q = quantile_gev_neg,
  qgeqv0 = qgev(quantile_gev_neg, loc = 0, scale = 1, shape = -1),
  qgeqv1 = qgev(quantile_gev_neg, loc = 0, scale = 1, shape = -2),
  qgeqv2 = qgev(quantile_gev_neg, loc = 0, scale = 1, shape = -4)
)
colnames(df_quantiles_neg) <- c("quantiles", "$gamma=-1$", "$gamma=-2$", "$gamma=-4$")

print(
  xtable(df_quantiles_neg, type = "tex"),
  file = "./tables/table_gev_neg_quantiles.tex"
)
