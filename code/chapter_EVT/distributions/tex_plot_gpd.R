# TODO: eigentlich ergibt es mehr sinn einen plot fuer negative und einen fuer
# positive extreme value indices zu zeichnen
#source("code/package_check.R")
support_neg_evi <- seq(0, 1.5, length.out = 1e4)
df_support_neg_evi <- data.frame(
  x = support_neg_evi,
  gpd0 = pgpd(support_neg_evi, loc = 0, scale = 1, shape = -3),
  gpd1 = pgpd(support_neg_evi, loc = 0, scale = 1, shape = -2),
  gpd2 = pgpd(support_neg_evi, loc = 0, scale = 1, shape = -1)
)

support_pos_evi <- seq(0, 10000, length.out = 1e6)
df_support_pos_evi <- data.frame(
  x = support_pos_evi,
  gpd0 = pgpd(support_pos_evi, loc = 0, scale = 1, shape = 1),
  gpd1 = pgpd(support_pos_evi, loc = 0, scale = 1, shape = 2),
  gpd2 = pgpd(support_pos_evi, loc = 0, scale = 1, shape = 3)
)

quantile_gpd_negative_evi <- c(0.25, 0.5, 0.75, 0.9, 0.95)
# we can give the value of F(gpd <= 1) directly, it is 1/extreme value index
# hence, for gamma = 2 we have that F(gpd_2 <= 1/2) = 1
df_quantiles_neg_evi <- data.frame(
  q = quantile_gpd_negative_evi,
  qgeqv0 = qgpd(quantile_gpd_negative_evi, loc = 0, scale = 1, shape = -3),
  qgeqv1 = qgpd(quantile_gpd_negative_evi, loc = 0, scale = 1, shape = -2),
  qgeqv2 = qgpd(quantile_gpd_negative_evi, loc = 0, scale = 1, shape = -1)
)

quantile_gpd_positive_evi <- c(0.25, 0.5, 0.75, 0.9, 0.95)
df_quantiles_pos_evi <- data.frame(
  q = quantile_gpd_positive_evi,
  qgeqv0 = qgpd(quantile_gpd_positive_evi, loc = 0, scale = 1, shape = 1),
  qgeqv1 = qgpd(quantile_gpd_positive_evi, loc = 0, scale = 1, shape = 2),
  qgeqv2 = qgpd(quantile_gpd_positive_evi, loc = 0, scale = 1, shape = 3)
)

colnames(df_quantiles_neg_evi) <-
  c(
    "quantiles",
    "$gamma=-3$",
    "$gamma=-2$",
    "$gamma=-1$"
  )

colnames(df_quantiles_pos_evi) <-
  c(
    "quantiles",
    "$gamma=1$",
    "$gamma=2$",
    "$gamma=3$"
  )

print(
  xtable(df_quantiles_neg_evi, type = "latex", digits = 4),
  file = "./tables/table_gpd_negative_evi_quantiles.tex"
)

print(
  xtable(df_quantiles_pos_evi, type = "latex", digits = 4),
  file = "./tables/table_gpd_positive_evi_quantiles.tex"
)

df_neg_evi <- gather(
  df_support_neg_evi,
  "gpd0",
  "gpd1",
  "gpd2",
  key = "gpd", value = "y"
)
plot_gpd_negative_evi <- ggplot(
  df_neg_evi,
  aes(x, y, colour = gpd)
) +
  geom_line() +
  labs(
    x = "support",
    y = unname(TeX("$ G_{a, b, \\gamma}(x)"))
  ) +
  scale_colour_discrete(
    name = "EVI",
    breaks = c("gpd0", "gpd1", "gpd2"),
    labels = c(
      expression(paste(gamma, " = -3")),
      expression(paste(gamma, " = -2")),
      expression(paste(gamma, " = -1"))
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
  )

df_pos_evi <- gather(
  df_support_pos_evi,
  "gpd0",
  "gpd1",
  "gpd2",
  key = "gpd", value = "y"
)
plot_gpd_positive_evi <- ggplot(
  df_pos_evi,
  aes(x, y, colour = gpd)
) +
  geom_line() +
  labs(x = "log(support)") +
  scale_colour_discrete(
    name = "EVI",
    breaks = c("gpd0", "gpd1", "gpd2"),
    labels = c(
      expression(paste(gamma, " = 1")),
      expression(paste(gamma, " = 2")),
      expression(paste(gamma, " = 3"))
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
    # panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey"),
    # panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_line(color = "black"),
    legend.text.align = 0
  ) +
  scale_x_continuous(trans = "log10")

gpd_arranged <- ggarrange(
  plot_gpd_negative_evi,
  plot_gpd_positive_evi
)

gpd_arranged <- annotate_figure(
  gpd_arranged,
  top = "Generalized Pareto Distribution"
)

ggsave(
  "./figures/gpd_arranged.png",
  plot = gpd_arranged,
  width = 7,
  height = 4.3
)
