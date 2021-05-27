# TODO: man kann hier prinzipiell auf der x achse noch vertikal einzeichnen, wo
# die .95 bzw. 1 quantile liegen fuer den fall dass die endlich sind
#source("code/package_check.R")
support <- seq(-7, 1, length.out = 1e6)
df_support <- data.frame(
  x = support,
  rweibull0 = prweibull(support, scale = 1, shape = 0.5),
  rweibull1 = prweibull(support, scale = 1, shape = 1),
  rweibull2 = prweibull(support, scale = 1, shape = 3)
)

quantile_rweibull <- c(0.25, 0.5, 0.75, 0.9, 0.95)
df_quantiles <- data.frame(
  q = quantile_rweibull,
  qgeqv0 = qrweibull(quantile_rweibull, scale = 1, shape = 0.5),
  qgeqv1 = qrweibull(quantile_rweibull, scale = 1, shape = 1),
  qgeqv2 = qrweibull(quantile_rweibull, scale = 1, shape = 3)
)
colnames(df_quantiles) <- c("quantiles", "$gamma=0.5$", "$gamma=1$", "$gamma=3$")

print(
  xtable(df_quantiles, type = "latex"),
  file = "./tables/table_rweibull_quantiles.tex"
)

df <- gather(
  df_support,
  "rweibull0",
  "rweibull1",
  "rweibull2",
  key = "rweibull", value = "y"
)
plot_rweibull <- ggplot(
  df,
  aes(x, y, colour = rweibull)
) +
  geom_line() +
  labs(
    x = "x",
    y = unname(TeX("$ \\Psi_{\\alpha}"))
  ) +
  scale_colour_discrete(
    #name = "Shape",
    breaks = c("rweibull0", "rweibull1", "rweibull2"),
    labels = c(
      unname(TeX("$ \\alpha = 0.5")),
      unname(TeX("$ \\alpha = 1")),
      unname(TeX("$ \\alpha = 3"))
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
    #panel.border = element_blank(),
    panel.grid.major = element_line(color = "grey"),
    # panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    legend.text.align = 0
  )

#plot_rweibull <- annotate_figure(
#  plot_rweibull,
#  top = "Reverse Weibull Distribution"
#)

ggsave(
  "./figures/rweibull_df_plot.png",
  plot = plot_rweibull,
  width = 7,
  height = 4.3
)
