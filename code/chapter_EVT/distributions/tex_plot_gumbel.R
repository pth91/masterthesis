#source("code/package_check.R")
support <- seq(-6, 10, length.out = 1e6)
df_support <- data.frame(
  x = support,
  gumbel0 = pgumbel(support, 0, 1),
  gumbel1 = pgumbel(support, 1, 2),
  gumbel2 = pgumbel(support, 1.5, 3)
)

df <- gather(
  df_support,
  "gumbel0",
  "gumbel1",
  "gumbel2",
  key = "gumbel", value = "y"
)
plot_gumbel <- ggplot(
  df,
  aes(x, y, colour = gumbel)
) +
  geom_line() +
  labs(
    x = "x",
    y = unname(TeX("$ \\Lambda"))
  ) +
  scale_colour_discrete(
    name = "Shape",
    breaks = c("gumbel0", "gumbel1", "gumbel2"),
    labels = c(
      unname(TeX("$ \\mu = 0,\\, \\sigma = 1")),
      unname(TeX("$ \\mu = 1,\\, \\sigma = 2")),
      unname(TeX("$ \\mu = 1.5,\\, \\sigma = 3"))
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
    panel.grid.major = element_line(color = "grey"),
    # panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    legend.text.align = 0
  )

#plot_gumbel <- annotate_figure(
#  plot_gumbel,
#  top = "Gumbel Distribution"
#)

ggsave(
  "./figures/gumbel_df_plot.png",
  plot = plot_gumbel,
  width = 7,
  height = 4.3
)
