#source("code/package_check.R")
support <- seq(0, 5, length.out = 1e6)
df_support <- data.frame(
  x = support,
  frechet0 = pfrechet(support, 0, 1, 1),
  frechet1 = pfrechet(support, 0, 1, 2),
  frechet2 = pfrechet(support, 0, 1, 3)
)

df <- gather(
  df_support,
  "frechet0",
  "frechet1",
  "frechet2",
  key = "frechet", value = "y"
)
plot_frechet <- ggplot(
  df,
  aes(x, y, colour = frechet)
) +
  geom_line() +
  labs(
    x = "x",
    y = unname(TeX("$ \\Phi_{\\alpha}"))
  ) +
  scale_colour_discrete(
    #name = "Shape",
    breaks = c("frechet0", "frechet1", "frechet2"),
    labels = c(
      expression(paste(" ", alpha, " = 1")),
      expression(paste(" ", alpha, " = 2")),
      expression(paste(" ", alpha, " = 3"))
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

#plot_frechet <- annotate_figure(
#  plot_frechet,
#  top = "Frechét Distribution"
#)

ggsave(
  "./figures/frechet_df_plot.png",
  plot = plot_frechet,
  width = 7,
  height = 4.3
)
