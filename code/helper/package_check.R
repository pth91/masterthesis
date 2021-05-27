packages <- c(
  "evd",
  "ggplot2",
  "latex2exp",
  "xtable",
  "tidyverse",
  "dplyr",
  "janitor",
  "ggpubr",
  "survival",
  "survminer",
  "writexl",
  "mice",
  "muhaz"
)

## Now load or install&load all
package_check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
