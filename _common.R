set.seed(25)
options(digits = 3)

# packages ---------------------------------------------------------------------

suppressMessages(library(knitr))
suppressMessages(library(tidyverse))
suppressMessages(library(kableExtra))
suppressMessages(library(patchwork))
suppressMessages(library(scales))
suppressMessages(library(DiagrammeR))
suppressMessages(library(readxl))

# knitr chunk options ----------------------------------------------------------

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  #cache = TRUE,
  echo = FALSE, # hide code unless otherwise noted in chunk options
  out.width = "70%",
  fig.align = 'center',
  fig.width = 6,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold"
)

# knit options -----------------------------------------------------------------

options(knitr.kable.NA = "")

# kableExtra options -----------------------------------------------------------

options(kableExtra.html.bsTable = TRUE)
