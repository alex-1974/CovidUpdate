library(tidyverse)
library(lubridate)
library(kableExtra)

table.ems.ages.bmsgpk.timeline <- ems.ages.bmsgpk.timeline |>
  filter(Datum == max(Datum)) |>
  select(Bundesland, ages = ages.inc, ems = ems.inc) |>
  knitr::kable(
    caption = "7-Tages-Inzidenzen der Bundesländer",
    booktabs = TRUE,
    digits=0) |>
  kable_styling(full_width = F) |>
  add_header_above(c(" ", "Datenquelle" = 2))
