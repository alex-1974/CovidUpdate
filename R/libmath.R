library(tidyverse)
library(zoo)
library(rlang)
library(furrr)

.notification_rate <- function(day, cases, pop, per, fill) {
  incidence = (zoo::rollsum(cases, day, align = "right", fill = fill) / pop) * per
}
incidence <- function(days, per, fill) {
  new_function(
    exprs(cases =, pop = ), 
    expr({
      (zoo::rollsum(cases, !!days, align = "right", fill = !!fill) / pop) * !!per
    }), 
    caller_env()
  )
}
incidence.7days <- incidence(7, 100000, 0)
incidence.7days
notification_rate_7d <- function(cases, pop, fill = 0) {
  return(.notification_rate(day = 7, cases = cases, pop = pop, fill = fill, per = 100000))
}

#' Berechnung der effektiven Reproduktionszahl (Nettoreproduktionszahl, Reff)
#' 
#' Der R-Wert ergibt sich durch Addition der Infektionszahlen einer Generationszeit 
#' geteilt durch die Infektionszahlen der vorherigen Generationszeit.
#' Damit aber ein Meldeverzug die Rechnung nicht verzerrt, werden die zurückliegenden drei Tage nicht mit einbezogen.
#' @param values
#' @param intervall Generationszeit
#' @param drop.last (default TRUE)
#' @param drop.n (default intervall)
#' @export
reff.rki <- function(values, intervall,drop.last = TRUE, drop.n = intervall) {
  #sma = values
  #sma = zoo::rollmean(x = values, k = days_average, align = "right", fill = NA)
  result = zoo::rollsum(x = values, k = intervall, align = "right", fill = NA) / zoo::rollsum(x = lag(values, intervall), k = intervall, align = "right", fill = NA)
  if(drop.last == TRUE)
    result[(length(result)-drop.n+1):length(result)] <- NA
  return(result)
}

reff.rki.nativ <- function(intervall, smooth = TRUE, drop.last = TRUE, drop.n = intervall) {
  body <- c("result <- values")
  if(smooth == TRUE) {
    body <- c(body, "result <- zoo::rollmean(x = result, k = !!intervall, align = 'right', fill = NA)")
  }
  body <- c(body, "result <- zoo::rollsum(x = result, k = !!intervall, align = 'right', fill = NA) / zoo::rollsum(x = lag(result, !!intervall), k = !!intervall, align = 'right', fill = NA)")
  if(drop.last == TRUE) {
    body <- c(body, "result[(length(result)-!!drop.n+1):length(result)] <- NA
                  return(result)")
  }
  return(
  new_function(
    args = exprs(values =), 
    body = parse_exprs(body),
    caller_env()
  )
  )
}
c(1:10) |> zoo::rollmean(3) |>
  zoo::rollsum(3)
reff.rki.4days = reff.rki.nativ(4)
reff.rki.4days
reff.rki.4days(c(1:20))
reff.rki.7days = reff.rki.nativ(7)
parse_expr("result <- zoo::rollsum(x = values, k = !!intervall, align = 'right', fill = NA) / zoo::rollsum(x = lag(values, !!intervall), k = !!intervall, align = 'right', fill = NA)")
########################################################
# Estimate Reff and CI
#
# see: https://www.datacamp.com/tutorial/replicating-in-r-covid19
########################################################

# r_t_range is a vector of possible values for R_t
#R_T_MAX = 12
#r_t_range = seq(0, R_T_MAX, length = R_T_MAX*100 + 1)

# Gamma is 1/serial interval
# https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
#gamma = 1/4

.smooth_new_cases<- function(df, cases) {
  cases <- enquo(cases)
  smooth <- df |>
    mutate(
      sma = round(zoo::rollmean(!!cases, 7, align = "right", fill = 0))
    ) 
}
.compute_likelihood <- function(df, gamma, r_t_range) {
  likelihood <- df |>
    filter(sma > 0 & !is.na(sma)) |>
    mutate(
      r_t = list(r_t_range),
      lambda = map(lag(sma, 1), ~ .x * exp(gamma * (r_t_range - 1))),
      likelihood_r_t = map2(sma, lambda, dpois, log = TRUE)
    ) |>
    slice(-1) |>
    select(-lambda) |>
    unnest(c(likelihood_r_t, r_t))
}

.compute_posterior <- function(likelihood, date) {
  date <- enquo(date)
  likelihood |>
    arrange((!!date)) |>
    group_by(r_t) |>
    mutate(
      posterior = exp(zoo::rollapplyr(likelihood_r_t, 7, sum, partial = TRUE))
    ) |>
    group_by(!!date) |>
    mutate(
      posterior = posterior / sum(posterior, na.rm = TRUE),
      posterior = if_else(is.nan(posterior), 0, posterior)
    ) |>
    ungroup() |>
    select(-likelihood_r_t)
}

.estimate_rt <- function(posteriors, date, groupby, r_t_range) {
  date <- enquo(date)
  groupby <- enquo(groupby)
  posteriors |>
    group_by(!!groupby,!!date) |>
    summarize(
      r_t_simulated = list(sample(r_t_range, 10000, replace = TRUE, prob = posterior)),
      r_t_most_likely = r_t_range[which.max(posterior)]
    ) |>
    mutate(
      r_t_lo = map_dbl(r_t_simulated, ~ HDInterval::hdi(.x)[1]),
      r_t_hi = map_dbl(r_t_simulated, ~ HDInterval::hdi(.x)[2])
    ) |>
    select(-r_t_simulated)
}

#' 
#' Der erste Schritt besteht darin, die "Ankunft" von Infektionen zu modellieren. 
#' Eine beliebte Wahl für die Verteilung der Neuzugänge ist die Poisson-Verteilung. 
#' Wenn also \lambda die durchschnittliche Infektionsrate pro Tag darstellt, 
#' dann ist die Wahrscheinlichkeit, dass an einem Tag k neue Fälle auftreten, gegeben durch
#' \eqn{ P(k|\lambda) = \frac{\lambda^k e^{-\lambda}}{k!} }
#' Die Verteilung von \lambda über k wird Likelihood-Funktion genannt 
#' und hat denselben mathematischen Ausdruck wie die Wahrscheinlichkeits-Massenfunktion, 
#' die wir zuvor verwendet haben. Wir können die Likelihood-Funktion veranschaulichen, 
#' indem wir die Anzahl der beobachteten neuen Fälle (k) festlegen und die Likelihood-Funktion über einen Bereich von Werten für \lambda berechnen.
#' \eqn{ L(\lambda| k) = \frac{\lambda^k e^{-\lambda}}{k!} }
#' Nach dieser Arbeit von Bettencourt & Ribeiro ist die Beziehung zwischen R und \lambda recht einfach
#' \eqn{ \lambda = k_{t-1}e^{\gamma(R_t-1)} }
#' Man beachte, dass \gamma hier der Kehrwert des seriellen Intervalls ist (etwa 4 Tage für COVID19), 
#' und $k_{t-1}$ ist die Anzahl der neuen Fälle, die im Zeitintervall $t-1$ beobachtet wurden
#' @export

estimate_r <- function(data, cases, date, groupby, intervall) {
  cases <- enquo(cases)
  date <- enquo(date)
  groupby <- enquo(groupby)
  # r_t_range is a vector of possible values for R_t
  r_t_max = 12
  r_t_range = seq(0, r_t_max, length = r_t_max*100 + 1)
  
  # Gamma is 1/serial interval
  # https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
  gamma = 1/intervall
  
  data |> 
    select(!!date, !!groupby, !!cases) |>
    group_by(!!groupby) |>
    group_split() |>
    future_map_dfr(~ {
      .x |>
        .smooth_new_cases(!!cases) |>
        .compute_likelihood(gamma, r_t_range) |>
        .compute_posterior(!!date) |>
        .estimate_rt(!!date,!!groupby, r_t_range)
    }, seed = TRUE) |>
    ungroup()
}

.is.date <- function(df, x) {
  x <- enquo(x)
  #is.Date(first(df |> pull(!!x))) |
  if(all(is.POSIXct(df |> pull(!!x)))  |  all(is.Date(df |> pull(!!x))))
    return(df |> mutate(!!x := as_datetime(!!x)) )
  else
    return(df)
}

.interpolate.x <- function(df, x, y, m) {
  x <- enquo(x)
  y <- enquo(y)
  df <- .is.date(df, !!x)
  df |>
    filter(!is.na(!!y)) |>
    mutate(
      #!!x := if_else(is.Date(first(df |> pull(!!x))))
      change = if_else(
        (!!y == m  | lag(!!y) == m) | 
        (!!y > m & lag(!!y,1, default = first(!!y)) > m) &
        (!!y < m & lag(!!y,1, default = first(!!y)) < m), FALSE, TRUE),
      section = as.factor(
        if_else(!!y > m, "above", 
                if_else(!!y < m, "below", "equal")
      )),
      mx = if_else(
        section == "equal" | lag(section) == "equal" | section %in% lag(section),
        NA_POSIXct_,
        !!x - (!!x - lag(!!x)) / (!!y - lag(!!y) * (!!y - m))
      ),
      take = if_else(
        change == TRUE | lag(change, default = first(change)) == TRUE | lead(change, default = last(change)) == TRUE, TRUE, FALSE
      )
    ) |>
   # filter(take == TRUE) |>
    mutate(
      mid.x = if_else(change == TRUE, !!x - (!!x - lag(!!x)) / (!!y - lag(!!y) * (!!y - m)), !!x)
    ) #|> 
   # filter(change == TRUE) |>
   # select(-take,-change, -!!x) |>
   # rename(!!x := mid.x) |>
   # mutate(!!y := !!m)
}

split.x <- function(df, x, y, split = 0) {
  x <- enquo(x)
  y <- enquo(y)
  rbind(
    .is.date(df, !!x)
    #.interpolate.x(df, !!x, !!y, split)
  ) |>
  arrange(!!x) |>
  filter(!is.na(!!x), !is.na(!!y))
}

test <- tibble(
  x = c(1:14),
  y = c(0,2,-2,0,1,-1,8,NA,1,7,7,3,-1,0)
) |>
  mutate(
    date = lubridate::as_date("2021-02-01") + x
  )

#.interpolate.x(test, date, y, 0)

split <- 0
p <- split.x(df = test, x = date, y = y, split = split)
p
p2 <- p[-1,] |>
  ggplot(aes(x = date, y = y)) +
  geom_ribbon(data = . %>% filter(y >= split),
              aes(ymin = split, ymax = y),
              fill = "blue") +
  geom_ribbon(data = . %>% filter(y <= split),
              aes(ymin = split, ymax = y),
              fill = "red")
p2
