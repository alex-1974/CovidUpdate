library(tidyverse)
library(lubridate)
library(ggpubr)
library(vistime)

source("R/theme_covid.R")
source("R/config.R")

if(!exists("period.from")) {
  period.from <- rollback(add_with_rollback(now(),months(-1)), roll_to_first = TRUE)
}
now2 <- add_with_rollback(now(),days(+1))

ages.timeline.österreich <- ages.timeline |> filter(Bundesland == "Österreich") |> rename(Datum = Testdatum)
testing.österreich <- testing |> filter(Bundesland == "Österreich")
hospitalization.österreich <- hospitalization |> filter(Bundesland == "Österreich")

###################
# Epicurve Graph
###################

.epicurve.start <- period.from

graph.epidemiologic_curve <- ages.timeline.österreich |>
  select(Datum, Faelle.neu) |>
  ggplot(aes(y = Faelle.neu, x = Datum)) +
  geom_bar(stat="identity") +
  #xlim(as.Date(c(.epicurve.start, now()))) +
  labs(
    title="Epidemische Kurve Österreich",
    subtitle = "Absolute Anzahl Neuerkrankter",
    caption = paste0("Quelle: Daten des EMS (Stand ", as_date(max(ages.timeline.österreich$Datum)), ")"),
    x="", y = "Neuerkrankte") +
  theme_covid()
graph.epidemiologic_curve
ggsave("graph_epidemiologic_curve.png", path = path.img, scale = 2, bg = "white")

graph.epidemiologic_curve.last <- graph.epidemiologic_curve +
  labs(subtitle = paste0("Absolute Anzahl Neuerkrankter seit ", as_date(.epicurve.start))) +
  coord_cartesian(
    xlim = as_date(c(.epicurve.start, now2)),
    ylim = (c(0, max(ages.timeline.österreich[ages.timeline.österreich$Datum >= .epicurve.start, ]$Faelle.neu, na.rm = TRUE)))
  )
graph.epidemiologic_curve.last 
ggsave("graph_epidemiologic_curve.last.png", path = path.img, scale = 2, bg = "white")


#########################
# Inzidenzen
#########################

.inz7d.start <- period.from

graph.inz7d.österreich.gesamt <- ages.timeline.österreich |>
  filter(Bundesland == "Österreich") |>
  ggplot(aes(y = Faelle.inz7d, x = Datum)) +
  geom_line(colour = mycolors["blue"]) +
  geom_area(fill=mycolors["blue"], alpha=.2)+
  labs(
    title= "Verlauf der Pandemie in Österreich",
    subtitle = "7-Tages-Inzidenz pro 100.000 Einwohner",
    caption = "Quelle: Daten des AGES",
    x="", y = "Inzidenz") +
  theme_covid()
graph.inz7d.österreich.gesamt
ggsave("graph_inz7d_östereich_gesamt.png", path = path.img, scale = 2, bg = "white")

graph.inz7d.österreich.last <- graph.inz7d.österreich.gesamt +
  labs(title = "7-Tages-Inzidenz in Österreich",
       subtitle = paste0("7-Tages-Inzidenz pro 100.000 Einwohner seit ", as_date(.inz7d.start)),
       caption = paste0("Quelle: Daten der AGES (Stand ", as_date(max(ages.timeline.österreich$Datum)), ")")
       ) +
  coord_cartesian(
    xlim = as_date(c(.inz7d.start, now2 )),
    ylim = c(0, max(ages.timeline.österreich[ages.timeline.österreich$Datum >= .inz7d.start, ]$Faelle.inz7d, na.rm = TRUE) )
  )
ggsave("graph_inz7d_östereich_last.png", path = path.img, scale = 2, bg = "white")
graph.inz7d.österreich.last

graph.inz7d.österreich.log.last <- graph.inz7d.österreich.gesamt +
  scale_y_log10(limits = c(1, max(ages.timeline.österreich[ages.timeline.österreich$Datum >= .inz7d.start, ]$Faelle.inz7d, na.rm = TRUE) ),
                expand =c(0,0)) +
  coord_cartesian(xlim = as_date(c(.inz7d.start, now2 )) ) +
  labs(
    title= "7-Tages-Inzidenz in Österreich",
    subtitle = paste0("7-Tages-Inzidenz pro 100.000 Einwohner seit ", as_date(.inz7d.start), ", logarithmische Skala"),
    caption = paste0("Quelle: Daten der AGES (Stand ", as_date(max(ages.timeline.österreich$Datum)), ")"),
    x="", y = "Inzidenz")
ggsave("graph_inz7d_östereich_log_last.png", path = path.img, scale = 2, bg = "white")
graph.inz7d.österreich.log.last

graph.inz7d.bundesländer.gesamt <- ages.timeline |>
  ggplot(aes(y = Faelle.inz7d, x = Testdatum, group = Bundesland, colour = Bundesland)) +
  geom_line() +
  labs(
    title= "7-Tages-Inzidenz der Bundesländer",
    subtitle = "Inzidenz pro 100.000 Einwohner",
    caption = "Quelle: Daten des AGES Dashboards",
    x="", y = "Inzidenz") +
  theme_covid()
graph.inz7d.bundesländer.gesamt
ggsave("graph_inz7d_bundesländer_gesamt.png", path = path.img, scale = 2, bg = "white")

graph.inz7d.bundesländer.last <- graph.inz7d.bundesländer.gesamt +
  coord_cartesian(
    xlim = as_date(c(.inz7d.start, now2 )),
    ylim = c(0, max(ages.timeline[ages.timeline$Testdatum >= .inz7d.start, ]$Faelle.inz7d, na.rm = TRUE) )
  ) +
  labs(title = "7-Tages-Inzidenz der Bundesländer",
       subtitle = paste0("Inzidenz pro 100.000 Einwohner seit ", as_date(.inz7d.start)),
       caption = paste0("Quelle: Daten der AGES (Stand ", as_date(max(ages.timeline$Testdatum)), ")")
  ) 

graph.inz7d.bundesländer.last
ggsave("graph_inz7d_bundesländer_last.png", path = path.img, scale = 2, bg = "white")

graph.inz7d.bundesländer.log.last <- graph.inz7d.bundesländer.gesamt +
  scale_y_log10(limits = c(1, max(ages.timeline[ages.timeline$Testdatum >= .inz7d.start, ]$Faelle.inz7d, na.rm = TRUE) ),
                expand =c(0,0)) +
  coord_cartesian(xlim = as_date(c(.inz7d.start, now2 )) ) +
  labs(
    title= "7-Tages-Inzidenz der Bundesländer",
    subtitle = paste0("7-Tages-Inzidenz pro 100.000 Einwohner seit ", as_date(.inz7d.start), ", logarithmische Skala"),
    caption = paste0("Quelle: Daten der AGES (Stand ", as_date(max(ages.timeline$Testdatum)), ")"),
    x="", y = "Inzidenz")
ggsave("graph_inz7d_bundesländer_log_last.png", path = path.img, scale = 2, bg = "white")
graph.inz7d.bundesländer.log.last

graph.inz7d.rate <- ages.timeline.österreich |>
  filter(
    Datum >= as_date("2020-05-01")
  ) |>
  mutate(
    ratio = Faelle.inz7d / lag(Faelle.inz7d, 7, order_by = Datum),
  ) |>
  ggplot(aes(y = ratio, x = Datum)) +
  geom_line() +
  geom_hline(yintercept = 1) +
  labs(
    title= "Verhältnis der 7-Tages-Inzidenz zur Vorwoche",
    subtitle = "",
    caption = "Quelle: Daten des AGES Dashboards",
    x="", y = "Inzidenzrate") +
  theme_covid()
graph.inz7d.rate
ggsave("graph_inz7d_rate.png", path = path.img, scale = 2, bg = "white")

graph.inz7d.rate.last <- graph.inz7d.rate +
  coord_cartesian(
    xlim = as_date(c(.inz7d.start, now2 )),
   # ylim = c(0, max(ages.inz7d.österreich.gesamt[ages.inz7d.österreich.gesamt$Datum >= .inz7d.start, ]$ratio, na.rm = TRUE) )
  ) +
  labs(title = "Verhältnis der 7-Tages-Inzidenz zur Vorwoche",
       subtitle = paste0("Inzidenzrate seit ", as_date(.inz7d.start)),
       caption = paste0("Quelle: Daten der AGES (Stand ", as_date(max(ages.timeline.österreich$Datum)), ")")
  ) 
graph.inz7d.rate.last
ggsave("graph_inz7d_rate_last.png", path = path.img, scale = 2, bg = "white")

###########################
# Hospitalisierung
###########################

graph.hospitalisierung.last <- hospitalization.österreich |>
  ungroup() |>
  filter(
    Datum >= .inz7d.start
  ) |>
  select(Datum,Normalbetten.belegt.cov19, ICU.belegt.cov19) |>
  gather(key="Station", value="Belegt", 2:3) |>
  ggplot(aes(x = Datum, y = Belegt, group = Station, colour = Station)) +
  geom_line() +
  theme_covid() +
  theme(legend.position="bottom") +
  coord_cartesian(
    xlim = as_date(c(.inz7d.start, now2)) 
  )+
  scale_colour_manual(values = c(mycolors[["vermillion"]], mycolors[["skyBlue"]]),
                      labels = c("Intensivbetten", "Normalbetten"))
  
graph.hospitalisierung.last
ggsave("graph_hospitalisierung_last.png", path = img.path, scale = 2, bg = "white")

#############################
# Tests
#############################

.test.start <- period.from

graph.bmsgpk.test.österreich <- testing.österreich |>
  filter(
    Datum >= .test.start,
  ) |>
  #select(Datum, Tests.gesamt.neu, Tests.gesamt.sma7) |>
  ggplot() +
  geom_line(aes(x = Datum, y = Tests.gesamt.neu), alpha = 0.2) +
  geom_line(aes(x = Datum, y = Tests.gesamt.sma7)) +
  labs(
    title="Testrate in Österreich",
    subtitle = paste0("Absolute Zahlen und 7-Tages-Durchschnitt seit ", as_date(.test.start)),
    caption = paste0("Quelle: Daten des BMSGPK (Stand ", as_date(max(testing.österreich$Datum)), ")"),
    x="", y = "Testrate") +
  theme_covid()
graph.bmsgpk.test.österreich

graph.bmsgpk.test.österreich.inz7d <- testing.österreich |>
  filter(
    Datum >= .test.start,
  ) |>
  #select(Datum, Tests.gesamt.neu, Testrate.gesamt.inz7d) |>
  ggplot(aes(x = Datum, y = Testrate.gesamt.inz7d)) +
  geom_line(colour = mycolors["orange"]) +
  geom_area(fill = mycolors["orange"], alpha = .2)+
  coord_cartesian(
    xlim = as_date(c(.test.start, now2))
  ) +
  labs(
    title="Testrate in Österreich",
    subtitle = paste0("Testinzidenz seit ", as_date(.test.start)),
    caption = paste0("Quelle: Daten des BMSGPK (Stand ", as_date(max(testing.österreich$Datum)), ")"),
    x="", y = "Testinzidenz") +
  theme_covid()
graph.bmsgpk.test.österreich.inz7d

graph.bmsgpk.test.bundesländer.inz7d <- testing |>
  filter(
    Datum >= .test.start,
    Bundesland != "Österreich"
  ) |>
  select(Datum, Bundesland, Testrate.gesamt.inz7d) |>
  group_by(Bundesland) |>
  ggplot() +
  geom_line(aes(x = Datum, y = Testrate.gesamt.inz7d, group = Bundesland, colour = Bundesland)) +
  labs(
    title="Testrate der Bundesländer",
    subtitle = paste0("Testinzidenz seit ", as_date(.test.start)),
    caption = paste0("Quelle: Daten des BMSGPK (Stand ", as_date(max(testing$Datum)), ")"),
    x="", y = "Testinzidenz") +
  theme_covid()
graph.bmsgpk.test.bundesländer.inz7d

graph.bmsgpk.test.positivrate.österreich <- testing.österreich |>
  select(Datum, Positivrate.PCR.sma7, Testrate.PCR.sma7) |>
  gather(key="type", value="value", 3:4) |>
  filter(Datum >= .test.start) |>
  ggplot() +
  geom_line(aes(x = Datum, y = value, group = type, color = type)) +
  labs(
    title="Testpositivrate in Österreich",
    subtitle = paste0("Positivrate als 7-Tages-Durchschnitt seit ", as_date(.test.start)),
    caption = paste0("Quelle: Daten des BMSGPK (Stand ", as_date(max(testing.österreich$Datum)), ")"),
    x="", y = "Rate") +
  scale_colour_manual(values = c(mycolors[["vermillion"]], mycolors[["skyBlue"]]),
                      labels = c("Positivrate", "Anzahl Tests")) +
  theme_covid()
graph.bmsgpk.test.positivrate.österreich
ggsave("graph_bmsgpk_test_positivrate_österreich.png", path = img.path, scale = 2, bg = "white")

#############################
# Reff
#############################

# Ages

graph.ages.reff.österreich <- ages.timeline.reff |> 
  filter(
    Bundesland == "Österreich",
    Testdatum >= period.from
  ) |>
  ungroup() |>
  select(Testdatum, r4, r4.now, r7, r7.now) |>
  gather(
    key = "r.method",
    value = "r.value",
    -Testdatum
  ) |>
  ggplot(aes(x = Testdatum, y = r.value, group = r.method, color = r.method)) +
  geom_line() +
  geom_hline(yintercept = 1) +
  theme_covid()

graph.ages.reff.österreich.nativ <- ages.reff.österreich |>
  filter(Datum >= period.from) |>
  ggplot(aes(x = Datum, y = R_eff)) +
  geom_line() +
  geom_ribbon(aes(ymin = R_eff_lwr, ymax = R_eff_upr), alpha = 0.1) +
  geom_hline(yintercept = 1) +
  theme_covid()
#graph.ages.reff.österreich.nativ

graph.ages.reff.bundesländer.nativ <- ages.reff.bundesländer |>
  filter(
    Datum >= period.from
  ) |>
  group_by(Bundesland) |>
  ggplot(aes(x = Datum, y = R_eff, group = Bundesland, colour = Bundesland)) +
  geom_line() +
  #geom_ribbon(aes(ymin = R_eff_lwr, ymax = R_eff_upr), alpha = 0.1) +
  geom_hline(yintercept = 1) +
  theme_covid()
#graph.ages.reff.bundesländer.nativ

graph.reff.österreich.gesamt <- reff.österreich |>
  filter(Bundesland == "Österreich") |>
  ggplot(aes(x = Datum, y = r_t_most_likely)) +
  geom_line() +
  geom_ribbon(aes(ymin = r_t_lo, ymax = r_t_hi), alpha = 0.1) +
  geom_hline(yintercept = 1) +
  theme_covid()
graph.reff.österreich.gesamt
ggsave("graph_reff_österreich_gesamt.png", path = img.path, scale = 2, bg = "white")

graph.reff.österreich.last <- graph.reff.österreich.gesamt +
  coord_cartesian(
    xlim = as_date(c(period.from, now2 )),
    ylim = c(0, max(reff.österreich[reff.österreich$Datum >= period.from, ]$r_t_hi, na.rm = TRUE) )
  ) 
labs(title = "7-Tages-Inzidenz der Bundesländer",
     subtitle = paste0("Inzidenz pro 100.000 Einwohner seit ", as_date(.inz7d.start)),
     caption = paste0("Quelle: Daten der AGES (Stand ", as_date(max(ages.timeline$Testdatum)), ")")
) 
graph.reff.österreich.last

# Estimate EMS

#graph.ems.estimate_r.österreich.gesamt <- ems.estimate_r.österreich.gesamt |>
#  filter(Bundesland == "Österreich") |>
#  ggplot(aes(x = Meldedatum, y = r_t_most_likely)) +
#  geom_line() +
#  geom_ribbon(aes(ymin = r_t_lo, ymax = r_t_hi), alpha = 0.1) +
#  geom_hline(yintercept = 1) +
#  labs(
#    title="Nettoreproduktionsrate (Reff) ",
#    subtitle = "Österreich gesamt",
#    caption = paste0("Quelle: Daten des EMS (Stand ", as_date(max(ems.timeline$Meldedatum)), ")"),
#    x="", y = "Reff") +
#  theme_covid()
#saveRDS(graph.ems.estimate_r.österreich.gesamt, file = paste0(rds.path, "graph_ems_reff_österreich_gesamt.rds"))
#ggsave("graph_ems_reff_österreich_gesamt.png", path = img.path, scale = 2, bg = "white")

#graph.ems.estimate_r.österreich.last <- graph.ems.estimate_r.österreich.gesamt +
#  xlim(as.Date(c(period.from, now2))) +
 # ylim(min(ems.estimate_r.österreich.gesamt[ems.estimate_r.österreich.gesamt$Meldedatum >= period.from, ]$r_t_most_likely, na.rm = TRUE),
#       max(ems.estimate_r.österreich.gesamt[ems.estimate_r.österreich.gesamt$Meldedatum >= period.from, ]$r_t_most_likely, na.rm = TRUE)) +
#  labs(subtitle = paste0("Österreich seit ", as_date(period.from))) 
#saveRDS(graph.ems.estimate_r.österreich.last, file = paste0(rds.path, "graph_ems_reff_österreich_last.rds"))
#ggsave("graph_ems_reff_österreich_last.png", path = img.path, scale = 2, bg = "white")
#graph.ems.estimate_r.österreich.last

# Estimate AGES

graph.ages.estimate_r.bundesländer.last <- reff.österreich |>
  filter(Bundesland != "Österreich", Datum >= period.from) |>
  ggplot(aes(x = Datum, y = r_t_most_likely, group = Bundesland, color = Bundesland)) +
  geom_line() +
  geom_hline(yintercept = 1) +
  theme_covid()
ggsave("graph_reff_bundesländer_last.png", path = img.path, scale = 2, bg = "white")
graph.ages.estimate_r.bundesländer.last

graph.ages.estimate_r.österreich.last <- ages.estimate_r.österreich.gesamt |>
  filter(
    Testdatum >= period.from,
    Bundesland == "Österreich"
  ) |>
  ggplot(aes(x = Testdatum, y = r_t_most_likely)) +
  geom_line() +
  geom_ribbon(aes(ymin = r_t_lo, ymax = r_t_hi), alpha = 0.1) +
  xlim(as.Date(c(period.from, now2))) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "Reff") +
  theme_covid() 
ggsave("graph_reff_österreich_last.png", path = img.path, scale = 2, bg = "white")



graph.ages.estimate_r.österreich.altersgruppe.last <- ages.estimate_r.österreich.altersgruppe.gesamt |>
  filter(Testdatum >= period.from) |>
  ggplot(aes(x = Testdatum, y = r_t_most_likely, group = Altersgruppe, color = Altersgruppe)) +
  geom_line() +
  geom_hline(yintercept = 1) +
  labs(
    title="Nettoreproduktionszahlen der Altersgruppen in Österreich",
    subtitle = paste0("Verlauf seit ", as_date(period.from)),
    caption = paste0("Quelle: Daten der AGES (Stand ", as_date(max(ages.estimate_r.österreich.altersgruppe.gesamt$Testdatum)), ")"),
    x="", y = "Nettoreproduktionszahl") +
  theme_covid()
graph.ages.estimate_r.österreich.altersgruppe.last
ggsave("graph_reff_altersgruppe_last.png", path = img.path, scale = 2, bg = "white")

############################
# Heatmap Altersgruppe Reff
############################

heatmap.reff.altersgruppe <- reff.altersgruppe.österreich |>
  filter(
    Testdatum >= period.from,
  ) |>
  ggplot(aes(x = Testdatum, y = Altersgruppe, fill = r_t_most_likely)) +
  geom_tile() +
  xlim(as.Date(c(period.from, now2))) +
  labs(fill = "Reff", x = "", y = "Altersgruppen") +
  theme_covid() +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", direction = 1)
heatmap.reff.altersgruppe

##########################
## Test covid Wellen

from <- as_date("2019-12-15")
to <- as_date("2020-02-15")
heatmap.r.altersgruppe.welle <- ages.estimate_r.österreich.altersgruppe.gesamt |>
  filter(
    Testdatum >= from,
    Testdatum <= to,
    Altersgruppe != ">84"
  ) |>
  ggplot(aes(x = Testdatum, y = Altersgruppe, fill = r_t_most_likely)) +
  geom_tile() +
  xlim(as.Date(c(from, to))) +
  labs(fill = "Reff", x = "", y = "Altersgruppen") +
  theme_covid() +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", direction = 1)
heatmap.r.altersgruppe.welle

graph.ages.estimate_r.österreich.last.welle <- ages.estimate_r.österreich.gesamt |>
  filter(
    Testdatum >= from,
    Testdatum <= to,
    Bundesland == "Österreich"
  ) |>
  ggplot(aes(x = Testdatum, y = r_t_most_likely)) +
  geom_line() +
  geom_ribbon(aes(ymin = r_t_lo, ymax = r_t_hi), alpha = 0.1) +
  xlim(as.Date(c(from, to))) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "Reff") +
  theme_covid() 
graph.ages.estimate_r.österreich.last.welle

graph.inz7d.österreich.welle <- graph.inz7d.österreich.gesamt +
  coord_cartesian(
    xlim = as_date(c(from, to )),
    ylim = c(0, max(ages.inz7d.österreich.gesamt[ages.inz7d.österreich.gesamt$Datum >= from, ]$Faelle, na.rm = TRUE) )
  )
graph.inz7d.österreich.welle

heatmap.r.combined.welle <- ggpubr::ggarrange(graph.inz7d.österreich.welle+ labs(title=element_blank(), subtitle=element_blank(),caption=element_blank()),
                                              graph.ages.estimate_r.österreich.last.welle+ labs(title=element_blank(), subtitle=element_blank(),caption=element_blank()), 
                                              heatmap.r.altersgruppe.welle+ labs(title=element_blank(), subtitle=element_blank(),caption=element_blank()), 
                                              ncol = 1, nrow = 3, align = "hv", heights = c(3,2,5)) |>
  annotate_figure(top = text_grob("Reff in den Altersgruppen",  family = "libertinus serif", size = 16, y = 0.5))
#ggsave("heatmap_reff_combined_österreich_welle06_22.png", path = img.path, scale = 2, bg = "white")
heatmap.r.combined.welle
#########################

heatmap.inz7d.altersgruppe <- ages.altersgruppe |>
  filter(Bundesland == "Österreich",  Testdatum >= period.from) |>
  group_by(Testdatum, Altersgruppe) |>
  summarise(Faelle = sum(Faelle.neu), Einwohner = sum(Einwohner)) |>
  group_by(Altersgruppe) |>
  mutate(
    Faelle.inz7d = notification_rate_7d(cases = Faelle, pop = Einwohner, fill = 0)
  ) |>
  ggplot(aes(x = Testdatum, y = Altersgruppe, fill = Faelle.inz7d)) +
  geom_tile() +
  theme_covid() +
  xlim(as.Date(c(period.from, now2))) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", direction = 1)
heatmap.inz7d.altersgruppe

heatmap.r.combined <- ggpubr::ggarrange(graph.ages.estimate_r.österreich.last, heatmap.r.altersgruppe, 
                                        ncol = 1, nrow = 2, align = "hv", heights = c(1,3)) |>
  annotate_figure(top = text_grob("Reff in den Altersgruppen",  family = "libertinus serif", size = 16, y = 0.5))
ggsave("heatmap_reff_combined_österreich_last.png", path = img.path, scale = 2, bg = "white")

heatmap.inz7d.altersgruppe <- ages.altersgruppe |>
  filter(Bundesland == "Österreich",  Testdatum >= period.from) |>
  group_by(Testdatum, Altersgruppe) |>
  summarise(Faelle = sum(Faelle.neu), Einwohner = sum(Einwohner)) |>
  group_by(Altersgruppe) |>
  mutate(
    Faelle.inz7d = notification_rate_7d(cases = Faelle, pop = Einwohner, fill = 0)
  ) |>
  ggplot(aes(x = Testdatum, y = Altersgruppe, fill = Faelle.inz7d)) +
  geom_tile() +
  theme_covid() +
  xlim(as.Date(c(period.from, now2))) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", direction = 1)
heatmap.inz7d.altersgruppe


#############################
# Combined Graphs
#############################

graph.combined <- ggpubr::ggarrange(graph.epidemiologic_curve.last+ labs(title=element_blank(), subtitle=element_blank(),caption=element_blank()), 
                                    graph.inz7d.österreich.last+ labs(title=element_blank(), subtitle=element_blank(),caption=element_blank()), 
                                    graph.ages.estimate_r.österreich.last+ labs(title=element_blank(),subtitle=element_blank(),caption=element_blank()), 
                                    graph.bmsgpk.test.österreich.inz7d+ labs(title=element_blank(),subtitle=element_blank(),caption=element_blank()), 
                                    graph.hospitalisierung.last+ labs(title=element_blank(),subtitle=element_blank(),caption=element_blank()),
                                    ncol = 1, nrow = 5, align = "hv", heights=c(2.5,1.8,2,1.5,2.2)) |>
                  annotate_figure(top = text_grob("Pandemieverlauf in Österreich",  family = "libertinus serif", size = 16, y = 0.5))
ggsave("graph_combined_last.png", path = img.path, scale = 4, bg = "white")

heatmap.r.combined <- ggpubr::ggarrange(graph.ages.estimate_r.österreich.last, heatmap.r.altersgruppe, 
                                        ncol = 1, nrow = 2, align = "hv", heights = c(1,3)) |>
  annotate_figure(top = text_grob("Reff in den Altersgruppen",  family = "libertinus serif", size = 16, y = 0.5))
ggsave("heatmap_reff_combined_österreich_last.png", path = img.path, scale = 2, bg = "white")
heatmap.r.combined

############################
# Timeline
############################
.timeline.covid <- timeline.covid |>
  mutate(
    color = case_when(
      subgroup == "full" ~ mycolors["blue"],
      subgroup == "light" ~ mycolors["skyBlue"],
      TRUE ~ mycolors["blueishGreen"]
    )
  ) |>
  gg_vistime(show_labels = FALSE) +
  xlim(as.POSIXct(c(min(ages.inz7d.österreich.gesamt$Datum), now2)))+
  theme_covid()+
  theme(
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(angle = 90,hjust = 0.5)
  )

.timeline.covid

graph.combined.timeline.inz7d.gesamt <- ggpubr::ggarrange(
  graph.inz7d.österreich.gesamt+ labs(title=element_blank(),subtitle=element_blank(),caption=element_blank()),
  .timeline.covid,
  ncol = 1, heights = c(2,0.2), align = "v"
)
graph.combined.timeline.inz7d.gesamt

