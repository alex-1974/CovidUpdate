

# Paths
path.data <-"data/"
path.img <- "img/"

# if path doesn't exist create it
if (!dir.exists(path.img)) dir.create(path.img)
if (!dir.exists(path.data)) dir.create(path.data)

# 
date_range <- seq(as_date("2020-01-01"), as_date(now()), by = 1)
#bundesländer.list <- c("Wien", "Niederösterreich", "Oberösterreich", "Burgenland", "Steiermark", "Tirol", "Kärnten", "Vorarlberg", "Salzburg")
#altersgruppe.list <- c("<5", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", ">84")
#vaccination.list <- c("AstraZeneca", "BioNTechPfizer", "Janssen", "Moderna", "Novavax", "Other")

# Factors
bundesländer.fct <- c("Wien", "Niederösterreich", "Oberösterreich", "Burgenland", "Steiermark", "Tirol", "Kärnten", "Vorarlberg", "Salzburg", "Österreich")
altersgruppe.fct <- c("<5", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", ">84")
sex.fct <- c("m", "w")
vaccination.fct <- c("AstraZeneca", "BioNTechPfizer", "Janssen", "Moderna", "Novavax", "Other")
