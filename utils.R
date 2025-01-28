library(tidyr)
library(dplyr)
library(shiny)
library(bslib)
library(reactable)
library(leaflet)
library(sf)
library(terra)
library(htmlwidgets)
library(plotly)
library(purrr)

### Load Data

#Tabelle mit PDFs
dat_laender <- read.csv("data/Visualisierung_Biodiversitaet_v2_laender.csv", sep=";")
#1153 Zeilen 

#Tabelle mit Ländercodes (zwei und drei Buchstaben)
codes <- read.csv("data/Visualisierung_Biodiversitaet_v1_codes.csv", sep=";")
colnames(codes) <- c("Land", "Code_A2", "Code_A3")

#Grenzen Länder (vereinfachter Datensatz)
borders <- read_sf("data/borders_countries.gpkg", layer = "countries_simplified")

### Datenvorbereitung

# Tabelle mit PDFs an Ländercode 3 an heften
dat <- left_join(dat_laender, codes[,-1], join_by("Code" == "Code_A2"), relationship = "many-to-one")


#Geodatensatz nach Länder in PDF-Tabelle filtern (wird nicht mehr benötigt)
# borders <- borders |> 
#   filter(GID_0 %in% unique(dat$Code_A3))

#Geodaten PDFs anheften (wird nicht hier benötigt)
# dat_countries <- merge(dat, borders, by.x = "Code_A3", by.y = "GID_0")
# dat_countries <- st_as_sf(dat_countries)
#1153 Zeilen

#lange Ländernamen abkürzen
unique(dat$Land)[nchar(unique(dat$Land)) > 20]
dat$Land[dat$Land=="Dominikanische Republik"] <- "Dom. Republik"
dat$Land[dat$Land=="Vereinigtes Königreich"] <- "UK"
dat$Land[dat$Land=="Bosnien und Herzegowina"] <- "Bosnien & Herzegowina"
dat$Land[dat$Land=="Vereinigte Arabische Emirate"] <- "VAE"


#Import Data 1kg durchschnittliches Produkt, welches in der Schweiz konsumiert wird

dat_uebersicht <- read.csv("data/Visualisierung_Biodiversitaet_v2_uebersicht.csv", sep=";")

stop <- nchar(dat_uebersicht$Anteil)-1

dat_uebersicht2 <- dat_uebersicht |> 
  filter(!is.na(Konsummengen_CH_kg_p_y )) |> 
  mutate(PDF_kg = as.numeric(formatC(PDF_kg, format = "e", digits = 2)),
         Total_PDF_Produkt_CH = as.numeric(formatC(Total_PDF_Produkt_CH, format = "e", digits = 2)),
         Anteil = as.numeric(substr(Anteil, start=-1, stop=stop)),
         Total_PDF_Produkt_CH_Woche = Total_PDF_Produkt_CH/52,
         Quelle = "⌀ CH Warenkorb/Woche") |> 
  arrange(Produkt) 

important_products <- c("Kakao","Kaffeebohnen","Olivenöl","Rind","Poulet","Schwein","Weizen",	"Wein")

important_products_df <- dat_uebersicht2 |> 
  mutate(impact_product=Total_PDF_Produkt_CH_Woche) |> 
  select(c(Produkt, impact_product, Quelle)) |> 
  filter(Produkt %in% important_products)

uebrige <- dat_uebersicht2 |> 
  filter(!(Produkt %in% important_products)) |> 
  summarise(impact_product = sum(Total_PDF_Produkt_CH_Woche))

dat_bar_chart <- rbind(important_products_df, data.frame(Produkt="Übrige", impact_product=uebrige, Quelle="⌀ CH Warenkorb/Woche"))

ch_pdf_woche <- sum(dat_bar_chart$impact_product)

### Variablen für Shiny

#Auswahlmöglichkeit  
products_warenkorb <- c("", sort(unique(dat_uebersicht2$Produkt)))

# products_map <- sort(unique(dat_laender$Produkt))

#Number of digits
digits <- 3


#Farbpalette (falls Palette über alle Werte/Produkte sein glecih sollte)
#pal <- colorNumeric(palette = "Spectral", dat$PDF_kg_pro_country, reverse = T)



#Funktion für Legende leaflet für wissenschaftliche Notation (workaround, falls bessere Option vorhanden ändern)
#Quelle: https://github.com/r-spatial/mapview/issues/258

labelFormatCustom = function (prefix = "", suffix = "", between = " &ndash; ", 
                              digits = 3, big.mark = ",", transform = identity, scientific=T) 
{
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = scientific,#TRUE, 
           big.mark = big.mark)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      paste0(prefix, formatNum(cuts), suffix)
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]), 
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", 
             prefix, p[-n], between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}



#Function to transform data for use in sunburst plot

#https://rpubs.com/DragonflyStats/Sunburst-Plots-With-Plotly

as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE){
  require(data.table)
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := "Total"]  
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
  
  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }
  
  hierarchyList <- list()
  
  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    setnames(currentDT, length(current_columns), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parent_columns) := NULL]
  return(hierarchyDT)
}

