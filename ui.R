### Load dependencies
source("utils.R")


### UI
page_fluid(
  includeCSS("bootstrap.css"),
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 2fr"),
        card(fill = F,
             min_height = 500,
             card_header("Warenkorb"),
             card_body(
               max_height = 200,
               value_box(
                 title = "Totale Summe gewählter Warenkorb (PDF)",
                 showcase = icon("shopping-cart"),
                 value = uiOutput("pdf_total"),
                 theme = value_box_theme(bg = "#2c3e50", fg = "#f8f9fa"),
                 class = "border")
             ),
             numericInput("n", "Anzahl Produkte", value = 1, min = 1, max=10),
             card_body(
               min_height = 700,
               layout_column_wrap(
                 width = 1/2,
                 card("Produkte",
                      uiOutput("product")),
                 card("Menge (kg)",
                      uiOutput("weight"))
                )
              ),
         ),
        card(height = 800,
             card_header("PDF des Warenkorbes"),
             card_body(
               layout_column_wrap(
                 min_height = 500,
                 width = 1/2,
                 leaflet::leafletOutput("map", width = 1000),
                 plotlyOutput("barchart_ch")
               )
             ),
             card_body(
               min_height = 200,
               layout_column_wrap(
                 width = 1/2,
                 plotlyOutput("sunburst"),
                 plotlyOutput("piechart_warenkorb")
                )
              )
        )
    )
)

