### Load dependencies
source("utils.R")

### UI
page_fluid(
  includeCSS("www/bootstrap.css"),
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 2fr"),
        card(fill = F,
             height = "100vh",
             card_header("Warenkorb"),
             card_body(
               max_height = 100,
               numericInput("n", "Anzahl Produkte", value = 1, min = 1, max=10)
             ),
             card_body(
               #min_height = 700,
               layout_column_wrap(
                 width = 1/2,
                 card("Produkte",
                      uiOutput("product")),
                 card("Menge (kg)",
                      uiOutput("weight"))
                )
              ),
             card_body(
               actionButton("display", "Berechne Biodiversity-Footprint", width = 200, class = "btn-primary btn-lg")
             )
         ),
        card(height = "80vh",
             card_header("PDF des Warenkorbes"),
             card_body(
               layout_column_wrap(
                 #min_height = 500,
                 width = 1/2,
                 leaflet::leafletOutput("map"),
                 plotlyOutput("sunburst")
               )
             ),
             card_body(
               #min_height = 200,
               layout_column_wrap(
                 width = 1/2,
                 plotlyOutput("barchart_countries"),
                 plotlyOutput("barchart_warenkorb")
                )
              )
        )
    )
)

