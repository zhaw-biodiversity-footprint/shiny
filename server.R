### Server-Funktion

function(input, output, session) {

  ### Basket
  
  #create list with product number depending on how many products can be selected
  product_input <- reactive(paste0("product", seq_len(input$n)))
  weights_input <- reactive(paste0("weight", seq_len(input$n)))

  #create as many select boxes for products as number selected
  output$product <- renderUI({
    map(product_input(), ~ selectInput(.x, NULL, choices = products_warenkorb, selected = input[[.x]]))
  })
  
  #create as many sliders for weight as number selected
  output$weight <- renderUI({
    # if(length(pdfs_products())>2){
    #   browser()
    #   if(is_null(map(weights_input(), ~input[[.x]])[[1]])){
        map(weights_input(), ~ numericInput(.x, NULL,  min = 0.1, max = 100, value = 1, step = 0.1))
      # }else{
      #   browser()
      #   map(weights_input(), ~ sliderInput(.x, NULL,  min = 1, max = 10, value = as.numeric(input[[.x]])))
      # }
    # }
  })
  
  ### Calculate data (preperation)
  
  #get pdfs of the selected products
  pdfs_products <- reactive({map(product_input(), ~ dat_uebersicht2$PDF_kg[dat_uebersicht2$Produkt==input[[.x]]])})

  #fill pdfs where no products is selected with 0
  pdf_products_full <- reactive(rapply(pdfs_products(),function(x) ifelse(identical(x, numeric(0)),0,x), how = "replace"))
  
  #get imput weights as vector
  weights <- reactive(unlist(map(weights_input(), ~input[[.x]])))
  
  #calculate pdfs of products with weight --> impacts as list
  impacts <- reactive({
    # if(length(pdfs_products())>1){
    #   browser()
    # }
    # as.list(unlist(pdf_products_full()) * unlist(map(weights_input(), ~input[[.x]]))[seq_along(unlist(pdf_products_full()))])
    as.list(unlist(pdf_products_full()) * unlist(map(weights_input(), ~input[[.x]]))) #gibt warnung
    })
  
  # have impacts per selected product as vector
  impacts_vector <- reactive({unlist(impacts())})

  #render impacts values as text (not necessary anymore, not visible in UI)
  # output$pdf <- renderUI({
  #   # if(length(pdfs_products())>2){
  #   #   browser()
  #   # }
  #   map(impacts(), ~ renderText(.x))
  # })
  
  # get names of products as list
  pdfs_names <- reactive({map(product_input(), ~ input[[.x]])})
  
  # have names of products as vector
  pdfs_names_vector <- reactive({unlist(pdfs_names())})
  
  
  # filter dat with pdf per country, only selected products in basket
  
  dat_selected <- reactive({
    if(sum(unlist(impacts()))>0){
      dat |> 
        filter(Produkt %in% pdfs_names_vector()) |> 
        filter(PDF_CH_mean > 0) |> 
        mutate(impact = 0)
    }
  })
  
  # calculate impacts per selected product and country
  
  calculate_impact <- function(data, products, weights){
    for(i in seq_along(products)){
      data$impact[data$Produkt==products[i]] <- data$PDF_CH_mean[data$Produkt==products[i]] * weights[i]
    }
    return(data)
  }
  
  dat_selected2 <- reactive({calculate_impact(dat_selected(), pdfs_names_vector(), weights())})
  
  
  ### Total sum of basket
  output$pdf_total <- renderText({formatC(sum(unlist(impacts())), format = "e", digits = 2)})
  
  
  ### Pie chart: percent of each product in selected basket
  
  ## make data frame 
  
  # sum of impact for each product 
  dat_selected_sum_prod <- reactive({
    if(sum(unlist(impacts()))>0){
      dat_selected2() |> 
        group_by(Produkt) |> 
        summarise(impact_product = sum(impact))
    }
  })
  
  # get per product the highest three impacts of countries 
  calculate_highest_countries <- function(data, data_sum){
    data_sum <- mutate(data_sum, top_country = 0)
    for(i in 1:nrow(data_sum)){
      top_country_n3 <- data |> 
        filter(Produkt==data_sum$Produkt[i]) |> 
        top_n(3, impact) |> 
        arrange(desc(impact))
      
      data_sum$top_country[i] <- paste(top_country_n3$Land, top_country_n3$impact, sep = ": ", collapse = ", ")
    }
    return(data_sum)
  }
  
  piechart_df <- reactive({calculate_highest_countries(dat_selected2(), dat_selected_sum_prod())})
  
  # plot only impact > 0
  piechart_df_filter <- reactive({
    if(sum(unlist(impacts()))>0){
      subset(piechart_df(), impact_product != "0")}
      })
  
  ## pie chart
  
  observe({
    if(sum(unlist(impacts()))>0){
      output$piechart_warenkorb <- renderPlotly({
        fig <- piechart_df_filter() |>
          plot_ly(labels = ~Produkt, values = ~impact_product, type = 'pie',
                  textinfo = 'percent',
                  hoverinfo ="text",
                  text = ~paste("PDF: ", impact_product, "<br>",
                        "Länder höchste Werte: ", top_country))
      })
    }
  })

  # observe({
  #   if(nrow(piechart_df()) > 0){
  #     output$piechart_warenkorb <- renderPlotly({
  #       fig <- piechart_df_filter() |>
  #         plot_ly(labels = ~products, values = ~values, type = 'pie')
  #       # layout(title = "Länder mit den 10 höchsten PDFs",
  #       #        yaxis = list(categoryorder = "array", categoryarray =  top_ten_worst()$Land, title = list(text = "")),
  #       #        xaxis = list(title = list(text = "PDF/kg", standoff = 20), exponentformat = "e"))
  #     })
  #   }
  # })
  
  ### Sunburst
  
  dat_selected3 <- reactive({
    dat_selected2() |>
      mutate(Land_Produkt = paste(dat_selected2()$Produkt, dat_selected2()$Land, sep = "_"))
  })
  
  dat_top3 <- reactive({
    dat_selected3() |>
      select(c(1,2,7,8)) |> 
      group_by(Produkt) |> 
      top_n(3, impact)
  })

  dat_rest <- reactive({
    dat_selected3() |> 
      filter(!(Land_Produkt %in% dat_top3()$Land_Produkt)) |> 
      group_by(Produkt) |> 
      summarise(impact = sum(impact)) |> 
      mutate(Land = "Übrige")
  })
  
  dat_all <- reactive({
    rbind(select(dat_top3(),-4),dat_rest()) |> 
      arrange(Produkt)
  })
  
  # df_sunburst <- reactive({
  #   if(sum(unlist(impacts()))>0){
  #     bind_rows(
  #       # Full total
  #       dat_all() |> summarise(labels = "Total", impact = sum(impact)),
  #       # "Class"-level totals
  #       dat_all() |> 
  #         group_by(labels = Produkt) |>  
  #         summarise(
  #           impact = sum(impact), 
  #           parents = "Total",
  #           .groups = "drop"),
  #       # Individual Class+Survived-level numbers
  #       dat_all() |> 
  #         rename(parents = Produkt, labels = Land) |> 
  #         mutate(parents = paste("Total", parents, sep = " - "))) |> 
  #       # Add unique ids column
  #       mutate(ids = if_else(
  #         is.na(parents), labels, paste(parents, labels, sep = " - ")))
  #   }
  # })
  
  df_sunburst <- reactive({
    if(sum(unlist(impacts()))>0){
     as.sunburstDF(dat_all(), value_column = "impact", add_root = T)
    }
  })

  observe({
    if(sum(unlist(impacts()))>0){
      output$sunburst <- renderPlotly({
        fig <- df_sunburst() |>
          plot_ly(ids = ~ids, labels = ~labels, parents = ~parents, 
                  values = ~values, type = "sunburst", branchvalues = "total")
      })
    }
  })
  
  
  ### Bar chart
  
  dat_bar <- reactive({dat_selected_sum_prod() |> 
      mutate(Quelle = "Selektierter Warenkorb")})
  
  dat_bar_important_prod <- reactive({dat_bar() |> 
      filter(Produkt %in% important_products)})

  dat_bar_uebrige <- reactive({dat_bar() |> 
      filter(!(Produkt %in% important_products)) |>
      summarise(impact_product = sum(impact_product))})
    
  dat_bar_selected <- reactive(rbind(dat_bar_important_prod(), data.frame(Produkt="Übrige", impact_product=dat_bar_uebrige(), Quelle="Selektierter Warenkorb")))
  
  dat_bar_chart2 <- reactive(rbind(dat_bar_chart, dat_bar_selected()))
  
  observe({
    if(sum(unlist(impacts())) > 0){
      output$barchart_ch <- renderPlotly({
        fig <- dat_bar_chart2() |>
          plot_ly(x = ~impact_product, y = ~Quelle, type = "bar", color = ~Produkt, showlegend = T,
                  hoverinfo ="text",
                  text = ~paste("Produkt: ", Produkt , "<br>", "PDF: ", impact_product))|> 
          layout(xaxis = list(title = 'PDF', exponentformat = "e"), yaxis = list(title = '', tickangle = 270), barmode = 'stack')
      })
    }
  })

  # piechart_sum <- reactive({
  #   data.frame(
  #     quelle = c("PDF CH total", "gewählter Warenkorb"),
  #     pdf_total_warenkorb = c(sum(dat_uebersicht$Total_PDF_Produkt_CH, na.rm = TRUE), sum(unlist(impacts())))
  #   )
  # })
  # 
  #percent of PDF (selected products and weight) compared to mean CH (muss noch angepasst werden)
  # observe({
  #   if(sum(unlist(impacts())) > 0){
  #     output$piechart_ch <- renderPlotly({
  #       fig <- piechart_sum() |>
  #         plot_ly(labels = ~quelle, values = ~pdf_total_warenkorb, type = 'pie')
  #       # layout(title = "Länder mit den 10 höchsten PDFs",
  #       #        yaxis = list(categoryorder = "array", categoryarray =  top_ten_worst()$Land, title = list(text = "")),
  #       #        xaxis = list(title = list(text = "PDF/kg", standoff = 20), exponentformat = "e"))
  #     })
  #   }
  # })

  
  ### Map
  
  ## create dataframe for map
  
  # sum of impact for each country 
  dat_selected_sum_country <- reactive({
    if(sum(unlist(impacts()))>0){
      dat_selected2() |> 
        group_by(Land, Code_A3) |> 
        summarise(impact_country = sum(impact), .groups = "keep")
    }
  })
  
  # join geometry of countries
  dat_countries <- reactive({
    if(sum(unlist(impacts()))>0){
      st_as_sf(left_join(dat_selected_sum_country(), borders, join_by("Code_A3"=="GID_0")))
    }
  })

  ## basemap
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = 8.25, lat =  46.85, zoom = 2)
  })

  ## map
  
  observe({
    if(sum(unlist(impacts()))>0){
      if(nrow(dat_countries())>1){ #distinguish between products only from CH from those which are imported from abroad
        qpal <- colorQuantile(palette = "RdYlGn", n = 8, domain = dat_countries()$impact_country, reverse = T)
        qpal_col <- qpal(dat_countries()$impact_country) # hex codes
        qpal_col_legend <- unique(qpal(sort(dat_countries()$impact_country))) # hex codes
        qpal_labs <- quantile(dat_countries()$impact_country, seq(0, 1, length.out = 9)) # depends on n from pal
        qpal_labs <- formatC(qpal_labs, format = "e", digits = 2)
        qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA
        leafletProxy("map", data = dat_countries(), session) |>
          clearShapes() |>
          clearControls() |>
          addPolygons(
            color = ~qpal_col,  # Using the colors for the border of polygons
            fillColor = ~qpal_col,  # Using the same colors for filling the polygons
            popup = paste("Land: ", dat_countries()$Land, "<br>",
                          "PDF/kg: ", dat_countries()$impact_country),
            popupOptions = popupOptions(maxWidth = 300, closeOnClick = TRUE),
            stroke = TRUE
          ) |>
          addLegend(
            position = "bottomleft",
            colors = qpal_col_legend,  # Providing the color values for the legend
            labels = qpal_labs,  # Labels corresponding to the color
            title = "PDF/kg"
          )
      }else{
        leafletProxy("map", data = dat_countries(), session) |>
          clearShapes() |>
          clearControls() |>
          addPolygons(
            color = "black",
            fillColor = "grey",
            popup = paste("Land: ", dat_countries()$Land, "<br>",
                          "PDF/kg: ", dat_countries()$impact_country),
            popupOptions = popupOptions(maxWidth = 300, closeOnClick = TRUE),
            stroke = TRUE
          ) |>
          addLegend(
            position = "bottomleft",
            colors = "grey",  # Providing the color values for the legend
            labels = dat_countries()$impact_country,  # Labels corresponding to the color
            title = "PDF/kg"
          )
      }
    }
  })

  
  #   filtered_dat_countries <- reactive({filter(dat_countries, Produkt == input$product_map)})
  #   filtered_dat_countries_poly <- reactive({select(filtered_dat_countries(), PDF_kg_prod_country)})
  #   
  # output$map <- renderLeaflet({
  #   leaflet() |>
  #     addProviderTiles(providers$CartoDB.Positron) |>
  #     setView(lng = 8.25, lat =  46.85, zoom = 2)
  # })
  # 
  # observe({
  #   qpal <- colorQuantile(palette = "Spectral", n = 8, domain = filtered_dat_countries_poly()$PDF_kg_prod_country, reverse = T)
  #   qpal_col <- qpal(filtered_dat_countries_poly()$PDF_kg_prod_country) # hex codes
  #   qpal_col_legend <- unique(qpal(sort(filtered_dat_countries_poly()$PDF_kg_prod_country))) # hex codes
  #   qpal_labs <- quantile(filtered_dat_countries_poly()$PDF_kg_prod_country, seq(0, 1, length.out = 9)) # depends on n from pal
  #   qpal_labs <- formatC(qpal_labs, format = "e", digits = 2)
  #   qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA
  #   leafletProxy("map", data = filtered_dat_countries_poly(), session) |>
  #     clearShapes() |>
  #     clearControls() |>
  #     addPolygons(
  #       color = ~qpal_col,  # Using the colors for the border of polygons
  #       fillColor = ~qpal_col,  # Using the same colors for filling the polygons
  #       popup = paste("Land: ", filtered_dat_countries()$Land, "<br>",
  #                     "PDF/kg: ", filtered_dat_countries()$PDF_kg_prod_country),
  #       popupOptions = popupOptions(maxWidth = 300, closeOnClick = TRUE),
  #       stroke = TRUE
  #       ) |>
  #     addLegend(
  #       position = "bottomleft",
  #       colors = qpal_col_legend,  # Providing the color values for the legend
  #       labels = qpal_labs,  # Labels corresponding to the color
  #       title = "PDF/kg"
  #       )
  # })

  #   
  
  ### Images
  
  output$image1 <- renderImage( 
    { 
      list(src = "images/Bild1.png", height = "25%") 
    }, 
    deleteFile = FALSE 
  ) 
  
  output$image2 <- renderImage( 
    { 
      list(src = "images/Bild2.jpg", height = "25%") 
    }, 
    deleteFile = FALSE 
  ) 
  
  output$image3 <- renderImage( 
    { 
      list(src = "images/Bild3.png", height = "25%") 
    }, 
    deleteFile = FALSE 
  ) 
  

}
