library(shiny)

load("rpartData.Rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  print("foo")
  input_shipper_country <- reactive({
    if (input$shipper_country %in% list_africa_other) {
      "Africa Other"
    } else if (input$shipper_country %in% list_americas_other) {
      "Americas Other"
    } else if (input$shipper_country %in% list_asia_other) {
      "Asia Other"
    } else if (input$shipper_country %in% list_europe_other) {
      "Europe Other"
    } else if (input$shipper_country %in% list_oceania_other) {
      "Oceania Other"
    } else{
      input$shipper_country
    }
  })
  df_predicting <- reactive({
    if (input$packaging_material != "Unknown") {
      df_predicting <- data.frame(
        Packaging.Material = input$packaging_material,
        Port.of.Entry..map. = input$port_of_entry,
        Shipper.Country = input_shipper_country(),
        Goods.Category = input$goods_category,
        Month = input$month
      )
    }
    else{
      data.frame(
        Port.of.Entry..map. = input$port_of_entry,
        Shipper.Country = input_shipper_country(),
        Goods.Category = input$goods_category,
        Month = input$month
      )
    }
    
  })
  
  model <- reactive({
    if (input$packaging_material != "Unknown") {
      rtree.fit
    } else {
      rtree.fit2
    }
  })
  
  output$result_raw <- renderText({
    result_raw <- predict(model(), df_predicting(), type = "class")
    if (result_raw == "Y") {
      paste(
        "The package is predicted as ",
        "<font color=\"#008000\"><b>",
        "Compliant",
        "</b></font>"
      )
    } else {
      paste(
        "The package is predicted as ",
        "<font color=\"#FF0000\"><b>",
        "Non-compliant",
        "</b></font>"
      )
    }
  })
  output$result_prob <- renderText({
    result_prob <- predict(model(), df_predicting(), type = "prob")
    if (result_prob[2] < 0.5) {
      paste(
        "The likelihood of non-compliance is ",
        "<font color=\"#008000\"><b>",
        percent(as.numeric(result_prob[2])),
        "</b></font>"
      )
    } else {
      paste(
        "The likelihood of non-compliance is ",
        "<font color=\"#FF0000\"><b>",
        percent(as.numeric(result_prob[2])),
        "</b></font>"
      )
    }
    
  })
})