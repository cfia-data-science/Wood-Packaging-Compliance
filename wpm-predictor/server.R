library(shiny)
library(caret)

load("model.Rdata")

shinyServer(
  function(input, output) {
    df_predicting <- reactive({
      df_predicting <- data.frame(
        Packaging.Material = input$packaging_material,
        Port.of.Entry..map. = input$port_of_entry,
        Shipper.Country = input$shipper_country,
        Goods.Category = input$goods_category
      )
    })
    
    output$result <- renderText({
      result_prob <- predict(cart_under, df_predicting(), type = "prob")
      if (result_prob[2] < 0.5) {
        paste(
          "<div class='result-row'>",
          "<img src='compliant.png' class='circle-img' class='result-img' width='150' height='150'></img>",
          "<svg class='circle-chart' viewbox='0 0 33.83098862 33.83098862' width='150' height='150' xmlns='http://www.w3.org/2000/svg'>",
          "<circle class='circle-chart__background' stroke='#3fa9f5' stroke-width='2' fill='none' cx='16.91549431' cy='16.91549431' r='15.91549431'/>",
          "<circle class='circle-chart__circle' stroke='#f15a24' stroke-width='2' stroke-dasharray='",
          as.numeric(result_prob[2])*100,
          ",100' stroke-linecap='round' fill='none' cx='16.91549431' cy='16.91549431' r='15.91549431'/>",
          "<g class='circle-chart__info'>",
          "<text class='circle-chart__percent' x='16.91549431' y='17' alignment-baseline='central' text-anchor='middle' fill='#3fa9f5' font-size='8'>",
          percent(as.numeric(1 - result_prob[2])),
          "</text>",
          "</text></g></svg></div>",
          "<div class='result-row'>",
          "<div class='result-column1'><b>Predicted as</b></div><div class='result-column2'><b>Compliance:</b></div>",
          "</div>",
          "<div class='result-row'>",
          "<div class='result-column1'><font color='#56AB2F'><b>Compliant</b></font></div><div class='result-column2'><font color='#56AB2F'><b>",
          percent(as.numeric(1 - result_prob[2])),
          "</b></font></div></div>"
        )
        # paste("<div>",
        #       "The package is predicted as ",
        #       "<font color=\"#008000\"><b>",
        #       "Compliant",
        #       "</b></font></div>",
        #       "<div>The likelihood of non-compliance is ",
        #       "<font color=\"#008000\"><b>",
        #       percent(as.numeric(result_prob[2])),
        #       "</b></font></div>")
      } else {
        paste(
          "<div class='result-row'>",
          "<img src='error.png' class='circle-img' class='result-img' width='150' height='150'></img>",
          "<svg class='circle-chart' viewbox='0 0 33.83098862 33.83098862' width='150' height='150' xmlns='http://www.w3.org/2000/svg'>",
          "<circle class='circle-chart__background' stroke='#3fa9f5' stroke-width='2' fill='none' cx='16.91549431' cy='16.91549431' r='15.91549431'/>",
          "<circle class='circle-chart__circle' stroke='#f15a24' stroke-width='2' stroke-dasharray='",
          as.numeric(result_prob[2])*100,
          ",100' stroke-linecap='round' fill='none' cx='16.91549431' cy='16.91549431' r='15.91549431'/>",
          "<g class='circle-chart__info'>",
          "<text class='circle-chart__percent' x='16.91549431' y='17' alignment-baseline='central' text-anchor='middle' fill='#f15a24' font-size='8'>",
          percent(as.numeric(result_prob[2])),
          "</text>",
          "</text></g></svg></div>",
          "<div class='result-row'>",
          "<div class='result-column1'><b>Predicted as</b></div><div class='result-column2'><b>Non-compliance:</b></div>",
          "</div>",
          "<div class='result-row'>",
          "<div class='result-column1'><font color='#FF4B2B'><b>Non-compliant</b></font></div><div class='result-column2'><font color='#FF4B2B'><b>",
          percent(as.numeric(result_prob[2])),
          "</b></font></div></div>"
        )
        # paste("<div>The package is predicted as ",
        #       "<font color=\"#FF0000\"><b>",
        #       "Non-compliant",
        #       "</b></font></div>",
        #       "<div>The likelihood of non-compliance is ",
        #       "<font color=\"#FF0000\"><b>",
        #       percent(as.numeric(result_prob[2])),
        #       "</b></font></div>")
      }
    })
  }
)