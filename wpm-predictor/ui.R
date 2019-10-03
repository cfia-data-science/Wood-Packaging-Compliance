library(shiny)

load("model.Rdata")

shinyUI(
  fluidPage(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(property = "og:title", content = "WPMCP"),
      tags$meta(property = "og:description", content = "Wood Packaging Material Compliance Predictor"),
      tags$meta(property = "og:image", content = "WPMCP.png"),
      tags$meta(property = "og:url", content = "//irmmodelling.shinyapps.io/wpm-predictor/"),
      tags$title("WPM Compliance Predictor"),
      includeScript("google-analytics.js")
    ),
    theme = "custom.css",
    fluidRow(
      class = "main-wrapper",
      column(
        6,
        class = "section left arrow-right",
        div(
          class = "container",
          div(
            class = "questions-wrapper",
            h2(
              "WPM Compliance Predictor"
            ),
            HTML('<div id="packaging_material" class="form-group shiny-input-radiogroup shiny-input-container shinyjs-resettable shiny-bound-input" data-shinyjs-resettable-id="packaging_material" data-shinyjs-resettable-type="RadioButtons" data-shinyjs-resettable-value="WPM">'),
            tags$label(
              "Packaging Material",
              class = "control-label"
            ),
            div(
              class = "shiny-options-group radio-group radio-group-wrapper",
              tags$ul(
                tags$li(
                  tags$input(type = "radio",
                             name = "packaging_material",
                             id = "WPM",
                             value = "WPM",
                             checked = "checked"),
                  HTML('<div class="check"></div><label for="WPM">WPM</label>')
                ),
                tags$li(
                  tags$input(type = "radio",
                             name = "packaging_material",
                             id = "WPM & MP",
                             value = "WPM & MP"),
                  HTML('<div class="check"><div class="inside"></div></div><label for="WPM & MP">WPM & MP</label>')
                ),
                tags$li(
                  tags$input(type = "radio",
                             name = "packaging_material",
                             id = "MP",
                             value = "MP"),
                  HTML('<div class="check"><div class="inside"></div></div><label for="MP">MP</label>')
                )
              )
            ),
            HTML('</div>'),
            div(
              selectInput("port_of_entry",
                          "Port of Entry",
                          list_port_of_entry),
              selectInput("shipper_country",
                          "Shipper Country",
                          list_shipper_country),
              selectInput("goods_category",
                          "Goods Category",
                          list_goods_category))
          )
        )
      ),
      column(
        6,
        class = "section right",
        div(
          class = "container",
          div(
            class = "result-wrapper",
            div(
              class = "result-wrapper-title",
              h2("Result:")
            ),
            div(
              class = "result-wrapper-content",
              htmlOutput("result", class = "result")
            )
          )
        )
      )
    )
  )
)