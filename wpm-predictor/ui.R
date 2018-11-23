library(shiny)

load("rpartData.Rdata")

shinyUI(
  fluidPage(
    titlePanel(title = "WPM Compliance Calculator"),
    fluidRow(
      sidebarPanel(
        radioButtons("packaging_material",
                     "Packaging Material",
                     c("WPM" = "WPM",
                       "WPM & MP" = "WPM & MP",
                       "MP" = "MP",
                       "Unknown" = "Unknown"),
                     inline = FALSE),
        selectInput("port_of_entry",
                    "Port of Entry",
                    list_port_of_entry),
        selectInput("shipper_country",
                    "Shipper Country",
                    list_shipper_country),
        selectInput("goods_category",
                    "Goods Category",
                    list_goods_category),
        selectInput("month",
                    "Month",
                    list_month)
      ),
      mainPanel(
        h2("Result:"),
        htmlOutput("result_raw"),
        htmlOutput("result_prob")
      )
    )
  )
)