library(plotly)
library(shiny)

# compute a correlation matrix
correlation <- round(cor(mtcars), 3)
nms <- names(mtcars)

ui <- fluidPage(
  mainPanel(
    plotlyOutput("heat"),
    plotlyOutput("scatterplot")
  ),
  verbatimTextOutput("selection")
)

server <- function(input, output, session) {
  output$heat <- renderPlotly({
    plot_ly(x = nms, y = nms, z = correlation, 
            key = correlation, type = "heatmap", source = "heatplot") %>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = ""))
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click", source = "heatplot")
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$scatterplot <- renderPlotly({
    s <- event_data("plotly_click", source = "heatplot")
    if (length(s)) {
      vars <- c(s[["x"]], s[["y"]])
      d <- setNames(mtcars[vars], c("x", "y"))
      yhat <- fitted(lm(y ~ x, data = d))
      plot_ly(d, x = ~x) %>%
        add_markers(y = ~y) %>%
        add_lines(y = ~yhat) %>%
        layout(xaxis = list(title = s[["x"]]), 
               yaxis = list(title = s[["y"]]), 
               showlegend = FALSE)
    } else {
      plotly_empty()
    }
  })
  
}

shinyApp(ui, server, options = list(display.mode = "showcase"))




fluidPage(
  titlePanel("Changing the values of inputs from the server"),
  fluidRow(
    column(3, wellPanel(
      h4("These inputs control the other inputs on the page"),
      textInput("control_label",
                "This controls some of the labels:",
                "LABEL TEXT"),
      sliderInput("control_num",
                  "This controls values:",
                  min = 1, max = 20, value = 15)
    )),
    
    column(3, wellPanel(
      textInput("inText",  "Text input:", value = "start text"),
      
      numericInput("inNumber", "Number input:",
                   min = 1, max = 20, value = 5, step = 0.5),
      numericInput("inNumber2", "Number input 2:",
                   min = 1, max = 20, value = 5, step = 0.5),
      
      sliderInput("inSlider", "Slider input:",
                  min = 1, max = 20, value = 15),
      sliderInput("inSlider2", "Slider input 2:",
                  min = 1, max = 20, value = c(5, 15)),
      sliderInput("inSlider3", "Slider input 3:",
                  min = 1, max = 20, value = c(5, 15)),
      
      dateInput("inDate", "Date input:"),
      
      dateRangeInput("inDateRange", "Date range input:")
    )),
    
    column(3,
           wellPanel(
             checkboxInput("inCheckbox", "Checkbox input",
                           value = FALSE),
             
             checkboxGroupInput("inCheckboxGroup",
                                "Checkbox group input:",
                                c("label 1" = "option1",
                                  "label 2" = "option2")),
             
             radioButtons("inRadio", "Radio buttons:",
                          c("label 1" = "option1",
                            "label 2" = "option2")),
             
             selectInput("inSelect", "Select input:",
                         c("label 1" = "option1",
                           "label 2" = "option2")),
             selectInput("inSelect2", "Select input 2:",
                         multiple = TRUE,
                         c("label 1" = "option1",
                           "label 2" = "option2"))
           ),
           
           tabsetPanel(id = "inTabset",
                       tabPanel("panel1", h2("This is the first panel.")),
                       tabPanel("panel2", h2("This is the second panel."))
           )
    )
  )
)

fluidPage(
  titlePanel("Dynamically generated user interface components"),
  fluidRow(
    
    column(3, wellPanel(
      selectInput("input_type", "Input type",
                  c("slider", "text", "numeric", "checkbox",
                    "checkboxGroup", "radioButtons", "selectInput",
                    "selectInput (multi)", "date", "daterange"
                  )
      )
    )),
    
    column(3, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui")
    )),
    
    column(3,
           tags$p("Input type:"),
           verbatimTextOutput("input_type_text"),
           tags$p("Dynamic input value:"),
           verbatimTextOutput("dynamic_value")
    )
  )
)



output$ui <- renderUI({
  if (is.null(input$input_crit))
    return()
  
  # Depending on input$input_type, we'll generate a different
  # UI component and send it to the client.
  switch(input$input_crit,
         "Maximum Cosine" = selectInput("teachtypemax", label="Select teaching type", choices=c("wis","ned","aard"), selected="wis"),
         "Average Cosine" = selectInput("teachtypeave", label="Select teaching type", choices=c("wiskunde","nederlands","aardrijskunde"), selected="wiskunde")
  )
})