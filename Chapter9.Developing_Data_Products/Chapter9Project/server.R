library(shiny)
library(datasets)

server <- shinyServer(
        function(input, output) {
                data <- reactive({
                        values <- renderText({input$regressors})
                        values <- unlist(strsplit(values(), split = " "))
                        values <- paste(values, collapse = "+")
                        values <- paste("mpg~", values)
                        lm(values, data = mtcars)
                        
                })
                output$oregressors <- renderPrint({input$regressors})
                output$omodel <- renderPrint({
                        input$go
                        if(input$go < 1) print(NULL)
                        else isolate(data())
                })
                output$ocoefp <- renderPrint({
                        input$go
                        if(input$go < 1) print(NULL)
                        else isolate(summary(data())$coef[, 4])
                })
                output$oplot <- renderPlot({
                        input$go
                        if(input$go < 1) print(NULL)
                        else {
                                par(mfrow = c(2, 2))
                                isolate(plot(data()))
                        }
                })
        }
)