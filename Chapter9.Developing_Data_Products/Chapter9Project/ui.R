library(shiny)
ui <- shinyUI(pageWithSidebar(
        headerPanel("Buillding a linear model to quantify the MPG difference of cars"),
        sidebarPanel(
                checkboxGroupInput(inputId = "regressors", label = "Select the Regressors",
                                   choices = c("Number of cylinders" = "cyl",
                                     "Displacement" = "disp",
                                     "Gross horsepower" = "hp",
                                     "Rear axle ratio" = "drat",
                                     "Weight (lb/1000)" = "wt",
                                     "1/4 mile tile" = "qsec",
                                     "V/S" = "vs",
                                     "Transmission (0 = automatic, 1 = manual)" = "am",
                                     "Number of forward gears" = "gear",
                                     "Number of cauburetors" = "carb"
                                     )),
                
                actionButton(inputId = "go", label = "Click to go"),
                h3('Instruction to use this appliation'),
                p('I found it was not easy to find the best set of predictors when doing the assignment 
                  of Regression Models in this data science specializatoin. There is a need to try 
                  different combinations of predictors and evaluate them based on the p-value and 
                  diagnostic plots.'),
                p('Understanding the output of this application requires some knowledge of regression 
                  models, but playing around it is definitely easy. Just tick the different 
                  variables/regressors of the cars and click the button "click to go". Then the 
                  application will auto calculate and give the result. Have fun!')
        ),
        mainPanel(
                h4('Regressors Selected'),
                verbatimTextOutput("oregressors"),
                h4('The linear model is'),
                verbatimTextOutput("omodel"),
                h4('The p-value of coefficients'),
                verbatimTextOutput("ocoefp"),
                h4('Diagnostic plots'),
                plotOutput("oplot")
        )
))