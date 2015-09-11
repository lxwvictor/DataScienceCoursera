## Shiny
if(require(shiny) == FALSE) install.packages("shiny")
if(require(manipulate) == FALSE) install.packages("manipulate")

library(shiny)
ui <- shinyUI(pageWithSidebar(
        headerPanel("Data science FTW!"),
        sidebarPanel(
                h3('Sidebar text')
        ),
        mainPanel(
                h3('Main Panel text')
        )
))

library(shiny)
server <- shinyServer(
        function(input, output) {
        }
)

shinyApp(ui = ui, server = server)

## Basic html and getting input
ui <- shinyUI(pageWithSidebar(
        headerPanel("Illustrating markup"),
        sidebarPanel(
                h1('Sidebar panel'),
                h1('H1 text'),
                h2('H2 Text'),
                h3('H3 Text'),
                h4('H4 Text')
                
        ),
        mainPanel(
                h3('Main Panel text'),
                code('some code'),
                p('some ordinary text')
        )
))

server <- shinyServer(
        function(input, output) {
        }
)
shinyApp(ui = ui, server = server)

# Illustrating inputs and outputs in ui.R
library(shiny)
ui <- shinyUI(pageWithSidebar(
        headerPanel("Illustrating inputs"),
        sidebarPanel(
                numericInput('id1', 'Numeric input, labeled id1', 0, min = 0, max = 10, step = 1),
                checkboxGroupInput("id2", "Checkbox",
                                   c("Value 1" = "1",
                                     "Value 2" = "2",
                                     "Value 3" = "3")),
                dateInput("date", "Date:")  
        ),
        mainPanel(
                h3('Illustrating outputs'),
                h4('You entered'),
                verbatimTextOutput("oid1"),
                h4('You entered'),
                verbatimTextOutput("oid2"),
                h4('You entered'),
                verbatimTextOutput("odate")
        )
))

server <- shinyServer(
        function(input, output) {
                output$oid1 <- renderPrint({input$id1})
                output$oid2 <- renderPrint({input$id2})
                output$odate <- renderPrint({input$date})
        }
)

shinyApp(ui = ui, server = server)

## Creating a very basic prediction function
ui <- shinyUI(
        pageWithSidebar(
                # Application title
                headerPanel("Diabetes prediction"),
                
                sidebarPanel(
                        numericInput('glucose', 'Glucose mg/dl', 90, min = 50, max = 200, step = 5),
                        submitButton('Submit')
                ),
                mainPanel(
                        h3('Results of prediction'),
                        h4('You entered'),
                        verbatimTextOutput("inputValue"),
                        h4('Which resulted in a prediction of '),
                        verbatimTextOutput("prediction")
                )
        )
)

diabetesRisk <- function(glucose) glucose / 200
server <- shinyServer(
        function(input, output) {
                output$inputValue <- renderPrint({input$glucose})
                output$prediction <- renderPrint({diabetesRisk(input$glucose)})
        }
)

shinyApp(ui = ui, server = server)

# Working with images
ui <- shinyUI(pageWithSidebar(
        headerPanel("Example plot"),
        sidebarPanel(
                sliderInput('mu', 'Guess at the mean',value = 70, min = 62, max = 74, step = 0.05,)
        ),
        mainPanel(
                plotOutput('newHist')
        )
))

library(UsingR)
data(galton)

server <- shinyServer(
        function(input, output) {
                output$newHist <- renderPlot({
                        hist(galton$child, xlab='child height', col='lightblue',main='Histogram')
                        mu <- input$mu
                        lines(c(mu, mu), c(0, 200),col="red",lwd=5)
                        mse <- mean((galton$child - mu)^2)
                        text(63, 150, paste("mu = ", mu))
                        text(63, 140, paste("MSE = ", round(mse, 2)))
                })
        }
)

shinyApp(ui = ui, server = server)

## Experiment
ui <- shinyUI(pageWithSidebar(
        headerPanel("Hello Shiny!"),
        sidebarPanel(
                textInput(inputId="text1", label = "Input Text1"),
                textInput(inputId="text2", label = "Input Text2")
        ),
        mainPanel(
                p('Output text1'),
                textOutput('text1'),
                p('Output text2'),
                textOutput('text2'),
                p('Output text3'),
                textOutput('text3'),
                p('Outside text'),
                textOutput('text4'),
                p('Inside text, but non-reactive'),
                textOutput('text5')
        )
))

x <<- 0
x <<- x + 1
y <<- 0

server <- shinyServer(
        function(input, output) {
                y <<- y + 1
                output$text1 <- renderText({input$text1})
                output$text2 <- renderText({input$text2})
                output$text3 <- renderText({as.numeric(input$text1)+1})
                output$text4 <- renderText(y)
                output$text5 <- renderText(x)
        }
)

shinyApp(ui = ui, server = server)

# Code from other sources
ui <- fluidPage("hellow world",
        actionButton(inputId = "clicks", label = "Click me"),
        sliderInput(inputId = "num",
                    label = "Choose a number",
                    value = 25, min = 1, max = 100),
        actionButton(inputId = "go", label = "Update"),
        textInput(inputId = "title",
                  label = "Write a title",
                  value = "Historgram of Random Normal Values"),
        plotOutput("hist"),
        verbatimTextOutput("stats")
)

server <- function(input, output) {
        observeEvent(input$clicks, {
                print(as.numeric(input$clicks))
        })
        data <- eventReactive(input$go, {
                rnorm(input$num)
        })
        output$hist <- renderPlot({
                hist(data(), main = isolate(input$title)) # data is reactive expression, need '()'
                })
        output$stats <- renderPrint({
                summary(data())
        })
}
shinyApp(ui = ui, server = server)

# 08-reactiveValues - Code from other sources

library(shiny)

ui <- fluidPage(
        actionButton(inputId = "norm", label = "Normal"),
        actionButton(inputId = "unif", label = "Uniform"),
        plotOutput("hist")
)

server <- function(input, output) {
        
        rv <- reactiveValues(data = rnorm(100))
        
        observeEvent(input$norm, { rv$data <- rnorm(100) })
        observeEvent(input$unif, { rv$data <- runif(100) })
        
        output$hist <- renderPlot({ 
                hist(rv$data) 
        })
}

shinyApp(ui = ui, server = server)

# Conditional execution
library(shiny)
ui <- shinyUI(pageWithSidebar(
        headerPanel("Hello Shiny!"),
        sidebarPanel(
                textInput(inputId="text1", label = "Input Text1"),
                textInput(inputId="text2", label = "Input Text2"),
                actionButton("goButton", "Go!")
        ),
        mainPanel(
                p('Output text1'),
                textOutput('text1'),
                p('Output text2'),
                textOutput('text2'),
                p('Output text3'),
                textOutput('text3')
        )
))

server <- shinyServer(
        function(input, output) {
                output$text1 <- renderText({input$text1})
                output$text2 <- renderText({input$text2})
                output$text3 <- renderText({
                        if (input$goButton == 0) "You have not pressed the button"
                        else if (input$goButton == 1) "you pressed it once"
                        else "OK quit pressing it"
                })
        }
)

shinyApp(ui = ui, server = server)


## Manipulate
library(manipulate)
manipulate(plot(1:x), x = slider(1, 1000))

library(manipulate)
library(UsingR)
myHist <- function(mu) {
        hist(galton$child,col="blue",breaks=100)
        lines(c(mu, mu), c(0, 150),col="red",lwd=5)
        mse <- mean((galton$child - mu)^2)
        text(63, 150, paste("mu = ", mu))
        text(63, 140, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

## rCharts
if(require(devtools) == FALSE) install.packages("devtools")
# need to install and load the `devtools` package first
require(devtools)

# install rCharts
install_github('ramnathv/rCharts')

# install slidify
install_github('ramnathv/slidify')
install_github('ramnathv/slidifyLibraries')
#if(require(rCharts) == FALSE) install.packages("rCharts")

require(rCharts)
haireye = as.data.frame(HairEyeColor)
n1 <- nPlot(Freq ~ Hair, group = 'Eye', type = 'multiBarChart',
            data = subset(haireye, Sex == 'Male')
)
n1$save('fig/n1.html', cdn = TRUE)
cat('<iframe src="fig/n1.html" width=100%, height=600></iframe>')

# Example 1 Faetted Scatterplot
names(iris) = gsub("\\.", "", names(iris))
r1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')
r1$save('fig/r1.html', cdn = TRUE)
cat('<iframe src="fig/r1.html" width=100%, height=600></iframe>')

# Example 2 Facetted Barplot
hair_eye = as.data.frame(HairEyeColor)
r2 <- rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')
r2$save('fig/r2.html', cdn = TRUE)
cat('<iframe src="fig/r2.html" width=100%, height=600></iframe>')

# How to get the js/html or publish an rChart
r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = "point", color = "gear")
r1$print("chart1") # print out the js 
r1$save('myPlot.html') #save as html file
r1$publish('myPlot', host = 'gist') # save to gist, rjson required
r1$publish('myPlot', host = 'rpubs') # save to rpubs

# morris example run
data(economics, package = "ggplot2")
econ <- transform(economics, date = as.character(date))
m1 <- mPlot(x = "date", y = c("psavert", "uempmed"), type = "Line", data = econ)
m1$set(pointSize = 0, lineWidth = 1)
m1$save('fig/m1.html', cdn = TRUE)
cat('<iframe src="fig/m1.html" width=100%, height=600></iframe>')

# xCharts
require(reshape2)
uspexp <- melt(USPersonalExpenditure)
names(uspexp)[1:2] = c("category", "year")
x1 <- xPlot(value ~ year, group = "category", data = uspexp, type = "line-dotted")
x1$save('fig/x1.html', cdn = TRUE)
cat('<iframe src="fig/x1.html" width=100%, height=600></iframe>')

# xCharts run
require(reshape2)
uspexp <- melt(USPersonalExpenditure)
names(uspexp)[1:2] = c("category", "year")
x1 <- xPlot(value ~ year, group = "category", data = uspexp, type = "line-dotted")
x1$save('fig/x1.html', cdn = TRUE)
cat('<iframe src="fig/x1.html" width=100%, height=600></iframe>')

# Leaflet
map3 <- Leaflet$new()
map3$setView(c(51.505, -0.09), zoom = 13)
map3$marker(c(51.5, -0.09), bindPopup = "<p> Hi. I am a popup </p>")
map3$marker(c(51.495, -0.083), bindPopup = "<p> Hi. I am another popup </p>")
map3$save('fig/map3.html', cdn = TRUE)
cat('<iframe src="fig/map3.html" width=100%, height=600></iframe>')

# Leaflet run
map3 <- Leaflet$new()
map3$setView(c(51.505, -0.09), zoom = 13)
map3$marker(c(51.5, -0.09), bindPopup = "<p> Hi. I am a popup </p>")
map3$marker(c(51.495, -0.083), bindPopup = "<p> Hi. I am another popup </p>")
map3$save('fig/map3.html', cdn = TRUE)
cat('<iframe src="fig/map3.html" width=100%, height=600></iframe>')

# Rickshaw
usp = reshape2::melt(USPersonalExpenditure)
# get the decades into a date Rickshaw likes
usp$Var2 <- as.numeric(as.POSIXct(paste0(usp$Var2, "-01-01")))
p4 <- Rickshaw$new()
p4$layer(value ~ Var2, group = "Var1", data = usp, type = "area", width = 560)
# add a helpful slider this easily; other features TRUE as a default
p4$set(slider = TRUE)
p4$save('fig/p4.html', cdn = TRUE)
cat('<iframe src="fig/p4.html" width=100%, height=600></iframe>')

# Rickshaw run
usp = reshape2::melt(USPersonalExpenditure)
# get the decades into a date Rickshaw likes
usp$Var2 <- as.numeric(as.POSIXct(paste0(usp$Var2, "-01-01")))
p4 <- Rickshaw$new()
p4$layer(value ~ Var2, group = "Var1", data = usp, type = "area", width = 560)
# add a helpful slider this easily; other features TRUE as a default
p4$set(slider = TRUE)
p4$save('fig/p4.html', cdn = TRUE)
cat('<iframe src="fig/p4.html" width=100%, height=600></iframe>')

# highchart
h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd", data = MASS::survey, type = c("line", 
                                                                      "bubble", "scatter"), group = "Clap", size = "Age")
h1$save('fig/h1.html', cdn = TRUE)
cat('<iframe src="fig/h1.html" width=100%, height=600></iframe>')

# highchart run
h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd", data = MASS::survey, type = c("line", 
                                                                      "bubble", "scatter"), group = "Clap", size = "Age")
h1$save('fig/h1.html', cdn = TRUE)
cat('<iframe src="fig/h1.html" width=100%, height=600></iframe>')

## GoogleVis
if(require(googleVis) == FALSE) install.packages("googleVis")
suppressPackageStartupMessages(library(googleVis))
M <- gvisMotionChart(Fruits, "Fruit", "Year",
                     options=list(width=600, height=400))
print(M,"chart")

# Plots on maps
G <- gvisGeoChart(Exports, locationvar="Country",
                  colorvar="Profit",options=list(width=600, height=400))
print(G,"chart")

# Specifying a region
G2 <- gvisGeoChart(Exports, locationvar="Country",
                   colorvar="Profit",options=list(width=600, height=400,region="150"))
print(G2,"chart")

# Setting more options
df <- data.frame(label=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
Line <- gvisLineChart(df, xvar="label", yvar=c("val1","val2"),
                      options=list(title="Hello World", legend="bottom",
                                   titleTextStyle="{color:'red', fontSize:18}",                         
                                   vAxis="{gridlines:{color:'red', count:3}}",
                                   hAxis="{title:'My Label', titleTextStyle:{color:'blue'}}",
                                   series="[{color:'green', targetAxisIndex: 0}, 
                                   {color: 'blue',targetAxisIndex:1}]",
                                   vAxes="[{title:'Value 1 (%)', format:'##,######%'}, 
                                   {title:'Value 2 (\U00A3)'}]",                          
                                   curveType="function", width=500, height=300                         
                      ))
print(Line,"chart")

# Combining multiple plots together
G <- gvisGeoChart(Exports, "Country", "Profit",options=list(width=200, height=100))
T1 <- gvisTable(Exports,options=list(width=200, height=270))
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=400, height=370))
GT <- gvisMerge(G,T1, horizontal=FALSE)
GTM <- gvisMerge(GT, M, horizontal=TRUE,tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")
print(GTM, "chart")
print(M)

## shinyApps.io
library(devtools)
devtools::install_github('rstudio/shinyapps')
shinyapps::setAccountInfo(name='lxwvictor', token='5CB8CFE15CA9B422A6282650E7D45BBA', secret='98zVk4ha4VggTtO+diU6vjFWBvq/+4zv3FXuGqPQ')
library(shinyapps)
deployApp("./6. Experiment")

## Plotly
if(require(viridis) == FALSE) install.packages("viridis")
if(require(plotly) == FALSE) install_github("ropensci/plotly")
library(plotly)

Sys.setenv("plotly_username" = "lxwvictor")
Sys.setenv("plotly_api_key" = "larctvckev")

load("courseraData.rda")
library(ggplot2)
# First do a bar plot in ggplot
g <- ggplot(myData, aes(y = enrollment, x = class, fill = offering)) 
g <- g + geom_bar(stat = "identity")
g

# Let's try to get it into plot.ly
#py <- plotly()
#out <- py$ggplotly(g)
#out$response$url
out <- ggplotly(g)
out
