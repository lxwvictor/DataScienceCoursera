## Shiny
if(require(shiny) == FALSE) install.packages("shiny")
if(require(manipulate) == FALSE) install.packages("manipulate")

library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("Data science FTW!"),
        siderbarPanel(
                h3('Sidebar text')
        ),
        mainPanel(
                h3('Main Panel text')
        )
))

library(shiny)
shinyServer(
        function(input, output) {
        }
)