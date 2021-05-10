#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("modeling.R")
library(shiny)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(
    #withMathJax(),
    # Application title
    theme = bs_theme(version=4,bootswatch = "minty"),
    titlePanel("Model Results"),
    p("Data: Contain states, counties, three reponse variables (score_infection,score_death,score_combine) and 102 predcitors if the with_interation is True, 14 predictors if with_interation is False."),
    p("Data Variables: Definitions of 3 dependent variables and 14 independent variables."),
    p("Correlation Plot: Correlation plot of 16 varibles in data variables section."),
    p('10-fold CV model selection: 10-fold cross-validation process to select the best hyper-parameter.'),
    p('Top 10 overperformed and underperformed counties: Ten selected counties with the higest residuals (underperformed) and lowest residuals (overperformed) given the weight'),
    # Sidebar with a slider input for number of bins 

    sidebarPanel(
        helpText('score = weight*score_infection+(1-weight)*score_death'),
            sliderInput("w",
                        "weight",
                        min = 0,
                        max = 1,
                        value = 1),
            sliderInput("lambda",
                        "log(lambda)",
                        min=-8,
                        max=0,
                        value = c(-2,0),step=0.1),
        radioButtons("interation","with_interation",
                     choices = list("Yes"=TRUE,"No"=FALSE),selected = FALSE)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = 'tabs',
                        tabPanel("Data",dataTableOutput("ex1")),
                        tabPanel("Data variables",verbatimTextOutput("ttt")),
                        tabPanel("Correlation plot",plotOutput("corr")),
                        tabPanel("10-fold CV model selection",plotOutput("cvresult")),
                        tabPanel("Coefficients with the best hyper-parameter", tableOutput("table")),
                        tabPanel('Top 10 overperformed and underperformed counties',tableOutput('residual'))
        )
        
       
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    x = reactive({
        consult_lasso(w=input$w,with_interation = input$interation,lambda = exp(seq(input$lambda[1],input$lambda[2],by=0.05)))$df
    })
    yy = reactive({
        consult_lasso(w=input$w,with_interation = input$interation,lambda = exp(seq(input$lambda[1],input$lambda[2],by=0.05)))$result_df
    })
    output$ex1 = renderDataTable(show_df(with_interation = input$interation),options = list(pageLength=20))
    output$cvresult <- renderPlot({
        # generate bins based on input$bins from ui.R
        x()
    })
    output$ttt = renderText(paste(
                                  "score_infection: Scores for infection rates in each county.", 
                                  "score_death: Scores for death rates in each county.", 
                                  "under5yr: Percentage of population under 5 years old in each county.",
                                  '15yrto44yr: Percentage of population between 15 to 44 years old in each county.',
                                  '65andOver: Percentage of population over 65 years old in each county.',
                                  '75andOver: Percentage of population over 75 years old in each county.',
                                  'bachelor: Percentage of population with a bachelor degree or above in each county.',
                                  'disabilityrate: Percentage of population with disabilities in each county.',
                                  'employment: Percentage of population with a job in each county.',
                                  'highrisk: Percentage of population with a high-risk job in each county.',
                                  'lessHighSchool: Percentage of population with a high school diploma or below in each county.',
                                  'nocomputer: Percentage of population without a computer in each county.',
                                  'noinsurance: Percentage of population without public or private insurance in each county.',
                                  'nointernet: Percentage of population without internet in each county.',
                                  'poverty: Percentage of population under the poverty line  in each county.',
                                  'black: Percentage of blacks in each county.',
                                  'white: Percentage of whites in each county.',
                                  sep="\n\n"))
    output$table = renderTable(x())
    output$residual = renderTable(yy())
    output$corr = renderPlot({ggcorrplot(corr,type = "lower",
                                         outline.col = "white",
                                         ggtheme = ggplot2::theme_gray,
                                         colors = c("#6D9EC1", "white", "#E46726"),lab=TRUE)},width = 750,height = 950)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
