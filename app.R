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
    p("Data summary: Descriptive statistics"),
    p("Data variables: Definitions of 3 dependent variables and 14 independent variables."),
    p("Correlation plot: Correlation plot of 16 varibles in data variables section."),
    p('LOO-CV model selection: Leave-one-out cross-validation process to select the best regularization term lambda.'),
    p('Coefficients: Coefficients with the best regularization term lambda.'),
    p('Top 10 (Table): Ten selected counties with the higest residuals (underperformed) and lowest residuals (overperformed) given the weight in a table'),
    # Sidebar with a slider input for number of bins 
    p('Top 10 (Geographic Graph): Ten selected counties with the higest residuals (underperformed) and lowest residuals (overperformed) given the weight in a geographic graph'),
    sidebarPanel(
        helpText('Score = Weight*Score_infection+(1-Weight)*Score_death'),
            sliderInput("w",
                        label = "Weight",
                        min = 0,
                        max = 1,
                        value = 1),
            sliderInput("lambda",
                        label = withMathJax("$$\\log(\\lambda)$$"),
                        min=-8,
                        max=0,
                        value = c(-2,0),step=0.1)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = 'tabs',
                        tabPanel("Data summary",dataTableOutput("ex1")),
                        tabPanel("Data variables",verbatimTextOutput("ttt")),
                        tabPanel("Correlation plot",plotOutput("corr")),
                        tabPanel("Scores by county (Geographic graph)",plotOutput("totalscore_geo")),
                        tabPanel("LOO-CV model selection",plotOutput("cvresult")),
                        tabPanel("Coefficients", tableOutput("table")),
                        tabPanel('Top 10 (Table)',tableOutput('residual')),
                        tabPanel('Top 10 (Geographic graph)',plotOutput('southest_geo'))

        )
        
       
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    x = reactive({
        consult_lasso(w=input$w,lambda = exp(seq(input$lambda[1],input$lambda[2],by=0.05)))$df
    })
    yy = reactive({
        consult_lasso(w=input$w,lambda = exp(seq(input$lambda[1],input$lambda[2],by=0.05)))$counties
    })
    ovpstate = reactive({
        consult_lasso(w=input$w,lambda = exp(seq(input$lambda[1],input$lambda[2],by=0.05)))$best_state
    })
    ovpcounty = reactive({
        consult_lasso(w=input$w,lambda = exp(seq(input$lambda[1],input$lambda[2],by=0.05)))$best_county
    })
    udpstate = reactive({
        consult_lasso(w=input$w,lambda = exp(seq(input$lambda[1],input$lambda[2],by=0.05)))$worst_state
    })
    udpcounty = reactive({
        consult_lasso(w=input$w,lambda = exp(seq(input$lambda[1],input$lambda[2],by=0.05)))$worst_county
    })
    output$ex1 = renderDataTable(show_df(),options = list(pageLength=20))
    output$cvresult <- renderPlot({
        # generate bins based on input$bins from ui.R
        x()
    })
    output$ttt = renderText(paste(
                                  "Score_infection: Scores for infection rates in each county.", 
                                  "Score_death: Scores for death rates in each county.", 
                                  "Under 5 Years: The proportion of the county population that is under the age of 5.",
                                  '15 to 44 Years: The proportion of the county population that is between ages 15 and 44.',
                                  '65 Years and Over: The proportion of the county population that is age 65 and older.',
                                  '75 Years and Over: The proportion of the county population that is age 75 and older.',
                                  "Bachelor: The proportion of the county population with a bachelor's degree.",
                                  'Disability Rate: Percentage of population with disabilities in each county.',
                                  'Employment: The proportion of the county population experiencing underemployment (as of 2019).',
                                  'High Risk: The proportion of the county population with a High Risk job.',
                                  'Less than High School: The proportion of the county population with less than a high school education.',
                                  'No Computer: The proportion of the county population without computer access.',
                                  'No Insurance: The proportion of the county population without health insurance.',
                                  'No Internet: The proportion of the county population without internet access.',
                                  'Poverty: The proportion of the county population under the poverty line.',
                                  'Black: The proportion of the county population who racially identify as black.',
                                  'White: Proportion of the county population who racially identify as white.',
                                  sep="\n\n"))
    output$table = renderTable(x())
    output$residual = renderTable(yy())
    
    output$southest_geo <- renderPlot({
        # Render the plot
        map_lasso(ovpstate = ovpstate(),
                  ovpcounty = ovpcounty(),
                  udpstate = udpstate(),
                  udpcounty = udpcounty())
        })
    output$totalscore_geo = renderPlot({
        mapw(w=input$w)
    },width = 1100,height = 650)
    output$corr = renderPlot({ggcorrplot(corr,type = "lower",
                                         outline.col = "white",
                                         ggtheme = ggplot2::theme_gray,
                                         colors = c("#6D9EC1", "white", "#E46726"),lab=TRUE)},width = 750,height = 950)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

