#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

source('generator_sourceCode.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Generate Data\n(Process Shift & Delayed Exponential)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            downloadButton("downloadData", "Download generated data"),
            downloadButton("downloadParams", "Download parameter settings"),
            radioButtons('fn',
                         'Which Learning Function?',
                         list('Process Shift' = 'ps',
                              '(Delayed) Exponential' = 'de')),
            numericInput('nsubs',
                        "Number of subjects:",
                        value = 10),
            numericInput("nitems",
                        "Number of items:",
                        value = 5),
            numericInput("ntrials",
                        "Number of trials:",
                        value = 50),
            numericInput('sigma',
                         'Overall sigma',
                         .2),
            numericInput('alpha',
                         'alpha',
                         .5),
            numericInput('alpha_sd',
                         'sd(alpha)',
                         .5),
            numericInput('beta',
                         'beta',
                         2),
            numericInput('beta_sd',
                         'sd(beta)',
                         2),
            numericInput('rate',
                         'rate',
                         .7),
            numericInput('rate_sd',
                         'rate',
                         .5),
            numericInput('mu',
                         'mu (ps only)',
                         1.8),
            numericInput('mu_sd',
                         'sd(mu) (ps only)',
                         .5),
            numericInput('pRet_A',
                         'P(retrieval)_A',
                         .25),
            numericInput('pRet_A_sd',
                         'P(retrieval)_A',
                         .25),
            numericInput('pRet_B',
                         'P(retrieval)_B',
                         25),
            numericInput('pRet_B_sd',
                         'P(retrieval)_B',
                         7),
            numericInput('tau',
                         'tau',
                         0),# fixed at 0 for now
            numericInput('tau_sd',
                         'sd(tau)',
                         0)# fixed at 0 for now
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput('params'),
            plotOutput("sub_item_plot"),
            tableOutput('data')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    RTs <- reactive({generate_RTs(input$fn, 
                        input$nsubs, input$nitems, input$ntrials,
                        input$alpha, input$beta, input$rate, input$sigma,
                        input$alpha_sd, input$beta_sd, input$rate_sd,
                        input$tau, input$tau_sd,
                        input$mu, input$mu_sd,
                        input$pRet_A, input$pRet_B,
                        input$pRet_A_sd, input$pRet_B_sd)})
    
    output$params <- renderTable({
        parameters <- tibble(param = c('fn', 'nsubs', 'nitems','ntrials',
                                       'alpha', 'beta', 'rate', 'sigma',
                                       'alpha_sd', 'beta_sd', 'rate_sd',
                                       'mu', 'mu_sd',
                                       'pRet_A', 'pRet_B',
                                       'pRet_A_sd', 'pRet_B_sd',
                                       'tau', 'tau_sd',
                                       '*'),
                             setting = c(input$fn, input$nsubs, input$nitems, input$ntrials,
                                         input$alpha, input$beta, input$rate, input$sigma,
                                         input$alpha_sd, input$beta_sd, input$rate_sd,
                                         input$mu, input$mu_sd,
                                         input$pRet_A, input$pRet_B,
                                         input$pRet_A_sd, input$pRet_B_sd,
                                         input$tau, input$tau_sd,
                                         NA),
                             description = c('which function to use to generate data',
                                             'how many subjects to simulate',
                                             'how many items to simulate',
                                             'how many trials/item to simulate',
                                             'the minimum reachable (i.e. asymptotic) RT (seconds)',
                                             'the difference between the minimum RT and the maximum RT (sec)',
                                             'the learning rate (slightly different interpretation per function)',
                                             'overall residual standard deviation (from underlying fn\'s mean',
                                             'sd of subj, item, and subj-item components of alpha parameter*',
                                             'sd of subj, item, and subj-item components of beta parameter*',
                                             'sd of subj, item, and subj-item components of rate parameter*',
                                             'mean of algorithm process for ps function (ignored for de)',
                                             'sd of subj, item, and subj-item components of mu parameter*',
                                             'slope of logistic modeling P(retrieval) ~ trial',
                                             'sd of subj, item, and subj-item components of pRet_A parameter*',
                                             'midpoint of logistic modeling P(retrieval) ~ trial',
                                             'sd of subj, item, and subj-item components of pRet_B parameter*',
                                             'delay parameter for DE model',
                                             'sd of subj, item, and subj-item components of tau parameter*',
                                             '* Note that sds for subj-item interactions are scaled down by 50% (arbitrary)'))
        parameters
    })
    
    output$sub_item_plot <- renderPlot({
        RTs() %>% 
            drawPlot(input$fn)
    })
    
    output$data <- renderTable({
        RTs()
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0('generated_RTs_',input$fn, ".csv")
        },
        content = function(file) {
            write_csv(RTs, file, col_names = TRUE)
        }
    )
    output$downloadParams <- downloadHandler(
        filename = function() {
            'params'
        },
        content = function(file) {
            write_csv(parameters, file, col_names = TRUE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
