#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SHINY APP ASSIGNMENT"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            #Input: Checkbox for LM model
          
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
       
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            actionButton("DO", "PLOT LINEAR MODEL AND STATS")
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
            tableOutput("contents"),
            textOutput("values")
           
    
        
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    lmdata <- reactiveValues()
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })

    observeEvent(input$DO,{
        update_lm()
        update_stats()
        lmplot()
    })
    update_lm <- function(){
        lmdata$model <- lm(y~x, data = dataInput())
        
       
        }
    update_stats <- function(){
        model <- lm(y~x, data = dataInput())
        model_sum <- summary(model)
        r <- model_sum$r.squared
        cf <- coef(model)
        lmdata$model <- model
        lmdata$model_sum <- model_sum
        lmdata$r <- r
        lmdata$inter <-cf[1]
        lmdata$slope <-cf[2]
        
        
       
        }
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y,xlab = 'X', ylab ='Y')
    })

    lmplot <- function(){
        output$lmPlot <- renderPlot({
            plot(dataInput()$x,dataInput()$y,xlab = 'X', ylab ='Y')
            title(main= 'Linear model', sub= paste( "Correlation Coefficient = ", lmdata$r, " ; ", "Intercept= ", lmdata$inter, " ; ", "Slope = ", lmdata$slope))
           abline(lmdata$model,  col = 'red')
        })
    }
    
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

    
# Run the application 
shinyApp(ui = ui, server = server)
