library(shiny)
#library(tidyverse)
library(daff)

# UI
ui <- fluidPage(
    
    navbarPage("Are we alike?",
               # PAGE 1 ----
               tabPanel("1. Basic Comparison",
                        sidebarLayout(
                            # Input(s) for Introduction ----
                            sidebarPanel(
                                h3("Welcome!"),
                                p("Select the two datasets you want to compare, the two datasets should be saved in csv format."),
                                
                                # Horizontal line ----
                                tags$hr(),
                                fileInput("file","Upload the CSV Files",
                                          multiple = TRUE,
                                          accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: check if the file has header ----
                                checkboxInput(inputId = "header",label = "Has a header?",value = TRUE),
                                checkboxInput(inputId = "stringAsFactors1","stringAsFactors for Data1", FALSE),
                                checkboxInput(inputId = "stringAsFactors2","stringAsFactors for Data2", FALSE),
                                radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
#                                numericInput("rounding", label = h3("the Number of Decimal Digits"), value = 2),
                                tags$hr()),
                            
                            mainPanel(
                                uiOutput("tb")
                                )
                            )
                        ),
               # PAGE 2 ----
               tabPanel("2. Advanced Comparison",
                        sidebarLayout(
                            sidebarPanel(
                                p("Let's find the exact differences!"),
                                tags$hr()),
                            mainPanel(
                                uiOutput("diff")
                                
               )
               )
               ),
               
               # PAGE 3
               tabPanel("3. More to come...")
    )
)



# Server ------------------------------------------------------
server <- function(input, output, session) {
    ## input$file is a data frame and contains the details around the name, size and temp location of the files uploaded
    # this reactive output display the content of the input$file dataframe
    output$filedf <- renderTable({
        if(is.null(input$file)){return ()}
        input$file # the file input data frame object that contains the file attributes
    })

    
    ## Below code to display the structure of the input file object
    output$fileob <- renderPrint({
        if(is.null(input$file)){return ()}
        str(input$file)
    })
    
    ## Side bar select input widget coming through renderUI()
    # Following code displays the select input widget with the list of file loaded by the user
    # output$selectfile <- renderUI({
    #     if(is.null(input$file)) {return()}
    #     list(hr(), 
    #          helpText("Select the files for which you need to see data and summary stats"),
    #          selectInput("Select", "Select", choices=input$file$name)
    #     )
    #     
    # })
    # Parse the 1st file
    dataset1 <- reactive({
        if(is.null(input$file)){return ()}
        data <- read.table(file=input$file$datapath[1],
                           header = input$header,
                           sep=input$sep,
                           stringsAsFactors = input$stringAsFactors1)
        data
    })
    # Parse the 2nd file
    dataset2 <- reactive({
        if(is.null(input$file)){return ()}
        data <- read.table(file=input$file$datapath[2],
                           header = input$header,
                           sep=input$sep,
                           stringsAsFactors = input$stringAsFactors1)
        data
    })
    
    ## Summary Stats code ##
    # this reactive output contains the summary of the dataset and display the summary in table format
    output$summ1 <- renderPrint({
        if(is.null(input$file)){return()}
        skimr::skim(dataset1())
        })
    
    output$summ2 <- renderPrint({
        if(is.null(input$file)){return()}
        skimr::skim(dataset2())
    })
    
    ## Dataset code ##
    # This reactive output contains the dataset and display the dataset in table format
    output$table1 <- DT::renderDataTable({ 
        if(is.null(input$file)){return()}
        DT::datatable(data = dataset1(), 
                      options = list(pageLength = 10), 
                      rownames = TRUE)
    })
    
    output$table2 <- DT::renderDataTable({ 
        if(is.null(input$file)){return()}
        DT::datatable(data = dataset2(), 
                      options = list(pageLength = 10), 
                      rownames = TRUE)
    })
    
    data_diff <- reactive({
        x <- render_diff(diff_data(data_ref=dataset1(), data=dataset2()))
        x
    })
    
    ## MainPanel tabset renderUI code ##
    # the following renderUI is used to dynamically generate the tabsets when the file is loaded. 
    # Until the file is loaded, app will not show the tabset.
    output$tb <- renderUI({
        if(is.null(input$file)) {return()}
        else
            tabsetPanel(
                tabPanel("Basic Info", tableOutput("filedf"),verbatimTextOutput("fileob")),
                tabPanel("View", DT::dataTableOutput("table1"),tags$hr(),DT::dataTableOutput("table2")),
                tabPanel("Summary Stats", verbatimTextOutput("summ1"),tags$hr(),verbatimTextOutput("summ2")))
    })
    
    output$diff <- renderUI({
        if(is.null(input$file)) {return()}
        else
            tabsetPanel(
                tabPanel("Differences",HTML(data_diff())))
    })
    
}  


# Create a Shiny app object
shinyApp(ui = ui, server = server)