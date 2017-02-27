server <- function(input, output) {
  output$contents <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df<-read.csv(inFile$datapath, header = input$header)
    dfNames<-names(df)
    fluidPage(
      sidebarPanel(
        checkboxGroupInput("Variables", "choose", dfNames)
      ),mainPanel(
        tabsetPanel(
          tabPanel("Histogram", 
                   output$data <- renderPlot({
                     validate(
                       need(input$Variables != "", "Please select a Varible")
                     )
                     y<-df[,c(input$Variables)]
                     hist(y)
                   })
          ), 
          tabPanel("Pi chart", 
                   output$data <- renderPlot({
                     validate(
                       need(input$Variables != "", "Please select a Varible")
                     )
                     y<-df[,c(input$Variables)]
                     slices<-unique(y)
                     lbls<-unique(y)
                     pie(slices, labels = lbls, main="Pie Chart of Countries")
                   })
          ), 
          tabPanel("Table", 
                   output$data<-renderTable({
                     validate(
                       need(input$Variables != "", "Please select a Varible")
                     )
                     y<-df[,c(input$Variables)]
                   })
                   
          )
        )
      )
    )
  })
}