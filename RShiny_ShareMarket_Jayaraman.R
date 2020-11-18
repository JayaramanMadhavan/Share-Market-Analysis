#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
Stock<-read.csv("C:/Users/Jayaraman/Desktop/Master of Business Analytics-Australia/Semester 3/Visual Analytics (VA)/Assignment-2/stocks.csv")
Stock$Year <- as.Date(Stock$date)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(HTML("<font color='#0000FF' size='8'>SHARE MARKET ANALYSIS</font>")),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("select", label = "Select Company you prefer", 
                           choices = list("Apple" = "Apple",
                                          "Alibaba" ="Alibaba",
                                          "Amazon" = "Amazon",
                                          "Facebook"="Facebook",
                                          "Google"="Google",
                                          "Intel"="Intel",
                                          "Microsoft"="Microsoft",
                                          "SAP"="SAP"),
                           selected = "Apple"),
        selectInput("select1", label = "Select the medium of analysis:", 
                    choices = list("closePrice" = "closePrice",
                                   "volume" ="volume"),
                    selected = "closePrice"),
        
        checkboxGroupInput("Variable", label = "Select Companies to compare:", 
                    choices = unique(Stock$company),
                    selected = 'Alibaba')
        
                       
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        fluidRow(
          column(width = 12,
                 HTML("<font color='#CC3300' size='5'>Variation of company shares over years</font>"),
                 plotOutput("distPlot",height=250)
                 ),
          fluidRow(
            column(width = 12,
                   HTML("<font color='#CC0099' size='5'>Comparison in variation of different company shares over years</font>"),
                   plotOutput("SecondPlot",height=250))
          )
        
          )
      
        
        )
      )
   

)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$distPlot<-renderPlot({
     if(input$select=="Apple"){
       ggplot(data = subset(Stock,company=='Apple'),aes_string(x='Year',y=input$select1,group=1))+
         geom_line(color="#FF6600")+
         xlab("Years")+
         ylab("Closing Price")+
         theme(
           axis.title.x = element_text(color="red", size=12, face="bold"),
           axis.title.y = element_text(color="#FF0099", size=12, face="bold"),
           panel.background = element_rect(fill = "white",
                                           colour = "#0000FF",
                                           size = 0.7, linetype = "solid")
         )
         }
       else 
         if(input$select=="Alibaba"){
       ggplot(data = subset(Stock,company=='Alibaba'),aes_string(x='Year',y=input$select1,group=1))+
         geom_line(color="#FF6600")+
             xlab("Years")+
             ylab("Closing Price")+
             theme(
               axis.title.x = element_text(color="red", size=12, face="bold"),
               axis.title.y = element_text(color="#FF0099", size=12, face="bold"),
               panel.background = element_rect(fill = "white",
                                               colour = "#0000FF",
                                               size = 0.7, linetype = "solid")
             )}
     else
       if(input$select=="Amazon"){
         ggplot(data = subset(Stock,company=='Amazon'),aes_string(x='Year',y=input$select1,group=1))+
           geom_line(color="#FF6600")+
           xlab("Years")+
           ylab("Closing Price")+
           theme(
             axis.title.x = element_text(color="red", size=12, face="bold"),
             axis.title.y = element_text(color="#FF0099", size=12, face="bold"),
             panel.background = element_rect(fill = "white",
                                             colour = "#0000FF",
                                             size = 0.7, linetype = "solid")
           )}
     else
       if(input$select=="Facebook"){
         ggplot(data = subset(Stock,company=='Facebook'),aes_string(x='Year',y=input$select1,group=1))+
           geom_line(color="#FF6600")+
           xlab("Years")+
           ylab("Closing Price")+
           theme(
             axis.title.x = element_text(color="red", size=12, face="bold"),
             axis.title.y = element_text(color="#FF0099", size=12, face="bold"),
             panel.background = element_rect(fill = "white",
                                             colour = "#0000FF",
                                             size = 0.7, linetype = "solid")
           )}
     else
       if(input$select=="Google"){
         ggplot(data = subset(Stock,company=='Google'),aes_string(x='Year',y=input$select1,group=1))+
           geom_line(color="#FF6600")+
           xlab("Years")+
           ylab("Closing Price")+
           theme(
             axis.title.x = element_text(color="red", size=12, face="bold"),
             axis.title.y = element_text(color="#FF0099", size=12, face="bold"),
             panel.background = element_rect(fill = "white",
                                             colour = "#0000FF",
                                             size = 0.7, linetype = "solid")
           )}
     else
       if(input$select=="Intel"){
         ggplot(data = subset(Stock,company=='Intel'),aes_string(x='Year',y=input$select1,group=1))+
           geom_line(color="#FF6600")+
           xlab("Years")+
           ylab("Closing Price")+
           theme(
             axis.title.x = element_text(color="red", size=12, face="bold"),
             axis.title.y = element_text(color="#FF0099", size=12, face="bold"),
             panel.background = element_rect(fill = "white",
                                             colour = "#0000FF",
                                             size = 0.7, linetype = "solid")
           )}
     else
       if(input$select=="Microsoft"){
         ggplot(data = subset(Stock,company=='Microsoft'),aes_string(x='Year',y=input$select1,group=1))+
           geom_line(color="#FF6600")+
           xlab("Years")+
           ylab("Closing Price")+
           theme(
             axis.title.x = element_text(color="red", size=12, face="bold"),
             axis.title.y = element_text(color="#FF0099", size=12, face="bold"),
             panel.background = element_rect(fill = "white",
                                             colour = "#0000FF",
                                             size = 0.7, linetype = "solid")
           )}
     else
       if(input$select=="SAP"){
         ggplot(data = subset(Stock,company=='SAP'),aes_string(x='Year',y=input$select1,group=1))+
           geom_line(color="#FF6600")+
           xlab("Years")+
           ylab("Closing Price")+
           theme(
             axis.title.x = element_text(color="red", size=12, face="bold"),
             axis.title.y = element_text(color="#FF0099", size=12, face="bold"),
             panel.background = element_rect(fill = "white",
                                             colour = "#0000FF",
                                             size = 0.7, linetype = "solid")
           )}
         })
   
   output$SecondPlot<-renderPlot({
     ggplot(data = subset(Stock, company == input$Variable), aes_string(x='Year',y=input$select1, color = 'company')) +
       geom_line()+
       xlab("Years")+
       ylab("Closing Price")+
       theme(
         axis.title.x = element_text(color="red", size=12, face="bold"),
         axis.title.y = element_text(color="#FF0099", size=12, face="bold"),
         panel.background = element_rect(fill = "white",
                                         colour = "black",
                                         size = 0.7, linetype = "solid")
       )
   })
   
   
      
     
}

# Run the application 
shinyApp(ui = ui, server = server)

