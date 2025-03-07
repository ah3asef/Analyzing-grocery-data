#Packages
library("arules")
library("shiny")
library("shinythemes")
library("shinydashboard")
ui <- fluidPage(
  titlePanel("Project Data Science"),
  #Tabs Has The The main topics like k-means apriori Rule.
  tabsetPanel(
    tabPanel("Data Cleaning",#Start with Data Cleaning.
             sidebarLayout(
               sidebarPanel(
                 fileInput("file",#First Input DataSet.
                           label = h3("Please Drag The Data Set"),
                           accept = ".csv",
                           width = "400px",
                           buttonLabel = "Drag",
                           placeholder = "Please enter csv file"),
                 hr(),
                 width = 10
               ),
               mainPanel(
                 h1("Information About DataSet"),
                 tags$hr(),
                 h3("Showing Data Set"),
                 fluidRow(column(12,tableOutput("DataSet"))),
                 tags$hr(),
                 h3("Showing The Duplicated Rows And NA Values"),
                 fluidRow(column(2,verbatimTextOutput("NA_Value")),column(2,verbatimTextOutput("Duplicated01"))),#end row3,
                 tags$hr(),
                 h3("Summary For Each Nummeric Column After Cleaning"),              
                 #Showing The Outlier Of Count And Age And rnd.
                 navlistPanel(
                   id = "outliers",
                   tabPanel(
                     "Age",
                     plotOutput("age")
                   ),
                   tabPanel(
                     "Count",
                     plotOutput("count")
                   ),
                   tabPanel(
                     "RND",
                     plotOutput("rnd")
                   )
                 ),
                 width = 10 
               ),#the end of mainpanel.(Data Cleaning)
               position = c("left","right")
             )#the end of sidebarlayout.(Data Cleaning)
    ),
    #end of dataVisualization
    tabPanel(#Third K-means Clusting.
      "K-Means Clustering",
      sidebarLayout(
      sidebarPanel(
            sliderInput("slider1", label = h3("Please Enter Number Of Centers:"), min = 2, max = 4, value = 3)
            ),
      mainPanel(
        fluidRow(column(12,tableOutput('kmean')))
      )#End Of main Panel.(kmeans)
      ),
      ),#the end of kmeans clustering
    tabPanel(#Fourth Apriori Rule.
      "Apriori Rule",
      navlistPanel(
        tabPanel(
          "Showing Transactions",
          verbatimTextOutput("transactions")
        ),
        tabPanel(
          "Showing Rules Table",
          sidebarLayout(
            sidebarPanel(
              sliderInput("supp", label = h3("Please Enter The Minimum Support:"), min = 0.001, max = 1, value = 0.1),
              sliderInput("conf", label = h3("Please Enter The Minimum Confidence:"), min = 0.001, max = 1, value = 0.1),
            ),
            mainPanel(
              tableOutput("arules")
            )
          )
        ),
        tabPanel(
          "Absloute Plotting",
          plotOutput("abs")
        ),
        tabPanel(
          "Realative Plotting",
          plotOutput("rela")
        )
    )
     ),#The End Of Apriori Rule.
    tabPanel("Dashboard",
             dashboardPage(
               dashboardHeader(title = "Visualizations"),
               dashboardSidebar(
                 radioButtons("ButtonVisualization", label = h3("Arrange Bars And Scatters"),
                              choices = list("Increasignly" = 1, "Decreasignly" = 2,"Randomly" = 3), 
                              selected = 2),
                 tags$hr(),
                 radioButtons("boxHist", label = h3("The Distribution Of Total Spending"),
                              choices = list("Histogram" = 1, "Box Plot" = 2), 
                              selected = 2)
               ),
               dashboardBody(
                   box(plotOutput("Scatter", height = 250)),
                   box(plotOutput("bar",height = 250)),
                   box(plotOutput("pie", height = 250)),
                   box(plotOutput("box", height = 250))
               )
             )
             )
  ),#The End Of TabSet Panel
  theme =shinytheme("darkly")
)#end ui
server <- function(input,output){
  grc1 <-reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  grc2 <- reactive({
    req(input$file)
    unique(read.csv(input$file$datapath))
  })
  read02 <- reactive({
    req(grc2())
    read.transactions(textConnection(grc2()$items),sep =",")
  })
  #The First Section(Data Cleaning & Showing Data)
  output$DataSet <-renderTable({
       req(grc2())
       head(grc2(),5)
  })
  output$NA_Value <- renderPrint({ 
      req(grc2())
      paste0("The NA Values: ",sum(is.na(grc1())))
  })
  output$Duplicated01 <- renderPrint({
    req(grc2())
    paste0("Duplicated Rows: ",sum(duplicated(grc1())))
  })
  output$age <- renderPlot({
      req(grc2())
      boxplot(grc2()$age, main ="Showing the Outlier Of Age")
  })
  output$count <- renderPlot({
      req(grc2())
      boxplot(grc2()$count,main = "Show the Distribution Of The Count")
  })
  output$rnd <- renderPlot({
      req(grc2())
      boxplot(grc2()$rnd,main = "Showing Research and Development")
  })
  #The End Of First Section.
  ####################################################################
  ####################################################################
  #Second Section(Data Visulization)
  #No.1 Compare Between Cash And Credit.
  output$pie <- renderPlot({ 
      req(grc2())
      x0 <- table(grc2()$paymentType)
      precentage0 <- paste0(round(100*x0/sum(x0), digits = 2),"%")
      pie(x0,labels = precentage0,main = "Compare cash and credit totals",col = c("cyan","pink"))
      legend("topright",legend = c("cash","credit"),fill = c("cyan","pink")) 
    
  })
  output$buttonVisual  <- renderPrint({input$ButtonVisualization})
  ###############################################################
  #No.2 Bar Visualization For Age And Total Spending.
  output$Scatter<- renderPlot({
     req(grc2())
     ages<-sort(unique(grc2()$age))
      total<-as.numeric(vector())
      for(age in ages){
        x=sum(grc2()$total[grc2()$age==age])
        total<-append(total,x)}
      age_total<-data.frame(ages, total)
      if(input$ButtonVisualization == 1){
        age_total <- age_total[order(age_total$total,decreasing =FALSE),]
        plot(
          age_total$total,
          type ="b",
          ylab = "Total Spending",
          col = "blue",
          xaxt = "n",
          xlab = "Ages",
        )
        axis(1, at=1:12, labels=as.character(age_total$ages))
      }#End Of Increasing
      if(input$ButtonVisualization == 2){
      age_total <- age_total[order(age_total$total,decreasing =TRUE),]
      plot(
        age_total$total,
        type ="b",
        ylab = "Total Spending",
        xaxt = "n",
        col = "red",
        xlab = "Ages",
      )
      axis(1, at=1:12, labels=as.character(age_total$ages))
        }#End Of Decreasing
      else if(input$ButtonVisualization == 3){
        plot(
          age_total$total,
          type ="b",
          ylab = "Total Spending",
          xaxt = "n",
          col ="black",
          xlab = "Ages",
        )
        axis(1, at=1:12, labels=as.character(age_total$ages))
      }#End Of Random
    
  })#End Of No.2
  ###################################################################
  #No.3 Comparing Between Every City And Total Spending.
  output$bar<- renderPlot({ 
      req(grc2())
      cities <-unique(grc2()$city)
      total_Spending <-as.numeric(vector())
      for(city in cities){
        y =sum(grc2()$total[grc2()$city == city])
        total_Spending <-append(total_Spending,y)
      }
      total_cities <- data.frame(total_Spending,cities)
      if(input$ButtonVisualization ==1){
        total_cities <- total_cities[order(total_Spending,decreasing = FALSE),]
        barplot(
          height= total_cities$total_Spending,
          names= total_cities$cities,
          col = "blue",
          main = "Compare the Sum Of total spending and cities decreasingly",
          ylab = "Total Spending",
          las = 3, 
          cex.names = 1
        )

      }#End Of Increasing
      else if(input$ButtonVisualization ==2){
        total_cities <- total_cities[order(total_Spending,decreasing = TRUE),]
        barplot(
          height= total_cities$total_Spending,
          names= total_cities$cities,
          col = "red",
          main = "Compare the Sum Of total spending and cities decreasingly",
          ylab = "Total Spending",
          las = 3, 
          cex.names = 1
        )
      }#End Of Decreasing.
      else if(input$ButtonVisualization ==3 ){
        barplot(
          height= total_Spending,
          names= cities,
          col = "black",
          main = "Compare the Sum Of total spending and cities decreasingly",
          ylab = "Total Spending",
          las = 3, 
          cex.names = 1
        )
      }#End Of Random
  })#End Of No.3
  ############################################################################
  #No.4 The Distibution Of Total Spending.
  output$box <- renderPlot({
      req(grc2())
      if(input$boxHist == 1){
      hist( 
        x  = grc2()$total,
        col = 'red',
        border = 'blue',
        main = "The Distibution Of Total Spending",
        xlab = "total_Spending",
        ylab = "frequently"
      )
      }#End Of Histogram
      else if(input$boxHist == 2){
        boxplot(grc2()$total,
            main = "The Distribution Of Total Spending"
            )
      }
    })#The End Of the
###############################################################################
###############################################################################
  #Section Three K-means Clustering.
  output$kmean <- renderTable({
         req(grc2())
         data_kmeans <- data.frame(
           Age = grc2()$age,
           Total = grc2()$total
         )
         kmeans_index <- kmeans(data_kmeans,centers = input$slider1) 
         displaying_kmeans <- data.frame(
           age = grc2()$age,
           total =  grc2()$total,
           customer =  grc2()$customer,
           cluster = kmeans_index$cluster
         )
     })#End Of K-Means  
###############################################################################
###############################################################################
#Section Four Apriori Rule
  #No.1 Showing Rules.
 output$arules <- renderTable({
    req(grc2())
    association_items2 <- apriori(
      read02(),
      parameter = list(support = as.numeric(input$supp),
                       confidence = as.numeric(input$conf),
                       maxlen = 10,maxtime = 10, minlen=2),
      control = list(memopt = TRUE,load = FALSE)
    )
    inspect(association_items2)
})#End Of No.1
#No.2 Plotting With Absolute Value  
output$abs <- renderPlot({
    req(grc2())
    itemFrequencyPlot(read02(),topN= 5,type ="absolute")
})#End Of No.2
#No.3 Plotting With Relative Values
output$rela <- renderPlot({
    req(grc2())
    itemFrequencyPlot(read02(),topN= 5,type ="relative")
})#End of No.3

#No.4 Showing Transactions
output$transactions <- renderPrint({
    req(grc2())
    inspect(read02())
})#End Of No.4
################################################################################
################################################################################
}#end server

shinyApp(ui = ui,server =server)
