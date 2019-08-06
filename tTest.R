library(shiny)
library(readxl)

ui <- fluidPage(
  
  titlePanel("T-TEST"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var1","Select Test",c("One Sample T-Test","Two Sample T-Test")),
      fileInput("uploadFile", "Data file",multiple = F),
      textInput("var3","Significant Level",value = "0.05"),
      selectInput("var4","Select Test Type",c("Two-Tailed"='two.sided',"Less"='less', "Greater"='greater')),
      textInput('var5',"Hypothesis Mean",value =0.0)
    ),
    
    mainPanel(
      conditionalPanel(condition = "input.var1=='One Sample T-Test'",
                       h1("One Sample T-Test"),
                       h3('Applying Shapiro Test'),
                       verbatimTextOutput('ver1'),
                       textOutput('text1'),
                       plotOutput('plot1'),
                       h3('Applying T-Test'),
                       verbatimTextOutput('ver2'),
                       textOutput('text2'),
                       h3('Conclusion'),
                       h4(textOutput('text3'))
                       
                       
      ),
      
      conditionalPanel(condition = "input.var1=='Two Sample T-Test'",
                       h1("Two sample T-Test"),
                       h3("Data"),
                       verbatimTextOutput("summary"),
                       verbatimTextOutput("verb21"),
                       verbatimTextOutput("verb21.2"),
                       h3("Check the normality"),
                       h5("Shapiro-Wilks test is used to check the normality of the data."),
                       verbatimTextOutput("verb22"),
                       verbatimTextOutput("verb22.2"),
                       textOutput("text211"),
                       textOutput("text211.2"),
                       # Visualize data using box plots
                       h3("Graphical representation of DATA"),
                       h4("Visualize data using box plots"),
                       plotOutput("plot23"),
                       plotOutput("plot23.2"),
                       # Performing T-Test
                       h3("Performing T-Test"),
                       verbatimTextOutput("verb24"),
                       textOutput("textstatistics"),
                       h3('Conclusion'),
                       h4(textOutput("text212"))
                       )
    )
  )  
  )

server <- function(input, output) {
  
  dataset<-reactive({ 
    inFile <- input$uploadFile
    dat<-read_excel(inFile$datapath)
    return(dat)
  })
  
  data1 <- reactive(dataset()$Sample1)

  sh <- reactive(shapiro.test(as.numeric(data1())))
  output$ver1 <- renderPrint(sh())
  
  output$text1 <- renderPrint(cat(sprintf('P-value= %0.5f, As p-value is greater than 0.05,  sample belongs to the normal distribution.', sh()$p.value)))

  output$plot1 <- renderPlot({qqnorm(data1())
    qqline(data1())})

  t_result <- reactive(t.test(data1(),mu = as.numeric(input$var5),alternative = input$var4,conf.level = as.numeric(input$var3)))
  output$ver2 <- renderPrint({
                t_result()
                    })

  output$text2 <- renderPrint({
    if(t_result()$p.value >=as.numeric(input$var3)){
            cat(sprintf('P-value= %0.5f\n and t-statistic= %0.5f,\n As p-value obtained for t-test is greater than %0.3f, we are accepting the NULL hypothesis.', t_result()$p.value,t_result()$statistic,as.numeric(input$var3)))
    }
    else{
      cat(sprintf('P-value= %0.5f and t-statistic= %0.5f, As p-value obtained for t-test is smaller than %0.3f, we are rejecting the NULL hypothesis.', t_result()$p.value,t_result()$statistic,as.numeric(input$var3)))
    }

    })

  output$text3 <- renderPrint({
    if(t_result()$p.value >=as.numeric(input$var3)){
      cat(sprintf('NULL HYPOTHESIS ACCEPTED!!'))
    }
    else{
      cat(sprintf('NULL HYPOTHESIS REJECTED!!'))
    }

  })


  ########################## Two-Sample Test ############################

  # Intrductory Data for Two Sample T-Test
  output$text1 <- renderPrint(
    cat("The two-sample t-test is one of the most commonly used hypothesis tests in Six Sigma work. It is applied to compare whether the average difference between two groups is really significant or if it is due instead to random chance. It helps to answer questions like whether the average success rate is higher after implementing a new sales tool than before or whether the test results of patients who received a drug are better than test results of those who received a placebo.")
  )
  # DATA_01 <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
  # DATA_02 <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)
  my_data <- reactive(data.frame(Sample1 = dataset()$Sample1,Sample2 = dataset()$Sample2))
  DATA_01 <- reactive(my_data()$Sample1)
  DATA_02 <- reactive(my_data()$Sample2)
  
  # Performing Two Sample T-Test
  # 1. Data
  output$verb21 <- renderPrint({
    cat("\nSummary for Data1\n")
    summary(DATA_01())})
  output$verb21.2 <- renderPrint({
    cat("\nSummary for Data2\n")
    summary(DATA_02())
    })

  # 2.Check the normality
  DATA_01.shapario <- reactive(shapiro.test(as.numeric(DATA_01())))
  DATA_02.shapario <- reactive(shapiro.test(as.numeric(DATA_02())))
  output$verb22 <- renderPrint(
    DATA_01.shapario())
  output$verb22.2 <- renderPrint(
    DATA_02.shapario())

  output$text211 <- renderPrint({
      cat(sprintf("P-value for shapario Test for Data_01: %f\n",DATA_01.shapario()$p.value))
      if(DATA_01.shapario()$p.value>=as.numeric(input$var3))
        {cat(sprintf("The p-value of the Shapiro-Wilk Normality test is not less than %0.03f, we fail to reject the null hypothesis about the nomality.",as.numeric(input$var3)))}
      else
        {cat(sprintf("The p-value of the Shapiro-Wilk Normality test is less than %0.03f, we fail to accept the null hypothesis about the nomality.",as.numeric(input$var3)))}
      # Data - 02
      cat(sprintf("\nP-value for shapario Test for Data_02: %f\n",DATA_02.shapario()$p.value))
      if(DATA_02.shapario()$p.value>=as.numeric(input$var3))
        {cat(sprintf("The p-value of the Shapiro-Wilk Normality test is not less than %0.03f, we fail to reject the null hypothesis about the nomality.",as.numeric(input$var3)))}
      else
        {cat(sprintf("The p-value of the Shapiro-Wilk Normality test is less than %0.03f, we fail to accept the null hypothesis about the nomality.",as.numeric(input$var3)))}
    })

  output$plot23 <- renderPlot({qqnorm(DATA_01())
    qqline(DATA_01())})
  output$plot23.2 <- renderPlot({qqnorm(DATA_02())
    qqline(DATA_02())})

  Result1 <- reactive(t.test(x = DATA_01(),y = DATA_02(),mu = as.numeric(input$var5),alternative = input$var4,conf.level = as.numeric(input$var3)))
  output$verb24 <- renderPrint({
    Result1()
  })

  output$textstatistics <- renderPrint({
    cat(sprintf('The test statistic is %0.04f and the p-value is %0.04f',Result1()$statistic,Result1()$p.value))
    if(Result1()$p.value>=as.numeric(input$var3))
    {cat(sprintf(". As the p-value is greater than %0.03f (level of significance) we accept the null hypothesis at %0.03f level of significance.",as.numeric(input$var3),as.numeric(input$var3)))}
    else
    {cat(sprintf(". As the p-value is less than %0.03f (level of significance) we reject the null hypothesis at %0.03f level of significance.",as.numeric(input$var3),as.numeric(input$var3)))}
  })
  output$text212 <- renderPrint({
    if(Result1()$p.value>=as.numeric(input$var3))
    {
      cat(sprintf('NULL HYPOTHESIS ACCEPTED!!!'))
    }
    else{
      cat(sprintf('NULL HYPOTHESIS REJECTED!!!'))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

