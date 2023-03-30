library(shiny)
library(ggplot2)

iris <- read.csv("iris.csv")
# Define UI ----
ui <- fluidPage(
  titlePanel("Assignment"),
  
  fluidRow(
    
    column(6,
           h3("Sepal-length"),
           sliderInput(inputId = "bins1",
                       label = "Number of bins:",
                       min = 5,
                       max = 30,
                       value = 20),
           selectInput(inputId = "select1", label = "color", 
                       choices = list("Blue" = "blue", "Green" = "green", "Black" = "black","Pink"="pink"), 
                       selected = "blue"),
           plotOutput(outputId = "slPlot"),
           ),
    
    column(6,
           h3("Sepal-width"),
           sliderInput(inputId = "bins2",
                       label = "Number of bins:",
                       min = 5,
                       max = 30,
                       value = 20),
           selectInput(inputId = "select2", label = "color", 
                       choices = list("Blue" = "blue", "Green" = "green", "Black" = "black","Pink"="pink"), 
                       selected = "blue"),
           plotOutput(outputId = "swPlot")
    )
    
  
  ),
  hr(),
  fluidRow(
    
    column(6,
           h3("Petal-length"),
           sliderInput(inputId = "bins3",
                       label = "Number of bins:",
                       min = 5,
                       max = 30,
                       value = 20),
           selectInput(inputId = "select3", label = "color", 
                       choices = list("Blue" = "blue", "Green" = "green", "Black" = "black","Pink"="pink"), 
                       selected = "blue"),
           plotOutput(outputId = "plPlot")
    ),
    
    column(6,
           h3("Petal-width"),
           sliderInput(inputId = "bins4",
                       label = "Number of bins:",
                       min = 5,
                       max = 30,
                       value = 20),
           selectInput(inputId = "select4", label = "color", 
                       choices = list("Blue" = "blue", "Green" = "green", "Black" = "black","Pink"="pink"), 
                       selected = "blue"),
           plotOutput(outputId = "pwPlot")
    )
  ),
  hr(),
  h2("Scatterplot"),
  fluidRow(
    
    column(3,
             selectInput(inputId = "select5", label = "On x-axis", 
                         choices = list("Sepal length" = "sepal.length", "Sepal width" = "sepal.width", "Petal length" = "petal.length","Petal width"="petal.width"), 
                         selected = "sepal.length"),
             selectInput(inputId = "select6", label = "On y-axis", 
                         choices = list("Sepal length" = "sepal.length", "Sepal width" = "sepal.width", "Petal length" = "petal.length","Petal width"="petal.width"), 
                         selected = "sepal.width")
             
    ),
    column(2,
           selectInput(inputId = "specs", label = "Display :", 
                       choices = list("All"="All","Setosa"="Setosa","Versicolor"="Versicolor","Virginica"="Virginica","Setosa and Versicolor"="Setosa.Versicolor","Setosa and Virginica"="Setosa.Virginica","Versicolor and Virginica"="Versicolor.Virginica","None"="None"), 
                       selected = "All")),
    column(2,
           radioButtons("checkGroup1", label = "Color for Setosa", 
                              choices = list("Blue" = "blue", "Orange" = "orange", "Red" = "red","Black"="black","Pink"="pink","Green"="green"),
                              selected = "blue")),
    column(2,
           radioButtons("checkGroup2", label = "Color for Versicolor", 
                              choices = list("Blue" = "blue", "Orange" = "orange", "Red" = "red","Black"="black","Pink"="pink","Green"="green"),
                              selected = "orange")),
    column(2,
           radioButtons("checkGroup3", label = "Color for Virginica", 
                              choices = list("Blue" = "blue", "Orange" = "orange", "Red" = "red","Black"="black","Pink"="pink","Green"="green"),
                              selected = "red"))

  ),
  fluidRow(
    column(3,
           sliderInput(inputId = "shape1",
                       label = "Shape for Setosa:",
                       min = 0,
                       max = 25,
                       value = 16)),
    column(3,
           sliderInput(inputId = "shape2",
                       label = "Shape for Versicolor:",
                       min = 0,
                       max = 25,
                       value = 16)),
    column(3,
           sliderInput(inputId = "shape3",
                       label = "Shape for Virginica:",
                       min = 0,
                       max = 25,
                       value = 16)),
    column(3, 
           h5("Help for shape"),
           helpText("Note: Each integer on the slider ", 
                    "corresponds to a particular shape",
                    "in ggplot."))
    
  ),
  fluidRow(
    column(3,
           sliderInput(inputId = "size1",
                       label = "Size for Setosa:",
                       min = 1,
                       max = 5,
                       value = 2)),
    column(3,
           sliderInput(inputId = "size2",
                       label = "Size for Versicolor:",
                       min = 1,
                       max = 5,
                       value = 2)),
    column(3,
           sliderInput(inputId = "size3",
                       label = "Size for Virginica:",
                       min = 1,
                       max = 5,
                       value = 2)),
    
  ),
  plotOutput(outputId = "scatterPlot"),
  hr(),
  h2("Box-Violin plot"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput(inputId = "s1", label = "Display :", 
                  choices = list("Box plot"="box","Violin plot"="violin"), 
                  selected = "box"),
      selectInput(inputId = "s2", label = "Select Feature", 
                  choices = list("Sepal length" = "sepal.length", "Sepal width" = "sepal.width", "Petal length" = "petal.length","Petal width"="petal.width"), 
                  selected = "sepal.length")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(

      plotOutput(outputId = "vioboxPlot")
      
    )
  )
)

# Define server logic ----

server <- function(input, output) {
  
  output$slPlot <- renderPlot({
    
    x    <- iris$sepal.length
    bins <- seq(min(x), max(x), length.out = input$bins1 + 1)
    
    hist(x, breaks = bins, col = input$select1, border = "white",
         xlab = "Sepal length",
         main = "Histogram of sepal length")
    
  })
  output$swPlot <- renderPlot({
    
    x    <- iris$sepal.width
    bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
    
    hist(x, breaks = bins, col = input$select2, border = "white",
         xlab = "Sepal width",
         main = "Histogram of sepal width")
    
  })
  output$plPlot <- renderPlot({
    
    x    <- iris$petal.length
    bins <- seq(min(x), max(x), length.out = input$bins3 + 1)
    
    hist(x, breaks = bins, col = input$select3, border = "white",
         xlab = "Petal length",
         main = "Histogram of petal length")
    
  })
  output$pwPlot <- renderPlot({
    
    x    <- iris$petal.width
    bins <- seq(min(x), max(x), length.out = input$bins4 + 1)
    
    hist(x, breaks = bins, col = input$select4, border = "white",
         xlab = "Petal width",
         main = "Histogram of petal width")
    
  })
  output$scatterPlot <- renderPlot({
    

    cols1 <- c(input$checkGroup1,input$checkGroup2,input$checkGroup3)
    
    if (input$specs == c("Setosa"))
      cols1 <-c(input$checkGroup1,"transparent","transparent")
    else if(input$specs == c("Versicolor"))
      cols1 <- c("transparent",input$checkGroup2,"transparent")
    else if(input$specs == c("Virginica"))
      cols1 <- c("transparent","transparent",input$checkGroup3)
    else if (input$specs == c("Setosa.Versicolor"))
      cols1 <- c(input$checkGroup1,input$checkGroup2,"transparent")
    else if (input$specs == c("Setosa.Virginica"))
      cols1 <- c(input$checkGroup1,"transparent",input$checkGroup3)
    else if (input$specs == c("Versicolor.Virginica"))
      cols1 <- c("transparent",input$checkGroup2,input$checkGroup3)
    else if (input$specs == "None")
      cols1 <- c("transparent","transparent","transparent")
    
    cols2 <- c(input$shape1,input$shape2,input$shape3)

    cols3 <-c(input$size1,input$size2,input$size3)
    
    ggplot(iris, aes_string(x = input$select5, y = input$select6, color = "variety")) + 
      geom_point(aes(size=variety,shape=variety, color=variety)) + 
      ggtitle(paste("Scatterplot of", input$select5, "vs", input$select6)) +
      xlab(input$select5) + 
      ylab(input$select6) + 
      theme(legend.position = "right")+
      scale_color_manual(values = cols1)+
      scale_shape_manual(values = cols2)+
      scale_size_manual(values = cols3)
  })
  
  output$vioboxPlot <- renderPlot({
    if(input$s1 =="box")
      ggplot(iris,aes_string(x = "variety", y = input$s2, fill = "variety")) +
      geom_boxplot()
    else if (input$s1=="violin")
      ggplot(iris,aes_string(x = "variety", y = input$s2, fill = "variety")) +
      geom_violin()+
      geom_boxplot(width = 0.1, fill = "white", alpha = 0)

  })
  
}  

# Run the app ----
shinyApp(ui = ui, server = server)