library(shiny)
library(tidyverse)
library(ggplot2)

job <- read_csv("https://github.com/hbnarayanan/stat479finalproject/raw/main/hr_analytics.csv")

non_useful <- c("EmployeeCount", "StandardHours", "EmployeeNumber", "DailyRate", "DistanceFromHome", "Education", "JobInvolvement", "JobSatisfaction", "JobSatisfaction", "PerformanceRating", "EnvironmentSatisfaction", "JobLevel", "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance", "HourlyRate", "MonthlyRate", "PercentSalaryHike", "TrainingTimesLastYear", "NumCompaniesWorked")

num_dat = select_if(job, is.numeric) %>% 
  select(-non_useful)

numeric_cols <- colnames(num_dat)

pca_data <- num_dat%>%
  prcomp(scale = TRUE)
PC <- data.frame(pca_data$x[,c(1,2)])
pca_data <- cbind(job, PC)


pca_plot <- function(data){
  pca_data <- data%>%
    prcomp(scale = TRUE)
  PC <- data.frame(pca_data$x[,c(1,2)])
  pca_data <- cbind(job, PC)
  
ggplot(pca_data, aes(PC1, PC2, col = Attrition, alpha = Attrition))+
  scale_alpha_discrete(range = c(0.3, 1))+
  geom_point() + 
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggtitle("PCA for most influential numerical features")
}


ui <- fluidPage(
    titlePanel("PCA test"),
    selectInput("dimensions", "Choose Numerical Variables for PCA",numeric_cols,selected = numeric_cols, multiple = T),
    plotOutput("PCA", brush = "brush"),
    dataTableOutput("selected_points")

  
    )

reset <- function(data, brush){
  
  brushedPoints(data, brush, allRows = T)$selected_
  
}

# Define server logic required to draw a histogram
server <- function(input, output) {

selected <- reactiveVal(T)
observeEvent(input$brush,
             {
               selected(reset(pca_data,input$brush))
               }
             )


    output$PCA <- renderPlot({
      pca_plot(pca_data 
               %>% select(input$dimensions))
    })
    
    output$selected_points <- renderDataTable(pca_data%>%
                                                filter(selected()) 
                                              %>% select(input$dimensions, everything()) %>%
                                              select(-c(PC1, PC2)) 
                                              
                                              
                                              )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
