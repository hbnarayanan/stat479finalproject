library(shiny)
library(tidyverse)
library(ggplot2)
library(data.table)
library(mltools)
library(scales)
library(plotly)


job <- read_csv("https://github.com/hbnarayanan/stat479finalproject/raw/main/hr_analytics.csv")
char <- sapply(job, is.character)
cat <- names(job[,char])
cat <- c(cat, "PerformanceRating", "WorkLifeBalance","StockOptionLevel","RelationshipSatisfaction","JobSatisfaction","JobLevel","JobInvolvement","EnvironmentSatisfaction","Education")
cat <- setdiff(cat, c("Over18"))
job[cat] <- lapply(job[cat], factor)
non_useful <- c("EmployeeCount", "StandardHours", "EmployeeNumber", "DailyRate", "DistanceFromHome", "Education", "JobInvolvement", "JobSatisfaction", "JobSatisfaction", "PerformanceRating", "EnvironmentSatisfaction", "JobLevel", "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance", "HourlyRate", "MonthlyRate", "PercentSalaryHike", "TrainingTimesLastYear", "NumCompaniesWorked")
non_useful <- setdiff(non_useful, cat)
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

cat_lolly <- function(data){
  data_gini <- gini_impurities(as.data.table(data %>%select(cat)))
  data_gini <- data_gini%>%filter(Var1 == "Attrition" & Var2 != "Attrition")
  data_gini  <- data_gini %>%
    mutate(Var2 = reorder(Var2,-GiniImpurity))
 
  plot <- ggplot(data_gini, aes(y = Var2, x = GiniImpurity))+
     geom_segment(aes(x = GiniImpurity,y = Var2, yend = Var2,xend = 0))+
    geom_point(aes(col = GiniImpurity), size  = 4)+
    scale_x_continuous(expand = c(0,0,0.1,0.1))+
    scale_colour_gradient(low = "green", high = "red") 
  ggplotly(plot, tooltip = "x")
  
}  

ui <- fluidPage(
    titlePanel("PCA test"),
    selectInput("dimensions", "Choose Numerical Variables for PCA",numeric_cols,selected = numeric_cols, multiple = T),
    plotOutput("PCA", brush = "brush"),
    dataTableOutput("selected_points"),
    plotlyOutput("Gini")

  
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
    
    output$Gini <- renderPlotly({
      cat_lolly(pca_data%>%
                  filter(selected()))
    })
    
    output$selected_points <- renderDataTable(pca_data%>%
                                                filter(selected()) 
                                              %>% select(input$dimensions, everything()) %>%
                                              select(-c(PC1, PC2)) 
                                              
                                              )
   
}

# Run the application 
shinyApp(ui = ui, server = server)
