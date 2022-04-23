###Neil's Library
library(shiny)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(shinydashboard)

####Ayushi's Library
library(shiny)
library(ggplot2)
library(tidyverse)
library(ggridges)
library(viridis)
library(DT)


###Harshita's Library

library(shiny)
library(plotly)
library(readr)
library(dplyr)
library(lmtest)

job_harshita <- read_csv("https://github.com/hbnarayanan/stat479finalproject/raw/main/hr_analytics.csv")


###Random Forest Model Library


library(tidyverse)
library(randomForest)
library(tree)
library(datasets)
library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(shiny)


###Neil's part###

job <- read_csv("https://github.com/hbnarayanan/stat479finalproject/raw/main/hr_analytics.csv")

non_useful <- c("EmployeeCount", "StandardHours", "EmployeeNumber", "DailyRate", "DistanceFromHome", "Education", "JobInvolvement", "JobSatisfaction", "JobSatisfaction", "PerformanceRating", "EnvironmentSatisfaction", "JobLevel", "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance", "HourlyRate", "MonthlyRate", "PercentSalaryHike", "TrainingTimesLastYear", "NumCompaniesWorked")

num_dat = select_if(job, is.numeric) %>% 
  select(-non_useful)

numeric_cols <- colnames(num_dat)

pca_data <- num_dat%>%
  prcomp(scale = TRUE)
PC <- data.frame(pca_data$x[,c(1,2)])
pca_data <- cbind(job, PC)
hclust_data <- pca_data %>% 
  select(PC1, PC2) %>%
  dist()
fit <- hclust(hclust_data)

pca_data <- pca_data %>% 
  mutate(cluster = cutree(fit, k = 4))

pca_plot <- function(data){
  pca_data <- data%>%
    prcomp(scale = TRUE)
  PC <- data.frame(pca_data$x[,c(1,2)])
  pca_data <- cbind(job, PC)
  
  hclust_data <- pca_data %>% 
    select(PC1, PC2) %>%
    dist()
  fit <- hclust(hclust_data)
  
  pca_data <- pca_data %>% 
    mutate(cluster = cutree(fit, k = 4))
  
  ggplot(pca_data, aes(PC1, PC2, col = Attrition, alpha = Attrition, shape = as.factor(cluster)))+
    scale_alpha_discrete(range = c(0.3, 1))+
    geom_point(size = 5) + 
    theme_bw() +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5)) +
    ggtitle("PCA for most influential numerical features")
}


component_plot <- function(data){
  pca_data <- data%>%
    prcomp(scale = TRUE)
  comp_data <- data.frame(pca_data$rotation) 
  comp_data <- comp_data %>%
    rownames_to_column("Feature") %>%
    select(Feature, PC1, PC2)
  
  comp_data %>%
    pivot_longer(c(PC1, PC2), names_to = "components", values_to = "values") %>%
    ggplot(aes (values, reorder_within(Feature,values, components), fill = values))+
    scale_y_reordered() +
    facet_wrap(~components, scales = "free_y")+ 
    geom_col() +
    theme_bw()
  
}

reset <- function(data, brush){
  
  brushedPoints(data, brush, allRows = T)$selected_
  
}


### Ayushi's part

job_Ayushi <- read_csv("https://github.com/hbnarayanan/stat479finalproject/raw/main/hr_analytics.csv")
correlatation <- data.frame(Categorical = c("EducationField","JobRole","BusinessTravel","JobRole", "Department"),
                            Numerical = c("YearsAtCompany","YearsSinceLastPromotion","YearsAtCompany","YearsAtCompany","YearsAtCompany"),
                            Pvalue = c(7.843325e-07, 0.05236382, 2.63279e-09, 1.439075e-05, 2.677685e-08))


###Ranfom Forest Part
job_rf <- read_csv("https://github.com/hbnarayanan/stat479finalproject/raw/main/hr_analytics.csv")
char <- sapply(job_rf, is.character)
col_names <- names(job_rf[,char])
num <- sapply(job_rf, is.numeric)
names <- names(job_rf[,num])
job_rf[col_names] <- lapply(job_rf[col_names], factor)
ind <- sample(2, nrow(job_rf), replace = TRUE, prob = c(0.7, 0.3))
train <- job_rf[ind==1,]
test <- job_rf[ind==2,]
train = sample (1:nrow(job_rf), nrow(job_rf)/2)
rf = randomForest(formula = Attrition ~ Age + NumCompaniesWorked + YearsSinceLastPromotion +
                    DailyRate + EducationField + JobRole + BusinessTravel + MaritalStatus +
                    Gender + OverTime + YearsWithCurrManager + YearsInCurrentRole +
                    YearsAtCompany + TrainingTimesLastYear, data = job_rf, proximity = TRUE, subset = train)

preds <- c('Age', 'NumCompaniesWorked', 'YearsSinceLastPromotion', 'DailyRate', 'EducationField', 'job_rfRole' , 'BusinessTravel', 'MaritalStatus', 'Gender', 'OverTime', 'YearsWithCurrManager', 'YearsInCurrentRole' , 'YearsAtCompany', 'TrainingTimesLastYear')

dtree.cp <- rpart(formula = Attrition ~ Age + NumCompaniesWorked + YearsSinceLastPromotion +
                    DailyRate + EducationField + JobRole + BusinessTravel + MaritalStatus +
                    Gender + OverTime + YearsWithCurrManager + YearsInCurrentRole +
                    YearsAtCompany + TrainingTimesLastYear, data = job_rf, control = rpart.control(cp = 0.005))




ui <- fluidPage(
  h1("HR Attrition Analysis"),
  
  ###Neil's section
  h2("PCA & EDA Analysis"),
  selectInput("dimensions", "Choose Numerical Variables for PCA",numeric_cols,selected = numeric_cols, multiple = T),
  plotOutput("Components"),
  plotOutput("PCA", brush = "brush"),
  selectInput("clusters", "Choose Cluster to view corresponding data in DataTable", c(1,2,3,4), selected = c(1,2,3,4), multiple = T),
  dataTableOutput("selected_points"),
  
  ###Ayushi's section
  h2("HR Attrition Analysis for Categorical and Numerical variable interactions"),
  h3("Click on a row of the Data Table to see a plot"),
  fluidRow(
    column(6, DT::dataTableOutput("table"), height = 500),
    column(6, plotOutput("density"), height = 500)
  ),
  
  ###Harshita's section
  h2("HR Attrition Analysis of Categorical Variables"), 
  selectInput("var1", "First Variable", colnames(job)[c(3,5,7,8,11,12,14,15,16,17,18,23,25,26,28,30,31)]),
  selectInput("facet", "Facet By", colnames(job)[c(5,3,7,8,11,12,14,15,16,17,18,23,25,26,28,30,31)]),
  plotlyOutput("barplot"),
  verbatimTextOutput("text"),
  
  ###Random Forest Section
  h2("Random Forest Model for prediction Attrition"), 
  h3("Variable Importance identified in Random Forest"), 
  plotOutput("varImp"), 
  h3("A Sample Decision Tree used in the Random Forest Model"),
  plotOutput("tree", height = "1000px")
  
  
  
)



# # Define server logic required to draw a histogram
server <- function(input, output) {
  
  ###Neil's code
  
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
  
  output$Components <- renderPlot({
    
    component_plot(pca_data
                   %>% select(input$dimensions))
  })
  
  output$selected_points <- renderDataTable(pca_data%>%
                                              filter(selected()) %>%
                                              filter(cluster %in% input$clusters)
                                            %>% select(input$dimensions, everything()) %>%
                                              select(-c(PC1, PC2))
                                            
                                            
  )
  
  ###Harshita's code 
  datachisquare <- reactive({
    # req(input$var1, input$facet)
    
    job_harshita %>%
      {
        table(.[[input$var1]],.[[input$facet]])
      }
    
    #     select(input$var1,input$facet,Attrition)
  })
  output$barplot <- renderPlotly({
    job_harshita %>% 
      # count(.data[[input$var1]], .data[[input$facet]],Attrition) %>% 
      group_by(.data[[input$var1]], .data[[input$facet]],Attrition) %>%
      summarise(n = n()) %>%
      mutate(freq = n / sum(n)) %>%
      ggplot() +
      geom_bar(aes(x=.data[[input$var1]],y=freq,fill=Attrition),stat="identity")+
      theme(axis.text.x = element_text(angle = 45, hjust=1))+
      facet_grid(~.data[[input$facet]])+
      labs(title = paste0("Frequency of Attrition in ", input$var1, " faceted by ", input$facet),x=paste0(input$var1),y="Attrition Frequency") 
  })
  
  
  
  output$text <- renderPrint({
    # print(datachisquare())
    # x=table(job$Attrition,datachisquare())
    # job %>% 
    #   select(MaritalStatus, StockOptionLevel,Attrition) %>% 
    #   mutate(MaritalStatus=as.factor(MaritalStatus),StockOptionLevel=as.factor(StockOptionLevel),Attrition=as.factor(Attrition)) %>% 
    # mod.c <- glm(Attrition ~ MaritalStatus+StockOptionLevel, family=binomial(link = "logit"))
    # # 
    # lrtest(mod.c)
    
    #  job<- job %>% 
    #    select(MaritalStatus, StockOptionLevel,Attrition) %>% 
    #    mutate(MaritalStatus=as.factor(MaritalStatus),StockOptionLevel=as.factor(StockOptionLevel),Attrition=as.factor(Attrition)) 
    # 
    #  mod.c <- glm(Attrition ~ MaritalStatus+StockOptionLevel, data = job, family=binomial(link = "logit"))
    #  # 
    #  z=lrtest(mod.c)
    # z$`Pr(>Chisq)`
    x=chisq.test(datachisquare())
    
    {paste("Chi-squared test p-value=", x$p.value)}
  })
  
  ###Ayushi's code
  
  output$table <- DT::renderDataTable({
    correlatation
  },  class="compact cell-border", selection = "single")
  
  
  output$density <- renderPlot({
    s <- input$table_rows_selected
    if(length(s)){
      row <- correlatation[s, ]
      ggplot(job_Ayushi, aes(x = .data[[row$Numerical]], y = .data[[row$Categorical]], fill = Attrition)) +
        geom_density_ridges() +
        labs(title = paste0("Density Graph of ", row$Categorical, " and ", row$Numerical, " vs Attrition")) +
        theme_bw() +
        theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5))
    }
    
  })
  
  
  ### Random Forest Code
  
  output$varImp <- renderPlot({
    varImpPlot(rf)
  }
  )
  
  output$tree <- renderPlot({
    rpart.plot(dtree.cp)
  })
  
}

# Run the application 

shinyApp(ui = ui, server = server)