###Neil's Library
library(shiny)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(shinydashboard)
library(data.table)
library(mltools)

####Ayushi's Library
library(shiny)
library(ggplot2)
library(tidyverse)
library(ggridges)
library(viridis)
library(DT)
library(dashboardthemes)
library(fresh)
library(tidyr)
library(magrittr)


###Harshita's Library

library(shiny)
library(plotly)
library(readr)
library(dplyr)
library(lmtest)

job_harshita <- read_csv("https://github.com/hbnarayanan/stat479finalproject/raw/main/hr_analytics.csv")


###Random Forest Model Library

#library(MASS)
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

### Ayushi's part

job_Ayushi <- read_csv("https://github.com/hbnarayanan/stat479finalproject/raw/main/hr_analytics.csv")
categorical <- c("BusinessTravel", "Department" , "EducationField", "Gender" ,"JobRole", "MaritalStatus" , "OverTime","Education",
                 "EnvironmentSatisfaction","JobInvolvement",  "JobLevel", "JobSatisfaction","PerformanceRating",   "RelationshipSatisfaction", "StockOptionLevel","WorkLifeBalance"  )      
col_names <- names(job_Ayushi[,categorical])
col_names <- c(col_names, "Attrition")
job[col_names] <- lapply(job_Ayushi[col_names], factor)
numeric <- c("Age", "DailyRate", "DistanceFromHome","HourlyRate"  ,  "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked"     
             ,"PercentSalaryHike"  ,"TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole", "YearsSinceLastPromotion" ,"YearsWithCurrManager")

a <- data.frame(matrix(nrow = 0, ncol = 4))


for(variable1 in categorical){
  for(variable2 in numeric){
    
    mod.c <- glm(job[["Attrition"]] ~ job[[variable1]] + job[[variable2]] + job[[variable1]]:job[[variable2]], family=binomial(link = "logit"))
    x=lrtest(mod.c, glm(job[["Attrition"]] ~ job[[variable1]] + job[[variable2]], family=binomial(link = "logit")))
    
    
    a <- rbind(a, list(variable1, variable2, x$`Pr(>Chisq)`[2]))
  }
}

colnames(a) <- c("Categorical", "Numerical", "PValue")



### Harshita's code
ex_df=tibble(var1_names=colnames(job)[c(3,5,7,8,11,12,14,15,16,17,18,23,25,26,28,30,31)],
             var2_names=colnames(job)[c(3,5,7,8,11,12,14,15,16,17,18,23,25,26,28,30,31)])
att=job$Attrition
ex_df =ex_df %>%
  expand(var1_names,var2_names) %>%
  filter(!duplicated(paste0(pmax(var1_names, var2_names), pmin(var1_names, var2_names)))) %>%
  filter(var1_names!=var2_names)
vector = c()
for(i in 1:nrow(ex_df)){
  mod.b=glm((job[["Attrition"]]) ~ (job[[ex_df$var1_names[i]]]) + (job[[ex_df$var2_names[i]]]) + (job[[ex_df$var1_names[i]]]):(job[[ex_df$var2_names[i]]]), family=binomial(link = "logit"))
  x=lrtest(mod.b, glm((job[["Attrition"]]) ~ (job[[ex_df$var1_names[i]]]) + (job[[ex_df$var2_names[i]]]), family=binomial(link = "logit")))
  vector <- c(vector, x$`Pr(>Chisq)`[2])
}
new_df=ex_df %>%
  mutate(p_val=vector) 

###Randfom Forest Part
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




# h1("HR Attrition Analysis"),
# tabsetPanel(
#   type = "tab",
#   tabPanel("PCA & EDA Analysis", h3("Brush over the Executive Owner or Control Deficieny Title"),selectInput("dimensions", "Choose Numerical Variables for PCA",numeric_cols,selected = numeric_cols, multiple = T),
#            plotOutput("Components"),
#            plotOutput("PCA", brush = "brush"),
#            selectInput("clusters", "Choose Cluster to view corresponding data in DataTable", c(1,2,3,4), selected = c(1,2,3,4), multiple = T),
#            dataTableOutput("selected_points")
#   ),
#   tabPanel("HR Attrition Analysis for Categorical and Numerical variable interactions" ,
#           h3("Click on a row of the Data Table to see a plot"),
#           fluidRow(
#             column(6, DT::dataTableOutput("table"), height = 500),
#             column(6, plotOutput("density"), height = 500))
#   ),
#   tabPanel("HR Attrition Analysis of Categorical Variables", selectInput("var1", "First Variable", colnames(job)[c(3,5,7,8,11,12,14,15,16,17,18,23,25,26,28,30,31)]),
#            selectInput("facet", "Facet By", colnames(job)[c(5,3,7,8,11,12,14,15,16,17,18,23,25,26,28,30,31)]),
#            plotlyOutput("barplot"),
#            verbatimTextOutput("text")
#   ),
#   tabPanel("Random Forest Model for prediction Attrition",h3("Variable Importance identified in Random Forest"), 
#            plotOutput("varImp"), 
#            h3("A Sample Decision Tree used in the Random Forest Model"),
#            plotOutput("tree", height = "1000px"))
# )


customLogo <- shinyDashboardLogoDIY(
  
  boldText = "HR Employee Attrition Analysis"
  ,mainText = ""
  ,textSize = 18
  ,badgeText = ""
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = ""
  ,badgeBorderRadius = 3
  
)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = customLogo,
                                    titleWidth = 300), 
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(
                        id = "SideBarMENU", 
                        
                        menuItem("PCA & EDA", tabName = "one", icon = icon("sliders"), selected = TRUE),
                        menuItem("Feature Analysis", icon = icon("chart-bar"),tabName = "two"),
                        menuItem("Random Forest", icon = icon("sitemap"),tabName = "three")
                      )
                    ),
                    dashboardBody(
                      use_theme(mytheme), 
                      tabItems(
                        # First tab content
                        tabItem("one",
                                fluidPage(
                                  h3("Brush over the Executive Owner or Control Deficieny Title"),
                                  selectInput("dimensions", "Choose Numerical Variables for PCA",numeric_cols,selected = numeric_cols, multiple = T),
                                  plotOutput("Components"),
                                  plotOutput("PCA", brush = "brush"),
                                  selectInput("clusters", "Choose Cluster to view corresponding data in DataTable", c(1,2,3,4), selected = c(1,2,3,4), multiple = T),
                                  DT::dataTableOutput("selected_points"),
                                  plotlyOutput("Gini")
                                )
                        ),
                        
                        # Second tab content
                        tabItem("two",
                                h3("Feature correlation to analyze attrition"),
                                sliderInput("slider", "Chose P-Value range", min = 0, max = 1, value = c(0, 0.05)),
                                h4("Click on a row of the Data Table to see a density plot depicting correlation between numerical features"),
                                fluidRow(column(6, DT::dataTableOutput("output1"), height = 500),
                                         column(6, plotOutput("density"), height = 500)),
                                h4("Click on a row of the Data Table to see a bar plot depicting correlation between categorical features"),
                                fluidRow(column(6, DT::dataTableOutput("output2"), height = 500),
                                         column(6, plotOutput("barplot"), height = 500))
                        ),
                        
                        # Third tab content
                        tabItem("three",
                                h3("Variable Importance identified in Random Forest"), 
                                plotOutput("varImp"),
                                h3("A Sample Decision Tree used in the Random Forest Model"),
                                plotOutput("tree", height = "1000px")
                        )
                      )
                    )
) 

###Neil's section
# h2("PCA & EDA Analysis"),
# selectInput("dimensions", "Choose Numerical Variables for PCA",numeric_cols,selected = numeric_cols, multiple = T),
# plotOutput("Components"),
# plotOutput("PCA", brush = "brush"),
# selectInput("clusters", "Choose Cluster to view corresponding data in DataTable", c(1,2,3,4), selected = c(1,2,3,4), multiple = T),
# dataTableOutput("selected_points"),

###Ayushi's section
# h2("HR Attrition Analysis for Categorical and Numerical variable interactions"),
# h3("Click on a row of the Data Table to see a plot"),
# fluidRow(
#   column(6, DT::dataTableOutput("table"), height = 500),
#   column(6, plotOutput("density"), height = 500)
# ),

###Harshita's section
# h2("HR Attrition Analysis of Categorical Variables"), 
# selectInput("var1", "First Variable", colnames(job)[c(3,5,7,8,11,12,14,15,16,17,18,23,25,26,28,30,31)]),
# selectInput("facet", "Facet By", colnames(job)[c(5,3,7,8,11,12,14,15,16,17,18,23,25,26,28,30,31)]),
# plotlyOutput("barplot"),
# verbatimTextOutput("text"),

###Random Forest Section
# h2("Random Forest Model for prediction Attrition"), 
# h3("Variable Importance identified in Random Forest"), 
# plotOutput("varImp"), 
# h3("A Sample Decision Tree used in the Random Forest Model"),
# plotOutput("tree", height = "1000px")







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
  
  output$selected_points <- DT::renderDataTable({
    table <- pca_data%>%filter(selected()) %>%filter(cluster %in% input$clusters)%>% select(input$dimensions, everything()) %>% select(-c(PC1, PC2))
    DT::datatable(table, options = list(scrollX = T))
    
    
  })
  
  
  
  
  output$Gini <- renderPlotly({
    cat_lolly(pca_data%>%
                filter(selected()))
  })
  
  ###Harshita's & Ayushi's code 
  cat_num <- reactive({
    a%>%
      filter(PValue >= input$slider[1] & PValue <= input$slider[2])
  })
  cat_cat <- reactive({
    new_df%>%
      filter(p_val >= input$slider[1] & p_val <= input$slider[2])
  })
  
  output$"output2" <- renderDataTable({
    cat_cat()
    
  }, class="compact cell-border", selection = "single")
  output$"output1" <- renderDataTable({
    cat_num()
    
  }, class="compact cell-border", selection = "single") 
  
  output$density <- renderPlot({
    
    
    s <- input$output1_rows_selected
    if(length(s)){
      row <- cat_num()[s, ]
      ggplot(job_Ayushi, aes(x = .data[[row$Numerical]], y = .data[[row$Categorical]], fill = Attrition)) +
        geom_density_ridges() +
        labs(title = paste0("Density Graph of ", row$Categorical, " and ", row$Numerical, " vs Attrition")) +
        theme_bw() +
        theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5))
    }
    
  })
  
  output$barplot <- renderPlot({
    s <- input$output2_rows_selected
    if(length(s)){
      row <- cat_cat()[s, ]
      d2 <- job_harshita %>%
        group_by(.data[[row$var1_names]], .data[[row$var2_names]],Attrition) %>%
        summarise(n = n()) %>%
        mutate(freq = n / sum(n))
      
      ggplot(d2,aes(x=.data[[row$var1_names]],y=freq,fill=Attrition) ) +
        geom_bar(stat="identity")+
        theme(axis.text.x = element_text(angle = 45, hjust=1))+
        facet_grid(~.data[[row$var2_names]])+
        labs(title = paste0("Frequency of Attrition in ", row$var1_names, " faceted by ", row$var2_names),x=paste0(row$var1_names),y="Attrition Frequency")
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
