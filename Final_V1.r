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
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "black"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "white"
  ,sidebarTabTextSize = 15
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "black"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)


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
                h3("Click on a row of the Data Table to see a plot"),
                          fluidRow(
                            column(6, DT::dataTableOutput("table"), height = 500),
                            column(6, plotOutput("density"), height = 500)),
                h3("HR Attrition Analysis of Categorical Variables"), 
                selectInput("var1", "First Variable", colnames(job)[c(3,5,7,8,11,12,14,15,16,17,18,23,25,26,28,30,31)]),
                           selectInput("facet", "Facet By", colnames(job)[c(5,3,7,8,11,12,14,15,16,17,18,23,25,26,28,30,31)]),
                           plotlyOutput("barplot"),
                           verbatimTextOutput("text")
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
