library(shiny)
library (readr)
library (knitr)
library(dplyr)
library (tidyr)
library(readxl)
library(AppliedPredictiveModeling)
library(caret)
library(factoextra)
library(plotly)
library(cowplot)

source("helpers.R")

ui <- navbarPage(
   "Global Financial Crisis",
   tabPanel("Introduction",
              mainPanel(
                p("Hello! Our research project looks at the 2008 global financial crisis, 
                  also known as the Great Recession. The crisis came about as a direct result of the 
                  burst of USA's housing market bubble, which was caused due to a high 
                  number of subprime loans being given out to buyers who wanted to become 
                  homeowners. However, many buyers could not repay these large loans, and once 
                  the high interest rates kicked in, they defaulted, 
                  leading the subrpime lenders to bankruptcy, and causing major 
                  private institutions to either go bankrupt as well, or ask the 
                  Fed for support. The crisis was felt globally, and resulted in a 
                  global wave of pessimism, leading investments to an all time low, 
                  and leaving many economies struggling. We are interested in learning more about disparities in the effects the recession had on countries- were 
                  there some countries that were much more worse off than others during 
                  the recession?"),
                p("This project looks at data on the following variables: Debt, Unemployment, Savings, FDI, Trade Balance, and Real GDP in 128 countries from 2005-2011. This app allows the user to view variable descriptions and their data sources, cluster the data to see which groups of countries are affected the most, view time-oriented scatterplots and line graphs for the 6 variables by country/region, and then finally run a difference-in-differences model to measure the impact of the recession between groups.")
              )),
   tabPanel("Data Collection and Processing",
            sidebarLayout(
              sidebarPanel(radioButtons(
                "variables", h3("Select Variable"),
                c("Debt" = 1, "Unemployment" = 2, "Savings" = 3, "FDI" = 4, "Trade Balance" = 5, "Real GDP" = 6), selected = 1
              )),
              mainPanel(
                strong("Variable Description:"), textOutput("info"), uiOutput("link"),
                tableOutput("tab")
              )
            )),
tabPanel("Clustering",
   sidebarLayout(
      sidebarPanel(
        radioButtons(
          "cluster", h3("How to Cluster"),
          c(Method = "method",
            Number = "num"),
          selected = NULL),
        conditionalPanel(
          condition = "input.cluster == 'method'",
          selectInput(
            "methods", h4("Methods of clustering"),
            c("Elbow Method" = "wss",
              "Silhouette Method" = "silhouette",
              "Gap Statistic" = "gap",
              "Average" = "avg"),
            selected = NULL)),
        conditionalPanel(
          condition = "input.cluster == 'num'",
          numericInput("clusters", h4("Number of Clusters"), value = 1, min = 1, max = 50)
        )
        ),
      mainPanel(
        tabsetPanel(
          tabPanel("Choosing Optimal no. of Clusters", plotOutput("cl_method")),
          tabPanel("Displaying Clusters", plotOutput("cluster")),
          tabPanel("Principal Components Analysis", plotOutput("pca")))
      ))
   ),
tabPanel("Data Visualizations",
         navlistPanel(widths = c(2,8),
                      tabPanel("Scatter over Time",
         sidebarLayout(
           sidebarPanel(width = 3,
             selectInput("var1", h3("X-Variable"),
                         c("Debt" = "debt", 
                           "Unemployment" = "unemp",
                           "Savings" = "savings",
                           "Foreign Direct Investment" = "fdi",
                           "Real GDP growth" = "real_gdp",
                           "Trade Balance" = "trade_balance"),
                         selected = NULL),
             selectInput("var2", h3("Y-Variable"),
                         c("Debt" = "debt", 
                           "Unemployment" = "unemp",
                           "Savings" = "savings",
                           "Foreign Direct Investment" = "fdi",
                           "Real GDP growth" = "real_gdp",
                           "Trade Balance" = "trade_balance"),
                         selected = NULL)
           ),
           mainPanel(plotlyOutput("scat")))),
         tabPanel("Line Graphs",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("country", h3("Select country"), rownames(data_cl)),
                      selectInput("var3", h3("Select Variable"), 
                                  c("Debt" = "debt", 
                                    "Unemployment" = "unemp",
                                    "Savings" = "savings",
                                    "Foreign Direct Investment" = "fdi",
                                    "Real GDP growth" = "real_gdp",
                                    "Trade Balance" = "trade_balance"),
                                  selected = NULL)),
                    mainPanel(plotlyOutput("line"))
                  ))
         )),
tabPanel("Modeling",
         sidebarLayout(
           sidebarPanel(
             selectInput("model", h3("Select Variable"),
                         c("Debt" = "debt", 
                           "Unemployment" = "unemp",
                           "Savings" = "savings",
                           "Foreign Direct Investment" = "fdi",
                           "Real GDP growth" = "real_gdp",
                           "Trade Balance" = "trade_balance"),
                         selected = "debt")),
           mainPanel(
              tabsetPanel(
                tabPanel("Cluster 1",
                          verbatimTextOutput("dif1")),
               tabPanel("Cluster 3",
                           verbatimTextOutput("dif2")),
               tabPanel("Cluster 4",
                           verbatimTextOutput("dif3")),
               tabPanel("Cluster 5",
                           verbatimTextOutput("dif4")))
           )
           )
         ),
tabPanel("Limitations",
         mainPanel(
           p("We believe our research has its limitations. To begin with, we only have a limited number of variables in our analysis, and have left out other possible variables that could show the impact the recession had on particular countries. We had to tackle missing data for some countries as well, and we cannot be sure as to whether or not the quality of the data holds across the different countries. In our modeling, we used a difference-in-differences estimation method, which requires a control group that is not affected by the treatment (in this case, the recession) at all, but we had to choose a control group that was minimally affected by the recession (i.e. we are using a synthetic control group as opposed to a natural one). This measures how clusters of countries were affected in comparison to this control cluster, which we believe is a more interesting question to explore, as the control cluster does have the highest number of countries.")
         ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$info <- renderText ({
    if(input$variables == 1){
      "This variable captures each country's central government debt from 2005 to 2011. We think this is an important variable because higher levels of debt indicate possible economic distress, 
      and would reflect the shock the 2008 recession caused. 
      Data for Debt has been sourced from: "
    }
    else if (input$variables == 2){
      "This variable captures each country's unemployment rate from 2005 to 2011. We think this is an important variable as according to the Phillips Curve, inflation and unemployment have a stable and inverse relationship; thus, a recession (the opposite of inflation) should accompany a rise in unemployment, leaving a country worse off due to the recession. Data for Unemployment has been sourced from: "
    }
    else if (input$variables == 3){
      "This variable captures each country's gross savings from 2005 to 2011. We think this is an important variable for two reasons: first, an increase in savings is usually indicative of pessimism in the economy, as people prefer to save their purchases for later when times are better. Second, a decrease in savings also indicates need for immediate liquid money, indicating a bad phase for the economy. Data for Savings has been sourced from: "
    }
    else if (input$variables == 4){
      "This variable captures each country's net foreign direct investment from 2005 to 2011. FDI is an important variable as a recession would cause bigger, richer countries to decrease the amount of welfare being provided to other countries, which indicates economic stress within the donating country, and also creating economic stress in the country receiving assistance. Data for FDI has been sourced from: "
    }
    else if (input$variables == 5){
      "This variable captures each country's trade balance (calculated by subtracting net imports from net exports) from 2005 to 2011. We think trade balance is an important variable as the recession would cause fewer demand for imports, and would negatively impact a country's economy. Data for Trade Balance has been sourced from: "
    }
    else if (input$variables == 6){
      "This variable captures each country's real gross domestic product from 2005 to 2011. As the best metric for a country's economic performance, Real GDP merits its inclusion. Data for Real GDP has been calculated through data on inflation and GDP growth, both sourced from: "
    }
  })
  output$link <- renderUI ({
    if(input$variables == 1){
      a("IMF.", href = "https://www.imf.org/external/datamapper/CG_DEBT_GDP@GDD/CHN/FRA/DEU/ITA/JPN/GBR/USA")
    }
    else if (input$variables == 2){
      a("ILO.", href = "http://www.ilo.org/ilostat/faces/oracle/webcenter/portalapp/pagehierarchy/Page3.jspx;ILOSTATCOOKIE=E_2vGBW4JErdXf7a-g2mJpvvLJPlqtN8rpVPnNMnXmjZG0gEMcea!-1292775148?MBI_ID=2&_afrLoop=245281395757352&_afrWindowMode=0&_afrWindowId=null#!%40%40%3F_afrWindowId%3Dnull%26_afrLoop%3D245281395757352%26MBI_ID%3D2%26_afrWindowMode%3D0%26_adf.ctrl-state%3D2mpwlz6c3_4")
    }
    else if (input$variables == 3){
      a("World Bank.", href = "https://data.worldbank.org/indicator/ny.gns.ictr.zs")
    }
    else if (input$variables == 4){
      a("World Bank.", href = "https://data.worldbank.org/indicator/bx.klt.dinv.cd.wd")
    }
    else if (input$variables == 5){
      a("The World Bank's WITS (World Integrated Trade Solution) platform.", href = "https://wits.worldbank.org/CountryProfile/en/Country/WLD/Year/2005/TradeFlow/EXPIMP")
    }
    else if (input$variables == 6){
      a("World Bank.", href = "https://data.worldbank.org/indicator/ny.gdp.mktp.kd.zg")
    }
    })
 
  output$tab <- renderTable ({
    if(input$variables == 1){
      head(global_debt)
    }
    else if (input$variables == 2){
      head(unemployment)
    }
    else if (input$variables == 3){
      head(savings)
    }
    else if (input$variables == 4){
      head(fdi)
          }
    else if (input$variables == 5){
      head(tb)
    }
    else if (input$variables == 6){
      head(gdp_growth)
    }
  })
  output$cl_method <- renderPlot(
    if(input$cluster == "method" & input$methods != "avg")
    {
      fviz_nbclust(scale(data_cl), kmeans, method = input$methods, k.max = 8)
    }
    else if (input$cluster == "num" | input$methods == "avg")
    {
      elbow <- fviz_nbclust(scale(data_cl), kmeans, method = "wss", k.max = 8)+labs(subtitle = "Elbow method")
      silhouette <- fviz_nbclust(scale(data_cl), kmeans, method = "silhouette", k.max = 8)+labs(subtitle = "Silhouette method")
      gap <- fviz_nbclust(scale(data_cl), kmeans, method = "gap", k.max = 8)+labs(subtitle = "Gap Statistic")
      plot_grid(elbow, silhouette, gap)
    }
  )
r <- reactive({
  set.seed(300)
  if(input$cluster == "method")
  {
    if (input$methods == "wss" | input$methods == "avg")
    {r <- kmeans(scale(data_cl), centers = 5)}
    else if(input$methods == "silhouette")
    {r <- kmeans(scale(data_cl), centers = 6)}
    else if(input$methods == "gap")
    {r <- kmeans(scale(data_cl), centers = 1)}
  }
  else{
    r <- kmeans(scale(data_cl), centers = input$clusters)
  }
})
  output$cluster <- renderPlot(
   fviz_cluster(r(), centers = 5, nstart=25, data = scale(data_cl), repel = TRUE)
  )
  output$scat <- renderPlotly(
    data %>%
      plot_ly(
        x = ~eval(parse(text = input$var1)), 
        y = ~eval(parse(text = input$var2)),
        #size = ~pop, 
        color = ~region, 
        frame = ~year, 
        text = ~country, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
      ) %>%
      layout(
        xaxis = list(
          title = input$var1
        ),
        yaxis = list(
          title = input$var2
        )
      )
    %>% 
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
      )
    )
  output$line <- renderPlotly(
    graph <- plot_ly(filter(data, country == input$country | country == "United States"), x = ~year, y = ~eval(parse(text = input$var3)), split = ~country, type = 'scatter', mode = 'lines')
    %>% add_trace(filter(data, country == input$country | country == "United States"), x = ~year, y = ~eval(parse(text = input$var3)), mode = 'lines') %>% layout(showlegend = FALSE)
    %>% layout(
      yaxis = list(
        title = input$var3
      ))
  )
  output$dif1 <- renderPrint(
    summary(lm(eval(parse(text=input$model)) ~ treated + time + did, data = data_df1))
  )
  output$dif2 <- renderPrint(
    summary(lm(eval(parse(text=input$model)) ~ treated + time + did, data = data_df2))
  )
  output$dif3 <- renderPrint(
    summary(lm(eval(parse(text=input$model)) ~ treated + time + did, data = data_df3))
  )
  output$dif4 <- renderPrint(
    summary(lm(eval(parse(text=input$model)) ~ treated + time + did, data = data_df4))
  )
  output$pca <- renderPlot(
    pca
  )
}

# Run the application 
shinyApp(ui = ui, server = server)