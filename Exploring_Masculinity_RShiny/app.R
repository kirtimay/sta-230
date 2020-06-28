#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("ggplot2")
library("dplyr")
library('scales')
library('shinyWidgets')
library('readr')

data <- read.csv('raw-responses.csv')

df <- data

df$age3 <- gsub("18 - 34", "18", df$age3, ignore.case = TRUE)
df$age3 <- gsub("35 - 64", "35", df$age3, ignore.case = TRUE)
df$age3 <- gsub("65 and up", "65", df$age3, ignore.case = TRUE)

df$q0034 <- gsub("\\$0-\\$9,999", "9999", df$q0034, ignore.case=TRUE)
df$q0034 <- gsub("\\$50,000-\\$74,999", "74999", df$q0034, ignore.case=TRUE)
df$q0034 <- gsub("\\$200,000\\+", "200000", df$q0034, ignore.case=TRUE)
df$q0034 <- gsub("\\$25,000-\\$49,999", "49999", df$q0034, ignore.case=TRUE)
df$q0034 <- gsub("\\$150,000-\\$174,999", "174999", df$q0034, ignore.case=TRUE)
df$q0034 <- gsub("\\$100,000-\\$124,999", "124999", df$q0034, ignore.case=TRUE)
df$q0034 <- gsub("\\$125,000-\\$149,999", "149999", df$q0034, ignore.case=TRUE)
df$q0034 <- gsub("\\$10,000-\\$24,999", "24999", df$q0034, ignore.case=TRUE)
df$q0034 <- gsub("Prefer not to answer", "NA", df$q0034, ignore.case=TRUE)
df$q0034 <- gsub("\\$75,000-\\$99,999", "99999", df$q0034, ignore.case=TRUE)
df$q0034 <- gsub("\\$175,000-\\$199,999", "199999", df$q0034, ignore.case=TRUE)
df$q0034 <- parse_integer(df$q0034)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Exploring Masculinity"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        helpText("Note: 'Importance of masculinity' aggregates responses to the question:
                 \"How important is it to you that others see you as masculine?\""),
        helpText("Note:
                 'Societal pressure' aggregates responses to the question:
                 \"Do you think that society puts pressure on men in a way that is unhealthy or bad
                 for them?\""),
        
        
        selectInput("var",
                    label = "Choose variable to display on x-axis",
                    choices = list(
                                   "Feeling of masculinity","Importance of masculinity","Societal Pressure?",  "Education",
                                   "Marraige",
                                   "Kids","Orientation","Race"),
                    selected ="Feeling of masculinity"),
        selectInput("var2",
                    label = "Choose variable to facet",
                    choices = list("None",
                                   "Feeling of masculinity",
                                   "Importance of masculinity",
                                   "Societal Pressure?","Education",
                                   "Marraige",
                                   "Kids","Orientation","Race"),
                    selected ="Race"),
        
        selectInput("subset",
                    label = "Choose a subset",
                    choices = list("None"),
                    selected ="None"),
        
        radioButtons("radio",
                     label = "Display chart in color or dodge" ,
                     choices = list("Color", "Dodge"),
                     selected = "Dodge"),
        
        radioButtons("radio2",
                     label = "Display chart in percentages or counts" ,
                     choices = list("Percentages", "Counts"),
                     selected = "Counts"),
        helpText("Note: The values can be weighted for age, race, education,",
                   " and geography using the Census Bureau’s American Community Survey ",
                   "to reflect the demographic composition of the United States age 18",
                   " and over."),
        
        radioButtons("radio3",
                     label = NULL ,
                     choices = list("Weighted", "Unweighted"),
                     selected = "Weighted"),

        
        helpText("Note: You can only select a range of values with the sliders bellow"),
        sliderTextInput("Age",
                        "Select Age Range",
                        choices = c("18", "35", "65","65" = "65+"),
                        selected = c("18", "65+"),
                        to_min = c("35")
                        ),

      sliderTextInput("Income",
                      "Select Income Pange",
                      choices = c("$0","$9,999", "$24,999","$49,999","$74,999","$99,999", "$124,999", "$149,999", "$174,999", "$199,999", "$200,000+"),
                      selected = c("$0","$200,000+"),
                      to_min = c("$9,999")
                      ),
      
      tags$head(tags$style(
        type = 'text/css',
        'form.well {max-height:600px; overflow-y: auto;}'
      ))
      
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3(textOutput("plot_title")),
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  output$plot_title <- renderText({
    paste("This is a ", tolower(input$radio3),"barplot of ", tolower(input$var),
          " colored by ", tolower(input$var2),
          " subsetted by ", tolower(input$subset))
  })
    
    observe({
      var1 <- switch(input$var, 
                     "Age" = unique(df$age3),
                     "Feeling of masculinity" = unique(df$q0001),
                     "Importance of masculinity"  = unique(df$q0002),
                     "Societal Pressure?"= unique(df$q0005),
                     "Education" = unique(df$educ4),
                     "Kids" = unique(df$kids),
                     "Orientation" = unique(df$orientation),
                     "Race" = unique(df$racethn4),
                     "Marraige"=unique(df$q0024)
      )
      choices1 <- c(as.character(var1), "None")
      updateSelectInput(session,"subset", choices = choices1, selected = "None")
    })
  
   output$distPlot <- renderPlot({
     
     var <- switch(input$var, 
                    "Age" = df$age3,
                    "Feeling of masculinity" = df$q0001,
                   "Importance of masculinity"  = df$q0002,
                   "Societal Pressure?"=df$q0005,
                    "Education" = df$educ4,
                    "Kids" = df$kids,
                    "Orientation" = df$orientation,
                    "Race" = df$racethn4,
                   "Marraige"=df$q0024
     )
     
     age1 <- switch(as.character( input$Age[1]),
                    "18"= 18,
                    "35"= 35,
                    "65"= 65,
                    "65+" = 66)
     
     age2 <- switch(as.character(input$Age[2]),
                    "18"= 18,
                    "35"= 35,
                    "65"= 65,
                    "65+" = 66)
     

     if (input$subset == "None"){
       df_subset <- reactive({
         a <- subset(df,(df$age3 >= age1 & df$age3 <= (age2 - 1))  & (df$q0034 >= (parse_number(as.character(input$Income[1])) + 1) & df$q0034 <= parse_number(as.character(input$Income[2]))))
         return(a)
       })
     }
     else{
       df_subset <- reactive({
         a <- subset(df, var == input$subset & (df$age3 >= age1 & df$age3 <= (age2 - 1)) & df$q0034 > parse_number(as.character(input$Income[1])) & df$q0034 <= parse_number(as.character(input$Income[2])))
         return(a)
       })
     }
     
     var1 <- switch(input$var, 
                    "Age" = df_subset()$age3,
                    "Feeling of masculinity" = df_subset()$q0001,
                    "Education" = df_subset()$educ4,
                    "Kids" = df_subset()$kids,
                    "Orientation" = df_subset()$orientation,
                    "Race" = df_subset()$racethn4,
                    "Feeling of masculinity" = df_subset()$q0001,
                    "Importance of masculinity"  = df_subset()$q0002,
                    "Marraige"=df_subset()$q0024,
                    "Societal Pressure?"=df_subset()$q0005,
     )
     
     var2 <- switch(input$var2,
                    "None" = "none",
                    "Age" = df_subset()$age3,
                    "Feeling of masculinity" = df_subset()$q0001,
                    "Education" = df_subset()$educ4,
                    "Kids" = df_subset()$kids,
                    "Orientation" = df_subset()$orientation,
                    "Race" = df_subset()$racethn4,
                    "Feeling of masculinity" = df_subset()$q0001,
                    "Importance of masculinity"  = df_subset()$q0002,
                    "Marraige"=df_subset()$q0024,
                    "Societal Pressure?"=df_subset()$q0005,
     )
     
     radio1 <- switch(input$radio,
                      "Color" = "stack",
                      "Dodge" = "dodge")
     
     
     if(input$radio3 == "Weighted"){
       if( input$radio2 ==  "Percentages" ){
         if(input$radio ==  "Color" ){
           ggplot(df_subset(), aes(y=df_subset()$weight, x=var1)) + geom_col(position= "fill",aes(fill = var2)) +
             theme(axis.text.x = element_text(angle=90, vjust=0.5, size = 13), axis.title=element_text(size=14))+
           scale_y_continuous(labels = percent_format())+
             labs(y = "Percentages", x = input$var, fill = input$var2 )
         }else{
           ggplot(df_subset(), aes(x= var2,  group=as.factor(var1))) + 
             geom_bar(aes(y = ..prop..,  weight = df_subset()$weight, fill = factor(..x..)), stat="count") +
             geom_text(aes( label = scales::percent(..prop..),
                            y= ..prop..,  weight = df_subset()$weight), stat= "count", vjust = -.5) +
             labs(y = "Percentages",  x = input$var2 , fill=input$var2) +
             facet_grid(var1) +
             scale_y_continuous(labels=percent) +
             theme(axis.text.x = element_text(angle=90, vjust=0.5, size = 13), axis.title=element_text(size=14)) + theme(legend.position = "none") 
         }
       }else{
         ggplot(df_subset(), aes(x = var1)) + 
           geom_bar(position=radio1,aes(fill = var2,weight = df_subset()$weight),  na.rm = TRUE) + 
           labs(y = "Counts", x = input$var, fill = input$var2 ) +
           theme(axis.text.x = element_text(angle=90, vjust=0.5, size = 13), axis.title=element_text(size=14))
       }
       
     }else{
      
        if( input$radio2 ==  "Percentages" ){
          if(input$radio ==  "Color" ){
            ggplot(df_subset(), aes(x = as.factor(var1))) + 
              geom_bar( position="fill",aes( fill = as.factor(var2))) + 
              scale_y_continuous(labels = percent_format())+
              labs(y = "Percentages", x = input$var, fill = input$var2 ) +
              theme(axis.text.x = element_text(angle=90, vjust=0.5, size = 13), axis.title=element_text(size=14))
          }else{
            ggplot(df_subset(), aes(x= var2,  group=as.factor(var1))) + 
              geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
              geom_text(aes( label = scales::percent(..prop..),
                             y= ..prop.. ), stat= "count", vjust = -.5) +
              labs(y = "Percentages",x = input$var2 , fill=input$var2) +
              facet_grid(var1) +
              scale_y_continuous(labels=percent) +
              theme(axis.text.x = element_text(angle=90, vjust=0.5, size = 13), axis.title=element_text(size=14))+ theme(legend.position = "none") 
            
          }
        }else {
          ggplot(df_subset(), aes(x = var1)) + 
            geom_bar(position=radio1,aes(fill = var2),  na.rm = TRUE) + 
            labs(y = "Counts", x = input$var, fill = input$var2 )  +
            theme(axis.text.x = element_text(angle=90, vjust=0.5, size = 13), axis.title=element_text(size=14))
        }
     }
      

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

