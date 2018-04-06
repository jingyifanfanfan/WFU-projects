################################################################################
# MSBA Data Visualization5
# November 2017
#
# HW5: Analysis with Shiny
#
# Your Name:Jingyi Fan
################################################################################
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
gfm <- read.csv("HW5_GoFundMe.csv")
levels(gfm$Category) <-gsub('_', '\n', levels(gfm$Category))
se <- function(x){ sqrt(var(x, na.rm=T)/length(x))}
#sapply(gfm,class)
#levels(gfm$Category)
################################################################################
# UI function
ui <- fluidPage(
  titlePanel("HW 5: Go Fund Me Insights"),
  tabsetPanel(
    tabPanel("Explore",
             sidebarLayout(
               sidebarPanel(
                 # Question 1a: create the user inputs here.
               selectInput('xvar',
                           'Categorical Variable (X-axis)',
                           choices =  names(select_if(gfm, is.factor)),
                           selected = 'Category'    
               ), 
               selectInput('yvar',
                           'Numeric Variable (Y-axis)',
                           choices = c(as.list(names(select_if(gfm, is.integer))),as.list(names(select_if(gfm, is.numeric)))),
                           selected = 'Goal'
                           ),
               actionButton("go", "Go")),
               mainPanel(
                 plotOutput("distPlot")
               )
    )), 
    tabPanel("Insight",
                 #for question 2
                 sidebarLayout(
                   sidebarPanel(
                     checkboxGroupInput('Feelings',
                                 'Feeling statistics to display:',
                                 choices =  c('Happiness', 'Fear', 'Surprise', 'Sadness', 
                                              'Neutral', 'Anger'),
                                 selected = 'Happiness'), 
                     sliderInput('top',
                                 'Choose number of categories to display (ordered descendingly 
                                 by average of amount raised)',
                                 min = 1,
                                 max = 112,
                                 value = 15,
                                 step = 1
                     ),
                     actionButton("go", "Go")),
                   mainPanel(                 
                     plotOutput("insightPlot"),
                    textOutput("insight1"),
                    textOutput("insight2"))
  ))
))
################################################################################
# Server function: 
# Server logic required to make the plots
server <- function(input, output) {
  restrictedData <- eventReactive(input$go,{
    # This verifies that the user inputs are the right class.
    if (!class(gfm[,input$xvar])%in% c("factor", "character")) { return(NULL) }
    if (!class(gfm[,input$yvar])%in% c("numeric", "integer")) { return(NULL) }
    cbind.data.frame(x=gfm[,input$xvar],y=gfm[,input$yvar])
  })
  # Question 1b: Create a reactive dataset for the plot
  plotData<- reactive({
     df<- restrictedData()%>%
      group_by(x)%>%
      na.omit()%>%
      summarise(mean = mean(y), se = se(y))%>%
      arrange(desc(mean))%>%
      head(n=10)
  })
  
  ##For Question 2
     sub<- 
      gfm%>%
      select(Category, AmountRaised, MeanHappiness, 
             MeanFear, MeanSurprise, MeanSadness, MeanNeutral, MeanAnger)
    names(sub)<- gsub('Mean','',names(sub))
    
    library(tidyverse) 
    library(stringr)
    
    set1<- reactive({sub%>%
        gather(var, value, 2:8)%>%
        group_by(Category,var)%>%
        na.omit()%>%
        summarise(mean = mean(value))%>%
        spread(var,mean)%>%
        arrange(desc(AmountRaised))%>%
        head(n=as.numeric(input$top))%>%
        gather(Feel, score, 3:8)
    })
    
    set2<- reactive({cbind.data.frame(set1(),Order = rep(c(1:as.numeric(input$top)),6))})
    
    plotData2<-reactive({set2()%>%
        group_by(Category,Order,Feel)%>%
        summarise(mean = mean(score))%>%
        spread(Feel,mean)%>%
        select(Category,Order,one_of(input$Feelings))%>%
        gather(feel, Score,-c(Category, Order))
    })
    
      ##end of part for Question 2
    
  # end Question 1b  
    observeEvent(input$go, {
    output$distPlot <- renderPlot({
      if (is.null(plotData())) {return(NULL)}
      # Question 1c: Create the categorical plot here.
    ggplot(data=plotData(),aes(x = reorder(x,-mean), y = mean))+
      geom_bar(stat="identity", fill="light blue")+
      geom_errorbar(data= plotData(),aes(ymin=mean-se, ymax = mean+se),
                    colour="black", width=0.5, size=1)+
      theme_bw()+
      ggtitle("Exploring the data")+
      theme(plot.title = element_text(face="bold",hjust=0.5))+
      labs(x =" ", y = " ")
    })
  })
 output$insightPlot <- renderPlot({
    # Q2a: Insert the insight plot here.
   ggplot(data = plotData2(),aes(x = reorder(Category, Order), y = Score,color = feel,
                                 group = feel))+
     geom_point()+
     geom_line()+
     theme_bw()+
     ggtitle("Average score of feelings by categories of fundraising events",
          subtitle = 'Categories are ordered descendingly from left to right by average of amount raised.')+
    labs(x =" ", y = " ")+
     theme(plot.title = element_text(face="bold",size = 16), 
           plot.subtitle =element_text(size = 12))
   
      })
  output$insight1 <- renderText({
    # Q2b: Insert the caption text here.
    paste("Insight 1:", "Among all 6 feelings, Happiness scores highest, meaning that people are more likely to donate when they feel more positively. 
          It also indicates that successful fundraising events are those arousing more positive feelings than negative feelings among people.")
  })
  output$insight2 <- renderText({
    # Q2b: Insert the caption text here.
    paste("Insight 2:", 'By looking at each feeling for the top 15 categories separately, Fund raising campaigns for AIDS Walks tend to be the "Outliner of category", given that its average score of feelings is the
           lowest for Happiness, Fear, Sadness and Neutral and highest for Surprise and Anger. As a decease that is hard to cure for now and somewhat special,
          people generally demonstrate stronger empathy and determination to fight against it instead of simply showing sympathy. This would be helpful when
          setting the tone or designing marketing campaign for AIDS-related fundraising activities.')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

