#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(class)
library(caret)
library(stringr)
library(tidyverse)
library(e1071)
library(dplyr)
library(RANN)

ui <- fluidPage(

  sidebarPanel(
    fileInput("file1", 'Choose Beer Data File',
              multiple=FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    tags$hr(),
    
    fileInput("file2", 'Choose Brewery Data File',
              multiple=FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    selectInput('state', label=h3('Choose State'),
                choices=list("All" = "", 
                             'Alabama'='AL','Alaska'='AK','Arizona'='AZ','Arkansas'='AR','California'='CA',
                             'Colorado'='CO','Connecticut'='CT','Delaware'='DE','Florida'='FL','Georgia'='GA',
                             'Hawaii'='HI','Idaho'='ID','Illinois'='IL','Indiana'='IN','Iowa'='IA',
                             'Kansas'='KS','Kentucky'='KY','Louisiana'='LA','Maine'='ME','Maryland'='MD',
                             'Massachusetts'='MA','Michigan'='MI','Minnesota'='MN','Mississippi'='MS','Missouri'='MO',
                             'Montana'='MT','Nebraska'='NE','Nevada'='NV','New Hampshire'='NH','New Jersey'='NJ',
                             'New Mexico'='NM','New York'='NY','North Carolina'='NC','North Dakota'='ND','Ohio'='OH',
                             'Oklahoma'='OK','Oregon'='OR','Pennsylvania'='PA','Rhode Island'='RI','South Carolina'='SC',
                             'South Dakota'='SD','Tennessee'='TN','Texas'='TX','Utah'='UT','Vermont'='VT',
                             'Virginia'='VA','Washington'='WA','West Virginia'='WV','Wisconsin'='WI','Wyoming'='WY')),
    
    tags$hr(),
    
    radioButtons('plotVar', 'ABV or IBU',
                 choices=c(ABV='ABV',
                           IBU='IBU'),
                 selected='ABV'),
    
    tags$hr(),
    
    radioButtons('plotType', 'Choose Plot to Display',
                 choices=c(Histogram='histogram',
                           Boxplot='boxplot'),
                 selected='histogram'),
    
    tags$hr(),
    
    radioButtons('plotType2', 'Choose Plot to Display',
                 choices=c(Scatter='scatter',
                           Imputation='imputePlot'),
                 selected='scatter'),
    
    tags$hr(),
    
    checkboxInput('regline','Add Regression Line', value=FALSE),
    
    checkboxInput('impute','Impute IBU using KNN', value=TRUE)
  ),
        mainPanel(
           plotOutput("distPlot"),plotOutput('scatterPlot')
        )
    )


server <- function(input, output) {
  
    df <- reactive({
      beerDF <- read.csv(input$file1$datapath,header=TRUE)
      brewDF <- read.csv(input$file2$datapath,header=TRUE)
      brewDF$State = trimws(brewDF$State)
      dfTemp <- merge(beerDF, brewDF, by.x='Brewery_id', by.y='Brew_ID')
      beerTypes = c('Ale',
                    'Lager',
                    'Stout',
                    'Porter',
                    'Pilsner',
                    'Pilsener',
                    'IPA',
                    'Cider',
                    'Oktoberfest',
                    'Witbier',
                    'Kölsch',
                    'Fruit',
                    'Hefeweizen',
                    'Other')
      
      # Group the beers by their type
      dfTemp$beerCat <- ifelse(grepl(beerTypes[1], dfTemp$Style), beerTypes[1],
                                            ifelse(grepl(beerTypes[2], dfTemp$Style), beerTypes[2],
                                                   ifelse(grepl(beerTypes[3], dfTemp$Style), beerTypes[3],
                                                          ifelse(grepl(beerTypes[4], dfTemp$Style), beerTypes[4],
                                                                 ifelse(grepl(beerTypes[5], dfTemp$Style), beerTypes[5],
                                                                        ifelse(grepl(beerTypes[6], dfTemp$Style), beerTypes[5],
                                                                               ifelse(grepl(beerTypes[7], dfTemp$Style), beerTypes[7],
                                                                                      ifelse(grepl(beerTypes[8], dfTemp$Style), beerTypes[8],
                                                                                             ifelse(grepl(beerTypes[9], dfTemp$Style), beerTypes[9],
                                                                                                    ifelse(grepl(beerTypes[10], dfTemp$Style), beerTypes[10],
                                                                                                           ifelse(grepl(beerTypes[11], dfTemp$Style), beerTypes[11],
                                                                                                                  ifelse(grepl(beerTypes[12], dfTemp$Style), beerTypes[12],
                                                                                                                         ifelse(grepl(beerTypes[13], dfTemp$Style), beerTypes[13],
                                                                                                                                beerTypes[14])))))))))))))
      # Make the beerCat column a factor
      dfTemp <- filter(dfTemp, !beerCat=='Cider')
      dfTemp$beerCat = factor(dfTemp$beerCat)
      dfTemp <- filter(dfTemp, !is.na(ABV))
      if(input$impute==FALSE){
        dfTemp
      } else {
        preProcValues <- preProcess(dfTemp %>%
                                      dplyr::select(
                                        ABV,
                                        IBU,
                                        beerCat),
                                    method=c('knnImpute'),
                                    k=5,
                                    knnSummary=mean)
        impute_IBU_info <- predict(preProcValues,
                                   dfTemp,
                                   na.action=na.pass)
        procNames <- data.frame(col=names(preProcValues$mean),
                                mean=preProcValues$mean,
                                sd=preProcValues$std)
        for(i in procNames$col){
          impute_IBU_info[i] <- impute_IBU_info[i]*preProcValues$std[i]+preProcValues$mean[i]
        }
        impute_IBU_info
      }
    })
    
    
    plotDF <- reactive({
      if(input$state==''){
        df()
      } else {
        filter(df(), State==input$state)
      }
    })
    

    output$distPlot <- renderPlot({
      
      if(input$plotVar == 'ABV'){
        if(input$plotType == 'histogram'){
          ggplot(plotDF(), aes(x=ABV))+
            geom_histogram(fill='forestgreen')+
            ggtitle('Distribution of ABV')+
            xlab('ABV')+
            ylab('Count')
        } else if(input$plotType == 'boxplot'){
          ggplot(plotDF(), aes(y=ABV))+
            geom_boxplot(fill='forestgreen')+
            coord_flip()+
            ggtitle('Distribution of ABV')+
            xlab('ABV')+
            ylab('Count')
        }
      } else {
        if(input$plotType == 'histogram'){
          ggplot(plotDF(), aes(x=IBU))+
            geom_histogram(fill='steelblue')+
            ggtitle('Distribution of IBU')+
            xlab('IBU')
        } else if(input$plotType == 'boxplot'){
          ggplot(plotDF(), aes(y=IBU))+
            geom_boxplot(fill='steelblue')+
            coord_flip()+
            ggtitle('Distribution of IBU')+
            xlab('IBU')
        }
      }
    })
    
  output$scatterPlot <- renderPlot({
    if(input$plotType2 == 'scatter'){
      if(input$regline==FALSE){
        ggplot(plotDF(),aes(x=ABV,y=IBU))+
          geom_point()+
          xlim(0,.150)+
          ylim(0,150)+
          ggtitle('ABV vs. IBU')+
          xlab('ABV')+
          ylab('IBU')
      } else {
        ggplot(plotDF(),aes(x=ABV,y=IBU))+
          geom_point()+
          geom_smooth(method='lm')+
          xlim(0,.150)+
          ylim(0,150)+
          ggtitle('ABV vs. IBU with linear regression')+
          xlab('ABV')+
          ylab('IBU')
      }
    } else {
      ggplot(df(), 
             aes(x = ABV, 
                 y = Brewery_id, 
                 color = IBU)) +
        geom_point(position = 'jitter') +
        facet_wrap(~beerCat) +
        scale_color_gradient(low = 'green', 
                             high = 'red',
                             na.value = 'blue')+
        ggtitle('Imputing missing IBU values')+
        xlab('ABV')+
        ylab('Brewery ID')
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
