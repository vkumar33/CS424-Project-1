#library(devtools)
library(shiny)
library(shinydashboard)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(tidyr)
library(plotly)
library(magrittr)
library(dplyr)
library(pillar)
library(tidyverse)
require(devtools)
#devtools::install_github('hadley/ggplot2')
library(ggplot2)
library(data.table)
#install_github("nik01010/dashboardthemes")
#library(dashboardthemes)

#library(maps)
#library(maptools)
#library(rworldmap)
#library(ggmap)

#If you want to load ggmaps then use the following parameters
#register_google(key = "AIzaSyCpkAR0EuIp5K2hK1ke-9JexMzBBVrcK6o")
#require(devtools)
#devtools::install_github("dkahle/ggmap", ref = "tidyup")



#Load files from directory
#f <- file.choose("aqs_sites.csv")
mapdata <- read.csv(file = "aqs_sites.csv",header = TRUE, sep = ",")
temp = list.files(pattern="^an.*csv")
allData2 <- lapply(temp, fread, stringsAsFactors = TRUE)
allData3 <- do.call(rbind, allData2)

allData3 <- allData3%>%
  arrange(State)

#Converting columns to character and numeric class
allData3$County <- as.character(allData3$County)
allData3$Year <- as.numeric(as.character(allData3$Year))
allData3$State <- as.character(allData3$State)

#Making a copy of original Dataframe
allData4 <- allData3

#Renaming column names
colnames(allData3)[c(5:10)] <- c("Good","Moderate","Unhealthy(Sensitive)","Unhealthy","Very Unhealthy","Hazardous")
colnames(allData3)[c(11:13)] <- c("Max AQI", "90th Percentile of AQI","Median AQI")
colnames(allData3)[c(14:19)] <- c("CO","NO2","Ozone","SO2","PM2_5","PM10")

#Making a copy of modified Dataframe
allData5 <- allData3

#Creating new column AQI Parameters and Days which has the aggregate days of all AQI parameters
allData3Gather <- gather(allData3, "AQI Parameters", "Days", 5:10)
#Creating new column Pollutants and Days which has the aggregate days of all Pollutants
allData3Gather2 <- gather(allData3, "Pollutants", "Days", 14:19)
#Creating new column Maxq and Values which has the aggregate of Max AQI, median and 90th Percentile of AQI
allData3Gather3 <- gather(allData3,"Maxq", "Values", 11:13)

#Creating new columns which has aggregate percentage of all pollutant types

allData4$CO <- (allData4$`Days CO` / allData4$`Days with AQI`)* 100
allData4$NO2 <- (allData4$`Days NO2` / allData4$`Days with AQI`) *100
allData4$Ozone <- (allData4$`Days Ozone` / allData4$`Days with AQI`) *100
allData4$SO2 <- (allData4$`Days SO2` / allData4$`Days with AQI`) * 100
allData4$PM2_5 <- (allData4$`Days PM2.5` / allData4$`Days with AQI`) * 100
allData4$PM_10 <- (allData4$`Days PM10` / allData4$`Days with AQI`) * 100


allData4 <- allData4 %>% mutate_if(is.numeric, round, 2)
allData4Gather <- gather(allData4,"PollutantsP","Values",20:25)

#Creating new columns which has aggregate percentage of all AQI types

allData5$Good <- (allData5$Good / allData5$`Days with AQI`)* 100
allData5$Moderate <- (allData5$Moderate / allData5$`Days with AQI`) *100
allData5$`Unhealthy(Sensitive)` <- (allData5$`Unhealthy(Sensitive)` / allData5$`Days with AQI`) *100
allData5$Unhealthy <- (allData5$Unhealthy / allData5$`Days with AQI`) * 100
allData5$`Very Unhealthy` <- (allData5$`Very Unhealthy` / allData5$`Days with AQI`) * 100
allData5$Hazardous <- (allData5$Hazardous / allData5$`Days with AQI`) * 100


allData5 <- allData5 %>% mutate_if(is.numeric, round, 2)
allData5Gather <- gather(allData5,"AQIP","Values",5:10)


# Create the menu items to select the different years, states and counties

years<-c(1980:2018)
counties <- allData3[,2]
states <- unique(allData3[,1])

years1 <-c(1980:2018)
counties1 <- allData3[,2]
states1 <- unique(allData3[,1])

years2 <-c(1980:2018)
counties2 <- allData3[,2]
states2 <- unique(allData3[,1])

years3 <-c(1980:2018)
counties3 <- allData3[,2]
states3 <- unique(allData3[,1])


# Dreate the shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Just Breathe"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   selectInput("Year", "Select the year to visualize", years, selected = 2018),
                   selectInput("State", "Select the State to visualize", states, selected = "Illinois"),
                   selectInput("County", "Select the County to visualize", counties, selected = "Cook"),

                   
                   sidebarMenu(
                     menuItem("About",tabName = "about"),
                     menuItem("AQI Quality",tabName = "aqiquality"),
                     menuItem("Pollutant Types",tabName = "Pollutant"),   
                     menuItem("Compare Data of 3 Counties", tabName = "comparedata")
                     
                   )),
  
  dashboardBody(
    tabItems(
      tabItem("about", includeHTML("about.html")),
      tabItem("aqiquality", 
              fluidRow(
                
                column(12,
                       fluidRow( 
                         box(
                           width = 18,
                           status = "info",
                           dataTableOutput("AQIQualityTable")
                         ),
                         box(
                           width=15,status = "info", plotlyOutput("AQImaxq")
                         )
                       )
                ),
                
                column(4,
                       fluidRow(
                         box(
                           width = 15,
                           status = "info",
                           plotlyOutput("AQIPiePlot")
                         )
                       )
                ),
                
   
                

                
                column(5,
                       fluidRow(
                         box(
                           width = 18,
                           status = "info",
                           plotlyOutput("AQIBarGraph")
                         )
                       )
                )
                
                
              )
      ),
      
      
      
      
      tabItem("Pollutant", 
              fluidRow(
                
                column(12,
                       fluidRow( 
                         box(
                           width = 14,
                           status = "info",
                           dataTableOutput("PollutantinfoTable")
                         ),
                         box(width = 14, status = "info",dataTableOutput("PollutantTypeTable")),
                         box(width=15,status = "info", plotlyOutput("Pollutantline")),
                         box(width=15,status = "info", plotlyOutput("Pollutantline1"))
                       )
                ),
                
                column(4,
                       fluidRow(
                         box(
                           width = 15,
                           status = "info",
                           plotlyOutput("PollutantinfoPiePlot")
                         )
                       )),
          
                column(4,fluidRow(
                         box(
                           width = 15,
                           status = "info",
                           plotlyOutput("PollutantinfoBarGraph")
                         
                       ))),

                
                column(4,fluidRow(
                       
                         box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                             leafletOutput("leaf")
                         )
                       
                )
                )
                
              )
      ),
      tabItem("comparedata",
              fluidRow(
                fluidRow(box(width = 4, selectInput("Year1", "Select the year to visualize", years1, selected = 2018, width="120px"),
                             selectInput("State1", "Select the 1st State to visualize", states1, selected = "Illinois", width="120px"),
                             selectInput("County1", "Select the 1st County to visualize", counties1, selected = "Cook", width="120px")),
                         
                         box(width = 4, selectInput("Year2", "Select the year to visualize", years2, selected = 2018, width="120px"),
                             selectInput("State2", "Select the 2nd State to visualize", states2, selected = "Illinois", width="120px"),
                             selectInput("County2", "Select the 2nd County to visualize", counties2, selected = "Cook",width="120px")),
                         
                         box(width = 4, selectInput("Year3", "Select the year to visualize", years3, selected = 2018, width="120px"),
                             selectInput("State3", "Select the 3rd State to visualize", states3, selected = "Illinois", width="120px"),
                             selectInput("County3", "Select the 3rd County to visualize", counties3, selected = "Cook",width="120px"))),
                
                column(12,
                       fluidRow(
                         box(
                           width = 15,
                           status = "info",
                           plotlyOutput("Compare_aqiBarGraph")
                         )
                       )),
                column(12,fluidRow(
                  box(
                    width = 15,
                    status = "info",
                    plotlyOutput("Compare_pollutantBarGraph")
                    
                  ))),
                
                column(12,fluidRow(
                  box(
                    width = 15,
                    status = "info",
                    plotlyOutput("Compare_aqiLineGraph")
                    
                  ))),
                column(12,fluidRow(
                  box(
                    width = 15,
                    status = "info",
                    plotlyOutput("Compare_pollutantLineGraph")
                    
                  )))
                
                
        
        
                  )
          )
    )
  )
 
)


server <- function(input,output,session){
  
  observe({
    
    # Can use character(0) to remove all choices
    if (is.null(input$State))
      input$State <- character(0)
    
    a <- unique(as.character(allData3[allData3$State == input$State,2]))
      
    # Can also set the label and select items
    updateSelectInput(session, "County",
                      choices = sort(a))
  })
  
  observe({
    
    # Can use character(0) to remove all choices
    if (is.null(input$State1))
      input$State1 <- character(0)
    
    b <- unique(as.character(allData3[allData3$State == input$State1,2]))
    
    # Can also set the label and select items
    updateSelectInput(session, "County1",
                      choices = sort(b))
  })
  
  observe({
    
    # Can use character(0) to remove all choices
    if (is.null(input$State2))
      input$State2 <- character(0)
    
    c <- unique(as.character(allData3[allData3$State == input$State2,2]))
    
    # Can also set the label and select items
    updateSelectInput(session, "County2",
                      choices = sort(c))
  })
  
  
  observe({
    
    # Can use character(0) to remove all choices
    if (is.null(input$State3))
      input$State3 <- character(0)
    
    d <- unique(as.character(allData3[allData3$State == input$State3,2]))
    
    # Can also set the label and select items
    updateSelectInput(session, "County3",
                      choices = sort(d))
  })
  
  data1 <- reactive({
    
    df1 <- allData3Gather[(allData3Gather$State == input$State1  & allData3Gather$County == input$County1 & allData3Gather$Year == input$Year1) | 
                           (allData3Gather$State == input$State2  & allData3Gather$County == input$County2 & allData3Gather$Year == input$Year2) | 
                           (allData3Gather$State == input$State3  & allData3Gather$County == input$County3 & allData3Gather$Year == input$Year3) ,c(1,2,14,15) ]
    
    df1 <- df1[order(df1$County),]
    
    df1$'County-State' <- paste0(df1$County,"(",df1$State,")")
    
    df1
  })
  
  
  
  
  data2 <- reactive({
    
    df2 <- allData3Gather2[(allData3Gather2$State == input$State1  & allData3Gather2$County == input$County1 & allData3Gather2$Year == input$Year1) | 
                             (allData3Gather2$State == input$State2  & allData3Gather2$County == input$County2 & allData3Gather2$Year == input$Year2) | 
                             (allData3Gather2$State == input$State3  & allData3Gather2$County == input$County3 & allData3Gather2$Year == input$Year3) ,c(1,2,14,15) ]
    
    df2 <- df2[order(df2$County),]
    
    df2$'County-State' <- paste0(df2$County,"(",df2$State,")")
    
    df2
  })
  
  data3 <- reactive({
    
    df3 <- allData5Gather[(allData5Gather$State == input$State1  & allData5Gather$County == input$County1 ) | 
                            (allData5Gather$State == input$State2  & allData5Gather$County == input$County2) | 
                            (allData5Gather$State == input$State3  & allData5Gather$County == input$County3) ,c(1,2,3,14,15)]
    
    df3 <- df3[order(df3$County),]
    
    df3$'County-State' <- paste0(df3$County,"(",df3$State,")")
    
    df3
  })
  
  data4 <- reactive({
    
    df4 <- allData4Gather[(allData4Gather$State == input$State1  & allData4Gather$County == input$County1) | 
                            (allData4Gather$State == input$State2  & allData4Gather$County == input$County2) | 
                            (allData4Gather$State == input$State3  & allData4Gather$County == input$County3) ,c(1,2,3,20,21)]
    
    df4 <- df4[order(df4$County),]
    
    df4$'County-State' <- paste0(df4$County,"(",df4$State,")")
    
    df4
  })
  
  
  data5 <- reactive({
    
    df5 <- allData3Gather[allData3Gather$State == input$State  & allData3Gather$County == input$County & allData3Gather$Year == input$Year, ]
    df5 <- df5[order(df5$County,-df5$Days),]
    
    df5
    
  })

  
  data6 <- reactive({
    
    df6 <- allData3Gather3[allData3Gather3$State == input$State & allData3Gather3$County == input$County ,]
    df6 <- df6[order(df6$County),]
    
    df6
    
  })
  
  data7 <- reactive({
    
    df7 <- allData3Gather2[allData3Gather2$State == input$State & allData3Gather2$County == input$County & allData3Gather2$Year == input$Year,]
    df7 <- df7 %>%
          arrange(County,desc(Days))
    
    df7
    
  })
  
  
  data8 <- reactive({
  
  df8 <- allData4Gather[allData4Gather$State == input$State & allData4Gather$County == input$County ,]
  df8 <- df8[order(df8$County),]
    
  })
  
  data10 <- reactive({
    
    df10 <- allData5Gather[allData5Gather$State == input$State & allData5Gather$County == input$County & allData5Gather$Year == input$Year,]
    df10 <- df10 %>%
      arrange(County)
    
    df10
    
  })
    
  output$AQIQualityTable <-
    renderDataTable(allData3[allData3$County == input$County & allData3$State == input$State & allData3$Year == input$Year ,1:11 ], options = list(columnDefs = list(list(
      targets = 1:11 , className = "right"
    )), pageLength = 5))
  
  
  
  output$AQIBarGraph  <- renderPlotly ({
    
    plot_ly(data = allData3Gather[allData3Gather$State == input$State  & allData3Gather$County == input$County & allData3Gather$Year == input$Year, ], x = reorder(allData3Gather[allData3Gather$State == input$State  & allData3Gather$County == input$County & allData3Gather$Year == input$Year, 14],-allData3Gather[allData3Gather$State == input$State  & allData3Gather$County == input$County & allData3Gather$Year == input$Year,15 ]), y = allData3Gather[allData3Gather$State == input$State  & allData3Gather$County == input$County & allData3Gather$Year == input$Year,15 ],
            color = (reorder(allData3Gather[allData3Gather$State == input$State  & allData3Gather$County == input$County & allData3Gather$Year == input$Year, 14],-allData3Gather[allData3Gather$State == input$State  & allData3Gather$County == input$County & allData3Gather$Year == input$Year,15 ])) , 
                     type = 'bar', colors = c("#2E4272","#3C8D2F","#E5E540","#AAA238","#C6590F","#C62B0F")) %>%
                     layout(title = paste("AQI types for", input$Year))
            
  })
  
  output$AQIBarGraph1  <- renderPlotly ({
    
    ggplot(data5(), aes(y=Days, x=`AQI Parameters`, color=`AQI Parameters`, fill=`AQI Parameters`)) + 
    geom_bar( stat="identity") + labs( y = "No of Days",title = paste("AQI types for", input$Year))
    
    
    
  })
  
  
  output$AQIPiePlot <- 
    renderPlotly({
      colors <- c("#3C8D2F","#2E4272","#E5E540","#AAA238","#C6590F","#C62B0F")
      plot_ly(allData3Gather[allData3Gather$State == input$State  & allData3Gather$County == input$County & allData3Gather$Year == input$Year, ], labels = ~ allData3Gather[allData3Gather$State == input$State  & allData3Gather$County == input$County & allData3Gather$Year == input$Year, 14], values = ~ allData3Gather[allData3Gather$State == input$State  & allData3Gather$County == input$County & allData3Gather$Year == input$Year, 15], type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),        showlegend = FALSE) %>%
        layout(title = paste('AQI Quality for year ', input$Year),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
  
  output$AQIPiePlot1  <- renderPlotly ({
    
    
    ggplot(data10(), aes("",Values, fill=AQIP)) + 
      geom_bar(width = 1, stat = "identity") + coord_polar("y") + labs(title = paste("AQI Quality for year", input$Year)) +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, color = "#666666"))
    
  })
  
  
  
  output$AQImaxq <- renderPlotly({ 
    
    plot_ly(data = allData3Gather3[allData3Gather3$State == input$State & allData3Gather3$County == input$County,], 
          x = allData3Gather3[allData3Gather3$State == input$State & allData3Gather3$County == input$County,3],
          y = allData3Gather3[allData3Gather3$State == input$State & allData3Gather3$County == input$County,18], 
          color = allData3Gather3[allData3Gather3$State == input$State & allData3Gather3$County == input$County,17],
          colors = c("#970D09","#172457","#077807"),type="scatter",mode="lines+markers") %>%
          layout(title = "AQI over the years")
    
  })
  
  output$AQImaxq1 <- renderPlotly({
    
    
    ggplot(data6(), aes(y=Values, x=Year, color=Maxq, group=Maxq)) + 
    geom_line() +  labs(y = "No of Days",title = "AQI over the years") + theme(axis.title.x=element_blank(),legend.title =element_blank())
    })
  
  output$PollutantinfoTable <-
    renderDataTable(allData3[allData3$County == input$County & allData3$State == input$State & allData3$Year == input$Year ,c(1,2,3,14,15,16,17,18,19) ], 
                    options = list(columnDefs = list(list(
                    targets = 1:9 , className = "right")), pageLength = 5))
  
  
  
  output$PollutantTypeTable <-
    renderDataTable(allData4[allData4$County == input$County & allData4$State == input$State ,c(1,2,3,20,21,22,23,24,25) ], 
                    options = list(columnDefs = list(list(targets = 1:9 , className = "right")), pageLength = 5))
  
  
  
  output$PollutantinfoBarGraph  <- renderPlotly ({
    
  plot_ly(data = allData3Gather2[allData3Gather2$State == input$State  & allData3Gather2$County == input$County & allData3Gather2$Year == input$Year, ], x = reorder(allData3Gather2[allData3Gather2$State == input$State  & allData3Gather2$County == input$County & allData3Gather2$Year == input$Year, 14],-allData3Gather2[allData3Gather2$State == input$State  & allData3Gather2$County == input$County & allData3Gather2$Year == input$Year, 15]), y = allData3Gather2[allData3Gather2$State == input$State  & allData3Gather2$County == input$County & allData3Gather2$Year == input$Year,15 ],
          color = reorder(allData3Gather2[allData3Gather2$State == input$State  & allData3Gather2$County == input$County & allData3Gather2$Year == input$Year, 14],-allData3Gather2[allData3Gather2$State == input$State  & allData3Gather2$County == input$County & allData3Gather2$Year == input$Year, 15]) , 
          type = 'bar', colors = c("#801515","#974909","#77074B","#482E74","#2F3F73","#729C34")) %>%
          layout(title = paste("Pollutant types for", input$Year))
    
    
  })
  
  output$PollutantinfoBarGraph1  <- renderPlotly ({
    
    ggplot(data7(), aes(y=Days, x=Pollutants, color=Pollutants, fill=Pollutants)) + 
      geom_bar( stat="identity") + labs( y = "No of Days",title = paste("Pollutant types for", input$Year))
    
  })
  
  
  
  output$PollutantinfoPiePlot <- 
    renderPlotly({
      colors <- c("#801515","#974909","#77074B","#482E74","#2F3F73","#729C34")
      plot_ly(allData3Gather2[allData3Gather2$State == input$State  & allData3Gather2$County == input$County & allData3Gather2$Year == input$Year, ], labels = ~ allData3Gather2[allData3Gather2$State == input$State  & allData3Gather2$County == input$County & allData3Gather2$Year == input$Year, 14], values = ~ allData3Gather2[allData3Gather2$State == input$State  & allData3Gather2$County == input$County & allData3Gather2$Year == input$Year, 15], type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),        showlegend = FALSE) %>%
        layout(title = paste('Pollutants for year ', input$Year) ,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
  
  
  output$PollutantinfoPiePlot1 <- renderPlotly({
    
    
    df9 <- allData4Gather[allData4Gather$State == input$State & allData4Gather$County == input$County & allData4Gather$Year == input$Year,]
    df9 <- df9 %>%
      arrange(County)
    
    
    
    pieplot <-ggplot(df9,aes(x ="",y=Values,  fill=PollutantsP)) + 
    geom_bar( width = 1, stat = "identity") + coord_polar("y") + 
    labs( title = paste("AQI types for", input$Year)) +
    theme_classic() +
    theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, color = "#666666"))
  
  pieplot
  
  })
  
  
  output$Pollutantline <- renderPlotly({ 
    
    plot_ly(data = allData4Gather[allData4Gather$State == input$State & allData4Gather$County == input$County,], 
            x = allData4Gather[allData4Gather$State == input$State & allData4Gather$County == input$County,3],
            y = allData4Gather[allData4Gather$State == input$State & allData4Gather$County == input$County,21], 
            color = allData4Gather[allData4Gather$State == input$State & allData4Gather$County == input$County,20],
            colors = c("#801515","#974909","#77074B","#482E74","#2F3F73","#729C34"),type="scatter",mode="lines+markers") %>%
      layout(title = "Pollutants % over the years")
    
  })
  
  output$Pollutantline1 <- renderPlotly({ 
    ggplot(data8(), aes(y=Values, x=Year, color=PollutantsP, group=PollutantsP)) + geom_line() +  labs(y = "No of Days",title = "Pollutants % over the years") +
      theme(axis.title.x=element_blank(),legend.title =element_blank())
    
    })
  
  output$leaf <- renderLeaflet({
  
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = mapdata[mapdata$County.Name == input$County & mapdata$State.Name == input$State,5][1], lat = mapdata[mapdata$County.Name == input$County & mapdata$State.Name == input$State,4][1], zoom = 2)
    #map <- addMarkers(map, lng = mapdata[mapdata$County.Name == input$County & mapdata$State.Name == input$State,5][1], lat = mapdata[mapdata$County.Name == input$County & mapdata$State.Name == input$State,4][1], popup = input$County, icon = list(iconUrl = "https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon-2x.png", iconSize = c(25,41)))
    map <- addCircleMarkers(map, lng = mapdata[mapdata$County.Name == input$County & mapdata$State.Name == input$State,5][1], lat = mapdata[mapdata$County.Name == input$County & mapdata$State.Name == input$State,4][1], popup = input$County)
    
    map
  })
  
  output$Compare_aqiBarGraph <- renderPlotly({
      
    
    ggplot(data1(), aes(y=Days, x=`AQI Parameters`, color=`AQI Parameters`, fill=`AQI Parameters`)) + 
      geom_bar( stat="identity") +  facet_wrap(~`County-State`) + labs( y = "No of Days",title = "AQI types")+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.title =element_blank())
    
  })

  output$Compare_pollutantBarGraph <- renderPlotly({

    ggplot(data2(), aes(y=Days, x=Pollutants, color=Pollutants, fill=Pollutants)) + 
      geom_bar( stat="identity") +  facet_wrap(~`County-State`) + labs(y = "No of Days",title = "Pollutant types")+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.title =element_blank())
    
    
  })
  
  output$Compare_aqiLineGraph <- renderPlotly({

    ggplot(data3(), aes(y=Values, x=Year, color=AQIP, group =AQIP)) + 
      geom_line() +  facet_wrap(~`County-State`) + labs( y = "No of Days",title = paste("AQI types % over the years"))+
      theme(axis.title.x=element_blank(),legend.title =element_blank())
  })
  
  output$Compare_pollutantLineGraph <- renderPlotly({

    
    ggplot(data4(), aes(y=Values, x=Year, color=PollutantsP, group =PollutantsP )) + 
      geom_line() +  facet_wrap(~`County-State`) + labs(y = "No of Days",title = paste("Pollutant types % over the years"))+
      theme(axis.title.x=element_blank(),legend.title =element_blank())
    
    
  })
  
  
}
shinyApp(server = server, ui = ui)