library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gdata)
library(stats)
library(curl)
library(stringr)

##
# Reading in the greenhouse data
data.all <- readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/greenhouse/getdata.php") 

# Sort the data based on groups and players
data.all$GroupID <- tolower(data.all$GroupID)
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))
# all_Season <- sort(unique(data.all$Season))

# Calculating the Overall money that a team has by the end of each season using 
# cumulative sum function cumsum
data.all <- data.all %>% group_by(GameNum) %>% mutate(OverallProfit = cumsum(Profit)[(length(cumsum(Profit)))])
data.all <- data.all %>% ungroup()    
data.all <- data.all %>% mutate(OverallMoney = 5000 + OverallProfit)

# Calculating the Seasonal Profit which is the gains and losses made after 
# every season.
data.all <- data.all %>% group_by(GameNum,Season) 
data.all <- data.all %>% mutate(SeasonalProfit = cumsum(Profit))


# Filter out repeating datapoints based on the max number of plots used in 
# a single season in aa single game.
data.all <- data.all %>% filter(Plot == max(Plot))
data.all <- data.all %>% ungroup()

# Restricting to Level 1 Data
data.all <- data.all %>% filter(Level == 1)


ui <- fluidPage(
  
  titlePanel("Level 1: GreenHouse High Scores"),
  fluidRow(
    column(3, 
           
           #Table Tab
           tabPanel("Leaderboard",
                    selectInput(inputId = "GroupID",
                                label = "Group ID:", 
                                choices =  c(all_groups),
                                multiple = TRUE,
                                selectize = TRUE,
                                selected = "ekustatsfall21"),
                    
                    selectInput(inputId = "PlayerID",
                                label = "Player ID:",
                                choices =  c("all", all_players),
                                multiple = TRUE,
                                selectize = TRUE,
                                selected = "all"),
                    
                    selectInput(inputId = "Season",
                                label = "Season:",
                                choices = c("1", "2", "3", "4", "5"),
                                selected = "1",
                                multiple = FALSE,
                                selectize = TRUE),
                    
                    selectInput(inputId = "yvar",
                                label = "Money",
                                choices = c("OverallMoney", "SeasonalProfit"),
                                selected = "OverallMoney",
                                multiple = FALSE)
                    
           )),
    
    
    
    column(9, 
           
           #Outputs
           
           tableOutput("summarytable"),
           uiOutput("summarytext")
           
    )))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  data.all <- reactive({
    
    data.all <- readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/greenhouse/getdata.php")  
    
    # Sort the data based on groups and players
    data.all$GroupID <- tolower(data.all$GroupID)
    all_groups <- sort(unique(data.all$GroupID))
    all_players <- sort(unique(data.all$PlayerID))
    # all_Season <- sort(unique(data.all$Season))
    
    # Calculating the Overall money that a team has by the end of each season using 
    # cumulative sum function cumsum
    data.all <- data.all %>% group_by(GameNum) %>% mutate(OverallProfit = cumsum(Profit)[(length(cumsum(Profit)))])
    data.all <- data.all %>% ungroup()    
    data.all <- data.all %>% mutate(OverallMoney = 5000 + OverallProfit)
    
    # Calculating the Seasonal Profit which is the gains and losses made after 
    # every season.
    data.all <- data.all %>% group_by(GameNum,Season)
    data.all <- data.all %>% mutate(SeasonalProfit = cumsum(Profit))
    
    
    # Filter out repeating datapoints based on the max number of plots used in 
    # a single season in aa single game.
    data.all <- data.all %>% filter(Plot == max(Plot))
    data.all <- data.all %>% ungroup()
    
    #Changing to Factor/Character
    data.all$Level <- as.factor(data.all$Level)
    data.all$GroupID <- as.character(data.all$GroupID)
    data.all$PlayerID <- as.character(data.all$PlayerID)
    data.all$Season <- as.factor(data.all$Season)
    data.all$Money <- as.factor(data.all$Money)
    data.all$Plot <- as.factor(data.all$Plot)
    data.all$Crop <- as.factor(data.all$Crop)
    
    #To use for Inputs
    all_groups <- sort(unique(data.all$GroupID))
    all_players <- sort(unique(data.all$PlayerID))
    all_Season <- sort(unique(data.all$Season))
    all_Money <- sort(unique(data.all$Money), decreasing = FALSE)
    all_Plot <- sort(unique(data.all$Plot))
    all_Crop <- sort(unique(data.all$Crop))
    
    
    data.all <- select(data.all, -Date)
    data.all <- na.omit(data.all)
    
    return(data.all)
    
  })
  
  #Reactive Data
  plotDataR <- reactive({
    
    data.all <- data.all()
    
    if("all" %in% input$GroupID){
      gamedata <- data.all
      
    } else {
      if("all" %in% input$PlayerID){
        gamedata <- filter(data.all, GroupID %in% input$GroupID)
        
      } else {
        gamedata <- filter(data.all, GroupID %in% input$GroupID, PlayerID %in% input$PlayerID)
        
      } 
    }
    
    return(gamedata)
    
  })
  #Dynamic PlayerID Input
  observe({
    
    data.all <- data.all()
    
    gamedata <- filter(data.all, GroupID %in% input$GroupID)
    
    updateSelectInput(session, 
                      "PlayerID",
                      choices = c("all", sort(unique(gamedata$PlayerID))),
                      selected = "all")
  }) 
  
  #Dynamic Help Text
  output$help <- renderUI({
    plotData <- plotDataR()
    helpText(paste("Number of data points left: ", nrow(plotData)))
  })
  
  output$summarytable <- renderTable({
    
    plotData <- plotDataR()
    
    #If there is data
    if(nrow(plotData) != 0){
      
      #Creating summary table
      plotData1 <- plotData %>% filter(Season == input$Season)
      stable1 <- plotData1 %>% select(GameNum, PlayerID, Season, input$yvar)
      names(stable1)[length(names(stable1))]<-"Money" 
      stable1 <- filter(stable1, Money != 0)
      stable <- stable1[order(rev(stable1$Money)),]
      stable <- stable[1:10,]
      
      if (nrow(stable) == 0) {
        
        #Empty data frame to return  
        stable <- data.frame()
        
        #Help Text
        output$summarytext <- renderUI(HTML(paste(
          em("There is no data with the selected characteristics"))))
        
      } else {
        
        #Removing dynamic help text
        output$summarytext <- renderUI({""})
        
      }
      
    } else {
      
      #Empty data frame to return  
      stable <- data.frame()
      
      #Help Text
      output$summarytext <- renderUI(HTML(paste(
        em("There is no data"))))
    }
    
    return(stable)
    
  })
  
  
  
  
  #Download Data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(plotDataR(), con)
    })
  
}



# Run the application 
shinyApp(ui = ui, server = server)