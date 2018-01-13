
library(shiny)
library(shinydashboard)
library(tidyverse)
set.seed(1)

batters <- read_csv('FanGraphs Leaderboard.csv')
batters <- filter(batters, G > 25)
pitchers <- read_csv('FanGraphs Leaderboard pitchers.csv')
pitchers <- filter(pitchers, G > 5)
batterNames <- select(batters, Name)
pitcherNames <- select(pitchers, Name)
all_names <- rbind(batterNames, pitcherNames)
players <- full_join(all_names, batters, by = c("Name", "Name"))
players <- full_join(players, pitchers, by = c("Name", "Name"))
#players <- read_csv('FanGraphs Leaderboard.csv')
players$teamDrafted <- NA
players$auctionPrice <- NA
players$positionDrafted <- NA
arrange(players, Name)

teams <- c('Fish', 'Matt', 'Jay', 'Kid', 'Adam', 'Sherm', 'Ron', 'Marc', 'Pat', 'Brendan', 'David')
position <- c('C', '1B', '2B', 'SS', '3B', 'CI', 'MI', 'OF', 'U', 'SP', 'RP')

ui <- dashboardPage(
  dashboardHeader(title = 'Fantasy Baseball 2017'),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Input Selection",
        uiOutput('selectInput'),
        numericInput('price', "Auction Price:", value = 0.50, min = 0.50, step = 0.25),
        radioButtons('team', label = 'Team Drated', choices = teams),
        radioButtons('pos', label = 'Select Positon', choices = position),
        actionButton('post', label = 'Post to Team')),
      tabBox(
        tabPanel('Adam', textOutput('adamTitle'),tableOutput('adamTable'), textOutput('adamLine')),
        tabPanel('Matt', textOutput('mattTitle'),tableOutput('mattTable'), textOutput('mattLine')),
        tabPanel('Jay', textOutput('jayTitle'),tableOutput('jayTable'), textOutput('jayLine')),
        tabPanel('Kid', textOutput('kidTitle'),tableOutput('kidTable'), textOutput('kidLine')),
        tabPanel('Fish', textOutput('fishTitle'),tableOutput('fishTable'), textOutput('fishLine')),
        tabPanel('Sherm', textOutput('shermTitle'),tableOutput('shermTable'), textOutput('shermLine')),
        tabPanel('RonT', textOutput('ronTitle'),tableOutput('ronTable'), textOutput('ronLine')),
        tabPanel('Marc', textOutput('marcTitle'),tableOutput('marcTable'), textOutput('marcLine')),
        tabPanel('Pat', textOutput('patTitle'),tableOutput('patTable'), textOutput('patLine')),
        tabPanel('Bren', textOutput('brenTitle'),tableOutput('brenTable'), textOutput('brenLine')),
        tabPanel('David', textOutput('davidTitle'),tableOutput('davidTable'), textOutput('davidLine'))
      ),
      tabBox(
        tabPanel('HR', tableOutput('hrTable')),
        tabPanel("RBI", tableOutput('rbiTable')),
        tabPanel('Search', uiOutput('searchInput'), tableOutput('playerTable'))
      )
    ),
    fluidRow(
      plotOutput('heat')
    ),
    fluidRow(
      tableOutput('summaryTable')
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  drafted_player <- reactive({input$p_select})
  drafted_value <- reactive({input$price})
  drafted_team <- reactive({input$team})
  drafted_position <- reactive({input$pos})
  
  
  rv <- reactiveValues(p = players)
  
  observeEvent(input$post, {
    thedf <- rv$p %>% 
      mutate(teamDrafted = ifelse(is.na(teamDrafted) & Name == drafted_player(), 
                                  drafted_team(), teamDrafted)) %>% 
      mutate(auctionPrice = ifelse(is.na(auctionPrice) & Name == drafted_player(), 
                                   drafted_value(), auctionPrice)) %>% 
      mutate(positionDrafted = ifelse(is.na(positionDrafted) & Name == drafted_player(), 
                                      drafted_position(), positionDrafted))
    rv$p <- thedf
  })
  
  
  output$selectInput <- renderUI({
    notDrafted <- rv$p %>% 
      filter(is.na(teamDrafted)) %>% 
      arrange(Name)
    selectInput("p_select", label = "Player Selection:", choices = notDrafted$Name, selected = 'none')
  })
  
  output$searchInput <- renderUI({
    notDrafted <- rv$p %>% 
      filter(is.na(teamDrafted)) %>% 
      arrange(Name)
    selectInput("s_select", label = "Search Player:", choices = notDrafted$Name, selected = 'none')
  })
  
  searchPlayer <- reactive({input$s_select})
  
  output$playerTable <- renderTable({
    rv$p %>% 
      filter(Name == searchPlayer()) %>% 
      select(HR.x, RBI, SB, ERA)
  })
  
  
  outTitle <- function(teamName) {
    print(paste0(teamName, "'s Drafted Team"))
  }
  
  output$adamTitle <- renderText({
    outTitle('Adam')
  })
  output$mattTitle <- renderText({
    outTitle("Matt")
  })
  output$jayTitle <- renderText({
    outTitle("Jay")
  })
  output$fishTitle <- renderText({
    outTitle("Fish")
  })
  output$kidTitle <- renderText({
    outTitle('Kid')
  })
  output$shermTitle <- renderText({
    outTitle('Sherm')
  })
  output$ronTitle <- renderText({
    outTitle("Ron")
  })
  output$marcTitle <- renderText({
    outTitle('Marc')
  })
  output$patTitle <- renderText({
    outTitle('Pat')
  })
  output$brenTitle <- renderText({
    outTitle('Brendan')
  })
  output$davidTitle <- renderText({
    outTitle('David')
  })
  
  outTable <- function(teamName) {
    rv$p %>% 
      filter(teamDrafted == teamName) %>% 
      select(Name, positionDrafted, auctionPrice) %>% 
      mutate(positionDrafted = factor(positionDrafted, levels = position)) %>% 
      arrange(positionDrafted)
  }
  
  output$adamTable <- renderTable({
    outTable('Adam')
  })
  output$mattTable <- renderTable({
    outTable('Matt')
  })
  output$jayTable <- renderTable({
    outTable('Jay')
  })
  output$fishTable <- renderTable({
    outTable('Fish')
  })
  output$kidTable <- renderTable({
    outTable('Kid')
  })
  output$shermTable <- renderTable({
    outTable('Sherm')
  })
  output$ronTable <- renderTable({
    outTable('Ron')
  })
  output$marcTable <- renderTable({
    outTable('Marc')
  })
  output$patTable <- renderTable({
    outTable('Pat')
  })
  output$brenTable <- renderTable({
    outTable('Brendan')
  })
  output$davidTable <- renderTable({
    outTable('David')
  })
  
  
  outLine <- function(teamName) {
    drafted <- rv$p %>% 
      filter(teamDrafted == teamName) %>% 
      nrow()
    left <- 16 - drafted
    
    salary <- rv$p %>% 
      filter(teamDrafted == teamName) %>% 
      summarize(sum(auctionPrice))
    
    salaryLeft = 63 - salary
    
    maxBid = salaryLeft - ((left-1) *0.5)
    
    print(paste0("Total Remaing Player: ", left, "\n. Total Remaing Salary: ", salaryLeft, "\n. Max Bid Left: ", maxBid))
  }
  
  output$mattLine <- renderText({
    outLine('Matt')
  })
  output$adamLine <- renderText({
    outLine('Adam')
  })
  output$jayLine <- renderText({
    outLine("Jay")
  })
  output$fishLine <- renderText({
    outLine("Fish")
  })
  output$kidLine <- renderText({
    outLine('Kid')
  })
  output$shermLine <- renderText({
    outLine('Sherm')
  })
  output$ronLine <- renderText({
    outLine("Ron")
  })
  output$marcLine <- renderText({
    outLine('Marc')
  })
  output$patLine <- renderText({
    outLine('Pat')
  })
  output$brenLine <- renderText({
    outLine('Brendan')
  })
  output$davidLine <- renderText({
    outLine('David')
  })
  
 
  output$hrTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(desc(HR.x)) %>%
      select(Name, HR.x) %>% 
      head()
  })
  
  output$rbiTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(desc(RBI)) %>%
      select(Name, RBI) %>% 
      head()
  })
  
  output$heat <- renderPlot({
    base_size <- 9
    
    test <- rv$p %>% 
      filter(!is.na(teamDrafted)) %>%
      group_by(teamDrafted) %>% 
      summarize(HR = sum(HR.x),
                RBI = sum(RBI),
                SB = sum(SB))
    test.m <- reshape2::melt(test)
    
    ggplot(test.m, aes(variable, teamDrafted)) + geom_tile(aes(fill = value), color = 'white') +
      scale_fill_gradient(low = 'white', high = 'steelblue') +
      theme_grey(base_size = base_size) + 
      labs(x = "", y = "") + 
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0))
      
    
  })
  
  output$summaryTable <- renderTable({
    test <- rv$p %>% 
      filter(!is.na(teamDrafted)) %>%
      group_by(teamDrafted) %>% 
      summarize(HR = sum(HR.x),
                RBI = sum(RBI),
                SB = sum(SB))
  })
  
    
}


shinyApp(ui = ui, server = server)

