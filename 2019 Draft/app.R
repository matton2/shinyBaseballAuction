
library(shiny)
library(shinydashboard)
library(plyr)
library(tidyverse)
library(scales)
set.seed(2)

# batters <- read_csv('2019 Draft/Steamer_B_600_feb.csv')    #read batters into data frame
batters <- read_csv('Steamer_B_600_feb.csv')
batters <- batters %>% 
  filter(G > 25) %>%
  mutate(batterIndex = row_number()) %>% 
  filter(batterIndex <= 400) %>% 
  select(Name, H, HR, OBP, R, RBI, SB, AB, BB, HBP, PA, batterIndex)   #select columns needed

# pitchers <- read_csv('2019 Draft/Steamer_P_600_feb.csv')
pitchers <- read_csv('Steamer_P_600_feb.csv')
pitchers <- pitchers %>% 
  filter(G > 5) %>% 
  rename(H.pitcher = H) %>%
  rename(BB.pitcher = BB) %>%
  mutate(pitcherIndex = row_number()) %>% 
  filter(pitcherIndex <= 200) %>% 
  select(Name, ERA, `K/9`, SV, W, WHIP, IP, ER, H.pitcher, SO, BB.pitcher, pitcherIndex)

batterNames <- select(batters, Name)
pitcherNames <- select(pitchers, Name)
all_names <- rbind(batterNames, pitcherNames)

players <- full_join(all_names, batters, by = c("Name", "Name"))
players <- full_join(players, pitchers, by = c("Name", "Name")) 

#batterAuction <- read_csv('2019 Draft/batter projections.csv')
batterAuction <- read_csv('batter projections.csv')
batterAuction <- batterAuction %>% 
  mutate(rowIndex = row_number()) %>% 
  filter(rowIndex <= 400) %>% 
  select(PlayerName, POS, Dollars) %>% 
  separate(Dollars, into = c('symbol', 'Dollars'), sep="\\$") %>% 
  mutate(Dollars = as.numeric(Dollars)) %>% 
  mutate(Dollars = ifelse(is.na(Dollars), 0.5, Dollars))

#pitcherAuction <- read_csv('2019 Draft/pitcher projections.csv')
pitcherAuction <- read_csv('pitcher projections.csv')
pitcherAuction <- pitcherAuction %>% 
  mutate(rowIndex = row_number()) %>% 
  filter(rowIndex <= 300) %>% 
  filter(!grepl('Rami', PlayerName)) %>% 
  select(PlayerName, POS, Dollars) %>% 
  separate(Dollars, into = c('symbol', 'Dollars'), sep="\\$") %>% 
  mutate(Dollars = as.numeric(Dollars)) %>% 
  mutate(Dollars = ifelse(is.na(Dollars), 0.5, Dollars)) 

allAuction <- rbind(batterAuction, pitcherAuction)

players <- left_join(players, allAuction, by = c('Name' = 'PlayerName')) %>% #changed from full to left
  select(Name, POS, everything())
players$teamDrafted <- NA
players$auctionPrice <- NA
players$positionDrafted <- NA
arrange(players, Name)

teams <- c('Matt', 'Fish', 'Jay', 'Kid', 'Adam', 'Sherm', 'Marc', 'Pat', 'Brendan', 'David')
position <- c('C', '1B', '2B', 'SS', '3B', 'CI', 'MI', 'OF', 'U', 'SP', 'RP')

ui <- dashboardPage(
  dashboardHeader(title = 'Fantasy Baseball 2018'),
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
        tabPanel('Matt', textOutput('mattTitle'),tableOutput('mattTable'), textOutput('mattLine')),
        tabPanel('Adam', textOutput('adamTitle'),tableOutput('adamTable'), textOutput('adamLine')),
        tabPanel('Jay', textOutput('jayTitle'),tableOutput('jayTable'), textOutput('jayLine')),
        tabPanel('Kid', textOutput('kidTitle'),tableOutput('kidTable'), textOutput('kidLine')),
        tabPanel('Fish', textOutput('fishTitle'),tableOutput('fishTable'), textOutput('fishLine')),
        tabPanel('Sherm', textOutput('shermTitle'),tableOutput('shermTable'), textOutput('shermLine')),
        tabPanel('Marc', textOutput('marcTitle'),tableOutput('marcTable'), textOutput('marcLine')),
        tabPanel('Pat', textOutput('patTitle'),tableOutput('patTable'), textOutput('patLine')),
        tabPanel('Bren', textOutput('brenTitle'),tableOutput('brenTable'), textOutput('brenLine')),
        tabPanel('David', textOutput('davidTitle'),tableOutput('davidTable'), textOutput('davidLine'))
      )),
    fluidRow(
      tabBox(
        tabPanel("TopBatter", tableOutput('topBatterTable')),
        tabPanel('C', tableOutput('catcherTable')),
        tabPanel('1B', tableOutput('firstTable')),
        tabPanel('2B', tableOutput('secondTable')),
        tabPanel('SS', tableOutput('ssTable')),
        tabPanel('3B', tableOutput('thirdTable')),
        tabPanel('OF', tableOutput('ofTable')),
        tabPanel('MI', tableOutput('miTable')),
        tabPanel('CI', tableOutput('ciTable')),
        tabPanel('TopPitcher', tableOutput('topPitcherTable')),
        tabPanel('SP', tableOutput('spTable')),
        tabPanel('RP', tableOutput('rpTable'))
      ),
      tabBox(
        tabPanel('H', tableOutput('hTable')),
        tabPanel('HR', tableOutput('hrTable')),
        tabPanel('OBP', tableOutput('obpTable')),
        tabPanel('R', tableOutput('rTable')),
        tabPanel("RBI", tableOutput('rbiTable')),
        tabPanel('SB', tableOutput('sbTable')),
        tabPanel('ERA', tableOutput('eraTable')),
        tabPanel('K/9', tableOutput('k9Table')),
        tabPanel('SV', tableOutput('svTable')),
        tabPanel('W', tableOutput('wTable')),
        tabPanel('WHIP', tableOutput('whipTable')),
        tabPanel('Search', uiOutput('searchInput'), tableOutput('playerTable'))
      )
    ),
    # fluidRow(
    #   plotOutput('heat')
    # ),
    fluidRow(
      title = 'Compiled Team Stats:',
      tableOutput('summaryTable')
    ),
    fluidRow(
      downloadButton('downloadData', 'Download All Players'),
      downloadButton('downloadTeamData', "Download Completed Rosters"),
      box(
        title = "Input Selection",
        uiOutput('removeInput'),
        actionButton('remove', label = 'Remove From Team'))
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
  
  remove_player <- reactive({input$remove_select})
  
  output$removeInput <- renderUI({
    drafted <- rv$p %>% 
      filter(!is.na(teamDrafted)) %>% 
      arrange(Name)
    selectInput("remove_select", label = "Player Selection:", choices = drafted$Name, selected = 'none')
  })
  
  observeEvent(input$remove, {
    thedf <- rv$p %>% 
      mutate(teamDrafted = ifelse(!is.na(teamDrafted) & Name == remove_player(), 
                                  NA, teamDrafted)) %>% 
      mutate(auctionPrice = ifelse(!is.na(auctionPrice) & Name == remove_player(), 
                                   NA, auctionPrice)) %>% 
      mutate(positionDrafted = ifelse(!is.na(positionDrafted) & Name == remove_player(), 
                                      NA, positionDrafted))
    rv$p <- thedf
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
      # case_when(
      #   (POS == 'SP'|'RP') ~ select(Name, POS, ERA, `K/9`, SV, W, WHIP, pitcherIndex, Dollars),
      #   (POS != 'SP'|'RP') ~ select(Name, POS, H, HR, OBP, R, RBI, SB, batterIndex, Dollars)
      # )
      # # {ifelse(('SP'|'RP' %in% POS), 
      # #   select(Name, POS, ERA, `K/9`, SV, W, WHIP, pitcherIndex, Dollars), 
      # #   select(Name, POS, H, HR, OBP, R, RBI, SB, batterIndex)) }
      select(Name, POS, H, HR, OBP, R, RBI, SB, batterIndex, ERA, `K/9`, SV, W, WHIP, pitcherIndex, Dollars)
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
      mutate(value = Dollars - auctionPrice, na.rm= TRUE) %>% 
      select(Name, positionDrafted, auctionPrice, value) %>% 
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
    
    salaryLeft = 60 - salary
    
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
  
  
  
  output$topBatterTable <- renderTable({
    rv$p %>% 
      filter(is.na(teamDrafted)) %>% 
      arrange(batterIndex) %>% 
      select(Name, POS, H, HR, OBP, R, RBI, SB, Dollars, batterIndex) %>% 
      head(n=10)
  })
  output$topPitcherTable <- renderTable({
    rv$p %>% 
      filter(is.na(teamDrafted)) %>% 
      arrange(pitcherIndex) %>% 
      select(Name, POS, ERA, `K/9`, SV, W, WHIP, Dollars, pitcherIndex) %>% 
      head(n=10)
  })
  output$hTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(desc(H)) %>%
      select(Name, H) %>% 
      head()
  })
  output$hrTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(desc(HR)) %>%
      select(Name, HR) %>% 
      head()
  })
  output$rbiTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(desc(RBI)) %>%
      select(Name, RBI) %>% 
      head()
  })
  output$obpTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(desc(OBP)) %>%
      select(Name, OBP) %>% 
      head()
  })
  output$rTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(desc(R)) %>%
      select(Name, R) %>% 
      head()
  })
  output$sbTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(desc(SB)) %>%
      select(Name, SB) %>% 
      head()
  })
  output$eraTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(ERA) %>%
      select(Name, ERA) %>% 
      head()
  })
  output$k9Table <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(desc(`K/9`)) %>%
      select(Name, `K/9`) %>% 
      head()
  })
  output$svTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(desc(SV)) %>%
      select(Name, SV) %>% 
      head()
  })
  output$wTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(desc(W)) %>%
      select(Name, W) %>% 
      head()
  })
  output$whipTable <- renderTable({
    rv$p %>%
      filter(is.na(teamDrafted)) %>% 
      arrange(WHIP) %>%
      select(Name, WHIP) %>% 
      head()
  })
  
  positionBatterTable <- function(pos){
    rv$p %>% 
      filter(is.na(teamDrafted)) %>% 
      filter(grepl(pos,POS)) %>%
      arrange(batterIndex) %>% 
      select(Name, POS, H, HR, OBP, R, RBI, SB, Dollars, batterIndex) %>% 
      head(n=10)
  }
  
  positionPitcherTable <- function(pos){
    rv$p %>% 
      filter(is.na(teamDrafted)) %>% 
      filter(grepl(pos,POS)) %>%
      arrange(pitcherIndex) %>% 
      select(Name, POS, ERA, `K/9`, SV, W, WHIP, Dollars, pitcherIndex) %>% 
      head(n=10)
  }
  
  output$catcherTable <- renderTable({
    positionBatterTable("C")
  })
  output$firstTable <- renderTable ({
    positionBatterTable("1B")
  })
  output$secondTable <- renderTable ({
    positionBatterTable("2B")
  })
  output$ssTable <- renderTable ({
    positionBatterTable("SS")
  })
  output$thirdTable <- renderTable ({
    positionBatterTable("3B")
  })
  output$ofTable <- renderTable ({
    positionBatterTable("OF")
  })
  output$miTable <- renderTable ({
    positionBatterTable("2B|SS")
  })
  output$ciTable <- renderTable({
    positionBatterTable('1B|3B')
  })
  output$spTable <- renderTable({
    positionPitcherTable("SP")
  })
  output$rpTable <- renderTable({
    positionPitcherTable('RP')
  })
  
  output$heat <- renderPlot({
    base_size <- 16
    
    heatData <- rv$p %>% 
      filter(!is.na(teamDrafted)) %>%
      group_by(teamDrafted) %>% 
      summarize(H = sum(H, na.rm = TRUE),
                HR = sum(HR, na.rm = TRUE),
                OBP = mean(OBP, na.rm = TRUE),
                R = mean(R, na.rm = TRUE),
                RBI = sum(RBI, na.rm = TRUE),
                SB = sum(SB, na.rm = TRUE),
                ERA = mean(ERA, na.rm = TRUE),
                K9 = mean(`K/9`, na.rm = TRUE),
                W = sum(W, na.rm = TRUE),
                SV = sum(SV, na.rm = TRUE),
                WHIP = mean(WHIP, na.rm = TRUE))
    heatData_m <- reshape2::melt(heatData)
    heatData_m <- ddply(heatData_m, .(variable), transform, rescale = rescale(value, na.rm=TRUE))
    
    ggplot(heatData_m, aes(variable, teamDrafted)) + geom_tile(aes(fill = rescale), color = 'white') +
      scale_fill_gradient(low = 'white', high = 'steelblue') +
      theme_grey(base_size = base_size) + 
      labs(x = "", y = "") + 
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0))
    
    
  })
  
  output$summaryTable <- renderTable({
    rv$p %>% 
      filter(!is.na(teamDrafted)) %>%
      mutate(value = Dollars - auctionPrice) %>%
      group_by(teamDrafted) %>%
      summarize(H = sum(H, na.rm = TRUE),
                HR = sum(HR, na.rm = TRUE),
                OBP = (sum(H, na.rm = TRUE) + sum(BB, na.rm = TRUE) + sum(HBP, na.rm = TRUE))/(sum(PA, na.rm = TRUE)),
                R = mean(R, na.rm = TRUE),
                RBI = sum(RBI, na.rm = TRUE),
                SB = sum(SB, na.rm = TRUE),
                ERA =9 * (sum(ER, na.rm = TRUE)/sum(IP, na.rm=TRUE)),
                K9 = 9 * (sum(SO, na.rm=TRUE)/sum(IP, na.rm = TRUE)),
                W = sum(W, na.rm = TRUE),
                SV = sum(SV, na.rm = TRUE),
                WHIP = (sum(H.pitcher, na.rm = TRUE) + sum(BB.pitcher, na.rm = TRUE))/sum(IP, na.rm=TRUE),
                Value = sum(value, na.rm= TRUE)) %>% 
      arrange(desc(H)) %>% 
      mutate(hPoints = c(10:1)) %>% 
      arrange(desc(HR)) %>% 
      mutate(hrPoints = c(10:1)) %>% 
      arrange(desc(OBP)) %>% 
      mutate(obpPoints = c(10:1)) %>% 
      arrange(desc(R)) %>% 
      mutate(rPoints = c(10:1)) %>% 
      arrange(desc(RBI)) %>% 
      mutate(rbiPoints = c(10:1)) %>% 
      arrange(desc(SB)) %>% 
      mutate(sbPoints = c(10:1)) %>% 
      arrange(ERA) %>% 
      mutate(eraPoints = c(10:1)) %>% 
      arrange(desc(K9)) %>% 
      mutate(k9Points = c(10:1)) %>% 
      arrange(desc(W)) %>% 
      mutate(wPoints = c(10:1)) %>% 
      arrange(desc(SV)) %>% 
      mutate(svPoints = c(10:1)) %>% 
      arrange(WHIP) %>% 
      mutate(whipPoints = c(10:1)) %>%   
      mutate(totalPoints = hPoints + hrPoints + obpPoints + rPoints + rbiPoints + sbPoints + eraPoints + k9Points + wPoints +
               svPoints + whipPoints) %>% 
      select(teamDrafted, H, HR, OBP, R, RBI, SB, ERA, K9, W, SV, WHIP, Value, totalPoints) %>% 
      arrange(desc(totalPoints))
  })
  
  
  
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("all_players", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(rv$p, file)
    }
  )
  
  output$downloadTeamData <- downloadHandler(
    filename = function() {
      paste("players_drafted", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      drafted <- filter(rv$p, !is.na(teamDrafted))
      write.csv(drafted, file)
    }
  )
  
  
  
}


shinyApp(ui = ui, server = server)

