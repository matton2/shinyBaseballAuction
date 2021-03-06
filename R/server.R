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
  
  output$adamTable <- renderTable({
    outTable(rv$p,'Adam')
  })
  output$mattTable <- renderTable({
    outTable(rv$p,'Matt')
  })
  output$jayTable <- renderTable({
    outTable(rv$p,'Jay')
  })
  output$fishTable <- renderTable({
    outTable(rv$p,'Fish')
  })
  output$kidTable <- renderTable({
    outTable(rv$p,'Kid')
  })
  output$shermTable <- renderTable({
    outTable(rv$p,'Sherm')
  })
  output$ronTable <- renderTable({
    outTable(rv$p,'Ron')
  })
  output$marcTable <- renderTable({
    outTable(rv$p,'Marc')
  })
  output$patTable <- renderTable({
    outTable(rv$p,'Pat')
  })
  output$brenTable <- renderTable({
    outTable(rv$p,'Brendan')
  })
  output$davidTable <- renderTable({
    outTable(rv$p,'David')
  })
  
  output$mattLine <- renderText({
    outLine(rv$p, 'Matt')
  })
  output$adamLine <- renderText({
    outLine(rv$p,'Adam')
  })
  output$jayLine <- renderText({
    outLine(rv$p,"Jay")
  })
  output$fishLine <- renderText({
    outLine(rv$p,"Fish")
  })
  output$kidLine <- renderText({
    outLine(rv$p,'Kid')
  })
  output$shermLine <- renderText({
    outLine(rv$p,'Sherm')
  })
  output$ronLine <- renderText({
    outLine(rv$p,"Ron")
  })
  output$marcLine <- renderText({
    outLine(rv$p,'Marc')
  })
  output$patLine <- renderText({
    outLine(rv$p,'Pat')
  })
  output$brenLine <- renderText({
    outLine(rv$p, 'Brendan')
  })
  output$davidLine <- renderText({
    outLine(rv$p,'David')
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
  
  output$catcherTable <- renderTable({
    positionBatterTable(rv$p, "C")
  })
  output$firstTable <- renderTable ({
    positionBatterTable(rv$p,"1B")
  })
  output$secondTable <- renderTable ({
    positionBatterTable(rv$p,"2B")
  })
  output$ssTable <- renderTable ({
    positionBatterTable(rv$p,"SS")
  })
  output$thirdTable <- renderTable ({
    positionBatterTable(rv$p,"3B")
  })
  output$ofTable <- renderTable ({
    positionBatterTable(rv$p,"OF")
  })
  output$miTable <- renderTable ({
    positionBatterTable(rv$p,"2B|SS")
  })
  output$ciTable <- renderTable({
    positionBatterTable(rv$p,'1B|3B')
  })
  output$spTable <- renderTable({
    positionPitcherTable(rv$p,"SP")
  })
  output$rpTable <- renderTable({
    positionPitcherTable(rv$p,'RP')
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
      mutate(hPoints = c(11:1)) %>% 
      arrange(desc(HR)) %>% 
      mutate(hrPoints = c(11:1)) %>% 
      arrange(desc(OBP)) %>% 
      mutate(obpPoints = c(11:1)) %>% 
      arrange(desc(R)) %>% 
      mutate(rPoints = c(11:1)) %>% 
      arrange(desc(RBI)) %>% 
      mutate(rbiPoints = c(11:1)) %>% 
      arrange(desc(SB)) %>% 
      mutate(sbPoints = c(11:1)) %>% 
      arrange(ERA) %>% 
      mutate(eraPoints = c(11:1)) %>% 
      arrange(desc(K9)) %>% 
      mutate(k9Points = c(11:1)) %>% 
      arrange(desc(W)) %>% 
      mutate(wPoints = c(11:1)) %>% 
      arrange(desc(SV)) %>% 
      mutate(svPoints = c(11:1)) %>% 
      arrange(WHIP) %>% 
      mutate(whipPoints = c(11:1)) %>%   
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

