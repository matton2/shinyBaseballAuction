outTitle <- function(teamName) {
  print(paste0(teamName, "'s Drafted Team"))
}

outTable <- function(data, teamName) {
  data %>% 
    filter(teamDrafted == teamName) %>% 
    mutate(value = Dollars - auctionPrice, na.rm= TRUE) %>% 
    select(Name, positionDrafted, auctionPrice, value) %>% 
    mutate(positionDrafted = factor(positionDrafted, levels = position)) %>% 
    arrange(positionDrafted)
}


outLine <- function(data, teamName) {
  drafted <- data %>% 
    filter(teamDrafted == teamName) %>% 
    nrow()
  left <- 16 - drafted
  
  salary <- data %>% 
    filter(teamDrafted == teamName) %>% 
    summarize(sum(auctionPrice))
  
  salaryLeft = 63 - salary
  
  maxBid = salaryLeft - ((left-1) *0.5)
  
  print(paste0("Total Remaing Player: ", left, "\n. Total Remaing Salary: ", salaryLeft, "\n. Max Bid Left: ", maxBid))
}

positionBatterTable <- function(data, pos){
  data %>% 
    filter(is.na(teamDrafted)) %>% 
    filter(grepl(pos,POS)) %>%
    arrange(batterIndex) %>% 
    select(Name, POS, H, HR, OBP, R, RBI, SB, Dollars, batterIndex) %>% 
    head(n=10)
}

positionPitcherTable <- function(data, pos){
  data %>% 
    filter(is.na(teamDrafted)) %>% 
    filter(grepl(pos,POS)) %>%
    arrange(pitcherIndex) %>% 
    select(Name, POS, ERA, `K/9`, SV, W, WHIP, Dollars, pitcherIndex) %>% 
    head(n=10)
}