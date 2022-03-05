
batters <- read_csv('data/2021_Batters.csv')    #read batters into data frame
batters <- batters %>% 
  filter(G > 25) %>%
  mutate(batterIndex = row_number()) %>% 
  filter(batterIndex <= 400) %>% 
  select(Name, H, HR, OBP, R, RBI, SB, AB, BB, HBP, PA, batterIndex)   #select columns needed

pitchers <- read_csv('data/2021_Pitchers.csv')
pitchers <- pitchers %>% 
  filter(G > 5) %>% 
  rename(H.pitcher = H) %>%
  rename(BB.pitcher = BB) %>%
  mutate(pitcherIndex = row_number()) %>% 
  filter(pitcherIndex <= 200) %>% 
  select(pitcherIndex, Name, ERA, `K/9`, SV, W, WHIP, IP, ER, H.pitcher, SO, BB.pitcher)

# TODO: figure out how to handle players with the same name
# TODO: keep all players

batterNames <- select(batters, Name)
pitcherNames <- select(pitchers, Name)
all_names <- rbind(batterNames, pitcherNames) %>% distinct()

players <- full_join(all_names, batters, by = c("Name" = "Name"))
players <- full_join(players, pitchers, by = c("Name" = "Name")) 

batterAuction <- read_csv('data/2021_Batters_Value.csv')
batterAuction <- batterAuction %>% 
  mutate(rowIndex = row_number()) %>% 
  filter(rowIndex <= 400) %>% 
  select(PlayerName, POS, Dollars) %>% 
  separate(Dollars, into = c('symbol', 'Dollars'), sep="\\$") %>% 
  mutate(Dollars = as.numeric(Dollars)) %>% 
  mutate(Dollars = ifelse(is.na(Dollars), 0.5, Dollars))

pitcherAuction <- read_csv('data/2021_Pitchers_Value.csv')
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

teams <- c('Fish', 'Matt', 'Jay', 'Kid', 'Adam', 'Sherm', 'Marc', 'Pat', 'Brendan', 'David')
position <- c('C', '1B', '2B', 'SS', '3B', 'CI', 'MI', 'OF', 'U', 'SP', 'RP')
