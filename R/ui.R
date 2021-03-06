
ui <- navbarPage(
  title = 'Fantasy Baseball 2021',
  tabPanel("Draft",
           fluidRow(
             column(6,
                    title = "Input Selection",
                    uiOutput('selectInput'),
                    numericInput('price', "Auction Price:", value = 0.50, min = 0.50, step = 0.25),
                    fluidRow(
                      column(6, 
                             radioButtons('team', label = 'Team Drated', choices = teams)),
                      column(6, 
                             radioButtons('pos', label = 'Select Positon', choices = position))
                    ),
                    fluidRow(
                      column(3),
                      column(4,actionButton('post', label = 'Post to Team')),
                      column(5)
                    )
             ),
             column(6, 
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
                    )
             )
           ),
           br(),
           hr(),
           br(),
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
           fluidRow(
             plotOutput('heat')
           ),
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

