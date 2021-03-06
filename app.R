library(shiny)
library(shinydashboard)
library(plyr)
library(tidyverse)
library(scales)
set.seed(2)

purrr::walk(list.files("R", ".*\\.R", full.names = TRUE), source)

shinyApp(ui = ui, server = server)