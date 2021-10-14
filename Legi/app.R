# library(rsconnect)
# rsconnect::deployApp('C:/R/legi_analysis/Legi')

# rm(list=ls())

library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(openxlsx)
library(fpp3)
library(knitr)
library(kableExtra)

setwd("C:/R/legi_analysis/Legi/")
## run UI
source("ui.R",local=TRUE)

## run server
source("server.R",local=TRUE)

# Run the application 
shinyApp(ui = ui, server = server)

