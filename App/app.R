## library(rsconnect)
## deployApp("C:/R/legi_analysis/App")
rm(list=ls())

library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(openxlsx)
library(fpp3)
library(knitr)
library(kableExtra)

## Setting working directory   
PATH="C:/R/legi_analysis/"
setwd(PATH)

## load functions
source(paste(PATH,"App/legi_functions.R",sep=""),local=TRUE)

## prepare the data
source(paste(PATH,"App/prepare_data.R",sep=""),local=TRUE)

## run UI
source(paste(PATH,"App/ui.R",sep=""),local=TRUE)

## run server
source(paste(PATH,"App/server.R",sep=""),local=TRUE)

# Run the application 
shinyApp(ui = ui, server = server)

