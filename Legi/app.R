## copy and run code to start the app
# shiny::runApp("C:/R/legi_analysis/Legi/app.R")

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

## load functions
source(paste(PATH,"Legi/legi_functions.R",sep=""),local=TRUE)

## prepare the data
source(paste(PATH,"Legi/prepare_data.R",sep=""),local=TRUE)

## run UI
source(paste(PATH,"Legi/ui.R",sep=""),local=TRUE)

## run server
source(paste(PATH,"Legi/server.R",sep=""),local=TRUE)

# Run the application 
shinyApp(ui = ui, server = server)

