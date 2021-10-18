
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
source(paste(PATH,"app/legi_functions.R",sep=""),local=TRUE)

## prepare the data
source(paste(PATH,"app/prepare_data.R",sep=""),local=TRUE)

## run UI
source(paste(PATH,"app/ui.R",sep=""),local=TRUE)

## run server
source(paste(PATH,"app/server.R",sep=""),local=TRUE)

# Run the application 
shinyApp(ui = ui, server = server)

