#Author: Dylan Saez
#Purpose: SOI UI
rm(list = ls())
#Install packages
library(tidyverse)
library(data.table)
library(magrittr)
library(yaml)
library(rlang)
library(ggtext)
library(tidycensus)
library(magrittr)
library(rvest)
library(shiny)
library(sf)
library(leaflet)

#Directories
setwd("/gpfs/gibbs/project/sarin/ds3228/Repositories/soi_tax_stats")

ui = fluidPage(
  
  titlePanel("Net Flows - Individuals Returns"),
  
  sidebarLayout(
    
    sidebarPanel = sidebarPanel(),
    mainPanel = mainPanel()
    
  )
  
)

server = function(input, output){}

shinyApp(ui, server)
