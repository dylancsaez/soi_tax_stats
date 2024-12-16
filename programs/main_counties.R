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

#Directories
setwd("/gpfs/gibbs/project/sarin/ds3228/Repositories/soi_tax_stats")
county_inflows <- "data/county_to_county_inflows"
county_outflows <- "data/county_to_county_outflows"

#U.S. Population Migration Data
#Based on year-to-year changes reported on
# individual income tax returns filed with the IRS.
# They present migration patterns by State or by county for the entire U.S.
# and are available for inflows - the number of new residents who moved to a
# State or county and where they went. The data are available for Filing Years
# 1991-2022 and include:
#       -Number of returns filed, which approximates the number of households
#         that migrated
#       -Number of personal exemptions claimed, which approximates the number of
#         individuals
#       -Total adjusted gross income, starting with Filing Year 1995
#       -Aggregate migration flows at the State lvel, by the size of adjusted
#         income (AGI) and age of the primary taxpayer, starting with Filing
#         Year 2011.
#       
#We also start with filing year 2011.

#Data: State Inflows
#The State-to-State inflow migration files represent the migration flows into
# the destination sate, in year two, from the origin state.
#The number of individuals and AGI are based on the year 2 tax return.
#y2_statefips: State FIPS Code of Destination from Year 2
#y1_statefips: State FIPS Code of Destination from Year 1
#y1_state: State abbreviation or postal code of origin from year 1
#y1_state_name: State name of origin from year 1
#n1: Number of returns
#n2: Number of individuals
#AGI: Adjusted Gross Income (AGI)

#Okay, let's pull in all State Inflows files
st_inflow_files <- as.data.frame(list.files(file.path(getwd(), county_inflows)))
colnames(st_inflow_files) <- "csv_names"

#We have all csvs listed, let's open them and attach a year to each
for (csv in 1:nrow(st_inflow_files)) {
  csv_file <- read.csv(file.path(file.path(getwd(), county_inflows), st_inflow_files$csv_names[csv]))
  #Will need this column name specified for future merge
  colnames(csv_file)<- paste(colnames(csv_file), "inflow", sep = "_")
  csv_file$year <- substring(st_inflow_files$csv_names[csv], 12, 15)
  csv_file$net_y2_y1 <- paste0(csv_file$y2_statefips, "-", csv_file$y1_statefips)
  assign(st_inflow_files$csv_names[csv], csv_file)
}

#Data: State Outflows
#The State-to-State outflow migration files represent the migration flows from
# the origin state, in year 1, to the destination state, in year 2.
#The number of individuals and AGI are based on the year 2 tax return
#y1_statefips: State FIPS Code of Origin from Year 1
#y2_statefips: State FIPS Code of Destination from Year 2
#y2_state: State Abbreviation or Postal Code of Destination from Year 2
#y2_state_name: state name of destination from year 2
#n1: Number of returns
#n2 :Number of individuals
#AGI: Adjusted Gross Income (AGI)

#Okay, let's pull in all State Inflows files
st_outflow_files <- as.data.frame(list.files(file.path(getwd(), county_outflows)))
colnames(st_outflow_files) <- "csv_names"

#We have all csvs listed, let's open them and attach a year to each
for (csv in 1:nrow(st_outflow_files)) {
  csv_file <- read.csv(file.path(file.path(getwd(), county_outflows), st_outflow_files$csv_names[csv]))
  #Will need this column name specified for future merge
  colnames(csv_file) <- paste(colnames(csv_file), "outflow", sep = "_")
  csv_file$net_y2_y1 <- paste0(csv_file$y1_statefips, "-", csv_file$y2_statefips)
  csv_file$year <- substring(st_outflow_files$csv_names[csv], 13, 16)
  
  assign(st_outflow_files$csv_names[csv], csv_file)
}


#Now, we want to create net matrices of the state inflow and outflow data for
# each pair of years.

for(q in 1:nrow(st_inflow_files)){
  df_list <- mget(ls(pattern = substring(st_inflow_files$csv_names[q], 12, 15)))
  b <- df_list %>% reduce(inner_join, by = c("net_y2_y1", "year")) 
  assign(paste0("netflows",substring(st_inflow_files$csv_names[q], 12, 15)), b)
}


#Clean our environment
rm(list = ls()[grep("^stateinflow", ls())])
rm(list = ls()[grep("^stateoutflow", ls())])
rm(b)
rm(csv_file)
rm(df_list)
#For instance, starting with 2013, let's create a net matrix that shows the net
# flow of tax data from 2013-2014.
Pattern1<-grep("netflows",names(.GlobalEnv),value=TRUE)
Pattern1_list<-do.call("list",mget(Pattern1))


for(p in 1:length(Pattern1_list)){
  df_list <- as.data.frame(mget(Pattern1[p]))
  colnames(df_list)<-gsub(paste0(Pattern1[p],"."),"", colnames(df_list))
  
  df_list$net_number_of_returns <- df_list$n1_inflow - df_list$n1_outflow
  df_list$net_number_of_individuals <- df_list$n2_inflow - df_list$n2_outflow
  df_list$net_AGI <- df_list$AGI_inflow - df_list$AGI_outflow
  df_list <- df_list %>% select(-c(y1_state_name_inflow, y2_state_name_outflow, y1_state_inflow, n1_inflow, n2_inflow, AGI_inflow,
                                   y1_statefips_outflow, y2_statefips_outflow, y2_statefips_outflow,
                                   n1_outflow, n2_outflow, AGI_outflow, y2_state_outflow))
  assign(Pattern1[p], df_list)
}

fips_code_census <- tidycensus::fips_codes %>%
  select(c(state_code, state_name))
fips_code_census$state_code <- sub("^0+", "", fips_code_census$state_code)
fips_code_census$y2_statefips_inflow <- fips_code_census$state_code
fips_code_census$y1_statefips_inflow <- fips_code_census$state_code
#y1
fips_code_census_y1 <- fips_code_census %>%
  select(-c(y2_statefips_inflow, state_code))
fips_code_census_y1$state_name_y1 <- fips_code_census_y1$state_name
fips_code_census_y1 <- fips_code_census_y1 %>%
  select(-c(state_name))
fips_code_census_y1 <- fips_code_census_y1[!duplicated(fips_code_census_y1), ]

#y2
fips_code_census_y2 <- fips_code_census %>%
  select(-c(y1_statefips_inflow, state_code))
fips_code_census_y2$state_name_y2 <- fips_code_census_y2$state_name
fips_code_census_y2 <- fips_code_census_y2 %>%
  select(-c(state_name))
fips_code_census_y2 <- fips_code_census_y2[!duplicated(fips_code_census_y2), ]


#Merge with FIPS names in order to create column names
# flow of tax data from 2013-2014.
Pattern1<-grep("netflows",names(.GlobalEnv),value=TRUE)
Pattern1_list<-do.call("list",mget(Pattern1))


for(g in 1:length(Pattern1_list)){
  df_list <- as.data.frame(mget(Pattern1[g]))
  colnames(df_list)<-gsub(paste0(Pattern1[g],"."),"", colnames(df_list))
  t <- merge(df_list, fips_code_census_y1, by = c("y1_statefips_inflow"))
  t <- merge(t, fips_code_census_y2, by = c("y2_statefips_inflow"))
  t <- t %>%
    select(-c(y1_statefips_inflow, y2_statefips_inflow, net_y2_y1, year))
  assign(Pattern1[g], t)
}

#Pivot to create a matrix
#net_number of individuals
for(m in 1:length(Pattern1_list)){
  df_list <- as.data.frame(mget(Pattern1[m]))
  colnames(df_list)<-gsub(paste0(Pattern1[m],"."),"", colnames(df_list))
  df_list <- df_list %>%
    select(-c(net_number_of_returns, net_AGI)) %>%
    pivot_wider(names_from = state_name_y2, values_from = net_number_of_individuals)
  df_list <- df_list %>% 
    mutate_if(is.numeric, funs(. * -1))
  df_list <- cbind(df_list, total_flows = rowSums(df_list[,-c(1)]))
  df_list <- df_list[order(df_list$state_name_y1), ]
  #df_list <- rbind(df_list, total_flows = colSums(df_list[,-c(1)]))
  assign(paste0(Pattern1[m], "_net_number_of_individuals"), df_list)
  writexl::write_xlsx(df_list, paste0("/gpfs/gibbs/project/sarin/ds3228/Repositories/soi_tax_stats/output/net_number_of_individuals/",paste0(Pattern1[m], "_net_number_of_individuals.xlsx")))
  
  
}
#net number of AGI
for(m in 1:length(Pattern1_list)){
  df_list <- as.data.frame(mget(Pattern1[m]))
  colnames(df_list)<-gsub(paste0(Pattern1[m],"."),"", colnames(df_list))
  df_list <- df_list %>%
    select(-c(net_number_of_returns, net_number_of_individuals)) %>%
    pivot_wider(names_from = state_name_y2, values_from = net_AGI)
  df_list <- df_list %>% 
    mutate_if(is.numeric, funs(. * -1))
  df_list <- cbind(df_list, total_flows = rowSums(df_list[,-c(1)]))
  df_list <- df_list[order(df_list$state_name_y1), ]
  #df_list <- rbind(df_list, total_flows = colSums(df_list[,-c(1)]))
  assign(paste0(Pattern1[m], "_net_AGI"), df_list)
  writexl::write_xlsx(df_list, paste0("/gpfs/gibbs/project/sarin/ds3228/Repositories/soi_tax_stats/output/net_AGI/",paste0(Pattern1[m], "_net_AGI.xlsx")))
  
  
}
#net number of returns
for(m in 1:length(Pattern1_list)){
  df_list <- as.data.frame(mget(Pattern1[m]))
  colnames(df_list)<-gsub(paste0(Pattern1[m],"."),"", colnames(df_list))
  df_list <- df_list %>%
    select(-c(net_AGI, net_number_of_individuals)) %>%
    pivot_wider(names_from = state_name_y2, values_from = net_number_of_returns)
  df_list <- df_list %>% 
    mutate_if(is.numeric, funs(. * -1))
  df_list <- cbind(df_list, total_flows = rowSums(df_list[,-c(1)]))
  df_list <- df_list[order(df_list$state_name_y1), ]
  #df_list <- rbind(df_list, total_flows = colSums(df_list[,-c(1)]))
  assign(paste0(Pattern1[m], "_net_number_of_returns"), df_list)
  writexl::write_xlsx(df_list, paste0("/gpfs/gibbs/project/sarin/ds3228/Repositories/soi_tax_stats/output/net_number_of_returns/",paste0(Pattern1[m], "_net_number_of_returns.xlsx")))
}

#Entire of creating matrices.

#-------------------------------------------------------------------------------
# #Okay, let's pull in all State Inflows files
# st_inflow_files <- as.data.frame(list.files(file.path(getwd(), county_inflows)))
# colnames(st_inflow_files) <- "csv_names"
# 
# #We have all csvs listed, let's open them and attach a year to each
# for (csv in 1:nrow(st_inflow_files)) {
#   csv_file <- read.csv(file.path(file.path(getwd(), county_inflows), st_inflow_files$csv_names[csv]))
#   #Will need this column name specified for future merge
#   colnames(csv_file)<- paste(colnames(csv_file), "inflow", sep = "_")
#   csv_file$year <- substring(st_inflow_files$csv_names[csv], 12, 15)
#   csv_file$net_y2_y1 <- paste0(csv_file$y2_statefips, "-",csv_file$y2_countyfips_inflow,"-", csv_file$y1_statefips,"-",csv_file$y1_countyfips_inflow )
#   assign(st_inflow_files$csv_names[csv], csv_file)
# }
# 
# #Data: State Outflows
# #The State-to-State outflow migration files represent the migration flows from
# # the origin state, in year 1, to the destination state, in year 2.
# #The number of individuals and AGI are based on the year 2 tax return
# #y1_statefips: State FIPS Code of Origin from Year 1
# #y2_statefips: State FIPS Code of Destination from Year 2
# #y2_state: State Abbreviation or Postal Code of Destination from Year 2
# #y2_state_name: state name of destination from year 2
# #n1: Number of returns
# #n2 :Number of individuals
# #AGI: Adjusted Gross Income (AGI)
# 
# #Okay, let's pull in all State Inflows files
# st_outflow_files <- as.data.frame(list.files(file.path(getwd(), county_outflows)))
# colnames(st_outflow_files) <- "csv_names"
# 
# #We have all csvs listed, let's open them and attach a year to each
# for (csv in 1:nrow(st_outflow_files)) {
#   csv_file <- read.csv(file.path(file.path(getwd(), county_outflows), st_outflow_files$csv_names[csv]))
#   #Will need this column name specified for future merge
#   colnames(csv_file) <- paste(colnames(csv_file), "outflow", sep = "_")
#   csv_file$net_y2_y1 <- paste0(csv_file$y1_statefips, "-", csv_file$y2_statefips)
#   csv_file$year <- substring(st_outflow_files$csv_names[csv], 13, 16)
#   
#   assign(st_outflow_files$csv_names[csv], csv_file)
# }
# 
# 
# #Now, we want to create net matrices of the state inflow and outflow data for
# # each pair of years.
# 
# for(q in 1:nrow(st_inflow_files)){
#   df_list <- mget(ls(pattern = substring(st_inflow_files$csv_names[q], 12, 15)))
#   b <- df_list %>% reduce(inner_join, by = c("net_y2_y1", "year")) 
#   assign(paste0("netflows",substring(st_inflow_files$csv_names[q], 12, 15)), b)
# }
# 
# 
# #Clean our environment
# rm(list = ls()[grep("^stateinflow", ls())])
# rm(list = ls()[grep("^stateoutflow", ls())])
# rm(b)
# rm(csv_file)
# rm(df_list)
# #For instance, starting with 2013, let's create a net matrix that shows the net
# # flow of tax data from 2013-2014.
# Pattern1<-grep("netflows",names(.GlobalEnv),value=TRUE)
# Pattern1_list<-do.call("list",mget(Pattern1))
# 
# 
# for(p in 1:length(Pattern1_list)){
#   df_list <- as.data.frame(mget(Pattern1[p]))
#   colnames(df_list)<-gsub(paste0(Pattern1[p],"."),"", colnames(df_list))
#   
#   df_list$net_number_of_returns <- df_list$n1_inflow - df_list$n1_outflow
#   df_list$net_number_of_individuals <- df_list$n2_inflow - df_list$n2_outflow
#   df_list$net_AGI <- df_list$AGI_inflow - df_list$AGI_outflow
#   df_list <- df_list %>% select(-c(y1_state_name_inflow, y2_state_name_outflow, y1_state_inflow, n1_inflow, n2_inflow, AGI_inflow,
#                                    y1_statefips_outflow, y2_statefips_outflow, y2_statefips_outflow,
#                                    n1_outflow, n2_outflow, AGI_outflow, y2_state_outflow))
#   assign(Pattern1[p], df_list)
# }
# 
# fips_code_census <- tidycensus::fips_codes %>%
#   select(c(state_code, state_name))
# fips_code_census$state_code <- sub("^0+", "", fips_code_census$state_code)
# fips_code_census$y2_statefips_inflow <- fips_code_census$state_code
# fips_code_census$y1_statefips_inflow <- fips_code_census$state_code
# #y1
# fips_code_census_y1 <- fips_code_census %>%
#   select(-c(y2_statefips_inflow, state_code))
# fips_code_census_y1$state_name_y1 <- fips_code_census_y1$state_name
# fips_code_census_y1 <- fips_code_census_y1 %>%
#   select(-c(state_name))
# fips_code_census_y1 <- fips_code_census_y1[!duplicated(fips_code_census_y1), ]
# 
# #y2
# fips_code_census_y2 <- fips_code_census %>%
#   select(-c(y1_statefips_inflow, state_code))
# fips_code_census_y2$state_name_y2 <- fips_code_census_y2$state_name
# fips_code_census_y2 <- fips_code_census_y2 %>%
#   select(-c(state_name))
# fips_code_census_y2 <- fips_code_census_y2[!duplicated(fips_code_census_y2), ]
# 
# 
# #Merge with FIPS names in order to create column names
# # flow of tax data from 2013-2014.
# Pattern1<-grep("netflows",names(.GlobalEnv),value=TRUE)
# Pattern1_list<-do.call("list",mget(Pattern1))
# 
# 
# for(g in 1:length(Pattern1_list)){
#   df_list <- as.data.frame(mget(Pattern1[g]))
#   colnames(df_list)<-gsub(paste0(Pattern1[g],"."),"", colnames(df_list))
#   t <- merge(df_list, fips_code_census_y1, by = c("y1_statefips_inflow"))
#   t <- merge(t, fips_code_census_y2, by = c("y2_statefips_inflow"))
#   t <- t %>%
#     select(-c(y1_statefips_inflow, y2_statefips_inflow, net_y2_y1, year))
#   assign(Pattern1[g], t)
# }
# 
# #Pivot to create a matrix
# #net_number of individuals
# for(m in 1:length(Pattern1_list)){
#   df_list <- as.data.frame(mget(Pattern1[m]))
#   colnames(df_list)<-gsub(paste0(Pattern1[m],"."),"", colnames(df_list))
#   df_list <- df_list %>%
#     select(-c(net_number_of_returns, net_AGI)) %>%
#     pivot_wider(names_from = state_name_y2, values_from = net_number_of_individuals)
#   df_list <- df_list %>% 
#     mutate_if(is.numeric, funs(. * -1))
#   df_list <- cbind(df_list, total_flows = rowSums(df_list[,-c(1)]))
#   #df_list <- rbind(df_list, total_flows = colSums(df_list[,-c(1)]))
#   assign(paste0(Pattern1[m], "_net_number_of_individuals"), df_list)
#   writexl::write_xlsx(df_list, paste0("/gpfs/gibbs/project/sarin/ds3228/Repositories/soi_tax_stats/output/net_number_of_individuals/",paste0(Pattern1[m], "_net_number_of_indiviuals.xlsx")))
#   
#   
# }
# #net number of AGI
# for(m in 1:length(Pattern1_list)){
#   df_list <- as.data.frame(mget(Pattern1[m]))
#   colnames(df_list)<-gsub(paste0(Pattern1[m],"."),"", colnames(df_list))
#   df_list <- df_list %>%
#     select(-c(net_number_of_returns, net_number_of_individuals)) %>%
#     pivot_wider(names_from = state_name_y2, values_from = net_AGI)
#   df_list <- df_list %>% 
#     mutate_if(is.numeric, funs(. * -1))
#   df_list <- cbind(df_list, total_flows = rowSums(df_list[,-c(1)]))
#   #df_list <- rbind(df_list, total_flows = colSums(df_list[,-c(1)]))
#   assign(paste0(Pattern1[m], "_net_AGI"), df_list)
#   writexl::write_xlsx(df_list, paste0("/gpfs/gibbs/project/sarin/ds3228/Repositories/soi_tax_stats/output/net_AGI/",paste0(Pattern1[m], "_net_AGI.xlsx")))
#   
#   
# }
# #net number of returns
# for(m in 1:length(Pattern1_list)){
#   df_list <- as.data.frame(mget(Pattern1[m]))
#   colnames(df_list)<-gsub(paste0(Pattern1[m],"."),"", colnames(df_list))
#   df_list <- df_list %>%
#     select(-c(net_AGI, net_number_of_individuals)) %>%
#     pivot_wider(names_from = state_name_y2, values_from = net_number_of_returns)
#   df_list <- df_list %>% 
#     mutate_if(is.numeric, funs(. * -1))
#   df_list <- cbind(df_list, total_flows = rowSums(df_list[,-c(1)]))
#   #df_list <- rbind(df_list, total_flows = colSums(df_list[,-c(1)]))
#   assign(paste0(Pattern1[m], "_net_number_of_returns"), df_list)
#   writexl::write_xlsx(df_list, paste0("/gpfs/gibbs/project/sarin/ds3228/Repositories/soi_tax_stats/output/net_number_of_returns/",paste0(Pattern1[m], "_net_number_of_returns.xlsx")))
# }
# 
# #Entire of creating matrices.
# 
# 
# 
# 
#Author: Dylan Saez
#Purpose: SOI UI
library(shiny)
library(shinyMatrix)
library(DT)
library(leaflet)
library(sf)

# VA counties - downloaded via the awesome tigris package
shape <- tigris::counties(state = "VA", class = "sf")

Pattern1<-grep("netflows",names(.GlobalEnv),value=TRUE)
Pattern1_list<-do.call("list",mget(Pattern1))


ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls(Pattern1_list)),
  downloadButton("download"),
  verbatimTextOutput("summary"),
  tableOutput("table"),
  
  
)


server <- function(input, output, session) {
  # Create a reactive expression
  dataset <- reactive({
    get(input$dataset, Pattern1_list)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".csv")
    },
    content = function(file) {
      write.csv(dataset(), file)
    }
  )
  
  output$summary <- renderPrint({
    # Use a reactive expression by calling it like a function
    summary(dataset())
  })
  
  output$table <- renderTable({
    dataset()
  })
  
  
}

shinyApp(ui, server)

