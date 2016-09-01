#################### Libraries and Data ###############################################################################


library(shiny) # for shiny app
library(shinydashboard) # for shiny dashboard layout
library(DT) # for using the JavaScript library DataTables
load("allData.rda") # load all the data tables (as data frames) from the EPIC ETL database (maintained by Adam)


#################### Data Table Info ##################################################################################


tableNames <- ls() # get a character vector of all the names of the data frames

tables <- lapply(1:length(tableNames), function(i) {
  menuSubItem(tableNames[i], tabName = tableNames[i], icon = icon("table")) # make menu sub items for the data tables
})

tableTabs <- lapply(1:length(tableNames), function(i) { 
  dataTableName <- paste(tableNames[i], "TableOutput", sep = "") # the output names of the data tables
  tabItem(tabName = tableNames[i], h2(paste(tableNames[i], sep = "")), 
          DT::dataTableOutput(dataTableName)) # make tab items to output the data tables
})



#################### Header ###########################################################################################


header <- dashboardHeader(title = "EPIC Data Explorer") # app title


#################### Sidebar ##########################################################################################


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("EXPLORE DATA", tabName = "exploreData", icon = icon("database"), # menu item for exploring the data tables  
             do.call(tagList, tables) # use a taglist to call the names of the data tables for sub menu items
             ), 
    menuItem("VARIABLE CART", tabName = "varaibleCart", icon = icon("shopping-cart"), 
             menuSubItem("variables", tabName = "variables", icon = icon("cubes")), 
             menuSubItem("process data", tabName = "processData", icon = icon("magic"))), # menu item for the variable cart 
    menuItem("ANALYSIS", tabName = "analysis", icon = icon("calculator")), # menu item for doing analysis
    menuItem("VISUALIZATION", tabName = "visualization", icon = icon("line-chart")) # menu item for data visualization
    )
  )


#################### Body #############################################################################################


body <- dashboardBody(  
  fluidRow(
    column(width = 4, 
           div(
             box(background = "aqua", width = 6, height = 170,
                 fluidRow(
                   column(width = 8, textInput("username", "Username"))
                 ), 
                 fluidRow(
                   column(width = 8, passwordInput("password", "Password")), 
                   column(width = 4, 
                          div(actionButton("login", "Log in"), style = "margin-top:-48px"),
                          div(actionButton("logout", "Log out"), style = "margin-top:40px"))
                 )
             ), style = "width:650px")
           )
    ), 
  uiOutput("loginUI")
  )


#################### Put the header, sidebar, and body together #######################################################


ui <- dashboardPage(
  header, 
  sidebar,
  body
  )

