#loading the packages
library(shinydashboard)
require(shiny)
require(highcharter)
library(dplyr)
library(tidyverse)
library(tidyr)
library(reshape2)
library(bigrquery)

#layout of the dashboard
#defining character vectors for select inputs
country<-c("India","United States","Mexico","Canada","China, People's Republic of","Japan","Russian Federation","Germany","United Kingdom","European Union",
           "ASEAN-5","New Zealand","Australia","Netherlands","Luxembourg",
           "France","Qatar","United Arab Emirates","Saudi Arabia")

unions<-c("Major advanced economies (G7)","European Union","Emerging and Developing Europe","ASEAN-5","Commonwealth of Independent States",
          "Emerging and Developing Asia","Latin America and the Caribbean",
          "Middle East, North Africa, Afghanistan, and Pakistan")
#function used to define the dashboard 
dashboardPage(
    #defines header

    #header of the dashboard
    dashboardHeader(
        title="Inflation Rates" ,
        dropdownMenu()
    ),
    #defines sidebar of the dashboard
    dashboardSidebar(
        sidebarMenu(
            #the sidebar menu items
            menuItem("Blockchain", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Miners", tabName = "miners", icon = icon("th"))  
        )),
    #defines the body of the dashboard
    dashboardBody(
        tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #292929;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #103965;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #292929;
                                }        

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #292929;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #103965;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #292929;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: ##029db3;
                                }
                                /* toggle button when hovered  */                    
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: ##029db3;
                                }
                                
                                 /* body */
                                .content-wrapper, .right-side {
                                background-color: #0d0d0d;
                                }
                                
                                .box.box-solid.box-primary>.box-header {
                                    color:#fff;
                                    background:#222d32
                                    }
                                
                                .box.box-solid.box-primary{
                                border-bottom-color:#222d32;
                                border-left-color:#222d32;
                                border-right-color:#222d32;
                                border-top-color:#222d32;
                                background:#222d32
                                }
                                '))),
        
        #to add external CSS tags$head(
        # Styling Well 
        tags$style(type = 'text/css','.well {
                       background-color: #00244a;
                       }'
        )
        ,
        tabItems(
            #First TAB Menu-dashboard- first argument should be the 'tabName' value of the menuItem function  
            tabItem(tabName = "dashboard",
                    fluidRow(
                        column(12,
                        ),#end column
                        column(12,
                               box(
                                   
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   background = "black",
                                   valueBoxOutput("semanalHashRate"),
                                   valueBoxOutput("semanalDirreciones"),
                                   valueBoxOutput("lagSemanalHashRateOut"),
                                   highchartOutput("hashRate"),
                                   width="12", status = "primary"),
                               box(
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   background = "black",
                                   
                                   highchartOutput("transactions"),
                                   width="6", status = "primary"),#end box2
                               box(
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   background = "black",
                                   
                                   highchartOutput("transactions_max"),
                                   width="6", status = "primary"),#end
                               box(
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   background = "black",
                                   
                                   highchartOutput("direccionesTotales"),
                                   width="12", status = "primary")
                        ), #end column
                        br(), #line break
                        h4("Relative inflation rates time series plot",align="center"),
                        br(),
                        
                    ),#end row
                    
            ),
            #second tab menu- ABOUT
            tabItem(tabName="miners",
                    br(),
                    fluidRow(
                        column(12,
                        ),#end column
                        column(12,
                               box(
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   background = "black",
                                   highchartOutput("minersGini"),
                                   width="6", status = "primary"), 
                               box(
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   background = "black",
                                   highchartOutput("minersRich"),
                                   width="6", status = "primary")#end box2
                               , 
                               box(
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   background = "black",
                                   highchartOutput("difficultadMedia"),
                                   width="12", status = "primary")
                        ), #end column
                        br(), #line break
                        br()
                        
                    )
            )
        )#end tabitems
    )#end body
    
)#end dashboardlibrary(shiny)
# library(shinydashboard)
# library(tidyverse)
# library(httpuv)
# library(gargle)
# library(bigrquery)
# 
# bq_auth(path="My First Project-44d4eb22b5fd.json")
# # 
# projectid = "studious-nature-260104"
# 
# sql <- "SELECT  *
#     FROM `bigquery-public-data.crypto_ethereum.transactions`
# LIMIT 1000"
# 
# df <- query_exec(sql, projectid, use_legacy_sql = FALSE)
# 
# 
# ui <- dashboardPage(
#     dashboardHeader(title = "Dynamic sidebar"),
#     dashboardSidebar(
#         sidebarMenuOutput("menu")
#     ),
#     dashboardBody(
#         tags$head(tags$style(HTML('/* body */
#                                 .content-wrapper, .right-side {
#                                 background-color: #000000;
#                                 }
#                                 
#                                 /* navbar (rest of the header) */
#                                 .skin-blue .main-header .navbar {
#                                 background-color: #4C4C4C;
#                                 }
#                                   
#                                  /* main sidebar */
#                                 .skin-blue .main-sidebar {
#                                 background-color: #4C4C4C;
#                                 }')))
#     )
# )