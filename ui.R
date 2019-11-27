#loading the packages
library(shinydashboard)
require(shiny)
require(highcharter)
library(dplyr)
library(tidyverse)
library(tidyr)
library(reshape2)
library(lubridate)

rsconnect::setAccountInfo(name='shadeslayerbi', token='7602E84548910F19D570B2B8B44B2FA2', secret='uCV9AW1dQuOwud/dgnf5G3wEk04O0a6QXXpLOSQW')


#layout of the dashboard
#defining character vectors for select inputs
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
        sidebarMenu(sliderInput("rango_fechas",
                                          "Rango de Fechas:",
                                          min = as.Date("2018-01-01","%Y-%m-%d"),
                                          max = as.Date(Sys.Date()),
                                          value=c(as.Date("2018-01-01","%Y-%m-%d"), as.Date(Sys.Date()) ),
                                          timeFormat="%Y-%m-%d"),
                    #the sidebar menu items
                    menuItem("Blockchain", tabName = "dashboard", icon = icon("dashboard")),
                    menuItem("Propiedades", tabName = "money", icon = icon("money")),
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
                                font-size: 20px;
                                font-color: white;
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
                                   width="6", status = "primary", 
                                   valueBoxOutput("semanalTransacs", width = 12),
                                   valueBoxOutput("lagSemanalTransactions", width = 12),
                               ),#end box2
                               box(
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   background = "black",
                                   highchartOutput("transactions_max"),
                                   width="6", status = "primary",
                                   valueBoxOutput("semanalTransacsMax", width = 12),
                                   valueBoxOutput("lagSemanalTransactionsMax", width = 12),),#end
                               box(
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   background = "black",
                                   
                                   highchartOutput("direccionesTotales"),
                                   width="12", status = "primary"),
                                   valueBoxOutput("semanalC", width = 6),
                                   valueBoxOutput("lagSemanalC", width = 6)
                               
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
            ),
            
            tabItem(tabName="money",
                    br(),
                    fluidRow(
                        column(12,
                        ),#end column
                        column(12,
                               box(
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   background = "black",
                                   highchartOutput("inflacion"),
                                   width="12", status = "primary"),
                               box(
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   background = "black",
                                   
                                   highchartOutput("tether"),
                                   width="12", status = "primary")
                        ), #end column
                        br(), #line break
                        br()
                        
                    )
            )
        )#end tabitems
    )#end body
    
)#end dashboardlibrary(shiny)