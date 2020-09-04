library(shinydashboard)
library(shiny)
library(conflicted)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(shinythemes)
library(tigris)
library(sf)
library(treemap)
library(readxl)
library(rsconnect)
library(leaflet)
library(plyr)
source('housing_methods.R')

sidebar <- dashboardSidebar(
      includeCSS("styles.css"),
      width = 350,
      sidebarMenu(
          id = "tabs",
          menuItem("Housing", tabName = "housing",icon = icon("home"),
                   menuSubItem("Payette vs Malhuer", tabName = "housing_vs"),
                   menuSubItem("Zone Type Distribution", tabName = "housing_zone"),
                   menuSubItem("House and Lot Square Feet Distribution", tabName = "housing_lot"),
                   menuItem("Home Ownership", tabName = "housing_ownership",
                            menuSubItem("Home Ownership Rates", tabName = "ownership_rate"),
                            menuSubItem("Mortgage/prop tax etc. % of median household income", tabName = "ownership_household"),
                            menuSubItem("Mortgage/prop tax etc. % of registered nurse income", tabName = "ownership_nurse")
                   ),
                   menuItem("Renters", tabName="housing_renter",
                            menuSubItem("Renter Rates", tabName = "renter_rate"),
                            menuSubItem("Rent % of median household income", tabName = "renter_household"),
                            menuSubItem("Rent % of registered nurse income", tabName = "renter_nurse")
                   )
          ),

          menuItem("DSPG", tabName = "home", selected = T, icon=icon("brain"))
      )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "housing_vs",
      fluidRow(shinydashboard::box(width=4, title="Housing Construction in Malheur vs Payette")),fluidRow(shinydashboard::box(plotlyOutput("housing_vs_plt"),width=6),shinydashboard::box(plotlyOutput("housing_vs_payette_plt"),width=6)), fluidRow(shinydashboard::box(sliderInput('slider1', h3('Year'), min = 1950, max = 2010, value = c(1950,2010))),
               shinydashboard::box("", radioButtons("housing_vs_input", h3('Select Zone Type'),choices = list("All Zone" = 1, "Residential" = 2, "Comercial"= 3, "Agricultural"=4))),
         #       shinydashboard::box(width=4, title = "Proportion of Residential Areas Built by Year"), shinydashboard::box(plotlyOutput("housing_residential_plt"),width=12),
         # shinydashboard::box(width=4, title = "Proportion of Comercial Areas Built by Year"), shinydashboard::box(plotlyOutput("housing_comercial_plt"),width=12),
         # shinydashboard::box(width=4, title = "Proportion of Agricultural Areas Built by Year"), shinydashboard::box(plotlyOutput("housing_agricultural_plt"),width=12),
         # shinydashboard::box("Source: Malheur Tax Assesors Office and Idaho Tax Assesors Office", width=12)
         ),
      fluidRow(shinydashboard::box(width=12, title = "README", "Source: Malheur and Payette Tax Accessors Office. Displays total number of construction in Malhuer and Payette respectively. User can select a range of years and specificy specific zone types to observe. Note: x-axis is in Log Scale"))
      ),
    tabItem(tabName = "housing_zone",
            fluidRow(shinydashboard::box(width=4, title = "Distribution of Lots by Zone Code")), 
                     fluidRow(shinydashboard::box("Malheur", plotlyOutput("housing_zone_all", height = '600px'), width =6, height = "600px"), shinydashboard::box("Payette",plotlyOutput("housing_zone_payette_all", height = '600px'),width = 6, height = '600px')),
                     fluidRow(shinydashboard::box("", radioButtons("housing_zone_input", h3('Select Zone Type'),choices = list("All Zone" = 1, "Residential" = 2, "Comercial"= 3, "Agricultural"=4))),
                              shinydashboard::box("", radioButtons("housing_zone_type",h3('X-axis Display'), choices = list("Count" = 1, "Percentage" = 2))),
                     shinydashboard::box("Source: Malheur and Payette Tax Accessor", width = 12))),
    tabItem(tabName = "housing_lot",
            fluidRow(shinydashboard::box(width=4, title = "Histogram of House and Lot Square Footage")), 
            fluidRow(shinydashboard::box("Malheur", plotlyOutput("housing_malheur_hist", height = '600px'), width =6, height = "600px"), shinydashboard::box("Payette",plotlyOutput("housing_payette_hist", height = '600px'),width = 6, height = '600px')),
            fluidRow(shinydashboard::box("", radioButtons("housing_lot_input", h3('Select Zone Type'),choices = list("All Zone" = 1, "Residential" = 2))),
            shinydashboard::box("", radioButtons("housing_lot_input_2", h3('Select Size Type'),choices = list("House Size" = 1, "Lot Size" = 2))),
                     shinydashboard::box("Source: Malheur and Payette Tax Accessor", width = 12))),
    tabItem(tabName = "ownership_rate",
        fluidRow(shinydashboard::box(width=4, title = "Housing Ownership Map"),shinydashboard::box(leafletOutput("housing_ownership_plt", height = "600px"),width=12, height = "600px"), 
                 shinydashboard::box("Source: Bureau of Labor Statistics, U.S. Department of Labor, Occupational Employment Statistics (2010-2018) and U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width=12))),
    tabItem(tabName = "renter_rate",
        fluidRow(shinydashboard::box(width=4,  title = "Housing Affordability Map"),shinydashboard::box(leafletOutput("housing_renter_plt", height = "600px"),width=12, height = "600px"),
                 shinydashboard::box("Source: Bureau of Labor Statistics, U.S. Department of Labor, Occupational Employment Statistics (2010-2018) and U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width=12))),
    tabItem(tabName = "renter_household",
        fluidRow(shinydashboard::box(width=4,  title = "Rent % of Median Household Income"),shinydashboard::box(leafletOutput("renter_household_plt", height = "600px"),width=12, height = "600px"), 
                 shinydashboard::box("Source: Bureau of Labor Statistics, U.S. Department of Labor, Occupational Employment Statistics (2010-2018) and U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width=12))),
    tabItem(tabName = "ownership_nurse",
        fluidRow(shinydashboard::box(width = 4, title = "Mortgage/prop tax etc. % of registered nurse income"), shinydashboard::box(leafletOutput("ownership_nurse_plt", height = "600px"), width = 12, height = "600px"),
                 shinydashboard::box("Source: Bureau of Labor Statistics, U.S. Department of Labor, Occupational Employment Statistics (2010-2018) and U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width = 12))),
    tabItem(tabName = "ownership_household",
        fluidRow(shinydashboard::box(width=4,  title = "Mortgage/prop tax etc. % of median household income"),shinydashboard::box(leafletOutput("ownership_household_plt", height = "600px"),width=12, height = "600px"), 
                 shinydashboard::box("Source: Bureau of Labor Statistics, U.S. Department of Labor, Occupational Employment Statistics (2010-2018) and U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width=12))),
    tabItem(tabName = "renter_nurse",
        fluidRow(shinydashboard::box(width=4,  title = "Rent % of registered nurse income"),shinydashboard::box(leafletOutput("renter_nurse_plt", height = "600px"),width=12, height = "600px"), 
                 shinydashboard::box("Source: Bureau of Labor Statistics, U.S. Department of Labor, Occupational Employment Statistics (2010-2018) and U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width=12))),
    tabItem(tabName = "home", shinydashboard::box(title = "What is the DSPG Program?",solidHeader = T, width = 12, "Rural Data Science for the Public Good is a project that supports the use of data science approaches to address issues of rural communities (defined broadly) through student summer projects and through engagement with Cooperative Extension. The project is part of a tri-state collaboration involving the University of Virginia, Oregon State University, Iowa State University, Virginia Tech, and Virginia State University. It is funded by USDA NIFA and by the Bill and Melinda Gates Foundation."),
                              shinydashboard::box(title = "Project Title", solidHeader = T, width =12, "Barriers to Economic Opportunities in the Eastern Oregon Border Region"),
                              shinydashboard::box(width = 12, title = "Project Members", solidHeader = T, shinydashboard::box(title = "Mentor",width=3,"Stuart Reitz, Professor (Extension), Oregon State University"),
                                                  shinydashboard::box(title = "Fellow", width =3, "Thamanna Vasan, Oregon State University, Economics"),
                                                  shinydashboard::box(title = "Interns", width = 3, "Melvin Ma, Oregon State University, Computer Science and Collin Robinson, Southern Oregon University, Computer Science"),
                                                  shinydashboard::box(title = "Stakeholder", width = 3, "Eastern Oregon Border Economic Development Board (Economic Mobility)"))
            )
    
    )
  )
  
ui <- dashboardPage(
      dashboardHeader(title = "EOBD DSPG Oregon", titleWidth = 350),
      sidebar = sidebar,
      body = body
)

server <- function(input, output, session) {

  #Plotly Graphs
  HousingVSInput <- reactive(getHousingVS())
  HousingVSInputCleaned <- reactive(getHousingVSCleaned())
  HousingVSPayetteInput <- reactive(getHousingVSPayette())
  PaycoPin <- reactive(getPaycoPin())
  
  output$housing_vs_plt <- renderPlotly(drawHousingVS(HousingVSInput(),HousingVSInputCleaned(), input$slider1, input$housing_vs_input))
  output$housing_vs_payette_plt <- renderPlotly(drawHousingVSPayette(HousingVSPayetteInput(), input$slider1, input$housing_vs_input))
  output$housing_zone_all <- renderPlotly(drawHousingZoneAll(HousingVSInput(),HousingVSInputCleaned(), input$housing_zone_input, input$housing_zone_type))
  output$housing_zone_payette_all <- renderPlotly(drawHousingZonePayetteAll(HousingVSPayetteInput(), PaycoPin(),input$housing_zone_input, input$housing_zone_type))
  output$housing_malheur_hist <- renderPlotly(drawMalheurHist(HousingVSInput(),HousingVSInputCleaned(),input$housing_lot_input, input$housing_lot_input_2))
  output$housing_payette_hist <- renderPlotly(drawPayetteHist(PaycoPin(),input$housing_lot_input, input$housing_lot_input_2) )
  #Leaflet Graphs
  #Reactive function to retrieve Cost and Affordability data
  Tract <- reactive(getReactiveTract())
  Or <- reactive(getReactiveOregon())
  Id <- reactive(getReactiveIdaho())
  Shape <- reactive(getReactiveShape())
  output$housing_ownership_plt <- renderLeaflet(drawHousingOwnership(Or(),Id(),Shape()))
  output$housing_renter_plt <- renderLeaflet(drawRenterRate(Or(),Id(),Shape()))
  output$renter_household_plt <- renderLeaflet(drawRenterHousehold(Tract(),Or(),Id(),Shape()))
  output$ownership_nurse_plt <- renderLeaflet(drawOwnershipNurse(Tract(),Or(),Id(),Shape()))
  output$ownership_household_plt <- renderLeaflet(drawOwnershipHousehold(Tract() ,Or(),Id(),Shape()))
  output$renter_nurse_plt <- renderLeaflet(drawRenterNurse(Tract(),Or(),Id(),Shape()))

  }

  # Run the application
  shinyApp(ui = ui, server = server)
