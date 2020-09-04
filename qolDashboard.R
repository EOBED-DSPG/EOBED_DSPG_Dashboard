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
source('qol_methods.R')
sidebar <- dashboardSidebar(
  includeCSS("styles.css"),
  width = 350,
  sidebarMenu(
    id = "tabs",
    menuItem("Quality of Life", tabName = "qol", icon = icon("balance-scale"),
             menuItem("State of Local Economy and Determinants of Economic Opportunity", tabName = "qol_top_local",
                      menuSubItem("State of Economic Opportunity", tabName = "local_state"),
                      menuSubItem("Determinants of Economic Opportunity", tabName = "local_determinants")
             ),
             menuItem("Education and Determinants of Economic Opportunity", tabName = "qol_top_education",
                      menuSubItem("Educational Outcomes", tabName = "education_outcomes"),
                      menuSubItem("Determinants of Opportunity: College and Career Readiness", tabName = "education_determinants"),
                      menuSubItem("Determinants of Opportunity", tabName = "education_opportunity")
             ),
             menuItem("Outcomes, Quality and Determinants of Health", tabName = "qol_top_health",
                      menuSubItem("Healthcare Outcomes and Quality", tabName = "health_state"),
                      menuSubItem("Determinants of health", tabName = "health_determinants")
             )
    ),
    menuItem("DSPG", tabName = "home", selected = T, icon=icon("brain"))
  )
)
body <- dashboardBody(
  tabItems(
    ########QOL#################
    tabItem(tabName = "local_state",
            fluidRow(shinydashboard::box(width=4, title="State of Economic Opportunity"),shinydashboard::box(leafletOutput("local_state_plt", height = "600px"),width=12, height = "600px"), 
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.",width=12)),
                     DT::dataTableOutput("local_state_table",width ='100%')
            ),
    tabItem(tabName = "local_determinants",
            fluidRow(shinydashboard::box(width=4, title ='Determinants of Economic Opportunity'),shinydashboard::box(leafletOutput("local_determinants_plt", height = "600px"),width=12, height = "600px"), 
                     shinydashboard::box("Source: United States Department of Education. Office for Civil Rights. 2015-16 Civil Rights Data Collection (CRDC)",width=12)),
                     DT::dataTableOutput("local_determinants_table",width ='100%')  
            ),
    tabItem(tabName = "education_outcomes",
            fluidRow(shinydashboard::box(width=4, title = "Educational Outcomes"), shinydashboard::box(leafletOutput("education_outcomes_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates and United States Department of Education. Office for Civil Rights. 2015-16 Civil Rights Data Collection (CRDC) Oregon Department of Education, 4 Year Cohort Graduation Rate (School Year 2018-2019); Idaho State Department of Education, 2015-2019 Four-Year Graduation Rate",width=12)),
                     DT::dataTableOutput("education_outcomes_table",width ='100%')
            ),
    tabItem(tabName = "education_determinants",
            fluidRow(shinydashboard::box(width=4, title = "Determinants of Opportunity: College and Career Readiness"), shinydashboard::box(leafletOutput("education_determinants_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates and United States Department of Education. Office for Civil Rights. 2015-16 Civil Rights Data Collection (CRDC)",width=12)),
                     DT::dataTableOutput("education_determinants_table",width ='100%')
            ),
    tabItem(tabName = "education_opportunity",
            fluidRow(shinydashboard::box(width=4, title = "Determinants of Opportunity"), shinydashboard::box(leafletOutput("education_oppportunity_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates and United States Department of Education. Office for Civil Rights. 2015-16 Civil Rights Data Collection (CRDC)",width=12)),
                     DT::dataTableOutput("education_opportunity_table",width ='100%')
            ),
    tabItem(tabName = "health_state",
            fluidRow(shinydashboard::box(width=4, title="Healthcare Outcomes and Quality"), shinydashboard::box(leafletOutput("health_state_plt", height = "600px"),width=12, height = "600px"), 
                     shinydashboard::box("Source: Centers for Medicaid and Medicare, Mapping Medicaid Disparities, Updated June 2020, Hospital Consumer Assessment of Healthcare Providers and Systems (HCAHPS) Survey, The Mapping Medicare Disparities (MMD) Hospital View, Centers for Medicare & Medicaid Services, June 2020, Inpatient Claims, The Mapping Medicare Disparities (MMD) Hospital View,, Centers for Medicare & Medicaid Services, June 2020, Bureau of Vital Records and Health Statistics, Idaho Vital Statistics, Idaho Department of Health and Welfare, 2018, Death Data, Center for Health Statistics, Oregon Health Authority, 2018 and Bureau of Vial Records and Health Statistics, Idaho Vital Statistics, Idaho Department of Health and Welfare, 2018, Tejada-Vera B, Bastian B, Arias E, Escobedo LA., Salant B, Life Expectancy Estimates by U.S. Census Tract, 2010-2015. National Center for Health Statistics. 2020",width=12)),
                     DT::dataTableOutput("health_state_table",width ='100%') 
            ),
    tabItem(tabName="health_determinants",
            fluidRow(shinydashboard::box(width=4, title ="Determinants of Health"), shinydashboard::box(leafletOutput("health_determinants_plt", height = "600px"),width=12, height = "600px"), 
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.",width=12)),
                     DT::dataTableOutput("health_determinants_table",width ='100%')  
            ),
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

  ################
  #              #
  #  Licensure   #
  #              #
  ################
  # OccupationInput <- reactive(getLicensureWageData())
  # 
  # output$licensure_wage_oregon_plt <- renderPlotly(drawLicensureWageOregon(OccupationInput(),input$wage_occupation))
  # output$licensure_wage_idaho_plt <- renderPlotly(drawLicensureWageIdaho(OccupationInput(),input$wage_occupation))
  # 
  # OccupationInput2 <- reactive(getLicensureReqData())
  # output$licensure_reqs_oregon_plt <- DT::renderDataTable(OccupationInput2(),options=list(scrollX = TRUE))
  
  ################
  #              #
  #  QOL         #
  #              #
  ################
  
  Shape <- reactive(getReactiveShape())
  output$local_state_plt <- renderLeaflet(drawLocalState())
  output$local_state_table <- DT::renderDataTable({getEduOpp()},options=list(autoWidth= TRUE,scrollX = TRUE, pageLength = 10))
  output$local_determinants_plt <- renderLeaflet(drawLocalDeterminants())
  output$local_determinants_table <- DT::renderDataTable({getLocalDeterminantsTable()},options=list(autoWidth= TRUE,scrollX = TRUE, pageLength = 10))
  
  EducationShape <- reactive(readEducationShape())
  output$education_outcomes_plt <- renderLeaflet(drawEducationOutcomes(EducationShape(),Shape()))
  output$education_outcomes_table <- DT::renderDataTable({getEducationOutcomesTable()},options=list(autoWidth= TRUE,scrollX = TRUE, pageLength = 10))

  output$education_determinants_plt <- renderLeaflet(drawEducationDeterminants(EducationShape(),Shape()))
  output$education_determinants_table <- DT::renderDataTable({getEducationDeterminantsTable()},options=list(autoWidth= TRUE,scrollX = TRUE, pageLength = 10))
  
  output$education_oppportunity_plt <- renderLeaflet(drawEducationOpportunity(EducationShape(),Shape()))
  output$education_opportunity_table <- DT::renderDataTable({getEducationOpportunityTable()},options=list(autoWidth= FALSE,scrollX = TRUE, pageLength = 10))
  
  output$health_state_plt <- renderLeaflet(drawHealthState())
  output$health_state_table <- DT::renderDataTable({getHealthStateTable()},options=list(autoWidth= TRUE,scrollX = TRUE, pageLength = 10))
  output$health_determinants_plt <- renderLeaflet(drawHealthDeterminants())
  output$health_determinants_table <- DT::renderDataTable({getHealthDeterminantsTable()},options=list(autoWidth= FALSE,scrollX = TRUE, pageLength = 10))
}

# Run the application 
shinyApp(ui = ui, server = server)
