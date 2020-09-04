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
source('general_methods.R')

getLicensureWageNames <- function(){
  df <- read.csv(file="Licensure/Wages and table info.csv")
  return(df[,1])
}


sidebar <- dashboardSidebar(
  includeCSS("styles.css"),
  width = 350,
  sidebarMenu(
    id = "tabs",
    menuItem("General", tabName = "general", icon = icon("info-circle"),
             menuSubItem("Population", tabName = "general_population"),
             menuItem("Median Income", tabName="general_median_income",
                      menuSubItem("Percent change in median income between 2010 and 2018", tabName = "median_income_percent"),
                      menuSubItem("15-24", tabName = "median_income_15"),
                      menuSubItem("25-44", tabName = "median_income_25"),
                      menuSubItem("45-64", tabName = "median_income_45"),
                      menuSubItem("65 and Over", tabName = "median_income_65"),
                      menuSubItem("Families", tabName = "median_income_families")
             ),
             menuSubItem("Unemployment", tabName = "general_unemployment"),
             menuSubItem("Race", tabName = "general_race"),
             menuSubItem("Poverty", tabName = "general_poverty"),
             menuItem("Labor Force Participation", tabName = "general_labor_force",
                      menuSubItem("Industry by Payroll", tabName = 'labor_payroll'),
                      menuSubItem("Industry by Employment", tabName ='labor_employment' ),
                      menuSubItem("Occupation Distribution", tabName = 'labor_occupation'),
                      menuSubItem("Job Flow", tabName = "labor_flow")
             )
    ),
    
    menuItem("Licensure", tabName = "licensure", icon = icon("id-badge"),
             menuSubItem("Licensure Requirements by Occupation", tabName = "licensure_reqs")
             # menuSubItem("Median Annual Income by Occupation", tabName= "lcensure_")
    ),
    
    menuItem("DSPG", tabName = "home", selected = T, icon=icon("brain"))
  )
)
body <- dashboardBody(
  tabItems(
    ######GENERAL##########
    tabItem(tabName = "general_population",
            fluidRow(shinydashboard::box(title="Population Map", width=4), shinydashboard::box(leafletOutput("general_population_plt", height = "600px"), width=12, height = "600px"),
                     shinydashboard::box("U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width = 12))),
    tabItem(tabName = "median_income_families",
            fluidRow(shinydashboard::box(title="Median Income Map", width = 4), shinydashboard::box(leafletOutput("median_income_families_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width = 12))),
    tabItem(tabName = "median_income_percent",
            fluidRow(shinydashboard::box(title="Percent change in median income between 2010 and 2018", width = 4), shinydashboard::box(leafletOutput("general_median_income_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width = 12))),
    tabItem(tabName = "median_income_15",
            fluidRow(shinydashboard::box(title="Median Income Map Ages 15-24", width = 4), shinydashboard::box(leafletOutput("median_income_15_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width = 12))),
    tabItem(tabName = "median_income_25",
            fluidRow(shinydashboard::box(title="Median Income Map Ages 25-44", width = 4), shinydashboard::box(leafletOutput("median_income_25_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width = 12))),
    tabItem(tabName = "median_income_45",
            fluidRow(shinydashboard::box(title="Median Income Map Ages 45-64", width = 4), shinydashboard::box(leafletOutput("median_income_45_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width = 12))),
    tabItem(tabName = "median_income_65",
            fluidRow(shinydashboard::box(title="Median Income Map Ages 65 and Over", width = 4), shinydashboard::box(leafletOutput("median_income_65_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width = 12))),
    tabItem(tabName = "general_poverty",
            fluidRow(shinydashboard::box(title="Poverty Map", width = 4), shinydashboard::box(leafletOutput("general_poverty_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width = 12))),
    tabItem(tabName = "general_race",
            fluidRow(shinydashboard::box(title="Race Distribution Map", width = 4), shinydashboard::box(leafletOutput("general_race_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("Source: U.S. Census Bureau (2010-2018).  American Community Survey 5-year estimates.", width = 12))),
    tabItem(tabName = "general_unemployment",
            fluidRow(shinydashboard::box(title="Unemployment Graph", width = 4), shinydashboard::box(plotOutput("general_unemployment_plt", height = "600px"),width=12, height = "600px"),
                     shinydashboard::box("U.S. Bureau of Labor Statistics, Local Area Unemployment Statistics (January 2010-May 2020)", width = 12))),
    tabItem(tabName = "labor_payroll",
            fluidRow(
              shinydashboard::box(title = "Industry Ranks by Payroll", width=4)
            ),
            fluidRow(
              shinydashboard::box(plotOutput("labor_payroll_plt_tree", click = "click_labor_payroll"), width = 5),
              shinydashboard::box(plotlyOutput("labor_payroll_plt_bar"), width = 7),
              shinydashboard::box(
                selectInput("labor_payroll_input", h3("Select County"),
                            choices = c("Malheur", "Canyon", "Payette", "Washington"))
              )
            ),
            fluidRow(
              shinydashboard::box("U.S. Census Bureau, 2018-2012 County Business patterns", width=12)
            )
    ),
    tabItem(tabName = "labor_employment",
            fluidRow(
              shinydashboard::box(title = "Industry Ranks by Employment", width =4)
            ),
            fluidRow(
              shinydashboard::box(plotOutput("labor_employment_plt_tree", click = "click_labor_employment"), width = 5),
              shinydashboard::box(plotlyOutput("labor_employment_plt_bar"), width = 7),
              shinydashboard::box(
                selectInput("labor_employment_input", h3("Select County"),
                            choices = c("Malheur", "Canyon", "Payette", "Washington"))
              )
            ),
            fluidRow(
              shinydashboard::box("U.S. Census Bureau, 2018-2012 County Business patterns", width=12)
            )
    ),
    tabItem(tabName = "labor_occupation",
            fluidRow(
              shinydashboard::box(title = "Distribution of Occupations", width=4)
            ),
            fluidRow(
              shinydashboard::box(plotOutput("labor_occupation_plt_tree", click = "click_labor_occupation"),width = 5),
              shinydashboard::box(plotlyOutput("labor_occupation_plt_bar"), width = 7),
              shinydashboard::box(
                selectInput("labor_occupation_input", h3("Select County"),
                            choices = c("Malheur", "Canyon", "Payette", "Washington"))
              )
            ),
            fluidRow(
              shinydashboard::box("U.S. Census Bureau, 2018-2012 County Business patterns", width=12)
            )
    ),
    tabItem(tabName = "labor_flow",
            fluidRow(shinydashboard::box(width=4,  title="Worker Flow of Malheur and Payette")
            ),
            fluidRow(
              shinydashboard::box(plotlyOutput("labor_flow_Malheur_plt"),width=12),
              shinydashboard::box(plotlyOutput("labor_flow_Payette_plt"),width=12)
            ),
            # fluidRow(
            #     shinydashboard::box(plotlyOutput("labor_flow_Payette_plt",width=12))
            # ),
            fluidRow(
              shinydashboard::box("U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics (Beginning of Quarter Employment, 2nd Quarter of 2002-2017)", width=12)
            )
    ),
    
    ###LICENSUREE
    tabItem(tabName = "licensure_reqs",
            fluidRow(shinydashboard::box(width=4, title="Licensure Requirements")),
            fluidRow(
              shinydashboard::box(title = "Oregon Wage", plotlyOutput("licensure_wage_oregon_plt"),width=6),
              shinydashboard::box(title = "Idaho Wage", plotlyOutput("licensure_wage_idaho_plt"),width=6),
              shinydashboard::box(selectInput("wage_occupation", h3("Select Occupation"), choices = getLicensureWageNames()))
            ),
            fluidRow(
              shinydashboard::box(title="Licensure Requirements", DT::dataTableOutput("licensure_reqs_oregon_plt"),width=12)
            ),
            fluidRow(
              shinydashboard::box("Source: The National Occupational Licensing Database, National Council of State Legislatures, 2020")
            )
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
  #  General     #
  #              #
  ################
  output$general_population_plt <- renderLeaflet(drawGeneralPopulation())
  output$general_median_income_plt <- renderLeaflet(drawGeneralMedianIncome())
  output$general_unemployment_plt <- renderPlot(drawGeneralUnemployment())
  output$median_income_families_plt <- renderLeaflet(drawFamilies())
  output$median_income_15_plt <- renderLeaflet(draw15())
  output$median_income_25_plt <- renderLeaflet(draw25())
  output$median_income_45_plt <- renderLeaflet(draw45())
  # draw65 <- reactive({create65()})
  output$median_income_65_plt <- renderLeaflet(draw65())
  output$general_poverty_plt <- renderLeaflet(drawGeneralPoverty())
  output$general_race_plt <- renderLeaflet(drawRace())
  #output$labor_payroll_plt_tree <- renderPlot(drawIndustryPayrollTree(getIndustryPayrollData(input$labor_payroll_input)))
  getPayrollInput <- reactive({path = paste("Occupations and Industries/Industries/County_IndustriesPayroll_",input$labor_payroll_input, ".csv",sep="")
  return(read.csv(file = path))
  })

  #payroll_df = getIndustryPayrollData(getPayrollInput())
  output$labor_payroll_plt_tree <- renderPlot(drawIndustryPayrollTree(getPayrollInput()))

  PayrollTree_clicked <- reactiveValues(center = NULL, for_condition=NULL)
  observeEvent(input$click_labor_payroll,{
    x <- input$click_labor_payroll$x
    y <- input$click_labor_payroll$y
    PayrollTree_clicked$center <- c(x,y)
  })
  getIndustryPayroll <- reactive({
    x <- PayrollTree_clicked$center[1]
    y <- PayrollTree_clicked$center[2]

    x <- (x - treeIndustryPayroll$vpCoorX[1]) / (treeIndustryPayroll$vpCoorX[2] - treeIndustryPayroll$vpCoorX[1])
    y <- (y - treeIndustryPayroll$vpCoorY[1]) / (treeIndustryPayroll$vpCoorY[2] - treeIndustryPayroll$vpCoorY[1])


    l <- tmLocate(list(x=x, y=y), treeIndustryPayroll)
    z=l[, 1:(ncol(l)-5)]


    if(is.na(z[,1]))
      return(NULL)

    col=as.character(z[,1])
    print(col)
    return(col)
    #filter(industry_data,name==col)
  })
  output$labor_payroll_plt_bar <- renderPlotly(drawIndustryPayrollYear(getPayrollInput(), getIndustryPayroll()))

  #####EMployment Section#########
  getEmploymentInput <- reactive({path = paste("Occupations and Industries/Industries/County_Industries_",input$labor_employment_input,".csv",sep="")
  print(path)
  return(read.csv(file = path))
  })
  output$labor_employment_plt_tree <- renderPlot(drawIndustryEmploymentTree(getEmploymentInput()))
  EmploymentTree_clicked <- reactiveValues(center = NULL, for_condition=NULL)
  observeEvent(input$click_labor_employment,{
    x <- input$click_labor_employment$x
    y <- input$click_labor_employment$y
    EmploymentTree_clicked$center <- c(x,y)
  })
  getIndustryEmployment <- reactive({
    x <- EmploymentTree_clicked$center[1]
    y <- EmploymentTree_clicked$center[2]

    x <- (x - treeIndustryEmployment$vpCoorX[1]) / (treeIndustryEmployment$vpCoorX[2] - treeIndustryEmployment$vpCoorX[1])
    y <- (y - treeIndustryEmployment$vpCoorY[1]) / (treeIndustryEmployment$vpCoorY[2] - treeIndustryEmployment$vpCoorY[1])


    l <- tmLocate(list(x=x, y=y), treeIndustryEmployment)
    z=l[, 1:(ncol(l)-5)]


    if(is.na(z[,1]))
      return(NULL)

    col=as.character(z[,1])
    print(col)
    return(col)
    #filter(industry_data,name==col)
  })
  output$labor_employment_plt_bar <- renderPlotly(drawIndustryEmploymentYear(getEmploymentInput(), getIndustryEmployment()))

  #####Labor section######
  getOccupationInput <- reactive({path = paste("Occupations and Industries/Occupations/County_Occupations_",input$labor_occupation_input,".csv",sep="")
  return(read.csv(file = path))
  })
  output$labor_occupation_plt_tree <- renderPlot(drawOccupationTree(getOccupationInput()))
  OccupationTree_clicked <- reactiveValues(center = NULL)
  observeEvent(input$click_labor_occupation,{
    x <- input$click_labor_occupation$x
    y <- input$click_labor_occupation$y
    OccupationTree_clicked$center <- c(x,y)
  })
  getOccupation <- reactive({
    x <- OccupationTree_clicked$center[1]
    y <- OccupationTree_clicked$center[2]
    x <- (x - treeOccupation$vpCoorX[1]) / (treeOccupation$vpCoorX[2] - treeOccupation$vpCoorX[1])
    y <- (y - treeOccupation$vpCoorY[1]) / (treeOccupation$vpCoorY[2] - treeOccupation$vpCoorY[1])
    l <- tmLocate(list(x=x, y=y), treeOccupation)
    z=l[, 1:(ncol(l)-5)]


    if(is.na(z[,1]))
      return(NULL)

    col=as.character(z[,1])
    print(col)
    return(col)
  })
  output$labor_occupation_plt_bar <- renderPlotly(drawOccupationYear(getOccupationInput(), getOccupation()))
  output$labor_flow_Malheur_plt <- renderPlotly(drawFlowMalhuer())
  output$labor_flow_Payette_plt <- renderPlotly(drawFlowPayette())

  ################
  #              #
  #  Licensure   #
  #              #
  ################
  OccupationInput <- reactive(getLicensureWageData())
  # output$licensure_reqs_plt <- renderPlotly(drawBarTestImproved(OccupationInput()))
  # output$licensure_reqs_txt <- renderText(drawTextTestImproved(OccupationInput()))
  output$licensure_wage_oregon_plt <- renderPlotly(drawLicensureWageOregon(OccupationInput(),input$wage_occupation))
  output$licensure_wage_idaho_plt <- renderPlotly(drawLicensureWageIdaho(OccupationInput(),input$wage_occupation))

  OccupationInput2 <- reactive(getLicensureReqData())
  # output$licensure_reqs_oregon_plt <- renderPrint(drawLicensureReq(OccupationInput2(),input$req_occupation, "Oregon"))
  # output$licensure_reqs_idaho_plt <- renderPrint(drawLicensureReq(OccupationInput2(),input$req_occupation,"Idaho"))
  output$licensure_reqs_oregon_plt <- DT::renderDataTable(OccupationInput2(),options=list(scrollX = TRUE))


}

# Run the application 
shinyApp(ui = ui, server = server)
