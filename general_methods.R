source("Population_Map/population_Map.R")
drawGeneralPopulation <- function(){
  drawPopulationMap()
}
source("Median_Income_Data/medianIncomeGraphics.R")

drawGeneralMedianIncome <- function(){
  main()
}
source('Median_Income_Data/family_Median_IncomeGr.R')
drawFamilies <- function(){
  drawFamily()
}
source("Median_Income_Data/age_15to24_MedianIncomeGr.R")
draw15 <- function(){
  create15()
}
source("Median_Income_Data/age_25to44_MedianIncomeGr.R")
draw25<- function(){
  create25()
}
source("Median_Income_Data/age_45to64_MedianIncomeGr.R")
draw45 <- function(){
  create45()
}
source("Median_Income_Data/age_65andOver_MedianIncomeGr.R")
draw65 <- function(){
  print("65 HAS BEEN RUNNN")
  create65()
}
source('Poverty_Data/povertyGraphics.R')

drawGeneralPoverty <- function(){
  print("I HAVE BEEN RUN ---- GENEERAL POVERTY")
  drawPovMap()
}
source('Unemployment_Data/unemploymentGraphs.R')
drawGeneralUnemployment <- function(){
  createUnemploymentPlt()
}
source('Race/demoChg.R')
drawRace <- function(){
  drawDemoChgMap()
}
getLicensureWageData <- function(name){
  df <- read.csv(file="Licensure/Wages and table info.csv")
  df[df=="na"]<-0
  return(df)
}
drawLicensureWageOregon <- function(df, name){
  temp <- df
  temp =  temp[temp[,'Career'] == name,]
  fig1 <- plot_ly(
    x = 2006:2019,
    y= as.numeric(as.vector(temp[1,2:15])),
    type = "bar",
    orientation="v",
  )
  fig1 %>% plotly::layout(showlegend= FALSE, title = "Oregon Wage")
  fig1
}
drawLicensureWageIdaho <- function(df, name){
  temp <- df
  temp =  temp[temp[,'Career'] == name,]
  fig2 <- plot_ly(
    x = 2006:2019,
    y = as.numeric(as.vector(temp[1,16:29])),
    type = "bar",
    orientation = "v",
  )
  fig2 %>% plotly::layout(showlegend = FALSE, title = "Idaho Wage")
  fig2
}

getLicensureReqData <- function(){
  df <- read.csv(file="Licensure/Licensure_Table_2020.csv")
  df <- df[2:24,]
  return(df)
}
getLicensureReqNames <- function(){
  df <- read.csv(file = "Licensure/Licensure_Table_2020.csv")
  df <- as.vector(df[2:23,1])
  df <- df[seq(1, length(df),2)]

  return(df)
}
tmLocate <- function(coor, tmSave) {
  tm <- tmSave$tm
  # retrieve selected rectangle
  rectInd <- which(tm$x0 < coor[1] &
                     (tm$x0 + tm$w) > coor[1] &
                     tm$y0 < coor[2] &
                     (tm$y0 + tm$h) > coor[2])

  return(tm[rectInd[1], ])
}


drawIndustryEmploymentTree <- function(df){
  df <- head(df,10)
  df$Share_employment <- as.numeric(df$Share_employment)
  treeIndustryEmployment <<- treemap(df,
                                     index = "NAICS2017_LABEL",
                                     vSize = "Share_employment",
                                     vColor = "Share_employment",
                                     type = "value",
                                     palette= "Purples",
                                     position.legend = "right",
                                     fontsize.labels=13,
                                     # tite = "Industry Ranks by Employment")
  )
  #border.lwds=10)
}
drawIndustryPayrollTree <- function(df){
  df <- head(df,10)
  df$Share_payroll <- as.numeric(df$Share_payroll)
  treeIndustryPayroll <<- treemap(df,
                                  index = "NAICS2017_LABEL",
                                  vSize = "Share_payroll",
                                  vColor = "Share_payroll",
                                  type = "value",
                                  palette= "Purples",
                                  position.legend = "right",
                                  fontsize.labels=13,
                                  title = "Industries Ranked by Payroll"
  )
  #border.lwds=10)
}
drawOccupationTree <- function(df){
  df <- head(df, 10)
  df$Percent_employed <- as.numeric(df$Percent_employed)
  treeOccupation <<- treemap(df,
                             index = "Occupation",
                             vSize = "Percent_employed",
                             vColor = "Percent_employed",
                             type = "value",
                             palette= "Purples",
                             position.legend = "right",
                             fontsize.labels=13,
                             title = "Top 10 Jobs"
  )
}
drawOccupationYear <- function(df, name){
  df$Year <- as.character(df$Year)
  df <- df[df[,'Occupation'] == name, ]
  fig <- plot_ly(
    df, x=~Year, y=~Percent_employed, type = 'bar',orientation='v'
  )
  fig
}
drawIndustryEmploymentYear <- function(df, name){
  df$YEAR <- as.character(df$YEAR)
  df <- df[df[,'NAICS2017_LABEL'] == name, ]
  fig <- plot_ly(
    df, x=~YEAR, y=~Share_employment, type = 'bar',orientation='v'
  )
  fig
}
drawIndustryPayrollYear <- function(df, name){
  df$YEAR <- as.character(df$YEAR)
  df <- df[df[,'NAICS2017_LABEL'] == name, ]
  fig <- plot_ly(
    df, x=~YEAR, y=~Share_payroll, type = 'bar',orientation='v'
  )
  fig
}
drawFlowMalhuer <- function(){
  y = c(9832,-697,-680,-192,-167,-1799,6297,2854,824,572,373,2723,13643)
  text = c("","-7.1%","-6.9%","2.0%","1.7%","18.3%", "","20.9%","6.0%","4.2%","2.7%","20.0%","")
  # x = list(c("Initial","Malheur Residents Working Locations","Malheur Residents Working Locations","Malheur Residents Working Locations","Malheur Residents Working Locations","Malheur Residents Working Locations", "Net", "Workers Residence Locations","Workers Residence Locations","Workers Residence Locations","Workers Residence Locations","Workers Residence Locations","Total"),
  #          c("Malheur Residents", "Canyon ID", "Payette ID", "Ada ID", "Jackson ID", "Other","Malheur Resident Workers","Payette ID", "Canyon ID", "Washington ID", "Ada ID", "Other", "Workers in Malheur"))
  x= list("Malheur Residents", "Work in Canyon ID", "Work in Payette ID", "Work in Ada ID", "Work in Jackson ID", "Work in Other","Malheur Resident Workers","Live in Payette, ID", "Live in Canyon, ID", "Live in Washington, ID", "Live in Ada, ID", "Live in Other", "Workers in Malheur")
  measure = c("absolute", "relative","relative","relative","relative","relative","total","relative","relative","relative","relative","relative","total")
  data = data.frame(x=factor(x,levels=x),measure,text,y)
  fig <- plot_ly(data,x=~x,y=~y,measure=~measure, text =~text, type = "waterfall", textposition= "outside",decreasing = list(marker = list(color = "Maroon", line = list(color = "red", width = 2))),
                 increasing = (marker = list(color = "Teal")),
                 totals = list(marker = list(color = "deep sky blue", line = list(color = 'blue', width = 3))))
  fig <- fig %>% plotly::layout(title = "Flow of Workers in Malheur", xaxis = list(title = "Locations"),yaxis = list(title="Number of people",range = c(0,15000)))
  fig
}
drawFlowPayette <- function(){
  y =c(9390, -2854,-1399,-1078,-222,-1043,2794,851,680,518,256,766,0)
  text = c("","-30.4%","-14.8%","-11.5%", "2.4%","11.1%","","14.5%","11.6%","8.8%","4.4%","13.1%","")
  x = list("Payette Residents", "Work in Malheur OR", "Work in Ada ID", "Work in Canyon ID", "Work in Washington ID", "Work in Other","Payette Resident Workers","Live in Canyon, ID", "Live in Malheur, OR", "Live in Ada, ID", "Live in Washing, ID", "Live in Other", "Workers in Payette")
  measure = c("absolute", "relative","relative","relative","relative","relative","total","relative","relative","relative","relative","relative","total")
  data = data.frame(x=factor(x,levels=x),measure,text,y)
  fig <- plot_ly(data,x=~x,y=~y,measure=~measure, type = "waterfall", text = ~text, textposition= "outside",decreasing = list(marker = list(color = "Maroon", line = list(color = "red", width = 2))),
                 increasing = (marker = list(color = "Teal")),
                 totals = list(marker = list(color = "deep sky blue", line = list(color = 'blue', width = 3))))
  fig <- fig %>% plotly::layout(title = "Flow of Workers in Payette", xaxis = list(title = "Locations"),yaxis = list(title="Number of people",range = c(0,15000)))
  fig
}