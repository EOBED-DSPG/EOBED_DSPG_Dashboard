# getLicensureWageData <- function(name){
#   df <- read.csv(file="Licensure/Wages and table info.csv")
#   df[df=="na"]<-0
#   return(df)
# }
# drawLicensureWageOregon <- function(df, name){
#   temp <- df
#   temp =  temp[temp[,'Career'] == name,]
#   fig1 <- plot_ly(
#     x = 2006:2019,
#     y= as.numeric(as.vector(temp[1,2:15])),
#     type = "bar",
#     orientation="v",
#   )
#   fig1 %>% plotly::layout(showlegend= FALSE, title = "Oregon Wage")
#   fig1
# }
# drawLicensureWageIdaho <- function(df, name){
#   temp <- df
#   temp =  temp[temp[,'Career'] == name,]
#   fig2 <- plot_ly(
#     x = 2006:2019,
#     y = as.numeric(as.vector(temp[1,16:29])),
#     type = "bar",
#     orientation = "v",
#   )
#   fig2 %>% plotly::layout(showlegend = FALSE, title = "Idaho Wage")
#   fig2
# }
# getLicensureWageNames <- function(){
#   df <- read.csv(file="Licensure/Wages and table info.csv")
#   return(df[,1])
# }
# getLicensureReqData <- function(){
#   df <- read.csv(file="Licensure/Licensure_Table_2020.csv")
#   df <- df[2:24,]
#   return(df)
# }
###########QOL######################
source('State of Economic Wellbeing and Determinants of Economic Opportunity/econStateGraphics.R')
drawLocalState <- function(){
  createEconStateMap()
}

source('Healthcare Outcomes and Quality/healthQualGraphics.R')
drawHealthState <- function(){
  drawhealthQualMap()
}
source('Determinants of Health/healthDeterGraphics.R')
drawHealthDeterminants <- function(){
  drawhealthDeterMap()
}
source('Determinants of Economic Opportunity/econOpDeterGraphics.R')
drawLocalDeterminants <- function(){
  drawEconOpDeterMap()
}
source("Education/Collin's_Education_Data/ccReadyMap.R")
drawEducationDeterminants <- function(edShape, shape){
  
  # City mappings for markers
  cityLng <- c(-117.2382,-116.9949,-116.9165)
  cityLat <- c(43.9821,43.8768,44.0077)
  cityNames <- c("Vale","Nyssa","Fruitland")
  
  cityLng2 <- c(-116.9629)
  cityLat2 <- c(44.0266)
  cityNames2 <- c("Ontario")
  
  
  ccr <- read.csv("Education/Collin's_Education_Data//2015_District_Data_CCReady.csv", header = T)
  ccr$INDEX_Overall <- as.numeric(ccr$INDEX_Overall)
  
  myDistricts <- edShape
  
  myCounts <- shape
  ccr$GEOID <- as.character(ccr$GEOID)
  
  ccr_merge <- geo_join(myDistricts, ccr, "GEOID","GEOID")
  ccr_merge$INDEX_Overall <- as.integer(ccr_merge$INDEX_Overall)
  
  myDomain <- as.integer(c(1,2,3,4,5))
  
  pal <- colorBin(
    palette = c("springgreen4","palegreen","yellow","orange","red"),
    domain = myDomain,
    bins = 5,
    pretty = F
  )
  
  stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)
  
  # Custom Marker Icon
  circle_black <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                           iconWidth = 12, iconHeight = 12)
  
  ccrMap <- leaflet() %>%
    
    addProviderTiles("CartoDB.DarkMatter") %>%
    
    addCCRShapes(df = ccr_merge, fOp = 1, fc = ~pal(INDEX_Overall)) %>%
    
    # Adds the state borders inside your map, put as the last added Polygon
    # so it is drawn on top of the other shapes. You can play with the weight
    # to change the border thickness and the color to change border color.
    addPolylines(data = stateM,
                 color = "#000000",                     # Black border line(must be hex)
                 fillOpacity = 0,                       # Make tracts opaque to differentiate
                 opacity = 1,                           # Make tract borders opaque
                 weight = 3,                            # Thickness of borders
                 smoothFactor = 0) %>%
    
    # Adds the county borders inside your map, put as the last added Polygon
    # so it is drawn on top of the other shapes. You can play with the weight
    # to change the border thickness and the color to change border color.
    addPolylines(data = myCounts,
                 color = "#000000",                        # Gray border line(must be hex)
                 fillOpacity = 0,                       # Make tracts opaque to differentiate
                 opacity = 1,                           # Make tract borders opaque
                 weight = 1,                            # Thickness of borders
                 smoothFactor = 0) %>%
    
    # Nyssa, Vale, Boise city markers
    addMarkers(lng=cityLng,
               lat=cityLat,
               label=cityNames,
               icon = circle_black,
               labelOptions = labelOptions(noHide = T,
                                           textsize = "12px",
                                           direction = "bottom")) %>%
    
    # Ontario city marker
    addMarkers(lng=cityLng2[1],
               lat=cityLat2[1],
               label=cityNames2[1],
               icon = circle_black,
               labelOptions = labelOptions(noHide = T,
                                           textsize = "12px",
                                           direction = "top")) %>%
    
    # Legend 
    addLegend(values = myDomain,                        # Same domain for legend
              opacity = 1,                              # Make legend opaque
              colors = c("green","palegreen","yellow","orange","red"),
              position = "bottomright",                 # Legend at bottom right
              title = "College Career Readiness Index (2015)", # Legend Title
              na.label = "No Data",
              labels = c(1,2,3,4,5))
  return(ccrMap)
}
source('Education/Collin\'s_Education_Data/edDeterMap.R')
drawEducationOpportunity <- function(edShape, shape){
  
  # City mappings for markers
  cityLng <- c(-117.2382,-116.9949,-116.9165)
  cityLat <- c(43.9821,43.8768,44.0077)
  cityNames <- c("Vale","Nyssa","Fruitland")

  cityLng2 <- c(-116.9629)
  cityLat2 <- c(44.0266)
  cityNames2 <- c("Ontario")


  edDeter <- read.csv("Education/Collin's_Education_Data/2015_District_Data_EducationDeterminants.csv", header = T)
  edDeter$INDEX_Overall <- as.numeric(edDeter$INDEX_Overall)

  myDistricts <- edShape

  myCounts <- shape
  edDeter$GEOID <- as.character(edDeter$GEOID)

  edDeter_merge <- geo_join(myDistricts, edDeter, "GEOID","GEOID")
  edDeter_merge$INDEX_Overall <- as.integer(edDeter_merge$INDEX_Overall)

  myDomain <- as.integer(c(1,2,3,4,5))

  pal <- colorBin(
     palette = c("springgreen4","palegreen","yellow","orange","red"),
     domain = myDomain,
     bins = 5,
     pretty = F
  )

  stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)

  # Custom Marker Icon
  circle_black <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                           iconWidth = 12, iconHeight = 12)

  groups <- c("Overall Quality Index", "Teacher to Student Ratio Index", "Counselor to Student Ratio Index",
              "Share of Experienced Teachers Index", "Teacher Salaray (as share of students) Index", "Number of Title I Schools Index")

  edDeterMap <- leaflet() %>%

     addProviderTiles("CartoDB.DarkMatter") %>%

     addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[1], fc = ~pal(INDEX_Overall), edDeter_merge$INDEX_Overall) %>%
     addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[2], fc = ~pal(INDEX_STR), edDeter_merge$INDEX_STR) %>%
     addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[3], fc = ~pal(INDEX_SCR), edDeter_merge$INDEX_SCR) %>%
     addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[4], fc = ~pal(INDEX_Experience), edDeter_merge$INDEX_Experience) %>%
     addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[5], fc = ~pal(INDEX_TeacherSalary), edDeter_merge$INDEX_TeacherSalary) %>%
     addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[6], fc = ~pal(INDEX_Title1), edDeter_merge$INDEX_Title1) %>%


     # Adds the state borders inside your map, put as the last added Polygon
     # so it is drawn on top of the other shapes. You can play with the weight
     # to change the border thickness and the color to change border color.
     addPolylines(data = stateM,
                  color = "#000000",                     # Black border line(must be hex)
                  fillOpacity = 0,                       # Make tracts opaque to differentiate
                  opacity = 1,                           # Make tract borders opaque
                  weight = 3,                            # Thickness of borders
                  smoothFactor = 0) %>%

     # Adds the county borders inside your map, put as the last added Polygon
     # so it is drawn on top of the other shapes. You can play with the weight
     # to change the border thickness and the color to change border color.
     addPolylines(data = myCounts,
                  color = "#000000",                        # Gray border line(must be hex)
                  fillOpacity = 0,                       # Make tracts opaque to differentiate
                  opacity = 1,                           # Make tract borders opaque
                  weight = 1,                            # Thickness of borders
                  smoothFactor = 0) %>%

     # Nyssa, Vale, Boise city markers
     addMarkers(lng=cityLng,
                lat=cityLat,
                label=cityNames,
                icon = circle_black,
                labelOptions = labelOptions(noHide = T,
                                            textsize = "12px",
                                            direction = "bottom")) %>%

     # Ontario city marker
     addMarkers(lng=cityLng2[1],
                lat=cityLat2[1],
                label=cityNames2[1],
                icon = circle_black,
                labelOptions = labelOptions(noHide = T,
                                            textsize = "12px",
                                            direction = "top")) %>%

     # Legend
     addLegend(values = myDomain,                        # Same domain for legend
               opacity = 1,                              # Make legend opaque
               colors = c("green","palegreen","yellow","orange","red"),
               position = "bottomright",                 # Legend at bottom right
               title = "Education Determinant Quality Index (2015)", # Legend Title
               na.label = "No Data",
               labels = c(1,2,3,4,5)) %>%

     addLayersControl(baseGroups = groups,
                      position = "topleft", options = layersControlOptions(collapsed = F))


  edDeterMap
}
source("Education/Collin's_Education_Data/edOutcomeMap.R")
drawEducationOutcomes <- function(edShape, shape){
  
  # City mappings for markers
  cityLng <- c(-117.2382,-116.9949,-116.9165)
  cityLat <- c(43.9821,43.8768,44.0077)
  cityNames <- c("Vale","Nyssa","Fruitland")
  
  cityLng2 <- c(-116.9629)
  cityLat2 <- c(44.0266)
  cityNames2 <- c("Ontario")
  
  
  edOut <- read.csv("Education/Collin's_Education_Data/2015_District_Data_EducationOutcomes.csv", header = T)
  edOut$INDEX_Overall <- as.numeric(edOut$INDEX_Overall)
  
  myDistricts <- edShape
  
  myCounts <- shape
  edOut$GEOID <- as.character(edOut$GEOID)
  
  edOut_merge <- geo_join(myDistricts, edOut, "GEOID","GEOID")
  edOut_merge$INDEX_Overall <- as.integer(edOut_merge$INDEX_Overall)
  
  myDomain <- as.integer(c(1,2,3,4,5))
  
  pal <- colorBin(
    palette = c("springgreen4","palegreen","yellow","orange","red"),
    domain = myDomain,
    bins = 5,
    pretty = F
  )
  
  stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)
  
  # Custom Marker Icon
  circle_black <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                           iconWidth = 12, iconHeight = 12)
  
  groups <- c("Overall Education Outcomes Index", "Attendance Index", "In-School Suspension Index",
              "Out-of_school Suspension Index", "Expulsion Index","Cohort Graduation Rate Index")
  
  edOutMap <- leaflet() %>%
    
    addProviderTiles("CartoDB.DarkMatter") %>%
    
    addEdOutShapes(df = edOut_merge, fOp = 1, group = groups[1], fc = ~pal(INDEX_Overall), edOut_merge$INDEX_Overall) %>%
    addEdOutShapes(df = edOut_merge, fOp = 1, group = groups[2], fc = ~pal(INDEX_Attendance), edOut_merge$INDEX_Attendance) %>%
    addEdOutShapes(df = edOut_merge, fOp = 1, group = groups[3], fc = ~pal(INDEX_ISS), edOut_merge$INDEX_ISS) %>%
    addEdOutShapes(df = edOut_merge, fOp = 1, group = groups[4], fc = ~pal(INDEX_OOS), edOut_merge$INDEX_OOS) %>%
    addEdOutShapes(df = edOut_merge, fOp = 1, group = groups[5], fc = ~pal(INDEX_Expulsion), edOut_merge$INDEX_Expulsion) %>%
    addEdOutShapes(df = edOut_merge, fOp = 1, group = groups[6], fc = ~pal(INDEX_Grad), edOut_merge$INDEX_Grad) %>%
    
    
    # Adds the state borders inside your map, put as the last added Polygon
    # so it is drawn on top of the other shapes. You can play with the weight
    # to change the border thickness and the color to change border color.
    addPolylines(data = stateM,
                 color = "#000000",                     # Black border line(must be hex)
                 fillOpacity = 0,                       # Make tracts opaque to differentiate
                 opacity = 1,                           # Make tract borders opaque
                 weight = 3,                            # Thickness of borders
                 smoothFactor = 0) %>%
    
    # Adds the county borders inside your map, put as the last added Polygon
    # so it is drawn on top of the other shapes. You can play with the weight
    # to change the border thickness and the color to change border color.
    addPolylines(data = myCounts,
                 color = "#000000",                        # Gray border line(must be hex)
                 fillOpacity = 0,                       # Make tracts opaque to differentiate
                 opacity = 1,                           # Make tract borders opaque
                 weight = 1,                            # Thickness of borders
                 smoothFactor = 0) %>%
    
    # Nyssa, Vale, Boise city markers
    addMarkers(lng=cityLng,
               lat=cityLat,
               label=cityNames,
               icon = circle_black,
               labelOptions = labelOptions(noHide = T,
                                           textsize = "12px",
                                           direction = "bottom")) %>%
    
    # Ontario city marker
    addMarkers(lng=cityLng2[1],
               lat=cityLat2[1],
               label=cityNames2[1],
               icon = circle_black,
               labelOptions = labelOptions(noHide = T,
                                           textsize = "12px",
                                           direction = "top")) %>%
    
    # Legend 
    addLegend(values = myDomain,                        # Same domain for legend
              opacity = 1,                              # Make legend opaque
              colors = c("green","palegreen","yellow","orange","red"),
              position = "bottomright",                 # Legend at bottom right
              title = "Education Outcomes Index (2015)", # Legend Title
              na.label = "No Data",
              labels = c(1,2,3,4,5)) %>%
    
    addLayersControl(baseGroups = groups, 
                     position = "topleft", options = layersControlOptions(collapsed = F))
  
  
  edOutMap
}
getEduOpp <- function(){
  df <- read.csv(file="State of Economic Wellbeing and Determinants of Economic Opportunity/2018_Tract_Data_StateofEconomicOpp.csv")
  df = subset(df,select=c(County,Tract.Name,Ranking.Overall,Quintile,Commute.Time,X..Employed,X..Below.Poverty, Median.Hours.Worked,X..Living.in.Poverty.Receiving.SNAP))
  df=df[2:47,]
  row.names(df)<-NULL
  names(df)[2] <- 'Tract'
  names(df)[3] <- 'Rank'
  names(df)[5] <- 'Commute Time'
  names(df)[6] <- '% Employed'
  names(df)[7] <- '% Below Poverty'
  names(df)[8] <- 'Median Hrs Worked'
  names(df)[9] <- '% in Poverty + SNAP'
  
  
  return(df)
}
getLocalDeterminantsTable <- function(){
  df <- read.csv(file="Determinants of Economic Opportunity/2018_Tract_Data_EconomicOpp.csv")
  df = subset(df,select=c(County,Tract.Name,Quintile,Ranking.Overall,X..of.Income.on.Housing,Access.to.Broadband.Subscription,X..Linguistically.Isolated, X..Population.With.Associate.s.Degree.of.Higher))
  df = df[2:47,]
  row.names(df) <- NULL
  names(df)[2] <- 'Tract'
  names(df)[4] <- 'Rank'
  names(df)[5] <- '% Income on Housing'
  names(df)[6] <- 'Broadband Access'
  names(df)[7] <- '% Lingusitic Isolated'
  names(df)[8] <- '% Population with Associate\'s Degree+'
  return(df)
}

getHealthDeterminantsTable <- function(){
  df <- read.csv(file="Determinants of Health/2018_Tract_Data_DeterminantsHealth.csv")
  df = subset(df,select=c(County,Census.Tract,Ranking.Overall,Quintile,Food.Desert.,X..Insured))
  df=df[2:47,]
  row.names(df)<-NULL
  names(df)[2] <- 'Tract'
  names(df)[3] <- 'Rank'
  names(df)[5] <- 'Is Food Desert'
  names(df)[6] <- '% Insured'

  return(df)
}

getHealthStateTable <- function(){
  df <- read.csv(file="Healthcare Outcomes and Quality/2018_Tract_Data_HealthQual.csv")
  df = subset(df,select=c(County,Census.Tract,Ranking.Overall,Quintile,X30.Day.Hospital.Wide.Readmission.Rate,Cleanliness...star.rating.1,Nurse.communication...star.rating.1,Doctor.communication...star.rating.1,Staff.responsiveness...star.rating.1,Communication.about.medicines...star.rating.1,Discharge.information...star.rating.1,Care.transition...star.rating.1,Overall.hospital.rating...star.rating.1,Quietness...star.rating.1,Recommend.hospital...star.rating.1,Life.Expectancy,Infant.Mortality.Rate))
  row.names(df)<-NULL
  names(df)[2] <- 'Tract'
  names(df)[3] <- 'Rank'
  names(df)[5] <- '30 Day Readmission Rate'
  names(df)[6] <- 'Cleanliness (star ct.)'
  names(df)[7] <- 'Nurse Communication (star ct.)'
  names(df)[8] <- 'Doctor communication (star ct.)'
  names(df)[9] <- 'Staff Responsiveness (star ct.)'
  names(df)[10] <- 'Medication Communication (star ct.)'
  names(df)[11] <- 'Discharge Info (star ct).'
  names(df)[12] <- 'Care Transition (star ct.)'
  names(df)[13] <- 'Hospital Rating (star ct.)'
  names(df)[14] <- 'Quietness (star ct.)'
  names(df)[15] <- 'Recommend Hospital (star ct.)'
  names(df)[16] <- 'Life Expectancy'
  names(df)[17] <- 'Infant Mortality Rate'
  
  return(df)
}

getEducationOutcomesTable <- function(){
  df <- read.csv(file="Education/2015_District_Data_EducationOutcomes.csv")
  df = subset(df,select=c(School.District,County,Rank.Overall,Quintile,Absenteeism.Rate,In.school.Suspension.Rate....,Out.of.school.Suspension.Rate....,Expulsion.Rate....,X4.year.graduation.rate))
  row.names(df)<-NULL
  names(df)[1] <- 'School District'
  names(df)[3] <- 'Rank'
  names(df)[5] <- 'Absenteeism Rate'
  names(df)[6] <- 'In-School Suspension (%)'
  names(df)[7] <- 'Out-of-School Suspension (%)'
  names(df)[8] <- 'Expulsion Rate (%)'
  names(df)[9] <- '4-Year Graduation Rate (%)'

  
  return(df)
}
getEducationDeterminantsTable <- function(){
  df <- read.csv(file="Education/2015_District_Data_CCReady.csv")
  df = subset(df,select=c(School.District,County,Overall.Ranking,Quintile,School.that.offers.AP.classes.,School.that.offers.gifted.and.talented.education.,School.that.offers.dual.enrollment.credit.recovery.,School.that.offers.IB.curriculum.,School.offers.Calculus.1.,School.offers.Chemistry., School.offers.Physics.))
  row.names(df)<-NULL
  names(df)[1] <- 'School District'
  names(df)[3] <- 'Rank'
  names(df)[5] <- 'AP Classes Offered'
  names(df)[6] <- 'Gifted and Talented Education Offered'
  names(df)[7] <- 'Dual Enrollment/Credit Recovery Offered'
  names(df)[8] <- 'IB Offered'
  names(df)[9] <- 'Calculus 1 Offered'
  names(df)[10] <- 'Chemistry Offered'
  names(df)[11] <- 'Physics Offered'
  
  
  return(df)
}
getEducationOpportunityTable <- function(){
  df <- read.csv(file="Education/2015_District_Data_EducationDeterminants.csv")
  df = subset(df,select=c(School.District,County,Rank.Overall,Quintile,Student.to.teacher.ratio,Student.to.Counselor.ratio,X..1st.and.2nd.Year.Teachers,Teacher.Salary..pupil..1,Number.of.Title.1.Schools))
  row.names(df)<-NULL
  names(df)[1] <- 'School District'
  names(df)[3] <- 'Rank'
  names(df)[5] <- 'Student/Teacher Ratio'
  names(df)[6] <- 'Student/Counselor Ratio'
  names(df)[7] <- '% of 1st and 2nd Year Teachers'
  names(df)[8] <- 'Teacher Salary (pupil)'
  names(df)[9] <- '# of Title 1 Schools'
  
  
  return(df)
}
readEducationShape <- function(){
  districts <- readOGR(dsn = "Education/Collin's_Education_Data/shapes", layer = "schooldistrict_sy1819_tl19_EORWID")
  myDistricts <- districts[districts$STATEFP=="16"|districts$STATEFP=="41",]
  myDistricts <- myDistricts[myDistricts$GEOID=="4101020"|myDistricts$GEOID=="4101350"|myDistricts$GEOID=="4101500"|myDistricts$GEOID=="1600510"|myDistricts$GEOID=="1600570"|
                               myDistricts$GEOID=="1600570"|myDistricts$GEOID=="1600152"|myDistricts$GEOID=="1601140"
                             |myDistricts$GEOID=="4106120"|myDistricts$GEOID=="1600161"|myDistricts$GEOID=="4106820"|myDistricts$GEOID=="4106960"|
                               myDistricts$GEOID=="1600158"|myDistricts$GEOID=="1600012"|myDistricts$GEOID=="1602070"|myDistricts$GEOID=="1602130"|
                               myDistricts$GEOID=="1602160"|myDistricts$GEOID=="1602340"|myDistricts$GEOID=="1602460"|
                               myDistricts$GEOID=="4109000"|myDistricts$GEOID=="4109270"|myDistricts$GEOID=="1602550"|
                               myDistricts$GEOID=="1602580"|myDistricts$GEOID=="4100014"|myDistricts$GEOID=="1600600"|
                               myDistricts$GEOID=="1600003"|myDistricts$GEOID=="1600141"|myDistricts$GEOID=="1603330"|myDistricts$GEOID=="1603480",]
  return(myDistricts)
}
getReactiveShape <- function(){
  myCounts <- counties(state = c("OR","ID"))
  print("reload mycounts")
  return(myCounts)
}

