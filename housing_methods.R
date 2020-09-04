
percentage <- function(x, nrow){
  return((x / nrow) * 100)
}
sumColumn <- function(x, y){
  return(x+y)
}
outerJoin <- function(df1, df2, col){
  df = merge(x=df1, y=df2, by=col, all = TRUE)
  df[is.na(df)] <- 0
  df$Sum <- mapply(sumColumn, df$Count.x, df$Count.y)
  return(df)
}
#Use user input to filter out different entries for Malhuer tax lot data
#Use malheur cleaned.csv when looking at ag beacuse there are no enteries in malheur2019
malheurFilter <- function(df, selection){
  if(selection == 2){
    df = df[(df[,'ZONE'] == 'O-RMH' | df[,"ZONE"] == 'O-RM' | df[,"ZONE"] == 'C-RR' | df[,"ZONE"] =='O-RD'| df[,"ZONE"] == 'O-RS' | df[,"ZONE"] == 'N-R1' | df[,"ZONE"] == 'N-R2' | df[,"ZONE"] == 'N-R3' | df[,"ZONE"] == 'N-R4' | df[,"ZONE"] == 'V-R1' | df[,"ZONE"] == 'V-R2'),]
  }
  if(selection ==3){
    df = df[(df[,'ZONE'] == 'C-CI' | df[,"ZONE"] == 'O-C1' | df[,"ZONE"] == 'O-C2' | df[,"ZONE"] =='O-C2H'| df[,"ZONE"] == 'O-C3' | df[,"ZONE"] == 'N-C' | df[,"ZONE"] == 'V-C' | df[,"ZONE"] == 'O-C2+'),]
  }
  if(selection == 4){
    names <- colnames(df)
    df <- data.frame(matrix(character(), nrow=0, ncol = length(names)))
    colnames(df)=names
  }
  return(df)
}

malheurCleanedFilter <- function(df, selection){
  df1 <- data.frame(matrix(character(),nrow = 0,ncol = length(colnames(df))))
  colnames(df1)=colnames(df)
  if(selection == 1 | selection == 4){
    df1 = df[(df[,'ZONE'] == 'C-A1' | df[,"ZONE"] == 'C-A2' | df[,"ZONE"] == 'C-A3' | df[,"ZONE"] =='C-I3'| df[,"ZONE"] == 'C-A2+'),]
  }
    return(df1)
}

#Use user input to filter out different entries for payette Tax lot data
payetteFilter <- function(df, selection){
  if(selection ==2){
    df = df[str_detect(df[,'PropClsDescr'], 'Res') == TRUE, ]
    
  }
  if(selection ==3){
    df = df[str_detect(df[,'PropClsDescr'], 'Comm') == TRUE, ]
    
  }
  if(selection==4){
    df = df[str_detect(df[,'PropClsDescr'], 'Ag') == TRUE, ]
  }
  return(df)
}

#get tax lot data for Payette and clean
getHousingVSPayette <- function(){
  yrbuilt = read.csv("Housing Data/PAYCO_IMPV.csv")
  yrbuilt = yrbuilt[yrbuilt[, 'YrBlt'] > 1949,]
  yrbuilt = yrbuilt[yrbuilt[, 'YrBlt'] < 2018,]
  return(yrbuilt)
}

#Count houses by year and calculate percentage distribution
drawHousingVSPayette <- function(df, n, selection){
  df_original = payetteFilter(df, selection)
  df_original = df_original %>% distinct(PIN, .keep_all=TRUE)
  df_original$Count <- 1:nrow(df_original)
  df_original = aggregate(Count~YrBlt, df_original, function(x) length(unique(x)))
  nrow = sum(df_original$Count)
  df_original$Percent <- mapply(percentage, df_original$Count, nrow)
  df_original = plyr::rename(df_original, c('YrBlt'='Year_Built'))
  fig <- df_original %>% plot_ly(x = ~Count, y=~Year_Built, color = ~Percent, type = "bar", orientation = "h")
  fig <- fig %>% plotly::layout(title = 'Payette Construction', yaxis = list(range=c(n[1],n[2])), xaxis = list(type='log',range=c(0,4)))
}
getHousingVSCleaned <- function(){
  df_clean = read.csv("Housing Data/MalhuerCleaned.csv")
  df_clean = df_clean[df_clean[,'YRBUILT'] > 1949,]
  df_clean <- df_clean[df_clean[,'YRBUILT'] < 2018,]
  return(df_clean)
}
addDataFrame <- function(df_original, df_clean){
  YRBUILT <- c(1950:2017)
  Count <- vector()
  for(i in YRBUILT){
    x=0
    y=0
    if(is.na(match(i,df_original$YRBUILT)) == FALSE){
      x = df_original$Count[match(i,df_original$YRBUILT)]
    }
    if(is.na(match(i, df_clean$YRBUILT)) == FALSE){
      y = df_clean$Count[match(i, df_clean$YRBUILT)]
    }
    Count <- append(Count, x+y)
  }
  df <- data.frame(YRBUILT, Count)
  return(df)
}
addZoneFrame <- function(df_original, df_clean, selection){
  ZONE = vector()
  if(selection ==1){
    ZONE = c('O-RMH','O-RM','C-RR','O-RD', 'O-RS', 'N-R1','N-R2','N-R3','N-R4','V-R1','V-R2','C-CI','O-C1','O-C2','O-C2H', 'O-C3','N-C','V-C','O-C2+','C-A1', 'C-A2', 'C-A3','C-I3', 'C-A2+','C-A1+')
  }
  else if(selection == 2){
    ZONE = c('O-RMH','O-RM','C-RR','O-RD', 'O-RS', 'N-R1','N-R2','N-R3','N-R4','V-R1','V-R2')
    
  }
  else if(selection ==3){
    ZONE = c('C-CI','O-C1','O-C2','O-C2H', 'O-C3','N-C','V-C','O-C2+', )

  }
  else if(selection == 4){
    ZONE = c('C-A1', 'C-A2', 'C-A3','C-I3', 'C-A2+','C-A1+')
    
  }
  Count <- vector()
  for(i in ZONE){
    x=0
    y=0
    if(is.na(match(i,df_original$ZONE)) == FALSE){
      x = df_original$Count[match(i,df_original$ZONE)]
    }
    if(is.na(match(i, df_clean$ZONE)) == FALSE){
      y = df_clean$Count[match(i, df_clean$ZONE)]
    }
    Count <- append(Count, x+y)
  }
  df <- data.frame(ZONE, Count)
  return(df)
}
#get tax lot data for Malheur and clean
getHousingVS <- function(){
  yrbuilt = read.csv("Housing Data/malheur2019.csv")
  yrbuilt = yrbuilt[yrbuilt[,'YRBUILT'] > 1949,]
  yrbuilt <- yrbuilt[yrbuilt[,'YRBUILT'] < 2018,]
  yrbuilt <- yrbuilt[yrbuilt[,'LOTSQFT'] != 0,]
  df = yrbuilt
  print("Reloaded Data")
  return(yrbuilt)
}

#Count houses by year and calculate Percentage Distribution
drawHousingVS <- function(df, df_clean, n, selection){
  df_clean =  malheurCleanedFilter(df_clean, selection)
  df_original = malheurFilter(df, selection)
  
  nrow_original = 0
  if(nrow(df_original) == 0){
    df_original = data.frame(matrix(numeric(), nrow =0, ncol = 2))
    colnames(df_original) = c('YRBUILT', 'Count')
  }
  else{
    df_original$Count <- 1:nrow(df_original)
    df_original = aggregate(Count~YRBUILT, df_original, function(x) length(unique(x)))
    nrow_original = sum(df_original$Count)
  }
  nrow_clean = 0
  if(nrow(df_clean)==0){
    df_clean = data.frame(matrix(numeric(), nrow =0, ncol = 2))
    colnames(df_clean) = c('YRBUILT', 'Count')
  }
  else{
    df_clean$Count <- 1:nrow(df_clean)
    df_clean = aggregate(Count~YRBUILT, df_clean, function(x) length(unique(x)))
    nrow_clean = sum(df_clean$Count)
  }
  
  nrow = nrow_original + nrow_clean
  df_original = outerJoin(df_original, df_clean, 'YRBUILT')
  df_original$Percent <- mapply(percentage, df_original$Sum, nrow)
  
  df_original = plyr::rename(df_original, c('YRBUILT'='Year_Built'))
  fig <- df_original %>% plot_ly(x = ~Sum, y=~Year_Built, color = ~Percent, type = "bar", orientation = "h")
  fig <- fig %>% plotly::layout(title = 'Malhuer Construction', yaxis = list(range=c(n[1],n[2])), xaxis = list(type='log',range=c(0,4)))
  
  return(fig)
}

drawHousingZoneAll <- function(df, df_clean, selection, type){
  #NOTE -- might want to change names
  
  #Count unique zone types and change factor levels to display in descending order
  df <- malheurFilter(df, selection)
  df_clean = malheurCleanedFilter(df_clean, selection)
  
  nrow = 0
  if(nrow(df) == 0){
    df = data.frame(matrix(numeric(), nrow =0, ncol = 2))
    colnames(df) = c('ZONE', 'Count')
  }
  else{
    df$Count <- 1:nrow(df)
    df = aggregate(Count ~ZONE, df, function(x) length(unique(x)))
    nrow = sum(df$Count)
  }
  nrow_clean =0
  if(nrow(df_clean)==0){
    df_clean = data.frame(matrix(numeric(), nrow =0, ncol = 2))
    colnames(df_clean) = c('ZONE', 'Count')
  }
  else{
    df_clean$Count <- 1:nrow(df_clean)
    df_clean = aggregate(Count~ZONE, df_clean, function(x) length(unique(x)))
    nrow_clean = sum(df_clean$Count)
  }
  
  nrow_original = nrow+nrow_clean
  df = outerJoin(df, df_clean,'ZONE')
  nrow_percentage = sum(df$Sum)
  df$Percent <- mapply(percentage, df$Sum, nrow_percentage)

  df = df[order(df$Sum, decreasing =TRUE),]
  rownames(df) <- NULL
  df$ZONE <- factor(df$ZONE, levels = unique(df$ZONE))
  if(selection ==1 ){
    df=head(df,15)
  }
  #plot graph
  fig <- NULL
  if(type==1){
    fig <- df %>% plot_ly(x = ~Sum, y=~ZONE, type = 'bar', orientation='h')
  }
  else{
    fig <- df %>% plot_ly(x = ~Percent, y=~ZONE, type = 'bar', orientation='h')
    fig <- fig %>% plotly::layout(xaxis = list(range=c(0,100)))
  }
  print(selection)
  return(fig)
  
}
getPaycoPin <- function(){
  df <- read.csv('Housing Data/PAYCO_PIN.csv')
  return(df)
}
drawHousingZonePayetteAll <- function(df, df_pin, selection, type){
  #Count unqiue zone types and change factor levels to display in descending order
  if(selection == 2){
    df <- df_pin
    df <- df[df[,'HouseType'] != '',]
    df$Count <- 1:nrow(df)
    df = aggregate(Count~HouseType, df, function(x) length(unique(x)))
  }
  else{
    df <- payetteFilter(df, selection)
    df$Count <- 1:nrow(df)
    df = aggregate(Count~PropClsDescr, df, function(x) length(unique(x)))
  }
  nrow = sum(df$Count)
  df$Percent <- mapply(percentage, df$Count, nrow)
  df = df[order(df$Count, decreasing = TRUE),]
  rownames(df) <- NULL
  if(selection==2){
    df$PropClsDescr <- factor(df$HouseType, levels = unique(df$HouseType))
  }
  else{
    df$PropClsDescr <- factor(df$PropClsDescr, levels = unique(df$PropClsDescr))
  }

  #plot graph
  fig <- NULL
  if(type==1){
    fig <- df %>% plot_ly(x = ~Count, y=~PropClsDescr, type = 'bar', orientation = 'h')

  }
  else{
    fig <- df %>% plot_ly(x = ~Percent, y=~PropClsDescr, type = 'bar', orientation = 'h')
    fig <- fig %>% plotly::layout(xaxis = list(range=c(0,100)))
    
  }
  return(fig)
}

drawMalheurHist <- function(df, df_clean, selection){
  df_clean =  malheurCleanedFilter(df_clean, selection)
  df_original = malheurFilter(df, selection)
  
  df_original = df_original[df_original[,'LOTSQFT'] <150000,]
  df_original <- df_original$LOTSQFT
  
  df_clean = df_clean[df_clean[,'SQUARE.FEET'] < 150000,]
  df_clean <- df_clean$SQUARE.FEET
  
  df <- append(df_original, df_clean)

  fig <- plot_ly(xaxis = list(range=c(0,150000)), nbinsx =20)
  fig <- fig %>% plotly::add_histogram(df, name = 'first')


  return(fig)
}
# drawPayetteHist <-function(){
#   
# }
source("Housing Data/Cost and affordability data/mortRN_cost_Data.R")
drawOwnershipNurse <- function(tract, Or, Id, Shape){
  cityLng <- c(-117.2382,-116.9949,-116.9165)
  cityLat <- c(43.9821,43.8768,44.0077)
  cityNames <- c("Vale","Nyssa","Fruitland")
  
  cityLng2 <- c(-116.9629)
  cityLat2 <- c(44.0266)
  cityNames2 <- c("Ontario")
  
  # Get Tract shape data for Malheur
  # OrTract <- tracts(state = "OR", county = 045)
  OrTract <- Or
  
  # Get Payette, Washington, and Canyon County Tract Shapes
  # IdTract <- tracts(state = "ID", county = c(27,75,87))
  IdTract <- Id
  
  # Get ID and OR county shape data
  # myCounts <- counties(state = c("OR","ID"))
  myCounts <- Shape
  
  # Combine local tract shape info
  myTracts <- rbind(IdTract, OrTract)
  
  # Aggregates the county data for drawing state lines.
  stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)

  groups <- c(2010:2018)
  groups <- as.character(groups)
  ###### Funcs ######
  
  # Set Global Domain to Normalize all maps
  myDomain <- 0:35
  
  # Set Global Palette to Normalize all maps
  pal <- colorNumeric(
    palette = c("green","yellow","red"),
    domain = myDomain,
    na.color = "red" # set na color
  )
  
  tract_merge <- getMortRNCostTracts(myTracts,tract)
  
  
  plotRnMortCost( tract_merge = tract_merge, groups = groups, pal = pal, myDomain = myDomain, cityLng = cityLng,
                 cityLat = cityLat, cityNames = cityNames, cityLng2 = cityLng2, cityLat2 = cityLat2, cityNames2 = cityNames2, stateM = stateM)
  # drawRnMortCostMap(tract, county)
}
source("Housing Data/Cost and affordability data/mortgage_cost_Data.R")
drawOwnershipHousehold <- function(tract, Or, Id, Shape){
  cityLng <- c(-117.2382,-116.9949,-116.9165)
  cityLat <- c(43.9821,43.8768,44.0077)
  cityNames <- c("Vale","Nyssa","Fruitland")
  
  cityLng2 <- c(-116.9629)
  cityLat2 <- c(44.0266)
  cityNames2 <- c("Ontario")
  
  # Get Tract shape data for Malheur
  # OrTract <- tracts(state = "OR", county = 045)
  OrTract <- Or
  # Get Payette, Washington, and Canyon County Tract Shapes
  # IdTract <- tracts(state = "ID", county = c(27,75,87))
  IdTract <- Id
  # Get ID and OR county shape data
  # myCounts <- counties(state = c("OR","ID"))
  myCounts <- Shape
  # Combine local tract shape info
  myTracts <- rbind(IdTract, OrTract)
  
  # Aggregates the county data for drawing state lines.
  stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)

  groups <- c(2010:2018)
  groups <- as.character(groups)
  ###### Funcs ######
  
  # Set Global Domain to Normalize all maps
  myDomain <- 0:35
  
  # Set Global Palette to Normalize all maps
  pal <- colorNumeric(
    palette = c("green","yellow","red"),
    domain = myDomain,
    na.color = "red" # set na color
  )
  
  tractMort_merge <- getMortCostTracts(myTracts,tract)
  
  
  plotHOCCost( tract_merge = tractMort_merge, groups = groups, pal = pal, myDomain = myDomain, cityLng = cityLng,
              cityLat = cityLat, cityNames = cityNames, cityLng2 = cityLng2, cityLat2 = cityLat2, cityNames2 = cityNames2, stateM = stateM)
}
source("Housing Data/Cost and affordability data/renterRN_cost_Data.R")
drawRenterNurse <- function(tract, Or, Id, Shape){
  cityLng <- c(-117.2382,-116.9949,-116.9165)
  cityLat <- c(43.9821,43.8768,44.0077)
  cityNames <- c("Vale","Nyssa","Fruitland")
  
  cityLng2 <- c(-116.9629)
  cityLat2 <- c(44.0266)
  cityNames2 <- c("Ontario")
  
  # Get Tract shape data for Malheur
  OrTract <- Or
  
  # Get Payette, Washington, and Canyon County Tract Shapes
  IdTract <- Id
  
  # Get ID and OR county shape data
  myCounts <- Shape
  
  # Combine local tract shape info
  myTracts <- rbind(IdTract, OrTract)
  
  # Aggregates the county data for drawing state lines.
  stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)

  groups <- c(2010:2018)
  groups <- as.character(groups)
  ###### Funcs ######
  
  # Set Global Domain to Normalize all maps
  myDomain <- 0:35
  
  # Set Global Palette to Normalize all maps
  pal <- colorNumeric(
    palette = c("green","yellow","red"),
    domain = myDomain,
    na.color = "red" # set na color
  )
  
  trRentRn_merge <- getRNCostTracts(myTracts, tract)
  
  
  plotRnRentCost( tract_merge = trRentRn_merge, groups = groups, pal = pal, myDomain = myDomain, cityLng = cityLng,
                 cityLat = cityLat, cityNames = cityNames, cityLng2 = cityLng2, cityLat2 = cityLat2, cityNames2 = cityNames2, stateM = stateM)
}
source("Housing Data/Homeownership/homeOwn.R")
drawHousingOwnership <- function(Or, Id, Shape){
  drawOwnerMap()
}
source("Housing Data/Homeownership/renterData.R")
drawRenterRate <- function(Or, Id, Shape){
  createRenterMap()
}
source("Housing Data/Cost and affordability data/renter_cost_Data.R")
drawRenterHousehold <- function(tract, Or, Id, Shape){
  cityLng <- c(-117.2382,-116.9949,-116.9165)
  cityLat <- c(43.9821,43.8768,44.0077)
  cityNames <- c("Vale","Nyssa","Fruitland")
  
  cityLng2 <- c(-116.9629)
  cityLat2 <- c(44.0266)
  cityNames2 <- c("Ontario")
  
  # Get Tract shape data for Malheur
  OrTract <- Or
  
  # Get Payette, Washington, and Canyon County Tract Shapes
  IdTract <- Id
  
  # Get ID and OR county shape data
  myCounts <- Shape
  
  # Combine local tract shape info
  myTracts <- rbind(IdTract, OrTract)
  
  # Aggregates the county data for drawing state lines.
  stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)
  
  groups <- c(2010:2018)
  groups <- as.character(groups)
  ###### Funcs ######
  
  # Set Global Domain to Normalize all maps
  myDomain <- 0:35
  
  # Set Global Palette to Normalize all maps
  pal <- colorNumeric(
    palette = c("green","yellow","red"),
    domain = myDomain,
    na.color = "red" # set na color
  )
  
  trRentCost_merge <- getRentCostTracts(myTracts,tract)
  
  plotRentCost( tract_merge = trRentCost_merge, groups = groups, pal = pal, myDomain = myDomain, cityLng = cityLng,
               cityLat = cityLat, cityNames = cityNames, cityLng2 = cityLng2, cityLat2 = cityLat2, cityNames2 = cityNames2, stateM = stateM)
}

getReactiveTract <- function(){
  files <- list.files(path="./Housing Data/Cost and affordability data",pattern="*Tract_Data_Housing.csv",full.names=T)
  ald <- sapply(files, read.csv, skip = 1, header = T, simplify = F)
  print("reload tract")
  return(ald)
}

getReactiveOregon <- function(){
  OrTract <- tracts(state = "OR", county = 045)
  print("reload or")
  return(OrTract)
}
getReactiveIdaho <- function(){
  IdTract <- tracts(state = "ID", county = c(27,75,87))
  print("reload id")
  return(IdTract)
}
getReactiveShape <- function(){
  myCounts <- counties(state = c("OR","ID"))
  print("reload mycounts")
  return(myCounts)
}

