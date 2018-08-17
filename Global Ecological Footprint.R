
 rm(list=ls(all=TRUE)) 

## download packages and library

# install.packages("lubridate")
# install.packages('tidyverse') 
# install.packages("leaflet", dependencies = TRUE)
# install.packages('knitr', dependencies = TRUE)
# install.packages("rmarkdown")
# install.packages("countrycode")
# install.packages("viridisLite")
# library(stats)
# library(scales)
# library(readr)
# library(stringr)
# install.packages("purrr")
# install.packages("maps")
# install.packages("treemap")
# install.packages("Amelia")
library(leaflet)
library(ggplot2)
library (dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(magrittr)
library(tidyverse)
library(manhattanly)
library(randomForest)
library(missForest)
library(mice)
library(VIM)
library(margins)
library(sunburstR)
library(ggcorrplot)
library(rpart)
library(forcats)
library(qqman)
library(MASS)  
library(glmnet)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(foreach)
library(purrr)
library(zoo)
library (doParallel)
library(highcharter)
library(DT)
library(viridisLite)
library(countrycode)
library(xts)
library(maps)
library(maptools)
library(treemap)
library(igraph)
library(Amelia)
library(gridExtra)


# _____________Importation des Data _____________________________________________#

setwd("C:/Users/Admin/Desktop/Projet Dany")
countries <- fread("./countries.csv")
Coun <-fread("./Country.csv")

#_____________Visualisation du datasets_____________________________________#

glimpse(countries)
summary(countries)

##________________________ recodage des Vars ____________##

# Codage des names

gsub(" ","_",colnames(countries))
names(countries)<-gsub(" ","_",colnames(countries))
names(countries)[3]<-"Population"
str(countries)    

# codage de la variables PIB

countries$GDP_per_Capita <- chartr(countries$GDP_per_Capita, old = "$", new = " ")
countries$GDP_per_Capita = gsub(",","",as.character(countries$GDP_per_Capita))
countries$GDP_per_Capita = gsub(" ","",as.character(countries$GDP_per_Capita))

countries <- countries %>% mutate(GDP_per_Capita=as.double(as.character(GDP_per_Capita)))

##########___________Traitement des NA & Data Selection____________######### 

# Any NA values?

anyNA(countries)

# How many rows contain NA values?

nrow(countries[rowSums(is.na(countries)) > 0,])

# Which columns contain NA values?

colnames(countries)[colSums(is.na(countries)) > 0]

# Visualisation des valeurs abérantes pour les quatres variables 
# la Variable GPD :
hcboxplot(x = countries$GDP_per_Capita) %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "GDP per Capita") %>%
  hc_subtitle(text = "visualization of The Outliers") %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
                href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
                style = list(fontSize = "12px"))

# par région:

hcboxplot(x = countries$GDP_per_Capita, var= countries$Region) %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "GDP per Capita per Region") %>%
  hc_subtitle(text = "visualization of The Outliers") %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))

# la variable Biocapacity

hcboxplot(x = countries$Biocapacity_Deficit_or_Reserve) %>% 
  hc_chart(type = "column")    %>% #to put box vertical
  hc_title(text = "Biocapacity Deficit or Reserve ") %>%
  hc_subtitle(text = "visualization of The Outliers") %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))

# par région:
hcboxplot(x = countries$Biocapacity_Deficit_or_Reserve, var= countries$Region) %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Biocapacity Deficit or Reserve per Region") %>%
  hc_subtitle(text = "visualization of The Outliers") %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))

# la variable HDI :

hcboxplot(x = countries$HDI) %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "HDI") %>%
  hc_subtitle(text = "visualization of The Outliers") %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))

# HDI par region :
hcboxplot(x = countries$HDI, var= countries$Region) %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "HDI per Region") %>%
  hc_subtitle(text = "visualization of The Outliers") %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))


# la variable Population :

hcboxplot(x = countries$Population) %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Population") %>%
  hc_subtitle(text = "visualization of The Outliers") %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))

#  par région :
hcboxplot(x = countries$Population, var= countries$Region) %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Population per Region") %>%
  hc_subtitle(text = "visualization of The Outliers") %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))

# ____________ ETUDES DES CORRELATION ENTRE LES VARIABLES ___________

### Matrix of linear correlations  :

hchart.cor <- function(object, ...) {
  
  df <- as.data.frame(object)
  is.num <- sapply(df, is.numeric)
  df[is.num] <- lapply(df[is.num], round, 2)
  dist <- NULL
  
  x <- y <- names(df)
  
  df <- tbl_df(cbind(x = y, df)) %>% 
    gather(y, dist, -x) %>% 
    mutate(x = as.character(x),
           y = as.character(y)) %>% 
    left_join(data_frame(x = y,
                         xid = seq(length(y)) - 1), by = "x") %>% 
    left_join(data_frame(y = y,
                         yid = seq(length(y)) - 1), by = "y")
  
  ds <- df %>% 
    select_("xid", "yid", "dist") %>% 
    list.parse2()
  
  fntltp <- JS("function(){
               return this.series.xAxis.categories[this.point.x] + ' ~ ' +
               this.series.yAxis.categories[this.point.y] + ': <b>' +
               Highcharts.numberFormat(this.point.value, 2)+'</b>';
               ; }")
  cor_colr <- list( list(0, '#FF5733'),
                    list(0.5, '#F8F5F5'),
                    list(1, '#2E86C1')
  )
  highchart() %>% 
    hc_chart(type = "heatmap") %>% 
    hc_xAxis(categories = y, title = NULL) %>% 
    hc_yAxis(categories = y, title = NULL) %>% 
    hc_add_series(data = ds) %>% 
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)
      )) %>% 
    hc_tooltip(formatter = fntltp) %>% 
    hc_legend(align = "right", layout = "vertical",
              margin = 0, verticalAlign = "top",
              y = 25, symbolHeight = 280) %>% 
    hc_colorAxis(  stops= cor_colr,min=-1,max=1)
  }

# sélection des valeurs numérique & calcule de la matrice de correlation 

countries_num<- countries %>% select_if(is.numeric) %>% na.omit()

x <-  countries_num  %>% cor()

hchart.cor(x) %>% 
  hc_title(text = "Matrix of Correlation") %>%
  hc_subtitle(text = "visualization of The Values") %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))

#########_________________ Visualization DataSet _____________________________########

########________________ Biocapacity_Deficit_or_Reserve vs Country___________#######

# Vision Map :

countries$iso3 <- countrycode::codelist_panel[match(countries$Country, codelist_panel$country.name), "iso3c"]

data(worldgeojson, package = "highcharter")

dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)),
                        c = substring(viridis(5 + 1, option = "D"), 0, 7)) %>%  list_parse2()

highchart() %>% 
  hc_add_series_map(worldgeojson, countries, value = "Biocapacity_Deficit_or_Reserve", joinBy = "iso3",
                    dataLabels = list(enabled = TRUE, format = '{point.name}'), name = "Country") %>% 
                   
  hc_colorAxis(stops = dshmstops) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_db()) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Biocapacity Deficit or Reserve 2016 ") %>%
  hc_subtitle(text = "visualization per Country") %>% 
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: Exploring Ecological Footprint and Biocapacity (Kaggle)", style = list(fontSize = "10px")) %>% 
  hc_tooltip(useHTML = TRUE,
             headerFormat = "<table>",
             pointFormat = paste("<tr><th>Country <td>{point.Country}</td></th></tr>",
                                 "<tr><th>Region </th><td>{point.Region}</td></tr>",
                                 "<tr><th>Population(M)<td>{point.Population}</td></th></tr>",
                                 "<tr><th>HDI </th><td>{point.HDI}</td></tr>",
                                 "<tr><th>GDP_per_Capita </th><td>{point.GDP_per_Capita}</td></tr>",
                                 "<tr><th>Total Ecological Footprint </th><td>{point.Total_Ecological_Footprint}</td></tr>",
                                 "<tr><th>Total Biocapacity </th><td>{point.Total_Biocapacity}</td></tr>",
                                 "<tr><th>Biocapacity </th><td>{point.Biocapacity_Deficit_or_Reserve}</td></tr>"),
             footerFormat = "</table>")

# vision Globale Biocapacity_Deficit_or_Reserve en chiffre et en pourcentage :

Vis_Glo<-countries %>%  arrange(desc(Biocapacity_Deficit_or_Reserve)) %>% 
  mutate(percent=signif(Biocapacity_Deficit_or_Reserve/sum(Biocapacity_Deficit_or_Reserve)*100))


highchart() %>% 
  hc_title(text = "Biocapacity Deficit or Reserve 2016 ") %>%
  hc_subtitle(text = "Global Vision In Number & Percentage ") %>% 
  hc_add_series(Vis_Glo, "column", hcaes(x = Country, y = Biocapacity_Deficit_or_Reserve), name = "Country", colorByPoint = TRUE) %>%
  hc_add_series(Vis_Glo, "pie", hcaes(name = Country, y = percent), name = "Percent %",
                size = 100, dataLabels = list(enabled = TRUE),Format = "{value}%") %>% 
  hc_yAxis(title = list(text = "Biocapacity In Number"),
           labels = list(format = "{value}")) %>% 
  hc_xAxis(categories = Vis_Glo$Country) %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px")) %>% 
  hc_legend(enabled = TRUE) %>%  
  hc_navigator(enabled = TRUE) %>% 
  hc_scrollbar(enabled = TRUE)


#  20 TOPS & FLOPS Country per Biocapacity_Deficit_or_Reserve grouped by Region
# les 20 TOP:

by_countries <- countries %>% arrange(desc(Biocapacity_Deficit_or_Reserve)) %>% head(20) 


t <- tooltip_table(c("Region :","Country :", "Total Ecological Footprint :","Total Biocapacity" ,"Biocapacity(Reserve/Deficit):"), 
                   c("{point.Region}","{point.Country}", "{point.Total_Ecological_Footprint}", "{point.Total_Biocapacity}","{point.Biocapacity_Deficit_or_Reserve}")) 

hchart(by_countries, type="bar", hcaes(x = Country, y = Biocapacity_Deficit_or_Reserve, group = Region)) %>%
  hc_yAxis(title = list(text = "Biocapacity In Number")) %>% 
  hc_title(text = "Top 20 Countries Biocapacity Reserve 2016") %>% 
  hc_subtitle(text = "Grouped By Region ") %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = t) %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))

# The 20 FLOP:

by_countries1 <- countries %>% arrange(Biocapacity_Deficit_or_Reserve) %>% head(20)

hchart(by_countries1, type="bar", hcaes(x = Country, y = Biocapacity_Deficit_or_Reserve, group = Region)) %>%
  hc_yAxis(title = list(text = "Biocapacity In Number")) %>%
  hc_title(text = "Flop 20 Countries Biocapacity Deficit 2016") %>% 
  hc_subtitle(text = "Grouped By Region ") %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = t) %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))
 
# Moyenne Bio_Deficit_Reserve par Région 

by_Region <- countries %>% group_by(Region)%>%summarise_at(vars(Biocapacity_Deficit_or_Reserve),mean)

highchart()%>%
  hc_add_series(by_Region, "bar", hcaes(x = Region, y = Biocapacity_Deficit_or_Reserve, color = Region)) %>%
  hc_yAxis(title = list(text = " Average Biocapacity Per Region")) %>%
  hc_xAxis(categories = by_Region$Region) %>%
  hc_title(text = "Biocapacity Reserve or Deficit") %>% 
  hc_subtitle(text = "Visualized By Region ") %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))


# mapping by Region and Country :

geo_country <- merge(countries,Coun,by="Country",all=F)

geo_country %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat=geo_country$latitude, lng=geo_country$longitude, clusterOptions = markerClusterOptions(),
             popup= paste("<br><strong> Region : </strong>",geo_country$Region,
                          "<br><strong> Biocapacity(Reserve/Deficit) per Region : </strong>",by_Region$Biocapacity_Deficit_or_Reserve,
                          "<br><strong> Country : </strong>", geo_country$Country,
                          "<br><strong> Biocapacity(Reserve/Deficit) for Country : </strong>", geo_country$Biocapacity_Deficit_or_Reserve
                          
             ))

##############________________ Analyse de la variable Biocapacity par Variable _____________#################

######_______________ Croisement par la variable Population

# Distribution de la variable Population 

By_population <- countries %>% arrange(desc(Population)) %>% head(60)

t1 <- tooltip_table(c("Region :","Country :", "Population :","Total Ecological Footprint :","Total Biocapacity" ,"Biocapacity(Reserve/Deficit):"), 
                    c("{point.Region}","{point.Country}", "{point.Population}","{point.Total_Ecological_Footprint}", "{point.Total_Biocapacity}","{point.Biocapacity_Deficit_or_Reserve}"))

highchart() %>%   
  hc_add_series(By_population, "column", hcaes(x = Country, y = Population), name = "Country", colorByPoint = TRUE) %>%
  hc_xAxis(categories = countries$Country) %>% 
  hc_title(text = "Population per Country 2016") %>%
  hc_subtitle(text = paste("Global Vision")) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = t1) %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))


#  By Region :
By_Region_Pop <-countries %>% group_by(Region) %>% na.omit() %>% summarise_at(vars(Population),mean)

highchart() %>%   
  
  hc_add_series(By_Region_Pop, "pie", hcaes(name = Region, y = Population), name = " Average Population") %>%
  hc_title(text = "Population per Region 2016") %>%
  hc_subtitle(text = paste("Global Vision per Average")) %>% 
  hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 70, beta = 0)) %>% 
  hc_plotOptions(pie = list(depth = 70)) %>% 
  hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
             href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
             style = list(fontSize = "12px"))

#  Relationship between Population & biocapacity :

  hchart(countries, type = 'scatter', hcaes(x = Biocapacity_Deficit_or_Reserve, y = Population, size = Biocapacity_Deficit_or_Reserve, color = Biocapacity_Deficit_or_Reserve)) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = t1) %>%
    hc_xAxis(title = list(text = 'Biocapacity Reserve or Deficit')) %>%
    hc_yAxis(title = list(text = 'Population')) %>%
    hc_title(text = 'Population & Biocapacity') %>% 
    hc_subtitle(text = paste("Relationship Between Population factors & Biocapacity Reserve or Deficit")) %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
  
 
  # on filtre sur les valeurs abérantes et revoit la distribution :
  
  hchart(filter(countries,Biocapacity_Deficit_or_Reserve< 20,Population< 500), type = 'scatter',
         hcaes(x = Biocapacity_Deficit_or_Reserve, y = Population, 
               size = Biocapacity_Deficit_or_Reserve, color = Biocapacity_Deficit_or_Reserve)) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = t1) %>%
    hc_xAxis(title = list(text = 'Biocapacity Reserve or Deficit')) %>%
    hc_yAxis(title = list(text = 'Population')) %>%
    hc_title(text = 'Population & Biocapacity') %>% 
    hc_subtitle(text = paste("Relationship Between Population factors & Biocapacity Reserve or Deficit")) %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
  
 # Distribution de la variable HDI :
 
  By_HDI <- countries %>% arrange(desc(HDI))
  
  By_Region_Hdi <-countries %>% group_by(Region) %>% na.omit() %>% summarise_at(vars(HDI),mean)
  
  # graphe par Pays et région :

    highchart() %>% 
    hc_title(text = "HDI per Country & Region 2016 ") %>%
    hc_subtitle(text = "Global Vision In Number & Average by Region ") %>% 
    hc_add_series(By_HDI, "column", hcaes(x = Country, y = HDI), name = "Country", colorByPoint = TRUE) %>%
    hc_add_series(By_Region_Hdi, "pie", hcaes(name = Region, y = HDI), name = " Average HDI",
                  size = 100, dataLabels = list(enabled = TRUE),Format = "{value}%") %>% 
    hc_yAxis(title = list(text = "HDI In Number"),
             labels = list(format = "{value}")) %>% 
    hc_xAxis(categories = By_HDI$Country) %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px")) %>% 
    hc_legend(enabled = TRUE)
    

  #  relationsheep HDI & biocapacity :
 
  t2<-tooltip_table(c("Region :","Country :", "HDI:","Total Ecological Footprint :","Total Biocapacity" ,"Biocapacity(Reserve/Deficit):"), 
                c("{point.Region}","{point.Country}", "{point.HDI}","{point.Total_Ecological_Footprint}", "{point.Total_Biocapacity}","{point.Biocapacity_Deficit_or_Reserve}"))
  
  
  hchart(countries, type = 'scatter', 
       hcaes(x = HDI, y = Population, 
             size = Biocapacity_Deficit_or_Reserve, color = Biocapacity_Deficit_or_Reserve)) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = t2) %>%
    hc_xAxis(title = list(text = 'HDI')) %>%
    hc_yAxis(title = list(text = 'Population')) %>%
    hc_title(text = 'Population & HDI') %>% 
    hc_subtitle(text = paste("Relationship Between Biocapacity Reserve or Deficit")) %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
  
  
 ##############________________Analyse de la variable Biocapacity par Ctégorie_____________#################
  

 #  création de nouvelles variables qui caractérisent la variable biocapacity :
 
  Cat_biocap <- countries %>% dplyr::select( Country,Region,Population, HDI, GDP_per_Capita,Cropland_Footprint, Grazing_Footprint,Forest_Footprint,Fish_Footprint, Cropland, Grazing_Land, Forest_Land,Fishing_Water,Total_Biocapacity,Total_Ecological_Footprint,Biocapacity_Deficit_or_Reserve) %>% na.omit() %>% 
    mutate( Biocapacity_gropLand = Cropland - Cropland_Footprint,  Biocapacity_grazing = Grazing_Land - Grazing_Footprint,
      Biocapacity_forest = Forest_Land - Forest_Footprint,
      Biocapacity_fish = Fishing_Water - Fish_Footprint) 

  
  #  visualisation de Biocapacity_gropLand par Country & Region :
  
 Cat_biocap$iso3 <- countrycode::codelist_panel[match(Cat_biocap$Country, codelist_panel$country.name), "iso3c"]
  
  data(worldgeojson, package = "highcharter")
  
  dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)),
                          c = substring(viridis(5 + 1, option = "D"), 0, 7)) %>%  list_parse2()
  
  highchart() %>% 
    hc_add_series_map(worldgeojson, Cat_biocap, value = "Biocapacity_gropLand", joinBy = "iso3",
                      dataLabels = list(enabled = TRUE, format = '{point.name}'), name = "Country") %>% 
    
    hc_colorAxis(stops = dshmstops) %>% 
    hc_legend(enabled = TRUE) %>% 
    hc_add_theme(hc_theme_db()) %>% 
    hc_mapNavigation(enabled = TRUE) %>%
    hc_title(text = "Biocapacity GropLand Deficit or Reserve 2016") %>%
    hc_subtitle(text = "Visualization per Country") %>% 
    hc_add_theme(hc_theme_google()) %>%
    hc_credits(enabled = TRUE, text = "Sources: Exploring Ecological Footprint and Biocapacity (Kaggle)", style = list(fontSize = "10px")) %>% 
    hc_tooltip(useHTML = TRUE,
               headerFormat = "<table>",
               pointFormat = paste("<tr><th>Country <td>{point.Country}</td></th></tr>",
                                   "<tr><th>Region </th><td>{point.Region}</td></tr>",
                                   "<tr><th>Population(M)<td>{point.Population}</td></th></tr>",
                                   "<tr><th>HDI </th><td>{point.HDI}</td></tr>",
                                   "<tr><th>GDP_per_Capita </th><td>{point.GDP_per_Capita}</td></tr>",
                                   "<tr><th>Biocapacity_gropLand</th><td>{point.Biocapacity_gropLand}</td></tr>",
                                   "<tr><th>Total Ecological Footprint </th><td>{point.Total_Ecological_Footprint}</td></tr>",
                                   "<tr><th>Total Biocapacity </th><td>{point.Total_Biocapacity}</td></tr>",
                                   "<tr><th>Biocapacity </th><td>{point.Biocapacity_Deficit_or_Reserve}</td></tr>"),
               footerFormat = "</table>")

 # bioncapacity_reserve vers Biocapacity_gropland : 
 
  BG_VIS<- Cat_biocap %>% arrange(desc(Biocapacity_Deficit_or_Reserve))
  
 
  t3<-tooltip_table(c("Region :","Country :", "Biocapacity_gropLand:","Total Ecological Footprint :","Total Biocapacity" ,"Biocapacity(Reserve/Deficit):"), 
                    c("{point.Region}","{point.Country}", "{point.Biocapacity_gropLand}","{point.Total_Ecological_Footprint}", "{point.Total_Biocapacity}","{point.Biocapacity_Deficit_or_Reserve}"))
  
  hchart(filter(Cat_biocap,Biocapacity_Deficit_or_Reserve< 20,Biocapacity_gropLand<2), type = 'scatter', 
         hcaes(x = Biocapacity_gropLand, y =Biocapacity_Deficit_or_Reserve, 
               size = Biocapacity_Deficit_or_Reserve, color = Biocapacity_Deficit_or_Reserve)) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "<table>",
               pointFormat = t3,
               footerFormat = "</table>") %>%
    hc_title(text = "Biocapacity_gropLand & Biocapacity ") %>% 
    hc_subtitle(text = "Relationship with Biocapacity Reserve or Deficit") %>% 
    hc_xAxis(title = list(text = "Biocapacity_gropLand")) %>% 
    hc_yAxis(title = list(text = "Biocapacity Deficit or Reserve")) %>% 
    hc_chart(zoomType = "xy") %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
  
 
   #  visualisation de Biocapacity_grazing per Country & Region :
  
  highchart() %>% 
    hc_add_series_map(worldgeojson, Cat_biocap, value = "Biocapacity_grazing", joinBy = "iso3",
                      dataLabels = list(enabled = TRUE, format = '{point.name}'), name = "Country") %>% 
    
    hc_colorAxis(stops = dshmstops) %>% 
    hc_legend(enabled = TRUE) %>% 
    hc_add_theme(hc_theme_db()) %>% 
    hc_mapNavigation(enabled = TRUE) %>%
    hc_title(text = "Biocapacity_grazing Deficit or Reserve 2016 per Country") %>%
    hc_add_theme(hc_theme_google()) %>%
    hc_credits(enabled = TRUE, text = "Sources: Exploring Ecological Footprint and Biocapacity (Kaggle)", style = list(fontSize = "10px")) %>% 
    hc_tooltip(useHTML = TRUE,
               headerFormat = "<table>",
               pointFormat = paste("<tr><th>Country <td>{point.Country}</td></th></tr>",
                                   "<tr><th>Region </th><td>{point.Region}</td></tr>",
                                   "<tr><th>Population(M)<td>{point.Population}</td></th></tr>",
                                   "<tr><th>HDI </th><td>{point.HDI}</td></tr>",
                                   "<tr><th>GDP_per_Capita </th><td>{point.GDP_per_Capita}</td></tr>",
                                   "<tr><th>Biocapacity_grazing</th><td>{point.Biocapacity_grazing}</td></tr>",
                                   "<tr><th>Total Ecological Footprint </th><td>{point.Total_Ecological_Footprint}</td></tr>",
                                   "<tr><th>Total Biocapacity </th><td>{point.Total_Biocapacity}</td></tr>",
                                   "<tr><th>Biocapacity </th><td>{point.Biocapacity_Deficit_or_Reserve}</td></tr>"),
               footerFormat = "</table>")
  
  
  # bioncapacity_reserve vers Biocapacity_grazing :
  
  t4<-tooltip_table(c("Region :","Country :", "Biocapacity_grazing:","Total Ecological Footprint :","Total Biocapacity" ,"Biocapacity(Reserve/Deficit):"), 
                    c("{point.Region}","{point.Country}", "{point.Biocapacity_grazing}","{point.Total_Ecological_Footprint}", "{point.Total_Biocapacity}","{point.Biocapacity_Deficit_or_Reserve}"))
  
  
  hchart(filter(Cat_biocap,Biocapacity_Deficit_or_Reserve< 20,Biocapacity_grazing<2), type = 'scatter', 
         hcaes(x = Biocapacity_grazing, y =Biocapacity_Deficit_or_Reserve, 
               size = Biocapacity_Deficit_or_Reserve, color = Biocapacity_Deficit_or_Reserve)) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "<table>",
             pointFormat = t4,
             footerFormat = "</table>") %>%
    hc_title(text = "Biocapacity Grazing & Biocapacity ") %>% 
    hc_subtitle(text = "Relationship with Biocapacity Reserve or Deficit") %>% 
    hc_xAxis(title = list(text = "Biocapacity Grazing")) %>% 
    hc_yAxis(title = list(text = "Biocapacity Deficit or Reserve")) %>% 
    hc_chart(zoomType = "xy") %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
  
 
 #  visualisation de Biocapacity_forest per Country & Region : 
  
  highchart() %>% 
    hc_add_series_map(worldgeojson, Cat_biocap, value = "Biocapacity_forest", joinBy = "iso3",
                      dataLabels = list(enabled = TRUE, format = '{point.name}'), name = "Country") %>% 
    hc_colorAxis(stops = dshmstops) %>% 
    hc_legend(enabled = TRUE) %>% 
    hc_add_theme(hc_theme_db()) %>% 
    hc_mapNavigation(enabled = TRUE) %>%
    hc_title(text = "Biocapacity_forest Deficit or Reserve 2016 per Country") %>%
    hc_add_theme(hc_theme_google()) %>%
    hc_credits(enabled = TRUE, text = "Sources: Exploring Ecological Footprint and Biocapacity (Kaggle)", style = list(fontSize = "10px")) %>% 
    hc_tooltip(useHTML = TRUE,
               headerFormat = "<table>",
               pointFormat = paste("<tr><th>Country <td>{point.Country}</td></th></tr>",
                                   "<tr><th>Region </th><td>{point.Region}</td></tr>",
                                   "<tr><th>Population(M)<td>{point.Population}</td></th></tr>",
                                   "<tr><th>HDI </th><td>{point.HDI}</td></tr>",
                                   "<tr><th>GDP_per_Capita </th><td>{point.GDP_per_Capita}</td></tr>",
                                   "<tr><th>Biocapacity_forest</th><td>{point.Biocapacity_forest}</td></tr>",
                                   "<tr><th>Total Ecological Footprint </th><td>{point.Total_Ecological_Footprint}</td></tr>",
                                   "<tr><th>Total Biocapacity </th><td>{point.Total_Biocapacity}</td></tr>",
                                   "<tr><th>Biocapacity </th><td>{point.Biocapacity_Deficit_or_Reserve}</td></tr>"),
               footerFormat = "</table>")  
  
  # bioncapacity_reserve vers Biocapacity_Forest :
  
  t5<-tooltip_table(c("Region :","Country :", "Biocapacity Forest:","Total Ecological Footprint :","Total Biocapacity" ,"Biocapacity(Reserve/Deficit):"), 
                    c("{point.Region}","{point.Country}", "{point.Biocapacity_forest}","{point.Total_Ecological_Footprint}", "{point.Total_Biocapacity}","{point.Biocapacity_Deficit_or_Reserve}"))
  
  
  hchart(  filter(Cat_biocap,Biocapacity_Deficit_or_Reserve< 20,Biocapacity_forest<20), type = 'scatter', 
         hcaes(x = Biocapacity_forest, y =Biocapacity_Deficit_or_Reserve, 
               size = Biocapacity_Deficit_or_Reserve, color = Biocapacity_Deficit_or_Reserve)) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "<table>",
               pointFormat = t5,
               footerFormat = "</table>") %>%
    hc_title(text = "Biocapacity Forest & Biocapacity ") %>% 
    hc_subtitle(text = "Relationship with Biocapacity Reserve or Deficit") %>% 
    hc_xAxis(title = list(text = "Biocapacity Forest")) %>% 
    hc_yAxis(title = list(text = "Biocapacity Deficit or Reserve")) %>% 
    hc_chart(zoomType = "xy") %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
 
  
  
  #  visualisation de Biocapacity_fish per Country & Region :
  
  Cat_biocap$iso3 <- countrycode::codelist_panel[match(Cat_biocap$Country, codelist_panel$country.name), "iso3c"]
  
  data(worldgeojson, package = "highcharter")
  
  dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)),
                          c = substring(viridis(5 + 1, option = "D"), 0, 7)) %>%  list_parse2()
  
  highchart() %>% 
    hc_add_series_map(worldgeojson, Cat_biocap, value = "Biocapacity_fish", joinBy = "iso3",
                      dataLabels = list(enabled = TRUE, format = '{point.name}'), name = "Country") %>% 
    
    hc_colorAxis(stops = dshmstops) %>% 
    hc_legend(enabled = TRUE) %>% 
    hc_add_theme(hc_theme_db()) %>% 
    hc_mapNavigation(enabled = TRUE) %>%
    hc_title(text = "Biocapacity GropLand Deficit or Reserve 2016") %>%
    hc_subtitle(text = "Visualization per Country") %>% 
    hc_add_theme(hc_theme_google()) %>%
    hc_credits(enabled = TRUE, text = "Sources: Exploring Ecological Footprint and Biocapacity (Kaggle)", style = list(fontSize = "10px")) %>% 
    hc_tooltip(useHTML = TRUE,
               headerFormat = "<table>",
               pointFormat = paste("<tr><th>Country <td>{point.Country}</td></th></tr>",
                                   "<tr><th>Region </th><td>{point.Region}</td></tr>",
                                   "<tr><th>Population(M)<td>{point.Population}</td></th></tr>",
                                   "<tr><th>HDI </th><td>{point.HDI}</td></tr>",
                                   "<tr><th>GDP_per_Capita </th><td>{point.GDP_per_Capita}</td></tr>",
                                   "<tr><th>Biocapacity Fish</th><td>{point.Biocapacity_fish}</td></tr>",
                                   "<tr><th>Total Ecological Footprint </th><td>{point.Total_Ecological_Footprint}</td></tr>",
                                   "<tr><th>Total Biocapacity </th><td>{point.Total_Biocapacity}</td></tr>",
                                   "<tr><th>Biocapacity(Reserve/Deficit) </th><td>{point.Biocapacity_Deficit_or_Reserve}</td></tr>"),
               footerFormat = "</table>")

  # bioncapacity_reserve vers Biocapacity_Fish : 
  
  
  tt<-tooltip_table(c("Region :","Country :", " Biocapacity Fish:","Total Ecological Footprint :","Total Biocapacity" ,"Biocapacity(Reserve/Deficit):"), 
                    c("{point.Region}","{point.Country}", "{point.Biocapacity_fish}","{point.Total_Ecological_Footprint}", "{point.Total_Biocapacity}","{point.Biocapacity_Deficit_or_Reserve}"))
  
  hchart(filter(Cat_biocap,Biocapacity_Deficit_or_Reserve< 20, Biocapacity_fish<10), type = 'scatter', 
         hcaes(x = Biocapacity_fish, y =Biocapacity_Deficit_or_Reserve, 
               size = Biocapacity_Deficit_or_Reserve, color = Biocapacity_Deficit_or_Reserve)) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "<table>",
               pointFormat = tt,
               footerFormat = "</table>") %>%
    hc_title(text = "Biocapacity Fish & Biocapacity ") %>% 
    hc_subtitle(text = "Relationship with Biocapacity Reserve or Deficit") %>% 
    hc_xAxis(title = list(text = "Biocapacity Fish")) %>% 
    hc_yAxis(title = list(text = "Biocapacity Deficit or Reserve")) %>% 
    hc_chart(zoomType = "xy") %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
  
  
# ____________________________Analyse plus Profonde ________________________________
  
  # Visualisation de la variable Total Ecological Foorprint  :

 Moda_Reg <- countries %>% group_by(Region)%>% na.omit() %>% summarise_at(vars(Biocapacity_Deficit_or_Reserve,Cropland_Footprint,Grazing_Footprint,Forest_Footprint,Carbon_Footprint,
                                                                   Fish_Footprint,Cropland,Grazing_Land,Forest_Land,Fishing_Water,Urban_Land,Total_Biocapacity,Earths_Required,Countries_Required,Total_Ecological_Footprint),mean)
  

  # Correlation avec la variable Countries Required 
  
 t6<-tooltip_table(c("Region :","Country :", "Countries Required:","Total Ecological Footprint :","Biocapacity(Reserve/Deficit):"), 
                    c("{point.Region}","{point.Country}", "{point.Countries_Required}","{point.Total_Ecological_Footprint}","{point.Biocapacity_Deficit_or_Reserve}"))
  

  hchart(countries, type = 'scatter', 
         hcaes(x = Countries_Required, y = Total_Ecological_Footprint, 
               size = Biocapacity_Deficit_or_Reserve, color = Biocapacity_Deficit_or_Reserve)) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = t6) %>%
    hc_xAxis(title = list(text = 'Countries Required')) %>%
    hc_yAxis(title = list(text = 'Total Ecological Footprint')) %>%
    hc_title(text = 'Countries Required & Total Ecological Footprint') %>% 
    hc_subtitle(text = paste("Relationship Between Biocapacity Reserve or Deficit")) %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
  
  # Correlation avec la variable Earths Required 
  
  t7<-tooltip_table(c("Region :","Country :", "Earths Required:","Total Ecological Footprint :","Biocapacity(Reserve/Deficit):"), 
                    c("{point.Region}","{point.Country}", "{point.Earths_Required}","{point.Total_Ecological_Footprint}","{point.Biocapacity_Deficit_or_Reserve}"))
  
  
  hchart(countries, type = 'scatter', 
         hcaes(x = Earths_Required, y = Total_Ecological_Footprint, 
               size = Biocapacity_Deficit_or_Reserve, color = Biocapacity_Deficit_or_Reserve)) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = t7) %>%
    hc_xAxis(title = list(text = 'Earths Required')) %>%
    hc_yAxis(title = list(text = 'Total Ecological Footprint')) %>%
    hc_title(text = 'Earths Required & Total Ecological Footprint') %>% 
    hc_subtitle(text = paste("Relationship Between Biocapacity Reserve or Deficit")) %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
  
  # Correlation avec la variable HDI
  
  t8<-tooltip_table(c("Region :","Country :", "HDI:","Total Ecological Footprint :","Biocapacity(Reserve/Deficit):"), 
                    c("{point.Region}","{point.Country}", "{point.HDI}","{point.Total_Ecological_Footprint}","{point.Biocapacity_Deficit_or_Reserve}"))
  
  
  hchart(countries, type = 'scatter', 
         hcaes(x = HDI, y = Total_Ecological_Footprint, 
               size = Biocapacity_Deficit_or_Reserve, color = Biocapacity_Deficit_or_Reserve)) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = t8) %>%
    hc_xAxis(title = list(text = 'HDI')) %>%
    hc_yAxis(title = list(text = 'Total Ecological Footprint')) %>%
    hc_title(text = 'HDI & Total Ecological Footprint') %>% 
    hc_subtitle(text = paste("Relationship Between Biocapacity Reserve or Deficit")) %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
    
  # Correlation avec la variable GDP_per_Capita
  
  t9<-tooltip_table(c("Region :","Country :", "HDI:","GDP:","Total Ecological Footprint :","Biocapacity(Reserve/Deficit):"), 
                    c("{point.Region}","{point.Country}", "{point.HDI}","{point.GDP_per_Capita}","{point.Total_Ecological_Footprint}","{point.Biocapacity_Deficit_or_Reserve}"))
  
  
  hchart(countries, type = 'scatter', 
         hcaes(x = GDP_per_Capita, y = Total_Ecological_Footprint , 
               size = Biocapacity_Deficit_or_Reserve, color = Biocapacity_Deficit_or_Reserve)) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = t9) %>%
    hc_xAxis(title = list(text = 'GDP_per_Capita')) %>%
    hc_yAxis(title = list(text = 'Total_Ecological_Footprint')) %>%
    hc_title(text = 'GDP per Capita & Total_Ecological_Footprint') %>% 
    hc_subtitle(text = paste("Relationship Between Biocapacity Deficit or Reserve")) %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
  
  
  # Distribution des terres requises par region:
  
  highchart() %>%   
    
    hc_add_series(Moda_Reg, "pie", hcaes(name = Region, y = Earths_Required), name = " Earths Required") %>%
    hc_title(text = "Earths Required per Region 2016") %>%
    hc_subtitle(text = paste("Global Vision per Average")) %>% 
    hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 50, beta = 0)) %>% 
    hc_plotOptions(pie = list(depth = 50)) %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))
  
  # Distribution des Countries_Required par region:
  
  highchart() %>%   
    
    hc_add_series(Moda_Reg, "pie", hcaes(name = Region, y = Countries_Required), name = "Countries Required") %>%
    hc_title(text = "Countries Required per Region 2016") %>%
    hc_subtitle(text = paste("Global Vision per Average")) %>% 
    hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 50, beta = 0)) %>% 
    hc_plotOptions(pie = list(depth = 50)) %>% 
    hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))


  # Distribution Categories of Total Ecological Foorprint per region :
  
    highchart() %>% 
      hc_title(text = "Categories of Total Ecological Foorprint") %>%
      hc_subtitle(text = " Average by Region 2016") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Cropland_Footprint),name="Cropland Footprint") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Grazing_Footprint),name="Grazing Footprint") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Forest_Footprint),name="Forest Footprint") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Carbon_Footprint),name="Carbon Footprint") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Fish_Footprint),name="Fish Footprint") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Total_Ecological_Footprint),name="Total Ecological Footprint") %>%
      hc_yAxis(title = list(text = "Categories In Number"),
               labels = list(format = "{value}")) %>% 
      hc_xAxis(categories = Moda_Reg$Region) %>% 
      hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
                 href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
                 style = list(fontSize = "12px")) %>% 
      hc_legend(enabled = TRUE)
    
    
    # Distribution Categories of Total Biocapacity per region :
    
    highchart() %>% 
      hc_title(text = "Categories of Total Biocapacity") %>%
      hc_subtitle(text = " Average by Region 2016") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Cropland),name="Cropland") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Grazing_Land),name="Grazing Land") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Forest_Land),name="Forest Land") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Fishing_Water),name="Fishing Water") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Urban_Land),name="Urban Land") %>% 
      hc_add_series(Moda_Reg, "column", hcaes(x = Region, y = Total_Biocapacity),name="Total Biocapacity") %>%
      hc_yAxis(title = list(text = "Categories In Number"),
               labels = list(format = "{value}")) %>% 
      hc_xAxis(categories = Moda_Reg$Region) %>% 
      hc_credits(enabled = TRUE, text = "Sourc: Exploring Ecological Footprint and Biocapacity (Kaggle)",
                 href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
                 style = list(fontSize = "12px")) %>% 
      hc_legend(enabled = TRUE)
    
