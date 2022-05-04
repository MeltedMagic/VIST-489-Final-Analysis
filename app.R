library(shiny)
library(ggplot2)
library(dplyr)
library(corrplot)
library(plotly)
library(leaflet)
library(maps)
library(shinythemes)
library(shinyWidgets)

final_data<-read.csv("https://raw.githubusercontent.com/CBrennan708/P6_DV/main/merged_data.csv")
data <- read.csv("https://raw.githubusercontent.com/CBrennan708/P6_DV/main/top20perc.csv")
Data <- read.csv("https://raw.githubusercontent.com/CBrennan708/P6_DV/main/merged_data.csv")
TopTwenty <- read.csv("https://raw.githubusercontent.com/CBrennan708/P6_DV/main/top20perc.csv")
MS3 <- TopTwenty[, c(4:6)]
MS4 <- TopTwenty[, c(17:19)]

Data <- Data[Data$Country!="Europe",]

Data$GlobalEmissions <- Data$Co2_Coal+Data$Co2_Gas+Data$Co2_Oil

TotalEmissions <- Data %>% 
    group_by(Country) %>% 
    summarise(sum(GlobalEmissions))
TotalEmissions <- TotalEmissions[TotalEmissions$Country!="Europe",]

TotalEmissions$Desc <- factor(TotalEmissions$Country, levels = unique(TotalEmissions$Country)[order(TotalEmissions$`sum(GlobalEmissions)`, decreasing = TRUE)])


final_data<-subset(final_data, Country!="Europe")
final_data<-final_data[,-1]
final_data$total_co2<-final_data$Co2_Gas+final_data$Co2_Coal+final_data$Co2_Oil
final_data$tot_co2pc<-final_data$Co2Gas_percapita+final_data$Co2Coal_percapita+final_data$Co2Oil_percapita
final_data$total_conspc<-final_data$Gas_percapita+final_data$Coal_percapita+final_data$Oil_percapita
final_data$total_renewpc<-final_data$Nuclear_percapita+final_data$Hydro_percapita+final_data$Wind_percapita+final_data$Solar_percapita

top20contries<-final_data%>% group_by(Country)%>%summarize_at(vars(total_co2),
                                                              list(co2total=sum))
top20contries<-top20contries[order(top20contries$co2total, decreasing = T),]
top20contries$Country <- factor(top20contries$Country, levels=top20contries$Country)
top20contries$cumulative <- cumsum(top20contries$co2total)

#map
map_data("world")
MapData <- map_data("world")
CountryData <- map_data("world") %>% group_by(region) %>% summarise(mean(lat), mean(long))

MapTT <- TopTwenty %>%
    group_by(Country) %>%
    summarize(mean(total_co2))

MapTT$Long <- c("-51.9253","-106.3468","104.1954","10.4515","78.9629","113.9213","53.6880","12.5674","138.2529","-102.5528","105.3188","22.9375","127.7669","-0.118092","-95.7129")
MapTT$Long <- as.numeric(MapTT$Long)
MapTT$Lat <- c("-14.2350","56.1304","35.8617","51.1657","20.5937","0.7893","32.4279","41.8719","36.2048","23.6345","61.5240","-30.5595","35.9078","51.509865","37.0902")
MapTT$Lat <- as.numeric(MapTT$Lat)
MapTT$Label <- c("Brazil 396920986", "Canada 546840629", "China 7171741041", "Germany 805625033", "India 1647442115", "Indonesia 418856582", "Iran 505220722",
                 "Italy 405751044", "Japan 1203513655", "Mexico 438644878", "Russia 1532468105", "South Africa 445750552", "South Korea 539396821", "United Kingdom 479373362", 
                 "United States 5552377121")



top20per<-subset(final_data, Country!="Algeria" & Country!="Argentina" & Country!="Australia" 
                 & Country!="Austria"  & Country!="Azerbaijan"  & Country!="Bangladesh" & Country!="Belarus" 
                 & Country!="Belgium"   & Country!="Bulgaria" & Country!="Chile" & Country!="Colombia" 
                 & Country!="Croatia" & Country!="Cyprus" & Country!="Czechia" & Country!="Denmark" & Country!="Egypt" 
                 & Country!="Estonia" & Country!="Finland" & Country!="France" & Country!="Greece" 
                 & Country!="Hong Kong" & Country!="Hungary"  & Country!="Iceland" & Country!="Ireland" 
                 & Country!="Israel" & Country!="Kazakhstan" & Country!="Latvia" & Country!="Lithuania" 
                 & Country!="Luxembourg" & Country!="Malaysia" & Country!="Morocco" & Country!="Netherlands" 
                 & Country!="New Zealand" & Country!="North Macedonia" & Country!="Norway" & Country!="Pakistan" 
                 & Country!="Peru" & Country!="Philippines" & Country!="Poland" & Country!="Portugal" 
                 & Country!="Romania" & Country!="Singapore" & Country!="Slovakia" & Country!="Slovenia" 
                 & Country!="Spain" & Country!="Sweden" & Country!="Switzerland" & Country!="Taiwan" 
                 & Country!="Thailand" & Country!="Turkey" & Country!="Ukraine" & Country!="United Arab Emirates" 
                 & Country!="Uzbekistan" & Country!="Venezuela" & Country!="Vietnam")

accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
}
df <- top20per%>% accumulate_by(~Year)

library(reshape2)

tot_conspc <- dcast(Year ~ Country,data=top20per[, c("Country", "Year", "total_conspc")],
                    value.var="total_conspc")

total_conspc_ts<-ts(data = tot_conspc, start=min(tot_conspc$Year),
                    end = max(tot_conspc$Year))
total_conspc_ts


tot_renewpc <- dcast(Year ~ Country,data=top20per[, c("Country", "Year", "total_renewpc")],
                     value.var="total_renewpc")


total_renewpc_ts<-ts(data = tot_renewpc, start=min(tot_renewpc$Year),
                     end = max(tot_renewpc$Year))
total_renewpc_ts


library(forecast)

arima_2<-auto.arima(total_conspc_ts[,2])
arima_forecast_fos_2 = forecast(arima_2, h = 5)
arima_3<-auto.arima(total_conspc_ts[,3])
arima_forecast_fos_3 = forecast(arima_3, h = 5)
arima_4<-auto.arima(total_conspc_ts[,4])
arima_forecast_fos_4 = forecast(arima_4, h = 5)
arima_5<-auto.arima(total_conspc_ts[,5])
arima_forecast_fos_5 = forecast(arima_5, h = 5)
arima_6<-auto.arima(total_conspc_ts[,6])
arima_forecast_fos_6 = forecast(arima_6, h = 5)
arima_7<-auto.arima(total_conspc_ts[,7])
arima_forecast_fos_7 = forecast(arima_7, h = 5)
arima_8<-auto.arima(total_conspc_ts[,8])
arima_forecast_fos_8 = forecast(arima_8, h = 5)
arima_9<-auto.arima(total_conspc_ts[,9])
arima_forecast_fos_9 = forecast(arima_9, h = 5)
arima_10<-auto.arima(total_conspc_ts[,10])
arima_forecast_fos_10 = forecast(arima_10, h = 5)
arima_11<-auto.arima(total_conspc_ts[,11])
arima_forecast_fos_11 = forecast(arima_11, h = 5)
arima_12<-auto.arima(total_conspc_ts[,12])
arima_forecast_fos_12 = forecast(arima_12, h = 5)
arima_13<-auto.arima(total_conspc_ts[,13])
arima_forecast_fos_13 = forecast(arima_13, h = 5)
arima_14<-auto.arima(total_conspc_ts[,14])
arima_forecast_fos_14 = forecast(arima_14, h = 5)
arima_15<-auto.arima(total_conspc_ts[,15])
arima_forecast_fos_15 = forecast(arima_15, h = 5)
arima_16<-auto.arima(total_conspc_ts[,16])
arima_forecast_fos_16 = forecast(arima_16, h = 5)

arimar_2<-auto.arima(total_renewpc_ts[,2])
arima_forecast_ren_2 = forecast(arimar_2, h = 5)
arimar_3<-auto.arima(total_renewpc_ts[,3])
arima_forecast_ren_3 = forecast(arimar_3, h = 5)
arimar_4<-auto.arima(total_renewpc_ts[,4])
arima_forecast_ren_4 = forecast(arimar_4, h = 5)
arimar_5<-auto.arima(total_renewpc_ts[,5])
arima_forecast_ren_5 = forecast(arimar_5, h = 5)
arimar_6<-auto.arima(total_renewpc_ts[,6])
arima_forecast_ren_6 = forecast(arimar_6, h = 5)
arimar_7<-auto.arima(total_renewpc_ts[,7])
arima_forecast_ren_7 = forecast(arimar_7, h = 5)
arimar_8<-auto.arima(total_renewpc_ts[,8])
arima_forecast_ren_8 = forecast(arimar_8, h = 5)
arimar_9<-auto.arima(total_renewpc_ts[,9])
arima_forecast_ren_9 = forecast(arimar_9, h = 5)
arimar_10<-auto.arima(total_renewpc_ts[,10])
arima_forecast_ren_10 = forecast(arimar_10, h = 5)
arimar_11<-auto.arima(total_renewpc_ts[,11])
arima_forecast_ren_11 = forecast(arimar_11, h = 5)
arimar_12<-auto.arima(total_renewpc_ts[,12])
arima_forecast_ren_12 = forecast(arimar_12, h = 5)
arimar_13<-auto.arima(total_renewpc_ts[,13])
arima_forecast_ren_13 = forecast(arimar_13, h = 5)
arimar_14<-auto.arima(total_renewpc_ts[,14])
arima_forecast_ren_14 = forecast(arimar_14, h = 5)
arimar_15<-auto.arima(total_renewpc_ts[,15])
arima_forecast_ren_15 = forecast(arimar_15, h = 5)
arimar_16<-auto.arima(total_renewpc_ts[,16])
arima_forecast_ren_16 = forecast(arimar_16, h = 5, level=c(80, 95,99))

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
    #shinythemes::themeSelector(),
    theme=shinythemes::shinytheme("simplex"),
    br(),
    br(),
    
    
    
    navbarPage(title="CO2 Emissions by Country",
               tabPanel("Top 20% of CO2 Emittors",
                        fig <- plot_ly(TotalEmissions, x = ~Desc, y = ~`sum(GlobalEmissions)`, type = 'bar',
                                       marker = list(color = c('black', 'black', 'black', 'black','black','black', 'black', 'black', 'red','black','red', 'black', 'red', 'black','black',
                                                               'black', 'black', 'black','black','black', 'black', 'black', 'red','black','black', 'black', 'black', 'red','red',
                                                               'red', 'black', 'black', 'red','red','black', 'black', 'black', 'black','black','red', 'black', 'black', 'black','black',
                                                               'black', 'black', 'black', 'black', 'black', 'black', 'black', 'red', 'black','black', 'black', 'red','red','black', 'black', 'black', 'black','black',
                                                               'black','black','black','red','red'))) %>%
                            layout(title='TOP 20% CO2 EMITTING COUNTRIES (IN RED)',
                                   xaxis = list(title = "Country"), yaxis = list(title = "Co2 Emissions (Tonnes)"))
                        
               )),
    br(),
    br(),
    
    navbarPage(title="World's Top Co2 Emittors",
               tabPanel("Map Of Highest 20% CO2 Emitting Countries",
                        leafletOutput("my_leaf"))),
    
    br(),
    br(),
    br(),
    
    navbarPage(title="Comparison of CO2 Emissions and Consumption",
               tabPanel("Correlation Matrix/Scatterplot",
                        
                        sidebarLayout(
                            
                            # Inputs: Select variables to plot
                            sidebarPanel(
                                radioButtons("PlotChoice", "Displayed plot:", 
                                             choices = c("Scatterplot", "Correlation Matrix"), selected = "Scatterplot"),
                                
                                # Select variable for y-axis
                                selectInput(
                                    inputId = "y",
                                    label = "Consumption",
                                    choices = c("Coal Consumption" = "Coal_Consumption", "Oil Consumption" ="Oil_Consumption", "Gas Consumption" = "Gas_Consumption"),
                                    selected = "Coal_Consumption"
                                ),
                                # Select variable for x-axis
                                selectInput(
                                    inputId = "x",
                                    label = "CO2 Emissions",
                                    choices = c("CO2 Coal" = "Co2_Coal", "CO2 Oil"= "Co2_Oil", "CO2 Gas"= "Co2_Gas", "Total" = "total_co2"),
                                    selected = "Co2_Coal"
                                ),
                                # Select variable for z-axis
                                selectInput(
                                    inputId = "z",
                                    label = "Colour by:",
                                    choices = c("Country" ="Country", "Year" = "Year"),
                                    selected = "Country")
                                
                            ),
                            # Output: Show scatterplot
                            mainPanel(
                                plotOutput(outputId = "SelectedPlot")
                            )))),
    br(), br(),
    
    
    navbarPage(
        title="Energy Consumption By Top 20 % Co2 Emitting Countries",
        tabPanel("Fossil Fuel Consumption",
                 fig_cons<-  
                     plot_ly(df,
                             x = ~Year, 
                             y = ~total_conspc, 
                             color = ~Country, 
                             frame = ~frame, 
                             text = ~Country, 
                             type = 'scatter',
                             mode = 'lines+markers',
                             line = list(simplyfy = F)
                             
                     )%>%layout(title='FOSSIL FUEL CONSUMPTION PER CAPITA',
                                yaxis=list(title='Consumption Per Capita (kWh)'),
                                xaxis=list(title='Year'))
        ),
        
        tabPanel("Renewable Energy Consumption",
                 fig_cons<-  
                     plot_ly(df,
                             x = ~Year, 
                             y = ~total_renewpc, 
                             color = ~Country, 
                             frame = ~frame, 
                             text = ~Country, 
                             type = 'scatter',
                             mode = 'lines+markers',
                             line = list(simplyfy = F)
                             
                     )%>%layout(title='RENEWABLE ENERGY CONSUMPTION PER CAPITA',
                                yaxis=list(title='Consumption Per Capita (kWh)'),
                                xaxis=list(title='Year'))
        )),
    
    br(),
    br(),
    
    navbarPage(title="Forecasting Of Energy Consumption Per Capita",
               
               
               sidebarPanel(
                   radioButtons("energytype", "Energy Type:", 
                                choices = c("Fossil", "Renewable"), selected = "Fossil"), width = 4,
                   
                   
                   selectInput("Prediction_Fos", "Country-Fossil:",
                               choices = c("Brazil", "Canada", "China", "Germany",
                                           "India", "Indonesia", "Iran", "Italy",
                                           "Japan", "Mexico", "Russia", "S Africa",
                                           "S Korea", "UK", "US"),
                               selected="US"),
                   
                   selectInput("Prediction_Ren", "Country-Renewable:",
                               choices = c("Brazil", "Canada", "China", "Germany",
                                           "India", "Indonesia", "Iran", "Italy",
                                           "Japan", "Mexico", "Russia", "S Africa",
                                           "S Korea", "UK", "US"),
                               selected="US")),
               
               mainPanel(plotOutput("plotf"))))



# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
    
    your_plot <- reactive({
        if(input$PlotChoice == "Correlation Matrix") {
            corrplot(cor(MS3, MS4),
                     method = "number")
        }
        else if (input$PlotChoice == "Scatterplot"){
            ggplot(data = data, aes_string(x = input$x, y = input$y, colour=input$z)) +
                geom_point()
        }
    })
    
    plot2 <- reactive({
        
        if (input$energytype=="Fossil"){
            
            if(input$Prediction_Fos == "Brazil") {
                
                "Brazil"=plot(arima_forecast_fos_2, col="#0066CC", ylab ="Consumption Per Capita (kWh)", 
                              xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN BRAZIL - FORECAST", 
                              col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "Canada") {
                "Canada"=plot(arima_forecast_fos_3, col="#FF9900", ylab ="Consumption Per Capita (kWh)", 
                              xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN CANADA - FORECAST", 
                              col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "China") {
                "China"=plot(arima_forecast_fos_4, col="#339966", ylab ="Consumption Per Capita (kWh)", 
                             xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN CHINA - FORECAST", 
                             col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "Germany") {
                "Germany"=plot(arima_forecast_fos_5, col="#993300", ylab ="Consumption Per Capita (kWh)", 
                               xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN GERMANY - FORECAST", 
                               col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "India") {
                "India"=plot(arima_forecast_fos_6, col="#993366", ylab ="Consumption Per Capita (kWh)", 
                             xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN INDIA - FORECAST", 
                             col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "Indonesia") {
                "Indonesia"=plot(arima_forecast_fos_7, col="#800000", ylab ="Consumption Per Capita (kWh)", 
                                 xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN INDONESIA - FORECAST", 
                                 col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "Iran") {
                "Iran"=plot(arima_forecast_fos_8, col="#FF99CC", ylab ="Consumption Per Capita (kWh)", 
                            xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN IRAN - FORECAST", 
                            col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "Italy") {
                "Italy"=plot(arima_forecast_fos_9, col="#333300", ylab ="Consumption Per Capita (kWh)", 
                             xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN ITALY - FORECAST", 
                             col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            
            else if (input$Prediction_Fos == "Japan") {
                "Japan"=plot(arima_forecast_fos_10, col="#99CC00", ylab ="Consumption Per Capita (kWh)", 
                             xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN JAPAN - FORECAST", 
                             col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "Mexico") {
                "Mexico"=plot(arima_forecast_fos_11, col="#00FFFF", ylab ="Consumption Per Capita (kWh)", 
                              xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN MEXICO - FORECAST", 
                              col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "Russia") {
                "Russia"=plot(arima_forecast_fos_12,col="#000080", ylab ="Consumption Per Capita (kWh)", 
                              xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN RUSSIA - FORECAST", 
                              col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "S Africa") {
                "S Africa"=plot(arima_forecast_fos_13, col="#FF8200", ylab ="Consumption Per Capita (kWh)", 
                                xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN SOUTH AFRICA - FORECAST", 
                                col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "S Korea") {
                "S Korea"=plot(arima_forecast_fos_14, col="#33B466", ylab ="Consumption Per Capita (kWh)", 
                               xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN SOUTH KOREA - FORECAST", 
                               col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "UK") {
                "UK"=plot(arima_forecast_fos_15, col="#C83300", ylab ="Consumption Per Capita (kWh)", 
                          xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN UK - FORECAST", 
                          col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Fos == "US") {
                "US"=plot(arima_forecast_fos_16, col="#800080", ylab ="Consumption Per Capita (kWh)", 
                          xlab ="Year", main="FOSSIL FUEL CONSUMPTION IN US - FORECAST", 
                          col.main="blue", lwd=3.0, las=0, cex.names=1)
            }}
        
        if (input$energytype=="Renewable"){ 
            
            if(input$Prediction_Ren == "Brazil") {
                "Brazil1"=plot(arima_forecast_ren_2, col="#0066CC", ylab ="Consumption Per Capita (kWh)", 
                               xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN CANADA - FORECAST", 
                               col.main="blue", lwd=3.0, las=0, cex.names=1)
            }
            else if (input$Prediction_Ren == "Canada") {
                "Canada1"=plot(arima_forecast_ren_3, col="#FF9900", ylab ="Consumption Per Capita (kWh)", 
                               xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN CHINA - FORECAST", 
                               col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "China") {
                "China1"=plot(arima_forecast_ren_4, col="#339966", ylab ="Consumption Per Capita (kWh)", 
                              xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN GERMANY - FORECAST", 
                              col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "Germany") {
                "Germany1"=plot(arima_forecast_ren_5, col="#993300", ylab ="Consumption Per Capita (kWh)", 
                                xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN GERMANY - FORECAST", 
                                col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "India") {
                "India1"=plot(arima_forecast_ren_6, col="#993366", ylab ="Consumption Per Capita (kWh)", 
                              xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN INDIA - FORECAST", 
                              col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "Indonesia") {
                "Indonesia1"=plot(arima_forecast_ren_7, col="#800000", ylab ="Consumption Per Capita (kWh)", 
                                  xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN INDONESIA - FORECAST", 
                                  col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "Iran") {
                "Iran1"=plot(arima_forecast_ren_8, col="#FF99CC", ylab ="Consumption Per Capita (kWh)", 
                             xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN IRAN - FORECAST", 
                             col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "Italy") {
                "Italy1"=plot(arima_forecast_ren_9, col="#333300", ylab ="Consumption Per Capita (kWh)", 
                              xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN ITALY - FORECAST", 
                              col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            
            else if (input$Prediction_Ren == "Japan") {
                "Japan1"=plot(arima_forecast_ren_10, col="#99CC00", ylab ="Consumption Per Capita (kWh)", 
                              xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN JAPAN - FORECAST", 
                              col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "Mexico") {
                "Mexico1"=plot(arima_forecast_ren_11, col="#00FFFF", ylab ="Consumption Per Capita (kWh)", 
                               xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN MEXICO - FORECAST", 
                               col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "Russia") {
                "Russia1"=plot(arima_forecast_ren_12,col="#000080", ylab ="Consumption Per Capita (kWh)", 
                               xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN RUSSIA - FORECAST", 
                               col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "S Africa") {
                "S Africa1"=plot(arima_forecast_ren_13, col="#FF8200", ylab ="Consumption Per Capita (kWh)", 
                                 xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN SOUTH AFRICA - FORECAST", 
                                 col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "S Korea") {
                "S Korea1"=plot(arima_forecast_ren_14, col="#33B466", ylab ="Consumption Per Capita (kWh)", 
                                xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN SOUTH KOREA - FORECAST", 
                                col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "UK") {
                "UK1"=plot(arima_forecast_ren_15, col="#C83300", ylab ="Consumption Per Capita (kWh)", 
                           xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN UK - FORECAST", 
                           col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }
            else if (input$Prediction_Ren == "US") {
                "US1"=plot(arima_forecast_ren_16, col="#800080", ylab ="Consumption Per Capita (kWh)", 
                           xlab ="Year", main="RENEWABLE ENERGY CONSUMPTION IN US - FORECAST", 
                           col.main="blue", lwd=3.0, las=0, cex.names=1
                )
            }}
    })
    
    output$my_leaf <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("Esri.WorldStreetMap", group="ESRI") %>%
            setView(lat =30, lng = -96, zoom=1) %>%
            addCircles(lat= MapTT$Lat, lng=MapTT$Long, 
                       radius=(MapTT$`mean(total_co2)`)*0.0001, col="red", fill = T, 
                       popup = MapTT$Label)
        
        
    })
    output$SelectedPlot <- renderPlot({ 
        
        your_plot()
    })
    
    output$plotf <- renderPlot({ 
        
        plot2()
    })
    
    
    
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)

