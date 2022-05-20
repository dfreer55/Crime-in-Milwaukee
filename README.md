# Crime-in-Milwaukee
# Milwaukee Crime Shiny App

# Libraries ---------------------------------------------------------------

library(stringr)  
library(leaflet)
library(tidyverse)
library(maps)
library(tigris)
library(readxl)
library(shiny)
library(sf)

# Data loading and cleaning -----------------------------------------------
setwd("C:/Users/Daniel Freer/Documents/R Projects")
##clear your working directory
rm(list=ls())

dat1 <-
  read_excel("./wibr.xlsx")

dat2 <-
  dat1 %>% 
  select(TRACT, Arson:VehicleTheft) %>% 
  pivot_longer(cols = Arson:VehicleTheft,
               names_to = "crime") %>% 
  group_by(TRACT, crime) %>% 
  summarise(sum = sum(value, na.rm = TRUE))

dat3 <-
  dat1 %>% 
  select(TRACT, Arson:VehicleTheft) %>% 
  pivot_longer(cols = Arson:VehicleTheft,
               names_to = "crime") %>% 
  group_by(TRACT, crime) %>% 
  summarise(sum = sum(value, na.rm = TRUE))

TractList <- unique(dat2$TRACT)
TractList <- as.data.frame(TractList)

n <- tracts("WI", "Milwaukee")

dat2$TRACT <- as.character(dat2$TRACT)
n$TRACTCE <- as.character(as.numeric(n$TRACTCE))

n <- left_join(n, dat2, by = c("TRACTCE" = "TRACT")) # Left join Tigris data and Crime data

# Create R shiny UI -------------------------------------------------------

ui <- shinyUI(navbarPage("Crime in Different Census Tracts - Milwaukee",
                   tabPanel("Map",
                            sidebarPanel(helpText("Select Crime"),
                                          selectInput(
                                            "crimeInput",
                                            label = "Crime:",
                                            list(
                                              "Arson" = "Arson",
                                              "Assault Offense" = "AssaultOffense",
                                              "Burglary" = "Burglary",
                                              "Criminal Damage" = "CriminalDamage",
                                              "Homicide" = "Homicide",
                                              "Locked Vehicle" = "LockedVehicle",
                                              "Robbery" = "Robbery",
                                              "Sex Offense" = "SexOffense",
                                              "Theft" = "Theft",
                                              "Vehicle Theft" = "VehicleTheft"
                                            )
                                          )),
                            mainPanel(leafletOutput("crimeMap"))
                   ),
                   
                   tabPanel("Graph",
                            sidebarPanel(helpText("Select Tract"),
                                          selectInput("TractInput",
                                                      label = "Tract:",
                                                      TractList)),
                            sidebarPanel(helpText("Census tracts are small, relatively permanent statistical subdivisions of the United States")),
                            mainPanel(plotOutput("crimePlot"))
                   )
))


# Define server logic to plot crime against year --------------------------

server <- function(input, output, session) {

n_react <- reactive({
  filter(n, crime == input$crimeInput)
  })

  output$crimeMap <- renderLeaflet({
      leaflet() %>%
      setView(lng = -87.9065,lat = 43.0389, zoom = 10.5)
  })
  
  observe({
    leafletProxy(mapId = "crimeMap") %>% 
      addTiles() %>% 
      addPolygons(
        data = n_react(),
        color = ~colorNumeric("YlOrRd", sum)(sum),
        stroke = "white",
        weight = 2,
        opacity = 1,
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.2,
          bringToFront = TRUE),
        label = paste0("<strong>", n_react()$crime, "</strong> in Tract ",
                       n_react()$TRACTCE,
                       "<br>Total: ",
                       n_react()$sum) %>% 
          lapply(htmltools::HTML), 
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>% 
      setView(-87.9065, 43.0389, zoom = 10.5)
  })
  
  output$crimePlot <- renderPlot({
    dat3 %>%
      filter(TRACT == input$TractInput) %>% 
      ggplot(aes(x = crime, y = sum)) +
      geom_bar(stat = "identity") +
      labs(x = "Crime", y = "Frequency") 
  }) 
  
  }

shinyApp(ui, server)
