# Load libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(gtable)
library(xtable)
library(shinythemes)
require(shinydashboard)
library(shiny)
library(shinythemes)
library(shinyBS)
library(rCharts)
library(plyr)
library(leaflet)
library(gridExtra)
library(grid)

server <- function(input, output) {
  # Make data with several positions
  data_fire2006=data.frame(FireLocat2006)
  data_fire2007=data.frame(FireLocat2007)
  data_fire2008=data.frame(FireLocat2008)
  data_fire2009=data.frame(FireLocat2009)
  data_fire2010=data.frame(FireLocat2010)
  data_fire2011=data.frame(FireLocat2011)
  data_fire2012=data.frame(FireLocat2012)
  data_fire2013=data.frame(FireLocat2013)
  data_fire2014=data.frame(FireLocat2014)
  data_fire2015=data.frame(FireLocat2015)
  data_FIRE_ADDRESS=data.frame(FIRE_ADDRESS)
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Initialize the leaflet map:
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(lng=-71, lat=42, zoom=7 ) %>%
      
      # Add two tiles
      addProviderTiles("Esri.WorldImagery", group="background 1") %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
      
      # Add 2 marker groups
      addCircleMarkers(data=data_fire2006, lng=~longitude , lat=~latitude, radius=10 , color="red", fillColor="red", stroke = TRUE, fillOpacity = 1.0, group="2006") %>%
      addCircleMarkers(data=data_fire2007, lng=~longitude , lat=~latitude, radius=10 , color="blue",  fillColor="blue", stroke = TRUE, fillOpacity = 1.0, group="2007") %>%
      addCircleMarkers(data=data_fire2008, lng=~longitude , lat=~latitude, radius=10 , color="green",  fillColor="green", stroke = TRUE, fillOpacity = 1.0, group="2008") %>%
      addCircleMarkers(data=data_fire2009, lng=~longitude , lat=~latitude, radius=10 , color="yellow",  fillColor="yellow", stroke = TRUE, fillOpacity = 1.0, group="2009") %>%
      addCircleMarkers(data=data_fire2010, lng=~longitude , lat=~latitude, radius=10 , color="deeppink",  fillColor="deeppink", stroke = TRUE, fillOpacity = 1.0, group="2010") %>%
      addCircleMarkers(data=data_fire2011, lng=~longitude , lat=~latitude, radius=10 , color="cyan",  fillColor="cyan", stroke = TRUE, fillOpacity = 1.0, group="2011") %>%
      addCircleMarkers(data=data_fire2012, lng=~longitude , lat=~latitude, radius=10 , color="burlywood",  fillColor="burlywood", stroke = TRUE, fillOpacity = 1.0, group="2012") %>%
      addCircleMarkers(data=data_fire2013, lng=~longitude , lat=~latitude, radius=10 , color="aquamarine",  fillColor="aquamarine", stroke = TRUE, fillOpacity = 1.0, group="2013") %>%
      addCircleMarkers(data=data_fire2014, lng=~longitude , lat=~latitude, radius=10 , color="burlywood4",  fillColor="burlywood4", stroke = TRUE, fillOpacity = 1.0, group="2014") %>%
      addCircleMarkers(data=data_fire2015, lng=~longitude , lat=~latitude, radius=10 , color="lightcyan",  fillColor="lightcyan", stroke = TRUE, fillOpacity = 1.0, group="2015") %>%
      
      
      # Add the control widget
      addLayersControl(overlayGroups = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015") , baseGroups = c("background 1","background 2"), options = layersControlOptions(collapsed = FALSE))
  })
  
  
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  myTable <- table(FIRE_ADDRESS$YEAR, FIRE_ADDRESS$INC_MONTH)
  colnames(myTable) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  g <- tableGrob(myTable)
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 2, b = nrow(g), l = 1, r = ncol(g))
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 1, l = 1, r = ncol(g))
  
  # Make a barplot or scatterplot depending of the selected point
  output$plot1=renderPlot({
    barplot(table(factor(FIRE_ADDRESS$YEAR, c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))),ylim=c(0,100),
            xlab="Year",ylab="Frequency",
            main='Fire caused by cigarette in 2006')
  }) 
  output$plot2=renderPlot({  grid.draw(g)
    
  })
  
}

caption <- ''

ui <- navbarPage(theme=shinytheme("cosmo"),
                 title=HTML('<div></div>'),
                 tabPanel("Final Project", value="commChart"),
                 tabPanel("About Daekyoo", value="about"),
                 windowTitle="CC4L",
                 collapsible=TRUE,
                 id="tsp",
                 tags$head(includeScript("https://github.com/ua-snap/shiny-apps/raw/master/cc4liteFinal/ga-cc4liteFinal.js"), includeScript("https://github.com/ua-snap/shiny-apps/raw/master/cc4liteFinal/ga-allapps.js")),
                 tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
                 column(5,leafletOutput("map", height="600px")),
                 column(7,plotOutput("plot1", height="300px")),
                 br(),
                 column(7,plotOutput("plot2", height="300px")),
                 br(),
                 fluidRow(
                   column(4, actionButton("help_loc_btn", "Protocol/Results", class="btn-block"), br()),
                   column(8, h5(HTML(paste(caption, '<a </a>'))))
                 ),
                 bsModal("modal_loc", "Monthly frequency of fire caused by cigarette from 2006 to 2015", "help_loc_btn", size="large",
                         HTML('
                              <p style="text-align:justify">For my ﬁnal project, I did an exploration of data related to ﬁre incidents caused by a dropped cigarette, which is
                              indicated in ‘Cause of Ignition’ (i.e., Heat Source Code is 61 in NFIRS).I ﬁrst collected the source of the cigarette that started the fire. 
                              After that, I merged the addresses between a dbf file of fireincident and a dbf file ofvincidentaddress, based on the zipcode.
                              Based on google map, I descripted how many fire incidents broke out from 2006 to 2015 using bar graph. 
                              It may tell us how dropping cigarrete is unpredictable risk factor, which has negatively affected ﬁre incidents in Massachusetts for the past 10 years.
                              In addition, I summarized and rearranged about how frequent fires were caused by cigaretts in every month for the last 10 years, implying the most frequently occuring month in Masschesetts.</p>'
                         )),
                 conditionalPanel("input.tsp=='about'", source("FinalProject_about.R",local=T)$value)
)

shinyApp(ui = ui, server = server)  