library(shiny)
library(dplyr)
library(tmaptools)
library(leaflet)
df_prop <- readRDS("SG_panel_prop.rds")
df_prop$pc80 <- 100 - df_prop$pc80
sg_map <- readRDS("SG_map.rds")

ui <- fluidPage(
   
   titlePanel("Visualising Singapore Census Data"),
   sidebarLayout(
     sidebarPanel(
         selectInput("startyr", "Start Year:", c("2000","2010","2015"), selected = "2015"),
         selectInput("endyr", "End Year:", c("2000","2010","2015"), selected = "2015"),
         radioButtons("colourscheme", "Plot Colour Scheme:",
                            choices = c("Divergent", "Sequential"), selected="Divergent",
                            inline = TRUE),
         selectInput("cat",
                     "Select a category:",
                     c("Race", "Religion", "Housing", "Income")),
         uiOutput("controls"),
         tags$hr(),
         p("Source: Singapore Census of Population (2000 & 2015), General Household Survey (2010)"),
         p("Note: Income data is linearly interpolated to calculate the percentile distribution and
           comparisons across time may be inaccurate.
           Full set of code and data to reproduce the plot can be found",
          a("here.", href="https://github.com/timlrx/sg-mapping"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3(textOutput("maptitle")),
        leafletOutput("map"),
        textOutput("totalstat")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$controls <- renderUI({
     selectInput("selection",
                 "Select a variable to visualise:",
                 switch(input$cat,
                        "Race"= names(df_prop)[13:16],
                        "Religion"= names(df_prop)[3:12],
                        "Housing"= names(df_prop)[17:23],
                        "Income"= c("<20th percentile", "<50th percentile", ">80th percentile")
                 )
     )
   })
   
   data <- reactive({
     protectStr <- function(x){
       ifelse(grepl(" ", x),paste0("`",x,"`"),x)
     }
     if (is.null(input$selection)) {
       return(NULL)
     }
     plot_select <- switch(input$selection,
                           "<20th percentile"="pc20",
                           "<50th percentile"="pc50",
                           ">80th percentile"="pc80",
                           input$selection)
     if(input$startyr==input$endyr){
       tmpdata <- df_prop[df_prop$Year==input$endyr,c("Year", "Level_3", plot_select)]
       tmpdata<- rename_(tmpdata, Value=protectStr(plot_select))
       cat(file=stderr(), "If loop")
     } else{
       tmpdata <- inner_join(df_prop[df_prop$Year==input$endyr,c("Year", "Level_3", plot_select)],
                             df_prop[df_prop$Year==input$startyr,c("Year", "Level_3", plot_select)],
                             by="Level_3")
       tmpdata["Value"] <- tmpdata[3] - tmpdata[5]
       cat(file=stderr(), "else loop")
     }
     tmaptools::append_data(sg_map, tmpdata, key.shp="PLN_AREA_N",key.data="Level_3")
   })
   
   data_national <- reactive({
     if (is.null(input$selection)) {
       return(NULL)
     }
     plot_select <- switch(input$selection,
                           "<20th percentile"="pc20",
                           "<50th percentile"="pc50",
                           ">80th percentile"="pc80",
                           input$selection)
     start <- df_prop[df_prop$Year==input$startyr & df_prop$Level_3=="TOTAL" , plot_select]
     end <- df_prop[df_prop$Year==input$endyr & df_prop$Level_3=="TOTAL" , plot_select]
     c(start, end)
   })
   
   palette_choice <- reactive({
     if(input$colourscheme=="Divergent"){
       col <- "RdYlGn"
     } else{
       col <- "Blues"
     }
     colorBin(col, domain = as.numeric(unlist(data()@data["Value"])), bins = 5)
   })
   
   output$maptitle <- renderText({
     if (is.null(input$selection)) {
       return(NULL)
     }
     if(input$cat=="Race"|input$cat=="Religion"){
       grp <- "Residents"
     } else{
       grp <- "Households"
       }
     if(input$startyr==input$endyr){
       paste0("Proportion of ", grp, " in the ", input$selection," Category in ", input$startyr)
     } else{
       paste0("Change in Proportion of ", grp, " in the ", input$selection," Category from ", input$startyr, " to ", input$endyr)
     }
    
   })
   
   output$totalstat <- renderText({
     if (is.null(input$selection)) {
       return(NULL)
     }
     if(input$startyr==input$endyr){
       paste0("Overall Proportion in ", input$startyr,
              ": ", round(data_national()[1],2))
     } else{
       paste0("Overall Change in Proportion from ", input$startyr,
              " to ", input$endyr, ": ", round(data_national()[2] - data_national()[1],2))
     }
   })
   
   output$map <- renderLeaflet({
     
     if (is.null(data())) {
       return()
     }
     
     leaflet(sg_map) %>% 
       addProviderTiles("CartoDB.Positron",
                        options = tileOptions(minZoom=10, maxZoom=13)) %>%
       setView(103.851959,1.3521,zoom=11)
       
   })
   
   observe({
     if (is.null(data())) {
       return()
     }
    
    pal <- palette_choice()  
   
    labels <- sprintf(
      "<strong>%s</strong><br/>%g &#37;",
      data()$PLN_AREA_N, round(as.numeric(unlist(data()@data["Value"])),2)
    ) %>% lapply(htmltools::HTML)

      leafletProxy("map") %>%
       clearShapes() %>%
       addPolygons(data=data(),
                   fillColor = pal(as.numeric(unlist(data()@data["Value"]))),
                   weight = 2,
                   opacity = 1,
                   color = "white",
                   dashArray = "3",
                   fillOpacity = 0.7,
                   popup = labels) %>%
        clearControls() %>%
        addLegend(pal = pal, values = as.numeric(unlist(data()@data["Value"])), opacity = 0.7,
                  title = NULL, position= "bottomright")
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

