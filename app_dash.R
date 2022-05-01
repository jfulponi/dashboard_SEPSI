#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinysky)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(plotly)

pal5 <- colorNumeric("viridis", total[which(total$variable=="perd_v"),]$valor)

ui <- dashboardPage(
  dashboardHeader(title = "SEPSI"),
  dashboardSidebar(
           pickerInput("producto", choices = unique(total$producto), multiple = F,  options = list(
               `actions-box` = TRUE), selected = "Soja", label = "Seleccionar cultivo."
           ),
           pickerInput("campaña", choices = unique(total$campaña), multiple = F,  options = list(
             `actions-box` = TRUE), selected = "2000/01", label = "Seleccionar campaña."
           ),
           pickerInput("variable", choices = list("Producción Tendencial"="prod_tend", 
                                                  "Pérdida Absoluta" = "perd_v", 
                                                  "Pérdida Porcentual" = "perd_porc"), 
                            multiple = F,  options = list(`actions-box` = TRUE), 
                       selected = "prod_tend", label = "Seleccionar variable a graficar."
           ),
         sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard"),
            menuItem("Análisis estadístico", tabName = "rawdata"),
            hr(),
            h4(HTML('&nbsp;'), "SEPSI, ProVul-FCE-UBA"),
            h5(HTML('&nbsp;'), "Plataforma financiada por el"),
            h5(HTML('&nbsp;'), "PICT 2018-XX, MinCyT.")
          ),
           uiOutput("tab")
        ),

        dashboardBody( 
          tabItems(
            tabItem("dashboard",
                    fluidRow(
                      valueBoxOutput("rate"),
                      valueBoxOutput("count"),
                      valueBoxOutput("users")
                    ),
                    fluidRow(
        leafletOutput("leaflet",height =760)
                    )
            ),
        tabItem("rawdata",
                div(plotlyOutput("scatter", width = "80%"), align = "center"),
                div(plotlyOutput("plot", width = "80%"), align = "center")
                
        )
        )
    ))

server <- function(input, output) {
  filteredData <- reactive({
  total[which(input$campaña == total$campaña & input$producto == total$producto & input$variable == total$variable), ] 
  })

  output$rate <- renderValueBox({
    valueBox(
      value = round(sum(total[which(total$producto==input$producto & total$variable == "prod_tend" & total$campaña == input$campaña), ]$valor)),
      subtitle = "Valor tendencial de la cosecha (mill. de USD)",
      icon = icon("tractor"),
      color = "aqua"
    )
  })
  
  output$count <- renderValueBox({
    valueBox(
      value=round(sum(total[which(total$producto==input$producto & total$variable == "prod_tend" & total$campaña == input$campaña), ]$valor)-sum(abs(total[which(total$producto==input$producto & total$variable == "perd_v" & total$campaña == input$campaña), ]$valor))),
      subtitle = "Valor real de la cosecha (mill. de USD)",
      icon = icon("seedling"),
      color = if (round(sum(total[which(total$producto==input$producto & total$variable == "prod_tend" & total$campaña == input$campaña), ]$valor)-sum(abs(total[which(total$producto==input$producto & total$variable == "perd_v" & total$campaña == input$campaña), ]$valor)))>0) "green" else "red"
    )
  })
  
  output$users <- renderValueBox({
    valueBox(
      value = round(sum(total[which(total$producto==input$producto & total$variable == "perd_v" & total$campaña == input$campaña), ]$valor)),
      subtitle = "Pérdida de la cosecha (en mill. de USD)",
      icon = icon("arrow-down"),
      color = "orange"
    )
  })
  
  colorpal <- reactive({
    if(input$variable=="prod_tend"){
    colorNumeric("Greens", filteredData()$valor)}else{
    colorNumeric("Reds", filteredData()$valor, reverse = T)
    }
  })
  
  unidad <- reactive({
    if(input$variable=="perd_porc"){
      "%"}else{"M USD"}
  })
    output$leaflet <- renderLeaflet({
          leaflet() %>% 
        addProviderTiles(providers$Stamen.Toner) %>% 
        fitBounds(-69.09526, -41.03571, -56.66412, -22.45393)    
         })

    observe({
      pal <- colorpal()
      
      leafletProxy("leaflet", data = filteredData()) %>%
        clearShapes() %>%
        clearControls() %>% 
        addLegend(position = "topright", pal = pal, values = filteredData()$valor, title =  if(input$variable=="perd_porc"){
          "%"}else{"Mill. de USD"}) %>% 
        addPolygons(stroke = NA, 
                    fillOpacity = .8, smoothFactor = 0.2,
                    fillColor = ~pal(valor),
                    label = paste(round(filteredData()$valor,2), if(input$variable=="perd_porc"){
                      "%"}else{"Mill. de USD"}, "-", filteredData()$departamento))
    })
  
            output$scatter <- renderPlotly({
              filt <- total %>% 
                st_drop_geometry() %>% 
                filter(producto == input$producto, valor != 0) %>% 
                pivot_wider(names_from = variable, values_from = valor) 
              
              ggplotly(ggplot(filt)+
                geom_point(aes(perd_v, prod_tend, color = provincia, text = departamento), size = .4) +
                geom_point(data = filt %>% 
                             dplyr::filter(campaña==input$campaña), 
                           mapping = aes(perd_v, prod_tend, text = departamento),
                           size = 2, color = "red") + 
                labs(x = "Pérdida nominal (mill. USD)", 
                     y = "Producción Tendencial (mill. USD)",
                    title = "Scatterplot de Producción y Pérdidas nominales"), 
                tooltip = "text")
              
            })
            output$plot <- renderPlotly({

              filt2 <- total %>% 
                filter(producto == input$producto, campaña == input$campaña,
                       variable == input$variable)
              
              ggplotly(ggplot(filt2) +
                         geom_histogram(aes(valor, fill = provincia)) + 
                         labs(x = if(input$variable=="perd_porc"){
                           "Porcentaje"}else{"Valor (millones de USD)"}, 
                              y = "Ocurrencia",
                              title = if(input$variable=="perd_porc"){
                                "Densidad de la pérdida porcentual por provincia y departamento"}
                           else if(input$variable=="perd_v"){"Densidad de la pérdida nominal por provincia y departamento"}
                             else{"Densidad del valor de la cosecha por provincia y densidad"}),
                       tooltip = "provincia") 
              
            })
    
}

shinyApp(ui = ui, server = server)

