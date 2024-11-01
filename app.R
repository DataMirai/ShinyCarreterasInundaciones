
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(leaflegend)
library(sf)

carreteras <- read.csv("/Users/mireiacamacho/Desktop/ShinySevilla/Mapa_inundaciones/carreteras_cortadas.csv", sep = ";")

idx <- na.omit(unique(carreteras$CARRETERA))

geo_carreteras_afectadas <- st_read("/Users/mireiacamacho/Desktop/ShinySevilla/Mapa_inundaciones/Tramos_de_carreteras/Tramos_de_carreteras.shp")%>%
  select(1:4,9,11,12,17,23,27,33,41,42) %>% 
  subset(nombre %in% idx) %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') 

carreteras_afectadas <- na.omit(unique(carreteras$CARRETERA))

#c_i <- subset(geo_carreteras_afectadas, nombre %in% idx)

#geo_carreteras_afectadas <- subset(tramos_carreteras, nombre %in% idx) %>% 
#  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
#  select(1:4,9,11,12,17,23,27,33,41,42)

c <- read.csv("/Users/mireiacamacho/Desktop/ShinySevilla/Mapa_inundaciones/incidentesDGT.csv")

## Inspeccionar https://infocar.dgt.es/etraffic/, Network
#a <- fromJSON(file="/Users/mireiacamacho/Desktop/BuscarElementos.json")
#b <- Map(as.data.frame, a) 
#datarbind <- rbindlist(b) 
#c <- subset(datarbind, carretera %in% idx) %>% 
#  mutate(circulacion = case_when(
#    nivel == "NEGRO" ~ "Interrumpida",
#    nivel == "ROJO" ~ "Retención/Corte",
#    nivel == "AMARILLO" ~ "Congestión",
#    nivel == "NO APLICA" ~ "Obstáculos"
#  ))

header <- shinydashboard::dashboardHeader(title = tags$a(href ="https://miraidata.es/", tags$img(src='https://raw.githubusercontent.com/DataMirai/website/refs/heads/main/assets/images/LOGO_redondas_negro_fondoBlanco.png',
                                                                     height='40', width='210')),
                                          tags$li(class = "dropdown", style="margin:7px 20px;",
                                                  dropMenu(
                                                    dropdownButton("", status = 'warning', icon = icon('circle-info'), size="sm"), ## Status és per canviar el color del cercle i a "icon" pots posar qualsevol icona de la web Font Awesome.
                                                    div(class="dropText", tags$style(type = "text/css", ".dropText{width: 600px; max-width: 100%; padding:20px;}"),
                                                      h3(strong('Cómo ayudar a los afectados')),
                                                      br(),
                                                      h4("Estas son algunas de las organizaciones que están recogiendo fondos, alimentos o enseres de ayuda ciudadana a las víctimas de la peor Dana del siglo en España:"),
                                                      br(),
                                                      h4(strong("Cruz Roja."), "La organización pide donativos", strong("a través de su web"),", pone a disposición de los donantes el", strong("teléfono 900104971, bizum en el 33512 y sms de donación de seis euros al enviar la palabra AYUDA al 38092."), " Junto a los servicios de emergencia, Cruz Roja Española, participa en las labores de evacuación, la atención de albergues habilitados para los afectados y la recuperación de las viviendas de las zonas damnificadas."),
                                                      br(),
                                                      h4(strong("Banco de Alimentos en Valencia."), "El campo de Mestalla del Valencia CF se ha convertido en punto de depósito de alimentos y enseres de primera necesidad. Desde este miércoles a las 17:00 horas, voluntarios del Banco de Alimentos de Valencia recogerán las aportaciones en los accesos al estadio en la Avenida de Aragón."),
                                                      br(),
                                                      h5("Esta información ha sido publicada", tags$a(em("en este artículo de El País."), href="https://elpais.com/espana/comunidad-valenciana/2024-10-30/como-ayudar-a-los-afectados-por-la-dana-donaciones-y-recogida-de-comida-y-enseres.html?ssm=TW_CM"), "Pincha en el enlace para ver más.")
                                                      ),
                                                    placement = "bottom", ## Espai on et sortirà el requadre informatiu
                                                    arrow = TRUE))
                                                  )

sidebar <- dashboardSidebar(
  width = "0px"
)

body <- shinydashboard::dashboardBody(
  bootstrapPage(div(class="outer",
                    tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                    leafletOutput("map", width = "100%", height = "100%"),
                    absolutePanel(
                      top = 0, right = 20, style = "margin-top: 40px; padding:0px 20px 10px 20px; z-index:500; text-align: center; background-color: rgba(255, 255, 255, 0.2); font-family: sans-serif;",
                      tags$h2("Carreteras afectadas por la DANA", style = "color:white; font-size:40px; top:0;"),
                      tags$h5("Busca los tramos de las carreteras inundadas", style = "color:white; font-size:20px; top:0")
                      #tags$a("About this tool", href="https://cultureofinsight.com/portfolio/crimewatch/")
                    ),
                    
                    div(id="textInput", tags$style(type = "text/css", "#textInput{color:white; font-size:20px;} .btn-warning{color:white; font-weight:bold; width: 100%;}"),
                      absolutePanel(
                      top = 200, left = 20, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px; font-family: sans-serif;",
                      textInput("geocode", "Introduce el nombre de una carretera o un pueblo:", placeholder = "Carretera o pueblo", width= "100%"),
                      actionButton("go", "¡Buscar!", class = "btn-warning")#,
                      #highchartOutput("selectstat")
                    )
                    ))
    
    
  
))

ui <- function(){
    dashboardPage(title = "Carreteras inundadas", skin = "black", header, sidebar, body) }

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    pal <- 
      colorFactor(palette = c("black", "red", "yellow", "white"), 
                  levels = c("Interrumpida", "Retención/Corte", "Congestión", "Obstáculos"))
    
    leaflet() %>%
      addProviderTiles("Stadia.AlidadeSmoothDark") %>%
      setView(-3.7492, 40.4636, zoom = 7) %>% 
      addPolylines(data = st_zm(geo_carreteras_afectadas),
                   fill = T,
                   stroke = T,
                   color = 'white',
                   weight = 2,
                   opacity = 0.5,) %>% 
      addCircleMarkers(data = c,
                       lng = ~lng,
                       lat = ~lat,
                       radius = 10,
                       opacity = 0.8,
                       fillOpacity = 0.5,
                       color = ~pal(circulacion),
                       clusterOptions = markerClusterOptions(),
                       popup = ~paste0("<span style='font-size: 18px;'>","<b>","📍", poblacion,", ", carretera, "</b>","</span>", "<br>", 
                                       "<span style='font-size: 14px;'>",causa, "<br>", "Del km ","<b>",pkIni,"</b>", " hasta el km ","<b>", pkFinal,"</b>", "</span>","<br>",
                                       "<span style='font-size: 14px;'>","Sentido: ", sentido, "</span>","<br>"
                       )) %>% 
      leaflegend::addLegendFactor(pal = pal, shape = 'circle', orientation = 'horizontal',
                                  position = "bottomright",
                                  width = 33,
                                  height = 30,
                                  values = c$circulacion,
                                  #labels = c("Interrumpida", "Retención/Corte", "Congestión", "Obstáculos"),
                                  fillOpacity = .7,
                                  title = "Aviso según el estado de la circulación"
      )
  })
  
}

shinyApp(ui, server)
