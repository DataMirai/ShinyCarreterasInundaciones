
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(leaflegend)
library(sf)
library(rsconnect)
rsconnect::setAccountInfo(name='mireia-camacho',
                          token='AB2B6EE886C2E58950E9A1BC7F508AFD',
                          secret='SXAR72YmdefjvRObJBBILEmJlCyfb/Fwo+7PJg4K')


carreteras <- read.csv("carreteras_cortadas.csv", sep = ";")

idx <- na.omit(unique(carreteras$CARRETERA))

#geo_carreteras_afectadas <- st_read("/Users/mireiacamacho/Desktop/ShinySevilla/Mapa_inundaciones/Tramos_de_carreteras/Tramos_de_carreteras.shp")%>%
#  select(1:4,9,11,12,17,23,27,33,41,42) %>% 
#  subset(nombre %in% idx) %>% 
 # sf::st_transform('+proj=longlat +datum=WGS84') 

#geo_carreteras_afectadas <- rmapshaper::ms_simplify(geo_carreteras_afectadas, keep = 0.01)

carreteras_afectadas <- na.omit(unique(carreteras$CARRETERA))

#c_i <- subset(geo_carreteras_afectadas, nombre %in% idx)

#geo_carreteras_afectadas <- subset(tramos_carreteras, nombre %in% idx) %>% 
#  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
#  select(1:4,9,11,12,17,23,27,33,41,42)

c <- read.csv("incidentesDGT.csv")

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

header <- shinydashboard::dashboardHeader(title = tags$a(href ="https://miraidata.es/", tags$img(src='https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/LOGO_redondas_negro_fondoBlanco.png',
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
                      selectizeInput("geocode", "Introduce el nombre de una carretera:", width= "100%",
                                  choices = unique(c$carretera), multiple = T, selected=NULL,
                                  options=list(
                                    allowEmptyOption=TRUE,
                                    showEmptyOptionInDropdown=FALSE,
                                    emptyOptionLabel="Todas")),
                      actionButton("go", "¡Buscar!", class = "btn-warning")#,
                      #highchartOutput("selectstat")
                    )
                    ))
    
    
  
))

ui <- function(){
    dashboardPage(title = "Carreteras inundadas", skin = "black", header, sidebar, body) }

server <- function(input, output, session) {
  
  filtered_data <- reactive({ 
    c %>% filter(carretera == input$geocode) 
  }) %>% bindEvent(input$go, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  
  output$map <- renderLeaflet({
    
    data <- if(is.null(input$geocode)){
      c
    }else{filtered_data()}
    
    
    n <- 40
    iconos <- iconList("INC_OHZ_OHX_NAP_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/1.png", iconWidth = n, iconHeight =n),
                       "INC_OHZ_MPA_NAP_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/1.png", iconWidth = n, iconHeight =n),
                       "INC_OHZ_VAC_NAP_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/1.png", iconWidth = n, iconHeight =n),
                       "MED_REG_DO_NAP_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/5.png", iconWidth = n, iconHeight =n),
                       "MED_REG_OTG_NAP_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/5.png", iconWidth = n, iconHeight =n),
                       "INC_OHZ_VST_NAP_CAC.png"= makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/1.png", iconWidth = n, iconHeight =n),
                       "INC_OHZ_VFR_NAP_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/1.png", iconWidth = n, iconHeight =n),
                       "INC_MET_FLD_NAP_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/8.png", iconWidth = n, iconHeight =n),
                       "MED_REG_RSR_NAP_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/5.png", iconWidth = n, iconHeight =n),
                       "MED_RCN_RAD_NAP_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/5.png", iconWidth = n, iconHeight =n),
                       "INC_FOS_OTU_NAP_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/1.png", iconWidth = n, iconHeight =n),
                       "INC_MET_RAI_NAP_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/8.png", iconWidth = n, iconHeight =n),
                       "INC_REN_TCN_LS3_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/13.png", iconWidth = n, iconHeight =n),
                       "INC_MET_FLD_LS2_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/10.png", iconWidth = n, iconHeight =n),
                       "INC_MET_FLD_LS1_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/11.png", iconWidth = n, iconHeight =n),
                       "INC_OHZ_ROC_LS1_CAC.png" = makeIcon("https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/4.png", iconWidth = n, iconHeight =n)
                       
    )
    
    html_legend <- c("<div style = 'display: grid; grid-template-columns: 1fr 1fr;'>
                 <h4 style= 'font-weight:bold;'>Incidencia</h4>
                 <h4 style= 'font-weight:bold;'>Circulación</h4>
                 
                 <div style = 'display: flex; justify-content:left; align-items:center;'><img src='https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/4.png' style='height:25px; weight:25px;'> 
                 <h5 style = 'margin-left:10px;'>Obstáculo</h5></div>
                 
                 
                 <div style = 'display: flex; justify-content:left; align-items:center;'><img src='https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/16.png' style='height:25px; weight:25px;'> 
                 <h5 style = 'margin-left:10px;'>Limitada</h5></div>
                 
                 <div style = 'display: flex; justify-content:left; align-items:center;'><img src='https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/15.png' style='height:25px; weight:25px;'>
                 <h5 style = 'margin-left:10px;'>Retenciones</h5></div>
                 
                 <div style = 'display: flex; justify-content:left; align-items:center;'><img src='https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/17.png' style='height:25px; weight:25px;'>
                 <h5 style = 'margin-left:10px;'>Congestión</h5></div>
                 
                 <div style = 'display: flex; justify-content:left; align-items:center;'><img src='https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/11.png' style='height:25px; weight:25px;'> 
                 <h5 style = 'margin-left:10px;'>Inundaciones</h5></div>
                 
                 <div style = 'display: flex; justify-content:left; align-items:center;'><img src='https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/18.png' style='height:25px; weight:25px;'> 
                 <h5 style = 'margin-left:10px;'>Retención/Corte</h5></div>
                 
                 <div style = 'display: flex; justify-content:left; align-items:center;'><img src='https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/7.png' style='height:25px; weight:25px;'>
                 <h5 style = 'margin-left:10px;'>Restricciones</h5></div>
                 
                <div style = 'display: flex; justify-content:left; align-items:center;'><img src='https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/img/19.png' style='height:25px; weight:25px;'>
                 <h5 style = 'margin-left:10px;'>Interrumpida/Corte</h5></div>
                 </div>"
    )
    
    
    leaflet() %>%
      addProviderTiles("Stadia.AlidadeSmoothDark") %>%
      setView(-3.7492, 40.4636, zoom = 7) %>% 
      #addPolylines(data = st_zm(geo_carreteras_afectadas),
       #            fill = T,
        #           stroke = T,
         #          color = 'white',
          #         weight = 2,
           #        opacity = 0.5,) %>% 
      addMarkers(data = data,
                 lng = ~lng,
                 lat = ~lat,
                 icon = ~iconos[icono],
                 #radius = 10,
                 #opacity = 0.8,
                 #fillOpacity = 0.5,
                 #color = ~pal(circulacion),
                 #clusterOptions = markerClusterOptions(),
                 popup = ~paste0("<span style='font-size: 18px;'>","<b>","📍", poblacion,", ", carretera, "</b>","</span>", "<br>", 
                                 "<span style='font-size: 14px;'>",causa, "<br>", "Del km ","<b>",pkIni,"</b>", " hasta el km ","<b>", pkFinal,"</b>", "</span>","<br>",
                                 "<span style='font-size: 14px;'>","Sentido: ", sentido, "</span>","<br>"
                 )) %>% 
      addControl(html = html_legend, position = "bottomright")
  })
  
 
 
  
}

shinyApp(ui, server)
