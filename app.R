#------------------------------------------------------------------------------
# FLIGHT EXPLORER
#------------------------------------------------------------------------------
# GRUPO 9:
# Carlos Rodriguez
# Francisco del Val
# Jose Lopez
# Octavio del Sueldo
# Iñigo Martiarena
#------------------------------------------------------------------------------

# LIBRERIAS
  # Shiny
library(shiny)
library(shinyWidgets)
library(shinydashboard)

  # Base de datos
library(nycflights13)
library(tidyverse)

  # Plots
library(ggplot2)
library(maps)
library(geosphere)
library(ggmap)
library(cowplot)


#------------------------------------------------------------------------------
# DATASET
    # Cargamos los datasets de vuelos y los visualizamos
data("flights")
head(flights)
data("airports")
head(airports)

# Realizamos un join con los datasets que queremos, cuya clave de union sera el 
# destino con faa
data_raw <- flights %>%
  inner_join(airports, c("dest" = "faa"))

# Realizamos un join con los datasets que queremos, cuya clave de union sera el 
# origen con faa
# De esta manera, tendremos tanto las coordenadas de destino como de origen

data_raw <- data_raw %>%
  inner_join(airports, c("origin" = "faa"))

# Seleccionamos aquellas columnas que nos interesan
data_raw <- select(.data = data_raw, c(sched_dep_time, dep_delay, arr_delay, 
                                       origin, dest, distance, 
                                       name.x, lat.x, lon.x, 
                                       name.y, lat.y, lon.y))

# Cambiamos los nombres de las variables a minusculas y sustituimos los 
# .x por dest
data_raw <- rename_with(data_raw, ~ tolower(gsub(".x", "_dest", 
                                                 .x, fixed = TRUE)))

# Cambiamos los nombres de las variables a minusculas y sustituimos los 
# .y por origin
data_raw <- rename_with(data_raw, ~ tolower(gsub(".y", "_origin", 
                                                 .x, fixed = TRUE)))

# Eliminamos los NaN
data <- drop_na(data_raw)

# Hacemos un attach para trabajar mas comodamente
attach(data)

# Una vez tenemos el dataset limpio ya podemos comenzar a trabajar con shiny


#------------------------------------------------------------------------------
# UI
ui <- dashboardPage(
    dashboardHeader(
        title = "Flight Explorer"
        ),
    dashboardSidebar(
        # Seleccionamos los aeropuertos de origen
        pickerInput(inputId = "origin",
                    label = "Origin",
                    choices = sort(unique(name_origin), decreasing = F),
                    options = list("actions-box" = TRUE),
                    multiple = TRUE, 
                    selected = "John F Kennedy Intl"
                    ),
        # Seleccionamos los aeropuertos de destino
        pickerInput(inputId = "dest",
                    label = "Destination",
                    choices = sort(unique(name_dest), decreasing = F),
                    options = list("actions-box" = TRUE),
                    multiple = TRUE,
                    selected = "Los Angeles Intl"
                    ),
        # Seleccionamos la variable agregada
        selectInput(inputId = "variable", 
                    label = "Size by", 
                    choices = list("Arrival delay" = "arr_delay", 
                                   "Departure delay" = "dep_delay", 
                                   "Distance" = "distance"),
                    multiple = FALSE,
                    selected = "distance"
                    )
    ),
    dashboardBody(
        # Plot del mapa
        plotOutput("map", 
                   brush = "selection"),
        
        # Plot de los histogtamas en box para que salga mas estetico
        fluidRow(
            box(title = "Arrival delay in minutes", 
                status = "primary", 
                plotOutput("hist_arr_delay")
                ),
            box(title = "Departure delay in minutes", 
                status = "primary", 
                plotOutput("hist_dep_delay")
                ),
            box(title = "Distance flown in milles", 
                status = "primary", 
                plotOutput("hist_dist")),
            box(title = "Scheduled departure time", 
                status = "primary", 
                plotOutput("hist_depart_time"))
            )
        
    )
)

#------------------------------------------------------------------------------
# SERVER
server <- function(input, output) {
  
  #----------------------------------------------------------------------------
  # HISTOGRAMAS
    
    # Definimos el filtro con el que haremos los plots
    data_filter <- reactive({
      # Si no has seleccionado variables el validate muestra un error
      validate(
        need(input$dest != "", "Please select destination"),
        need(input$origin != "", "Please select origin")
        )
      
      # Generamos el dataset de la seleccion (brushed)
      data_brushed <- brushedPoints(data, 
                                    input$selection, 
                                    xvar = "lon_dest", 
                                    yvar = "lat_dest")
      
      # Creamos una condición, si el usuario ha hecho una selección en el mapa,
      # utilizas ese dataset, sino utiliza las varibales indicadasd en el ui
      if (nrow(data_brushed) > 0) {
        return(data_brushed)
      } else {
          return(filter(.data = data, 
                        name_origin == input$origin,
                        name_dest == input$dest))
        }
      
    })
    
    # Ploteamos los histogramas
        
        # Histograma arr_delay
    output$hist_arr_delay <- renderPlot({
      
        # Si no hay información de los histogramas, utilizamos validate()
      validate(
        need(nrow(data_filter()) > 0, "No flights in this corridor!")
      )
        ggplot(data = data_filter(), aes(x = arr_delay)) +
            geom_histogram(col = "cadetblue4", fill = "cadetblue3") +
            xlab("Minutes")
    })
    
        # Histograma dep_delay
    output$hist_dep_delay <- renderPlot({
      
      # Si no hay información de los histogramas, utilizamos validate()
      validate(
        need(nrow(data_filter()) > 0, "No flights in this corridor!")
        )
      
      ggplot(data = data_filter(), aes(x = dep_delay)) +
        geom_histogram(col = "cadetblue4", fill = "cadetblue3") +
        xlab("Minutes")
    })
    
        # Histograma distance
    output$hist_dist <- renderPlot({
      
      # Si no hay información de los histogramas, utilizamos validate()
      validate(
        need(nrow(data_filter()) > 0, "No flights in this corridor!"),
        need(length(unique(data_filter()$distance)) != 1, 
             paste0(unique(data_filter()$distance)))
        )
      
      ggplot(data = data_filter(), aes(x = distance)) +
        geom_histogram(col = "cadetblue4", fill = "cadetblue3") +
        xlab("Milles")
    })
    
        # Histograma dep_hour
    output$hist_depart_time <- renderPlot({
      
      # Si no hay información de los histogramas, utilizamos validate()
      validate(
        need(nrow(data_filter()) > 0, "No flights in this corridor!")
        )
        
      ggplot(data = data_filter(), aes(x = sched_dep_time)) +
        geom_histogram(col = "cadetblue4", fill = "cadetblue3") +
        xlab("Minutes")
    })
    
  #----------------------------------------------------------------------------
  # MAPA

    output$map <- renderPlot({
      usa <- map_data("usa")
      states <- map_data("state")
      
      # Mapa de USA
      mapa <- ggplot() + 
        geom_polygon(data = usa, 
                     aes(x = long, y = lat, group = group), 
                     fill = NA, 
                     color = "black") + 
        coord_fixed(1.3) 
      
      # Mapa de los Estados
      mapa <- mapa + 
        geom_polygon(data = states, 
                     aes(x = long, y = lat, group = group), 
                     color = "gray35", fill = "lemonchiffon1") + 
        guides(fill = FALSE)
      
      # Mapa con los aeropuertos
      
                  # Si quisieramos ver la media de las variables agregadas (Size by)
                  # deberíamos utilizar este dataset, el problema es que no puedes hacer
                  # la media de un string, por lo que no funciona. En el ggplot, se debería
                  # cambiar input$variable por data_size$avg.
                  data_size <- data_filter() %>% 
                    group_by(name_dest, lon_dest, lat_dest) %>%
                    summarise(avg = mean(input$variable))
      
      # Ploteamos
      mapa <- mapa +
        geom_point(data = data_filter(), 
                   aes_string(x = "lon_dest", y = "lat_dest", size = input$variable), 
                   color = "tomato1", fill = "tomato1", shape = 1)
    
      mapa <- mapa +
          geom_point(data = data_filter(), 
                     aes(x = lon_origin, y = lat_origin), 
                     color = "green", fill = "green", shape = 19, size = 3) 
      mapa <- mapa +
          geom_segment(data = data_filter(),
                       aes(x = lon_origin, y = lat_origin, 
                           xend = lon_dest, yend = lat_dest), 
                       linetype = 2, lwd = 0.75)
      
      mapa + 
        theme(panel.background = element_rect(fill = "lightblue", 
                                              colour = "lightblue"))


    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
