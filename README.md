# Flight Explorer
## Practica Final Técnicas de Visualización en Data Science. CUNEF 2020
https://jose-lopez-galdon.shinyapps.io/flight_explorer/
### Integrantes del grupo
Carlos Rodriguez: c.rodriguezvina@cunef.edu

Francisco del Val: francisco.delval@cunef.edu

Jose Lopez: jose.lopez@cunef.edu

Octavio del Sueldo: hugo.delsueldo@cunef.edu

Iñigo Martiarena: inigo.martiarena@cunef.edu

### Objetivo del trabajo
Herramienta que permite explorar el dataset de delays de vuelos con dos filtros por Origen y destino.

Se muestran histogramas de varias variables y en el caso de que los filtros elijan origen y destino las variables constantes (p.e: distancia) se muestran como un número constante en vez de una distribución.

También se puede filtrar haciendo un recuadro en el mapa. Además se puede elegir el mapeo de otra variable alternativa y de una función agregadora para que el tamaño de los puntos de los aeropuertos cambie.

### Base de datos utilizada
`nycflights13`
Datos de puntualidad de la aerolínea para todos los vuelos que salen de Nueva York en 2013. También incluye 'metadatos' útiles sobre aerolíneas, aeropuertos, clima y aviones.

### Paquetes utilizados
`library(shiny)`
`library(shinyWidgets)`
`library(shinydashboard)`
`library(nycflights13)`
`library(tidyverse)`
`library(ggplot2)`
`library(maps)`
`library(geosphere)`
`library(ggmap)`
`library(cowplot)`
