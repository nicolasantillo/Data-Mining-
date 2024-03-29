packs <- c("tidyverse", "magrittr", "highcharter", "leaflet", "lubridate", 'shinycssloaders', 'dslabs', 'mclust', 'cluster', 'GGally', 'dendextend', 'tidyr' , 'kableExtra', 'FactoMineR', 'NbClust',
           "shiny", "shinydashboard", "shinyWidgets", 'data.table', 'leaflet.extras','geojsonio','ggtext','jsonlite', "shinythemes",'DT', "shinyjs", "shinyauthr", 'plotly', 'ggplot2', 'factoextra')
lapply(packs, require, character.only = TRUE)

library(shiny) # shiny features
library(shinydashboard) # shinydashboard functions
library(DT)  # for DT tables
library(dplyr)  # for pipe operator & data manipulations
library(plotly) # for data visualization and plots using plotly 
library(ggplot2)
library(gamlss)
library(shinyauthr)
library(lme4)
library(lmerTest)
library(shinyjs)
library(DT)  
library(ggtext) 
library(maps) 
library(ggcorrplot) 
library(shinycssloaders) 
library(GGally)
library(rnaturalearth)
library(devtools)
library(ggdark)
library(leaflet)
library(fmsb)
library(shinyBS)
library(babynames)
library(hrbrthemes)
library(viridis)
library(psych)
library(rmarkdown)
library(knitr)
library(shinyWidgets)
library(ggridges)
library(tidyverse)
library(recipes)
library(shinyalert)        
library(plyr)             
library(ggrepel)     
library(caret)
library(InformationValue)
library(pander)
library(survey)
library(rayshader)
library(ppsr)
library(correlationfunnel)
library(patchwork)
library(ggdist)
library(tidyquant)
library(collapsibleTree)
library(igraph)
library(tidyverse)
library(RColorBrewer)
library(ggraph)
library(ggThemeAssist)
library(dygraphs)
library(ggiraph)
library(timetk)
library(radar)
library(corrplot)
library(ggforce)
library(modeldata)
library(modeltime)
library(tidymodels)
library(lubridate)
library(ggjoy)
library(MASS)
library(ggside)
library(gghalves)
library(forecast)
library(dygraphs)
library(prophet)
library(ggalt)
library(ggExtra)
library(scales)
library(gganimate)
library(gapminder)
library(reshape2)
library(ERSA)
library(shinycssloaders)
library(rio)
library(stargazer)
library(magrittr)
library(cluster)
library(cluster.datasets)
library(cowplot)
library(NbClust)
library(clValid)
library(ggfortify)
library(clustree)
library(dendextend)
library(FactoMineR)
library(ggiraphExtra)
library(knitr)
library(kableExtra)
library(clustvarsel) # mclust
library(sparcl) # sparse k-means
library(vscc) # teigen and mclust
library(SelvarMix) # Lasso as in Zhou et al. (2009)
library(Rmixmod)
library(glasso)
library(ContaminatedMixt)
library(teigen)
library(parameters)
library(see)
library(cluster)


olive2 <- olive
# standardizzo le variabili creando un nuovo data.frame
olive.scaled <- data.frame(scale(olive))
# creo un data.frame con i valori standardizzati, cambiando i nomi alle regioni e alle area-
olive.nom.scaled <- olive.scaled
#
switch_Region <- function(val){
  switch (as.character(val),
          '-0.813171041183559' = 'Sud-Italia',
          '0.34966354770893' = 'Sardegna',
          '1.51249813660142' = 'Nord-Italia')}
#
olive.nom.scaled$Region <- sapply(olive.nom.scaled$Region, switch_Region)
#
switch_Area <- function(val){
  switch (as.character(val),
          '-1.5274195447917' = 'Puglia-Nord',
          '-1.10309512535467' = 'Calabria',
          '-0.678770705917633' = 'Puglia-Sud',
          '-0.254446286480599' = 'Sicilia',
          '0.169878132956435' = 'Sardegna Interna',
          '0.594202552393469' = 'Sardegna Costiera',
          '1.0185269718305' =  'Liguria-Est',
          '1.44285139126754' = 'Liguria-Ovest',
          '1.86717581070457' = 'Umbria'
          
  )
}
#
olive.nom.scaled$Area <- sapply(olive.nom.scaled$Area, switch_Area)
# dataset per modelli
olive.feat.scaled <- olive.scaled[,c(3,4,5,6,7,8,9,10)]
# pca grafici
ris.pca <- PCA(olive.feat.scaled, graph = FALSE)
#

#per lanciare i due grafici cluster poi vi spiego meglio
{mc <- Mclust(olive.feat.scaled)
  dens <- densityMclust(olive.feat.scaled)}

final <- kmeans(olive.feat.scaled, 5, nstart = 30)

#
olive.perc <- olive

olive.dist <- dist(olive.feat.scaled, method = "euclidean")
hc.out_olive <- hclust(olive.dist, method = "ward.D2")

#Generazione mappa
{df <- read_delim("olive.csv", delim = ",")
  df$longitude <- as.numeric(df$longitude)
  df$latitude <- as.numeric(df$latitude)
  df$tot <- as.numeric(df$tot)
  bins <- c(100, 150, 200, 250, 300, 400, 450, 500, Inf)
  pal <- colorBin("YlOrRd", domain = df$area, bins = bins)
  labels <- str_c("<strong>", df$area,"</strong>",
                  " totale: ", df$tot, "<br>" ) %>%
    lapply(htmltools::HTML)}

# selezione variabili

#vscc

x.data <- olive.feat.scaled

clust.vscc <- vscc(x.data, G=1:5, automate = "mclust", initial = NULL, train = NULL, forcereduction = FALSE)


# clustvarsel

clust.f <- clustvarsel(x.data,G=1:5) #

clust.b <- clustvarsel(x.data,G=1:5,direction = "backward")


# teigen
olive.teigen <- teigen(x.data, G=1:5, models = "all", verbose = TRUE)


# lasso
lambda <- seq(0.1, 100, length = 25)
rho <- seq(1, 2, length=2)
lasso <- SelvarClustLasso(x=olive.feat.scaled, nbcluster=1:5,criterio="ICL",lambda=lambda,rho=rho)


# contaminato

olive.contaminated <- CNmixt(olive.feat.scaled, G = 1:5)


ui <- {dashboardPage( skin = "green",
                      dashboardHeader(title = "Oli Italiani", titleWidth = 600, dropdownMenu(type = "messages",
                                                                                             notificationItem(
                                                                                               text = tags$p("Sviluppato da: Fermo, Ianeri, Santillo", color = "white"),
                                                                                               icon("info")))),
                      dashboardSidebar(
                        sidebarMenu(id = "sidebar",
                                    #Creiamo un menu per il dataset, assegnando un identificativo e un'icona
                                    menuItem("Introduzione", tabName = "intro", icon = icon("hand-point-right", class = "horizontal")),
                                    
                                    menuItem("Dataset", tabName = "data", icon = icon("book", class = "pulse")),
                                    
                                    menuItem("Selezione delle variabili", icon = icon("magnifying-glass", class = "tada"),
                                             menuItem("vscc", tabName = "vscc", icon = icon("signal")),
                                             menuItem("clustvarsel", tabName = "clustvarsel", icon = icon("signal"))),
                                    
                                    menuItem("Analisi Descrittiva", tabName = "rel", icon = icon("chart-line", class = "wrench"),
                                             #Creiamo un sotto menù all'interno del menu Grafici, assegnando un identificcativo e icona
                                             menuSubItem("Bar-plot", tabName = "bar", icon = icon("chart-bar")),
                                             menuSubItem("Scatter-Plot", tabName = "scatter", icon = icon("signal")),
                                             menuSubItem("Correlazione", tabName = "bar1", icon = icon("signal"))),
                                    
                                    #Creiamo un menu per la regressione, assegnando un identificativo e un'icona
                                    menuItem("Analisi Clustering", icon = icon("magnifying-glass", class = "tada"),
                                             #Creiamo un sotto menù all'interno del menu Regressione, assegnando un identificcativo e icona
                                             menuSubItem("Clustering k-Means", tabName = "k-mean", icon = icon("signal")),
                                             menuSubItem("N° ottimale di clsuter", tabName = "deter", icon = icon("signal")),
                                             menuSubItem("Clustering basato sul Modello", tabName = "model", icon = icon("signal")),
                                             menuSubItem("Clustering Gerarchico", tabName = "gerar", icon = icon("signal")),
                                             menuSubItem("Matrice di Confusione", tabName = "plot", icon = icon("signal"))),
                                    
                                    menuItem("Teigen", tabName = "teigen", icon = icon("info")),
                                    
                                    menuItem("Lasso", tabName = "lasso", icon = icon("info")),
                                    
                                    menuItem("Contaminato", tabName = "contaminato", icon = icon("info")),
                                    
                                    menuItem("Conclusioni", tabName = "end", icon = icon("hand-point-right", class = "horizontal")))),
                      
                      #Tramite il seguente comando inseriamo nelle finestre i contenuti da mostrare
                      dashboardBody(
                        tabItems(
                          
                          tabItem(tabName = "intro",
                                  tabBox(id="t0", width = 12,
                                         tabPanel("Una breve introduzione", icon = icon("binoculars")),
                                         fluidRow(
                                           column(width = 8, tags$img(src='https://www.medicalindependent.ie/wp-content/uploads/2021/04/Page-36-Food-and-Drink-2048x1365.jpg', width = 600, height  = 300, align = "center"))),
                                         fluidRow(
                                           column(width = 12, tags$br(),
                                                  tags$h4(strong("In quale regione italiana si produce l'olio migliore ?")),
                                                  tags$br(),
                                                  tags$p(strong("Acido palmitico:"), "Acido grasso saturo solido ottenuto da grassi vegetali e animali."),
                                                  tags$p(strong("Acido palmitoleico:"), "Agisce efficacemente sulla perdita di peso a livello del giro vita, possiede proprietà rigenerative, idratanti e antinvecchiamento per la pelle, ma possiede anche agenti protettivi per una corretta digestione e per un sistema urogenitale in salute."),
                                                  tags$p(strong("Acido linolenico:"), "Acido grasso polinsaturo (con un doppio legame in più rispetto all'acido linoleico) presente come gliceride nei semi di lino."),
                                                  tags$p(strong("Acido linoleico:"), "Svolge un ruolo nel mantenimento in salute delle membrane cellulari e nella crescita cellulare e sarebbe coinvolto nella sintesi di alcune molecole implicate in importanti processi come la coagulazione del sangue e la mediazione della risposta infiammatoria."),
                                                  tags$p(strong("Acido oleico:"), "Ha proprietà antiossidanti ed è in grado di mantenere nella norma di livelli di colesterolo nel sangue."),
                                                  tags$p(strong("Acido stearico:"), "Acido grasso saturo solido ottenuto da grassi animali o vegetali."),
                                                  tags$p(strong("Acido eicosenoico:"), "E' un acido grasso monoinsaturo omega-9 presente in una varietà di oli vegetali e frutta a guscio; olio di jojoba. È uno dei vari acidi eicosenoici."),
                                                  tags$p(strong("Acido arachidico:"), "Contribuisce al mantenimento di livelli normali di colesterolo nel sangue."))))),
                          #Creiamo una finestra per mostrare la composizione del dataset
                          tabItem(tabName = "data",
                                  tabBox(id="t1", width = 12,
                                         #Creiamo un pannello assegnando il titolo, l'identificativo per l'output, e un icona
                                         tabPanel("Dataset olive", dataTableOutput("dataT"), icon = icon("hand-point-down")),
                                         tabPanel("Dataset descrittivo", dataTableOutput("dataT3"), icon = icon("hand-point-down")),
                                         tabPanel("Dataset per clustering", dataTableOutput("dataT2"), icon = icon("hand-point-down")))),
                          
                          tabItem(tabName = "vscc",
                                  tabBox(id = "vscc", width = 12,
                                         tabPanel("subset", verbatimTextOutput("subsetvscc"), icon = icon("info")),
                                         tabPanel("area", verbatimTextOutput("areavscc"), icon = icon("info")),
                                         tabPanel("regione", verbatimTextOutput("regionevscc"), icon = icon("info")),
                                         tabPanel("regione teigen", verbatimTextOutput("regionevsccteigen"), icon = icon("info")))),
                          
                          tabItem(tabName = "clustvarsel",
                                  tabBox(id = "clustvarsel", width = 12,
                                         tabPanel("subset.f", verbatimTextOutput("subsetclustvarsel.f"), icon = icon("info")),
                                         tabPanel("area.f", verbatimTextOutput("areaclustvarsel.f"), icon = icon("info")),
                                         tabPanel("regione.f", verbatimTextOutput("regioneclustvarsel.f"), icon = icon("info")),
                                         
                                         tabPanel("subset.b", verbatimTextOutput("subsetclustvarsel.b"), icon = icon("info")),
                                         tabPanel("area.b", verbatimTextOutput("areaclustvarsel.b"), icon = icon("info")),
                                         tabPanel("regione.b", verbatimTextOutput("regioneclustvarsel.b"), icon = icon("info")))),
                          
                          tabItem(tabName = "teigen",
                                  tabBox(id = "teigen", width = 12,
                                         
                                         tabPanel("summary", verbatimTextOutput("summaryt"), icon = icon("info")),
                                         tabPanel("table", verbatimTextOutput("tablet"), icon = icon("info")),
                                         tabPanel("indice", verbatimTextOutput("indicet"), icon = icon("info")))),
                          
                          
                          tabItem(tabName = "lasso",
                                  tabBox(id = "teigen", width = 12,
                                         
                                         tabPanel("summary", verbatimTextOutput("summaryl"), icon = icon("info")),
                                         tabPanel("table", verbatimTextOutput("tablel"), icon = icon("info")))),
                          
                          tabItem(tabName = "contaminato",
                                  tabBox(id = "contaminato", width = 12,
                                         
                                         tabPanel("summary", verbatimTextOutput("summarycontaminato"), icon = icon("info")))),
                          
                          tabItem(tabName = "bar1",
                                  tabBox(id="t9", width = 12,
                                         tabPanel("Variabili", plotOutput("pca2"), icon = icon("chart-line")),
                                  )),
                          
                          #Creiamo una tabella per studiare i grafici, ottenuti con il pacchetto GGPLOT2,GGCORRPLOT
                          tabItem(tabName = "bar",
                                  tabBox(id="t3", width = 12,
                                         tabPanel("Acido palmitoleico", plotlyOutput("bar2"), icon = icon("chart-line")),
                                         tabPanel("Acido palmitico", plotlyOutput("bar1"), icon = icon("chart-line")),
                                         tabPanel("Acido stearico", plotlyOutput("bar3"), icon = icon("chart-line")),
                                         tabPanel("Acido oleico", plotlyOutput("bar4"), icon = icon("chart-line")),
                                         tabPanel("Acido linoleico", plotlyOutput("bar5"), icon = icon("chart-line")),
                                         tabPanel("Acido linolenico", plotlyOutput("bar6"), icon = icon("chart-line")),
                                         tabPanel("Acido arachidico", plotlyOutput("bar7"), icon = icon("chart-line")),
                                         tabPanel("Acido eicosenoico", plotlyOutput("bar8"), icon = icon("chart-line")),
                                         
                                  )),
                          
                          tabItem(tabName = "plot",
                                  tabBox(id="t7", width = 12,
                                         tabPanel("Matrice per area", plotlyOutput("grafico1"), icon = icon("chart-line")),
                                         tabPanel("Matrice per regione", plotlyOutput("grafico2"), icon = icon("chart-line")))),
                          tabItem(tabName = "scatter",
                                  tabBox(id="t7", width = 12,
                                         tabPanel('Oleico',plotlyOutput("olei"), icon = icon("chart-line")),
                                         tabPanel('Oleico-Palmitoleico',plotlyOutput("top"), icon = icon("chart-line")),
                                         tabPanel('Oleico-Linoleico',plotlyOutput("tol"), icon = icon("chart-line")),
                                         tabPanel('Palmitoleico-Linoleico',plotlyOutput("pallin"), icon = icon("chart-line")),
                                         tabPanel('Stearico-palmitico',plotlyOutput("salut"), icon = icon("chart-line")),                                        
                                         
                                  )),
                          
                          
                          tabItem(tabName = "end",
                                  tabBox(id="t0", width = 12,
                                         tabPanel("Conclusioni", icon = icon("binoculars"),
                                                  fluidRow(
                                                    column(width = 8,tags$img(src='https://www.heinens.com/wp-content/uploads/2021/06/Olive-Oil-Header_800x550.jpg',width = 600, heigt = 300, align = "center"))),
                                                  fluidRow(
                                                    column(width = 12, tags$br(),
                                                           tags$h4(strong("La qualità migliore")),
                                                           tags$br(),
                                                           tags$p("Grazie alla bassa percentuale di acido oleico, l'olio della", strong("Puglia del sud"),"risulta essere quello con il livello di qualità più alto"),
                                                           tags$br(),
                                                           tags$h4(strong("Il più sano")),
                                                           tags$br(),
                                                           tags$p("Grazie alla bassa percentuale di acidi grassi saturi solidi, l'olio della", strong("Puglia del nord"),"risulta essere quello più sano"),
                                                           tags$br(),
                                                           tags$p(strong("Per maggiori informazioni sulla storia culturale delle olive in Puglia potete consultare il seguente"), a(href="https://www.istitutopuglieseconsumo.it/olio-evo-pugliese-tra-storia-e-curiosita/",'link',target="_blank"),
                                                                  style="text-align:center;color:black")))),
                                         tabPanel("Mappa",icon = icon('map-pin'),leafletOutput("mymap")))),
                          
                          
                          tabItem(tabName = "k-mean",
                                  tabBox(id="t8", width = 12,
                                         tabPanel("K-means con 1 cluster", verbatimTextOutput("kmeans1"), icon = icon("chart-line")),
                                         tabPanel("K-means con 2 cluster", verbatimTextOutput("kmeans2"), icon = icon("chart-line")),
                                         tabPanel("K-means con 3 cluster", verbatimTextOutput("kmeans3"), icon = icon("chart-line")),
                                         tabPanel("K-means con 4 cluster", verbatimTextOutput("kmeans4"), icon = icon("chart-line")),
                                         tabPanel("K-means con 5 cluster", verbatimTextOutput("kmeans5"), icon = icon("chart-line")),
                                         tabPanel("K-means con 6 cluster", verbatimTextOutput("kmeans6"), icon = icon("chart-line")),
                                         tabPanel("K-means con 7 cluster", verbatimTextOutput("kmeans7"), icon = icon("chart-line")),
                                         tabPanel("K-means con 8 cluster", verbatimTextOutput("kmeans8"), icon = icon("chart-line")),
                                         tabPanel("K-means con 9 cluster", verbatimTextOutput("kmeans9"), icon = icon("chart-line")),
                                         tabPanel("K-means con 10 cluster", verbatimTextOutput("kmeans10"), icon = icon("chart-line")),
                                  )),
                          tabItem(tabName = "deter",
                                  tabBox(id = "t10", width = 12,
                                         tabPanel("Metodo Elbow", plotOutput("elbow"), icon = icon("chart-line")),
                                         tabPanel("Metodo Silhouette", plotOutput("silhouette"), icon = icon("chart-line")),
                                         tabPanel("Metodo Statistico GAP", plotOutput("gap"), icon = icon("chart-line")),
                                         tabPanel("Algoritmo basato sul consenso", plotOutput("consens"), icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 12, tags$br(),
                                                           tags$h4(strong("La scelta di 5 cluster è supportata da 9 (32,14%) metodi su 28 (Silhouette, Ch, Hartigan, Tracew, Rubin, DB, PtBiserial, Dunn, SDindex)."))))),
                                         tabPanel("Clusters Silhouette Plot", plotOutput("vis1"), icon = icon("chart-line")),
                                         tabPanel("Cluster Plot", plotOutput("vis2"), icon = icon("chart-line")),
                                  )),
                          tabItem(tabName = "gerar",
                                  tabBox(id = "t11", width = 12,
                                         tabPanel("Dendrogramma ad albero", plotOutput("tree"), icon = icon("chart-line")),
                                  )),
                          tabItem(tabName = "model",
                                  tabBox(id="t9", width = 12,
                                         tabPanel("Grafico BIC",plotOutput("mlust1"), icon = icon("chart-line")),
                                         tabPanel("Grafico Classificazione, k=9",plotOutput("mlust5"), icon = icon("chart-line")),
                                         tabPanel("Grafico Densità, k=9",plotOutput("mlust2"), icon = icon("chart-line")),
                                         tabPanel("Grafico Densità 3D, k=9",plotOutput("mlust3"), icon = icon("chart-line")),
                                         tabPanel("Grafico Incertezza, k=9",plotOutput("mlust4"), icon = icon("chart-line")),
                                         tabPanel("Grafico Classificazione, k=5",plotOutput("mlust9"), icon = icon("chart-line")),
                                         tabPanel("Grafico Densità, k=5",plotOutput("mlust6"), icon = icon("chart-line")),
                                         tabPanel("Grafico Densità 3D, k=5",plotOutput("mlust7"), icon = icon("chart-line")),
                                         tabPanel("Grafico Incertezza, k=5",plotOutput("mlust8"), icon = icon("chart-line")),
                                  ))
                        )))}

#Comandi per rendere l'interfaccia utente più reattiva
server <- function(input,output,session) {
  #Usando il comando output avviamo il dataset positivi
  output$dataT <- renderDataTable(olive2)
  #Rendiamo operativa la selezione delle osservazioni nella barra comando grafici
  {dataset <- reactive({
    olive2[sample(nrow(olive2), input$sampleSize)]})}
  
  #Usando il comando output avviamo il dataset positivi
  output$dataT2 <- renderDataTable(olive.feat.scaled)
  #Rendiamo operativa la selezione delle osservazioni nella barra comando grafici
  {dataset <- reactive({
    olive.feat.scaled[sample(nrow(olive.feat.scaled), input$sampleSize)]})}
  
  output$dataT3 <- renderDataTable(olive.perc)
  #Rendiamo operativa la selezione delle osservazioni nella barra comando grafici
  {dataset <- reactive({
    olive.perc[sample(nrow(olive.perc), input$sampleSize)]})}
  
  {output$pca1 <- renderPlotly({
    pca <- fviz_screeplot(ris.pca, addlabels = TRUE, ylim = c(0, 50))
    print(pca)})}
  
  {output$pca2 <- renderPlot({
    pca1 <- fviz_pca_var(ris.pca, col.var = 'contrib', gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'), repel = TRUE) + theme_classic()
    
    print(pca1)}, height = 700)}
  
  {output$pca3 <- renderPlotly({
    pca2 <- fviz_contrib(ris.pca, choice = 'var', axes = 1, top = 10)
    print(pca2)})}
  
  {output$pca4 <- renderPlotly({
    pca3 <-fviz_contrib(ris.pca, choice = 'var', axes = 2, top = 10)
    print(pca3)})}
  
  {output$pca5 <- renderPlotly({
    pca4 <- fviz_contrib(ris.pca, choice = 'var', axes = 3, top = 10)
    print(pca4)})}
  
  
  
  {output$grafico1 <- renderPlotly({
    
    olive.nom.scaled.clust <- olive.nom.scaled
    
    olive.nom.scaled.clust$cluster <- final$cluster
    
    olive.nom.scaled.clust$cluster <- as.character(olive.nom.scaled.clust$cluster)
    
    confusion_matrix_region <- olive.nom.scaled.clust %>%
      dplyr::select(Region, Area, cluster) %>%
      dplyr::mutate(idx = 1) %>%
      dplyr::group_by(Region, Area, cluster) %>%
      dplyr::summarise(Frequency = sum(idx), .groups = "keep") %>%
      spread(key = "cluster", value = "Frequency", fill = 0) %>%
      dplyr::arrange(Region, Area)
    
    confusion_matrix_region %>%
      gather(key = "cluster", value = "Frequency", -Area, -Region) %>%
      dplyr::mutate(cluster = as.numeric(cluster)) %>%
      ggplot(aes(
        x = cluster,
        y = fct_reorder(Area, dplyr::desc(Region)),
        fill = Frequency,
        label = Frequency
      )) +
      geom_tile() +
      geom_text() +
      scale_fill_gradient(low = "#E7B800", high = "#FC4E07", guide="none") +
      labs(
        x = ' ',
        y = ' ') +
      theme_light()
  })}
  
  
  {output$grafico2 <- renderPlotly({
    
    
    olive.nom.scaled.clust <- olive.nom.scaled
    
    olive.nom.scaled.clust$cluster <- final$cluster
    
    olive.nom.scaled.clust$cluster <- as.character(olive.nom.scaled.clust$cluster)
    
    confusion_matrix_region <- olive.nom.scaled.clust %>%
      dplyr::select(Region, Area, cluster) %>%
      dplyr::mutate(idx = 1) %>%
      dplyr::group_by(Region, Area, cluster) %>%
      dplyr::summarise(Frequency = sum(idx), .groups = "keep") %>%
      spread(key = "cluster", value = "Frequency", fill = 0) %>%
      dplyr::arrange(Region, Area)
    
    
    table(olive.nom.scaled.clust %>% dplyr::select(Region, cluster)) %>%
      data.frame() %>%
      ggplot(aes(
        x = cluster,
        y = fct_reorder(Region, desc(Region)),
        fill = Freq,
        label = Freq
      )) +
      geom_tile() +
      geom_text() +
      scale_fill_gradient(low = "#E7B800", high = "#FC4E07" , guide="none") +
      labs(  
        x = ' ',
        y = ' ') +
      theme_light()
  })}
  
  
  output$mymap <- renderLeaflet({
    leaflet(regioni) %>%
      addTiles() %>%
      setView(13, 42, 5) %>%
      addProviderTiles("MapBox", options = providerTileOptions( id = "mapbox.light",
                                                                accessToken = Sys.getenv('pk.eyJ1IjoiZG9taW51c3Y4IiwiYSI6ImNsMXQzaG42aDE3YW8za3B0ajkyYmdwOWYifQ.0C1Ev7hRz5WEkCT3jzF5pA'))) %>%
      addPolygons(fillColor = 'green',
                  weight = 2, opacity = 1,color = "white",dashArray = "3",
                  fillOpacity = 0.7, label=~labels, highlightOptions = highlightOptions(weight = 5,color = "#666", dashArray = "",
                                                                                        fillOpacity = 0.7,bringToFront = TRUE),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto"))
  })
  
  
  
  {output$bar1 <- renderPlotly({
    p <- ggplot(olive.perc) +
      aes(x = palmitic, fill = region) +
      geom_histogram(bins = 30L) +
      scale_fill_hue(direction = 1) +
      labs(
        x = "Percentuale %",
        y = " ",
        title = " ",
        fill = "Gruppi di Regione"
      ) +
      theme_minimal() +
      facet_wrap(vars(area))
    print(p)})}
  
  {output$bar2 <- renderPlotly({
    p2 <- ggplot(olive.perc) +
      aes(x = palmitoleic, fill = region) +
      geom_histogram(bins = 30L) +
      scale_fill_hue(direction = 1) +
      labs(
        x = "Percentuale %",
        y = " ",
        title = " ",
        fill = "Gruppi di Regione"
      ) +
      theme_minimal() +
      facet_wrap(vars(area))
    print(p2)})}
  
  {output$bar3 <- renderPlotly({
    p3 <- ggplot(olive.perc) +
      aes(x = stearic, fill = region) +
      geom_histogram(bins = 30L) +
      scale_fill_hue(direction = 1) +
      labs(
        x = "Percentuale %",
        y = " ",
        title = " ",
        fill = "Gruppi di Regione"
      ) +
      theme_minimal() +
      facet_wrap(vars(area))
    print(p3)})}
  
  {output$bar4 <- renderPlotly({
    p4 <- ggplot(olive.perc) +
      aes(x = oleic, fill = region) +
      geom_histogram(bins = 30L) +
      scale_fill_hue(direction = 1) +
      labs(
        x = "Percentuale %",
        y = " ",
        title = " ",
        fill = "Gruppi di Regione"
      ) +
      theme_minimal() +
      facet_wrap(vars(area))
    print(p4)})}
  
  
  
  {output$bar5 <- renderPlotly({
    p5 <-  ggplot(olive.perc) +
      aes(x = linoleic, fill = region) +
      geom_histogram(bins = 30L) +
      scale_fill_hue(direction = 1) +
      labs(
        x = "Percentuale %",
        y = " ",
        title = " ",
        fill = "Gruppi di Regione"
      ) +
      theme_minimal() +
      facet_wrap(vars(area))
    print(p5)})}
  
  {output$bar6 <- renderPlotly({
    p6 <- ggplot(olive.perc) +
      aes(x = linolenic, fill = region) +
      geom_histogram(bins = 30L) +
      scale_fill_hue(direction = 1) +
      labs(
        x = "Percentuale %",
        y = " ",
        title = "",
        fill = "Gruppi di Regione"
      ) +
      theme_minimal() +
      facet_wrap(vars(area))
    print(p6)})}
  
  
  
  {output$bar7 <- renderPlotly({
    p7 <-  ggplot(olive.perc) +
      aes(x = arachidic, fill = region) +
      geom_histogram(bins = 30L) +
      scale_fill_hue(direction = 1) +
      labs(
        x = "Percentuale %",
        y = " ",
        title = " ",
        fill = "Gruppi di Regione"
      ) +
      theme_minimal() +
      facet_wrap(vars(area))
    print(p7)})}
  
  
  {output$bar8 <- renderPlotly({
    p8 <- ggplot(olive.perc) +
      aes(x = eicosenoic, fill = region) +
      geom_histogram(bins = 30L) +
      scale_fill_hue(direction = 1) +
      labs(
        x = "Percentuale %",
        y = " ",
        title = "",
        fill = "Gruppi di Regione"
      ) +
      theme_minimal() +
      facet_wrap(vars(area))
    print(p8)})}
  
  
  
  # analisi percentuale
  
  {output$olei <- renderPlotly({
    oleic <- ggplot(olive.perc) +
      aes(x = oleic, fill = area) +
      geom_histogram(bins = 30L) +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      facet_wrap(vars(area))
    print(oleic)})}
  
  {output$top <- renderPlotly({
    top1 <- ggplot(olive.perc) +
      aes(x = oleic, y = palmitoleic, colour = area) +
      geom_point(shape = "circle", size = 1.5) +
      scale_color_hue(direction = 1) +
      theme_minimal()
    print(top1)})}
  
  
  
  {output$salut <- renderPlotly({
    sal <-  ggplot(olive.perc) +
      aes(x = stearic, y = palmitic, colour = area) +
      geom_point(shape = "circle", size = 1.5) +
      scale_color_hue(direction = 1) +
      theme_minimal()
    print(sal)})}
  
  {output$pallin <- renderPlotly({
    pall <-  ggplot(olive.perc) +
      aes(x = palmitoleic, y = linoleic, colour = area) +
      geom_point(shape = "circle", size = 1.5) +
      scale_color_hue(direction = 1) +
      theme_minimal()
    print(pall)})}
  
  
  
  {output$tol <- renderPlotly({
    lot1 <- ggplot(olive.perc) +
      aes(x = oleic, y = linoleic, colour = area) +
      geom_point(shape = "circle", size = 1.5) +
      scale_color_hue(direction = 1) +
      theme_minimal()
    print(lot1)})}
  
  {output$kmeans1 <- renderPrint({
    fitk <- kmeans(olive.feat.scaled, 1)
    print(fitk)})}
  
  {output$kmeans2 <- renderPrint({
    fitk2 <- kmeans(olive.feat.scaled, 2)
    print(fitk2)})}
  
  {output$kmeans3 <- renderPrint({
    fitk3 <- kmeans(olive.feat.scaled, 3)
    print(fitk3)})}
  
  {output$kmeans4 <- renderPrint({
    fitk4 <- kmeans(olive.feat.scaled, 4)
    print(fitk4)})}
  
  {output$kmeans5 <- renderPrint({
    fitk5 <- kmeans(olive.feat.scaled, 5)
    print(fitk5)})}
  
  {output$kmeans6 <- renderPrint({
    fitk6 <- kmeans(olive.feat.scaled, 6)
    print(fitk6)})}
  
  {output$kmeans7 <- renderPrint({
    fitk7 <- kmeans(olive.feat.scaled, 7)
    print(fitk7)})}
  
  {output$kmeans8 <- renderPrint({
    fitk8 <- kmeans(olive.feat.scaled, 8)
    print(fitk8)})}
  
  {output$kmeans9 <- renderPrint({
    fitk9 <- kmeans(olive.feat.scaled, 9)
    print(fitk9)})}
  
  {output$kmeans10 <- renderPrint({
    fitk10 <- kmeans(olive.feat.scaled, 10)
    print(fitk10)})}
  
  {output$line <- renderPlot({
    cl0 <- fviz_nbclust(olive.feat.scaled, kmeans, method = 'silhouette', k.max = 10) + theme_minimal() + ggtitle('the silhouette plot')
    print(cl0)}, height = 700)}
  
  
  {output$scatter <- renderPlot({
    cl <- plot(olive.scaled[,c(-1,-2)], col =  fitk5$cluster)
    print(cl)}, height = 700)}
  
  {output$mlust1 <- renderPlot({
    bic <- fviz_mclust_bic(mc)
    print(bic)}, height = 700)}
  
  {output$mlust2 <- renderPlot({
    dens <- densityMclust(olive.feat.scaled)
    print(dens)}, height = 700)}
  
  {output$mlust3 <- renderPlot({
    dd <- plot(dens,what = 'density', type = 'persp')
    print(dd)}, height = 700)}
  
  {output$mlust5  <- renderPlot({
    model3 <- Mclust(olive.feat.scaled, modelNames = "VVE")
    class <- plot(model3, what = c("classification"))
    print(class)}, height = 700)}
  
  {output$mlust4  <- renderPlot({
    model3 <- Mclust(olive.feat.scaled, modelNames = "VVE")
    incert <- plot(model3, what = c("uncertainty"))
    print(incert)}, height = 700)}
  
  {output$mlust7 <- renderPlot({
    model2 <- Mclust(olive.feat.scaled, modelNames = "VVV")
    dd2 <- plot(model2, what = c("density"), type = 'persp')
    print(dd2)}, height = 700)}
  
  {output$mlust9  <- renderPlot({
    model2 <- Mclust(olive.feat.scaled, modelNames = "VVV")
    class2 <- plot(model2, what = c("classification"))
    print(class2)}, height = 700)}
  
  {output$mlust8  <- renderPlot({
    model2 <- Mclust(olive.feat.scaled, modelNames = "VVV")
    incert2 <- plot(model2, what = c("uncertainty"))
    print(incert2)}, height = 700)}
  
  {output$mlust6  <- renderPlot({
    model2 <- Mclust(olive.feat.scaled, modelNames = "VVV")
    dens2 <- plot(model2, what = c("density"))
    print(dens2)}, height = 700)}
  
  {output$elbow <- renderPlot({
    metodo1 <- fviz_nbclust(olive.feat.scaled, kmeans, method = "wss", k.max = 10) + geom_vline(xintercept = 5, linetype = 2) + ggtitle("Metodo di Elbow")
    print(metodo1)}, height = 700)}
  
  {output$silhouette <- renderPlot({
    metodo2 <- fviz_nbclust(olive.feat.scaled, kmeans, method = "silhouette", k.max = 10) + theme_minimal() + ggtitle("Metodo di Silhouette")
    print(metodo2)}, height = 700)}
  
  {output$gap <- renderPlot({
    metodo3 <-fviz_nbclust(olive.feat.scaled, kmeans, nstart = 20, method = "gap_stat",labs(subtitle = "Gap statistic method"))
    print(metodo3)}, height = 700)}
  
  {output$consens <- renderPlot({
    metodo4 <- n_clusters(olive.feat.scaled,package = c("easystats", "NbClust", "mclust"))
    plot(metodo4)})}
  
  {output$vis1 <- renderPlot({
    km_res <- kmeans(olive.feat.scaled, centers = 5, nstart = 20)
    metodo5 <- silhouette(km_res$cluster, dist(olive.feat.scaled))
    fviz_silhouette(metodo5)}, height = 700)}
  
  {output$vis2 <- renderPlot({
    metodo6 <- fviz_cluster(km_res, olive.feat.scaled, ellipse.type = "norm")
    plot(metodo6)}, height = 700)}
  
  
  {output$tree <- renderPlot({
    olive.dist <- dist(olive.feat.scaled, method = "euclidean")
    hc.out_olive <- hclust(olive.dist, method = "ward.D2")
    albero <- fviz_dend(hc.out_olive, cex = 1.0, lwd = 0.1, k = 5, 
                        rect = TRUE,
                        k_colors = c("jco"),
                        rect_border = "jco",
                        rect_fill = TRUE,
                        type = "phylogenic",
                        repel = TRUE,
                        phylo_layout = "layout_as_tree")
    print(albero)}, height = 700)}
  
  {output$mymap <- renderLeaflet({
    leaflet(df) %>%
      addTiles() %>%
      setView(13, 42, 5) %>%
      addProviderTiles("MapBox", options = providerTileOptions( id = "mapbox.light",
                                                                accessToken = Sys.getenv('pk.eyJ1IjoiZG9taW51c3Y4IiwiYSI6ImNsMXQzaG42aDE3YW8za3B0ajkyYmdwOWYifQ.0C1Ev7hRz5WEkCT3jzF5pA'))) %>%
      addCircleMarkers(data = df, lat = ~df$latitude, lng = ~df$longitude,
                       stroke = F, label=~labels, color = "green",
                       fillOpacity = 0.5,
                       labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                   textsize = "18px", direction = "auto"))})}
  
  
  # selezione variabili
  
  # vscc
  {output$subsetvscc <- renderPrint({
    print(head(clust.vscc$topselected))})}
  
  {output$areavscc <- renderPrint({
    print(table(olive.nom.scaled[,1], clust.vscc$bestmodel$classification))})}
  
  {output$regionevscc <- renderPrint({
    print(table(olive.nom.scaled[,2], clust.vscc$bestmodel$classification))})}
  
  # clustversal
  
  
  {output$subsetclustvarsel.f <- renderPrint({
    print(clust.f$subset)})}
  
  {output$areaclustvarsel.f <- renderPrint({
    print(table(clust.f$model$classification,olive.nom.scaled[,1]))})}
  
  {output$regioneclustvarsel.f <- renderPrint({
    print(table(clust.f$model$classification,olive.nom.scaled[,2]))})}
  
  
  {output$subsetclustvarsel.b <- renderPrint({
    print(clust.b$subset)})}
  
  {output$areaclustvarsel.b <- renderPrint({
    print(table(clust.b$model$classification,olive.nom.scaled[,1]))})}
  
  {output$regioneclustvarsel.b <- renderPrint({
    print(table(clust.b$model$classification,olive.nom.scaled[,2]))})}  
  
  
  # teigen
  
  {output$summaryt <- renderPrint({
    print(summary(olive.teigen))})}  
  
  {output$tablet <- renderPrint({
    print(table(olive.nom.scaled[,1],olive.teigen$iclresults$classification))})}  
  
  {output$indicet <- renderPrint({
    print(adjustedRandIndex(olive.nom.scaled[,1],olive.teigen$iclresults$classification))})}
  
  # lasso
  
  
  {output$summaryl <- renderPrint({
    print(summary(lasso))})}  
  
  {output$tablel <- renderPrint({
    print(table(lasso$partition,olive2[,1]))})}
  
  
  {output$subsetvsccteigen <- renderPrint({
    print(head(clust.vscc.teigen$topselected))})}
  
  {output$areavsccteigen <- renderPrint({
    print(table(olive.nom.scaled[,1], clust.vscc.teigen$bestmodel$classification))})}
  
  {output$regionevsccteigen <- renderPrint({
    print(table(olive.nom.scaled[,2], clust.vscc.teigen$bestmodel$classification))})}
  
  # contaminato
  {output$summarycontaminato <- renderPrint({
    print(olive.contaminated )})}}

#Avviamo l'app tramite il comando seguente
shinyApp(ui,server)