# librerie usate

packs <- c("tidyverse", "magrittr", "highcharter", "leaflet", "lubridate", 'shinycssloaders', 'dslabs', 'mclust', 'cluster', 'GGally', 'dendextend', 'tidyr' , 'kableExtra', 'FactoMineR', 'NbClust',
           "shiny", "shinydashboard", "shinyWidgets", 'data.table', 'leaflet.extras','geojsonio','ggtext','jsonlite', "shinythemes",'DT', "shinyjs", "shinyauthr", 'plotly', 'ggplot2', 'factoextra')
lapply(packs, require, character.only = TRUE)


olive2 <- olive


# standardizzo le variabili creando un nuovo data.frame
olive.scaled <- data.frame(scale(olive))


# creo un data.frame con i valori standardizzati, cambiando i nomi alle regioni e alle area---------------------------

olive.nom.scaled <- olive.scaled

switch_Region <- function(val){
  switch (as.character(val),
          '-0.813171041183559' = 'Sud-Italia',
          '0.34966354770893' = 'Sardegna',
          '1.51249813660142' = 'Nord-Italia'
  )
}

olive.nom.scaled$Region <- sapply(olive.nom.scaled$Region, switch_Region)

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
olive.nom.scaled$Area <- sapply(olive.nom.scaled$Area, switch_Area)

#----------------------


# vedo il nuovo data.frame
View(olive.nom.scaled)

# dataset per modelli
olive.feat.scaled <- olive.scaled[,c(3,4,5,6,7,8,9,10)]


# pca grafici

ris.pca <- PCA(olive.feat.scaled, graph = FALSE)


my_height = "30em"

model2 <- Mclust(olive.feat.scaled, modelNames = "VVV")
model3 <- Mclust(olive.feat.scaled, modelNames = "VVE")
#per lanciare i due grafici cluster poi vi spiego meglio
{mc <- Mclust(olive.feat.scaled)
  dens <- densityMclust(olive.feat.scaled)}

#kmeans
{fitk <- kmeans(olive.feat.scaled, 1)
  fitk2 <- kmeans(olive.feat.scaled, 2)
  fitk3 <- kmeans(olive.feat.scaled, 3)
  fitk4 <- kmeans(olive.feat.scaled, 4)
  fitk5 <- kmeans(olive.feat.scaled, 5)
  fitk6 <- kmeans(olive.feat.scaled, 6)
  fitk7 <- kmeans(olive.feat.scaled, 7)
  fitk8 <- kmeans(olive.feat.scaled, 8)
  fitk9 <- kmeans(olive.feat.scaled, 9)
  fitk10 <- kmeans(olive.feat.scaled, 10)}

final <- kmeans(olive.feat.scaled, 5, nstart = 30)

data("olive")
olive.perc <- olive

olive.dist <- dist(olive.feat.scaled, method = "euclidean")
hc.out_olive <- hclust(olive.dist, method = "ward.D2")
# Dati per mappa-----------------------------------

regioni <- geojsonio::geojson_read("https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_regions.geojson", what = "sp")
bins <- c(100, 150, 200, 250, 300, 400, 450, 500, Inf)
pal <- colorBin("YlOrRd", domain = regioni$reg_name, bins = bins)
labels <- str_c("<strong>", regioni$reg_name,"</strong>") %>%
  lapply(htmltools::HTML)

ui <- {dashboardPage( skin = "green",
                      dashboardHeader(title = "Oli Italiani", titleWidth = 600, dropdownMenu(type = "messages",
                                                                                             notificationItem(
                                                                                               text = tags$p("Developed by: Fermo, Ianeri, Santillo", color = "white"),
                                                                                               icon("info")))),
                      dashboardSidebar(
                        sidebarMenu(id = "sidebar",
                                    #Creiamo un menu per il dataset, assegnando un identificativo e un'icona
                                    menuItem("Introduzione", tabName = "intro", icon = icon("book")),
                                    
                                    menuItem("Dataset", tabName = "data", icon = icon("book")),
                                    
                                    menuItem("Grafici", tabName = "rel", icon = icon("chart-line"),
                                             #Creiamo un sotto menù all'interno del menu Grafici, assegnando un identificcativo e icona
                                             menuSubItem("Bar-plot", tabName = "bar", icon = icon("chart-bar")),
                                             menuSubItem("Metodo PCA", tabName = "bar1", icon = icon("signal"))),
                                    
                                    #Creiamo un menu per la regressione, assegnando un identificativo e un'icona
                                    menuItem("Clustering", icon = icon("chart-line"),
                                             #Creiamo un sotto menù all'interno del menu Regressione, assegnando un identificcativo e icona
                                             menuSubItem("Clustering k-Means", tabName = "k-mean", icon = icon("signal")),
                                             menuSubItem("Determinazione n° cluster", tabName = "deter", icon = icon("signal")),
                                             menuSubItem("Clustering per Modello", tabName = "model", icon = icon("signal")),
                                             menuSubItem("Clustering Gerarchico", tabName = "gerar", icon = icon("signal")),
                                             menuSubItem("Matrice di Confusione", tabName = "plot", icon = icon("signal")),
                                             menuSubItem("Scatter-Plot", tabName = "scatter", icon = icon("chart-line"))),
                                    menuItem("Conclusione", tabName = "end", icon = icon("book")))),
                      
                      #Tramite il seguente comando inseriamo nelle finestre i contenuti da mostrare
                      dashboardBody(
                        tabItems(
                          
                          tabItem(tabName = "intro",
                                  tabBox(id="t0", width = 12,
                                         tabPanel("Una breve introduzione", icon = icon("book")),
                                         fluidRow(
                                           title = "",
                                           img(src='https://www.ilgiornaledelcibo.it/wp-content/uploads/2014/01/propriet%C3%A0-dell-olio-extravergine-d-oliva.jpg',
                                               align = "center", style = paste0("width: 100%; height: ", my_height, ";"))),
                                         fluidRow(
                                           column(width = 12, tags$br(),
                                                  tags$h4(p(strong("In quale regione italiana si produce l'olio migliore ?"))),
                                                  tags$br(),
                                                  tags$p("Acido palmitico: Acido grasso saturo solido ottenuto da grassi vegetali e animali."),
                                                  tags$p("Acido palmitoleico: Agisce efficacemente sulla perdita di peso a livello del giro vita, possiede proprietà rigenerative, idratanti e antinvecchiamento per la pelle, ma possiede anche agenti protettivi per una corretta digestione e per un sistema urogenitale in salute."),
                                                  tags$p("Acido palmitico: Acido grasso saturo solido ottenuto da grassi vegetali e animali."),
                                                  tags$p("Acido linolenico: Acido grasso polinsaturo (con un doppio legame in più rispetto all'acido linoleico) presente come gliceride nei semi di lino."),
                                                  tags$p("Acido linoleico: Svolge un ruolo nel mantenimento in salute delle membrane cellulari e nella crescita cellulare e sarebbe coinvolto nella sintesi di alcune molecole implicate in importanti processi come la coagulazione del sangue e la mediazione della risposta infiammatoria."),
                                                  tags$p("Acido oleico: Ha proprietà antiossidanti ed è in grado di mantenere nella norma di livelli di colesterolo nel sangue."),
                                                  tags$p("Acido stearico: Acido grasso saturo solido ottenuto da grassi animali o vegetali."),
                                                  tags$p("Acido eicosenoico: E' un acido grasso monoinsaturo omega-9 presente in una varietà di oli vegetali e frutta a guscio; olio di jojoba. È uno dei vari acidi eicosenoici."),
                                                  tags$p("Acido arachidico: Contribuisce al mantenimento di livelli normali di colesterolo nel sangue."))))),
                          #Creiamo una finestra per mostrare la composizione del dataset
                          tabItem(tabName = "data",
                                  tabBox(id="t1", width = 12,
                                         #Creiamo un pannello assegnando il titolo, l'identificativo per l'output, e un icona
                                         tabPanel("Dataset olive", dataTableOutput("dataT"), icon = icon("book")),
                                         tabPanel("Dataset descrittivo", dataTableOutput("dataT3"), icon = icon("book")),
                                         tabPanel("Dataset per clustering", dataTableOutput("dataT2"), icon = icon("book")))),
                          
                          tabItem(tabName = "bar1",
                                  tabBox(id="t9", width = 12,
                                         tabPanel("Bar-plot", plotlyOutput("pca1"), icon = icon("chart-line")),
                                         tabPanel("Contributo pc1", plotlyOutput("pca3"), icon = icon("chart-line")),
                                         tabPanel("Contributo pc2", plotlyOutput("pca4"), icon = icon("chart-line")),
                                         tabPanel("Contributo pc3", plotlyOutput("pca5"), icon = icon("chart-line")),
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
                                         tabPanel("Matrice per regione", plotlyOutput("grafico2"), icon = icon("chart-line")),
                                         tabPanel(title = "Mappa",icon = icon('map-pin'), value = "map2",
                                                  fluidRow(
                                                    box(width = 12, status = 'primary', solidHeader = T, withSpinner(leafletOutput(outputId = "mymap")))))
                                         
                                         
                                  )),
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
                                         tabPanel("Conclusioni", icon = icon("book")),
                                         fluidRow(
                                           title = "",
                                           img(src='https://media.lavorincasa.it/post/19/18074/data/olio-cesta-olive-casa-naturallycurly-com.jpg',
                                               align = "center", style = paste0("width: 100%; height: ", my_height, ";"))),
                                         fluidRow(
                                           column(width = 12, tags$br(),
                                                  tags$h4(p(strong("La qualità migliore"))),
                                                  tags$br(),
                                                  tags$p("Grazie alla bassa percentuale di acido oleico, l'olio della", strong("Puglia del sud"),"risulta essere quello con il livello di qualità più alto"),
                                                  tags$br(),
                                                  tags$h4(p(strong("Il più sano"))),
                                                  tags$br(),
                                                  tags$p("Grazie alla bassa percentuale di acidi grassi saturi solidi, l'olio della", strong("Puglia del nord"),"risulta essere quello più sano"),
                                                  tags$br(),
                                                  tags$p("Per maggiori informazioni sulla storia culturale delle olive in Puglia potete consultare il seguente", a(href="https://www.istitutopuglieseconsumo.it/olio-evo-pugliese-tra-storia-e-curiosita/",'link',target="_blank"),
                                                         style="text-align:center;color:black")),
                                         ))),
                          
                          
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
                                  )),
                          tabItem(tabName = "gerar",
                                  tabBox(id = "t11", width = 12,
                                         tabPanel("Dendrogramma ad albero", plotOutput("tree"), icon = icon("chart-line")),
                                         tabPanel("Plot-Cluster", plotOutput("scatter"), icon = icon("chart-line")),
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
    class <- plot(model3, what = c("classification"))
    print(class)}, height = 700)}
  
  {output$mlust4  <- renderPlot({
    incert <- plot(model3, what = c("uncertainty"))
    print(incert)}, height = 700)}
  
  {output$mlust7 <- renderPlot({
    dd2 <- plot(model2, what = c("density"), type = 'persp')
    print(dd2)}, height = 700)}
  
  {output$mlust9  <- renderPlot({
    class2 <- plot(model2, what = c("classification"))
    print(class2)}, height = 700)}
  
  {output$mlust8  <- renderPlot({
    incert2 <- plot(model2, what = c("uncertainty"))
    print(incert2)}, height = 700)}
  
  {output$mlust6  <- renderPlot({
    dens2 <- plot(model2, what = c("density"))
    print(dens2)}, height = 700)}
  
  {output$elbow <- renderPlot({
    metodo1 <- fviz_nbclust(olive.feat.scaled, kmeans, method = "wss", k.max = 10) + theme_minimal() + ggtitle("Metodo di Elbow")
    print(metodo1)}, height = 700)}
  
  {output$silhouette <- renderPlot({
    metodo3 <- fviz_nbclust(olive.feat.scaled, kmeans, method = "silhouette", k.max = 10) + theme_minimal() + ggtitle("Metodo di Silhouette")
    print(metodo3)}, height = 700)}
  
  
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
    print(albero)}, height = 700)}}

#Avviamo l'app tramite il comando seguente
shinyApp(ui,server)