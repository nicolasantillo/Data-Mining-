dataset <- data_SRHS_long
dataset2 <- within(dataset, {
  gender <- factor(gender, levels = 1:2, labels = c("Maschio", "Femmina"))
  race <- factor(race, levels = 1:3, labels = c("Bianco", "Nero", "Altro"))
  education <- factor(education, levels = 1:5, labels = c("Scuola","Diploma scuola media","Diploma scuola superiore","Università","Altro"))
  srhs <- factor(srhs, levels = 1:5, labels = c("Eccellente","Molto bene","Buono","Discreto","Scarso"))
})
attach(dataset2)
dataset3 <- dataset
switch_srhs <- function(val){
  switch (as.character(val),
          '1' = 0,
          '2' = 0,
          '3' = 0,
          '4' = 1,
          '5' = 1
  )}
dataset3$srhs <- sapply(dataset3$srhs, switch_srhs)
View(dataset3)
attach(dataset2)
attach(dataset3)


library(gtsummary)
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
#Interfaccia utente
ui <- {dashboardPage( skin = "purple",
                      dashboardHeader(title = "Stato di salute autodichiarato", titleWidth = 700),
                      #Creiamo un menù verticale con al suo interno gli item di ciò che si vuole visualizzare per studiare il dataset.
                      dashboardSidebar(
                        sidebarMenu(id = "sidebar",
                                    #Creiamo un menu per il dataset, assegnando un identificativo e un'icona
                                    menuItem("Dataset", tabName = "data", icon = icon("book")),
                                    
                                    menuItem("Panoramica", tabName = "pan", icon = icon("binoculars")),
                                    
                                    menuItem("Grafici", tabName = "rel", icon = icon("chart-line"),
                                             #Creiamo un sotto menù all'interno del menu Grafici, assegnando un identificcativo e icona
                                             menuSubItem("Bar-plot", tabName = "bar", icon = icon("chart-line"))),
                                    #Creiamo un menu per la regressione, assegnando un identificativo e un'icona
                                    menuItem("Regressione", icon = icon("book"),
                                             #Creiamo un sotto menù all'interno del menu Regressione, assegnando un identificcativo e icona
                                             menuSubItem("GLMER", tabName = "regression", icon = icon("signal")),
                                             menuSubItem("Plot-GLMER", tabName = "plot", icon = icon("signal"))))),
                      #Tramite il seguente comando inseriamo nelle finestre i contenuti da mostrare
                      dashboardBody(
                        tabItems(
                          #Creiamo una finestra per mostrare la composizione del dataset
                          tabItem(tabName = "data", 
                                  tabBox(id="t1", width = 12,
                                         #Creiamo un pannello assegnando il titolo, l'identificativo per l'output, e un icona
                                         tabPanel("Dataset SRHS", dataTableOutput("dataT"), icon = icon("book")),
                                         tabPanel("Dataset per analisi descrittiva", dataTableOutput("dataT2"), icon = icon("book")),
                                         tabPanel("Dataset per regressione", dataTableOutput("dataT3"), icon = icon("book")))),
                          tabItem(tabName = "pan", 
                                  tabBox(id="t9", width = 12, 
                                         tabPanel("Dati", plotOutput("dat"), icon = icon("info")),
                                         tabPanel("Dati", plotOutput("dat1"), icon = icon("info")))),
                          #Creiamo una tabella per studiare i grafici, ottenuti con il pacchetto GGPLOT2,GGCORRPLOT
                          tabItem(tabName = "bar", 
                                  tabBox(id="t3", width = 12, 
                                         tabPanel("Livello educazione per genere", plotlyOutput("bar1"), icon = icon("chart-line")),
                                         tabPanel("Livello educazione per razza", plotlyOutput("bar2"), icon = icon("chart-line")),
                                         tabPanel("Livello salute per genere", plotlyOutput("bar3"), icon = icon("chart-line")),
                                         tabPanel("Livello salute per razza", plotlyOutput("bar4"), icon = icon("chart-line")),
                                         tabPanel("Livello salute per educazione", plotlyOutput("bar5"), icon = icon("chart-line")),
                                         tabPanel("Livello salute per età ed educazione", plotlyOutput("bar6"), icon = icon("chart-line")))),
                          #Creiamo una tabella per studiare la regressione di questo modello 
                          tabItem(tabName = "regression",
                                  tabBox(id="t5", width = 12,
                                         tabPanel("Modello base", verbatimTextOutput("zero"), icon = icon("signal")),
                                         tabPanel("1°Modello", verbatimTextOutput("first"), icon = icon("signal")),
                                         tabPanel("2°Modello", verbatimTextOutput("second"), icon = icon("signal")),
                                         tabPanel("3°Modello", verbatimTextOutput("third"), icon = icon("signal")))),
                          tabItem(tabName = "plot", 
                                  tabBox(id="t7", width = 12, 
                                         tabPanel("Plot Residual Diagnostics", plotOutput("grafico1"), icon = icon("chart-line")),
                                         tabPanel("Plot Residual Diagnostics", plotOutput("grafico2"), icon = icon("chart-line")),
                                         tabPanel("Plot Residual Diagnostics", plotOutput("grafico3"), icon = icon("chart-line")))))))}

#Comandi per rendere l'interfaccia utente più reattiva
server <- function(input,output,session) {
  #Usando il comando output avviamo il dataset positivi
  output$dataT <- renderDataTable(data_SRHS_long)
  #Rendiamo operativa la selezione delle osservazioni nella barra comando grafici
  {dataset <- reactive({
    data_SRHS_long[sample(nrow(data_SRHS_long), input$sampleSize)]})}
  
  #Usando il comando output avviamo il dataset positivi
  output$dataT2 <- renderDataTable(dataset2)
  #Rendiamo operativa la selezione delle osservazioni nella barra comando grafici
  {dataset <- reactive({
    dataset2[sample(nrow(dataset2), input$sampleSize)]})}
  
  output$dataT3 <- renderDataTable(dataset3)
  #Rendiamo operativa la selezione delle osservazioni nella barra comando grafici
  {dataset <- reactive({
    dataset3[sample(nrow(dataset3), input$sampleSize)]})}
  
  {output$zero <- renderPrint({
    interceptonlymodel <- glmer(srhs~1 +(1|id), family = "binomial", data = dataset3, nAGQ = 5)
    summary(interceptonlymodel)})}
  
  {output$first <- renderPrint({
    mod1 <- glmer(srhs~1 + gender + education + race + age +(1|id), family = "binomial", data = dataset3, nAGQ = 5)
    summary(mod1)})}
  
  {output$second <- renderPrint({
    model2<-glmer(srhs~1 + education + race + age +(1 + age|id), data=dataset3, family = 'binomial')
    summary(model2)})}
  
  {output$third <- renderPrint({ 
    model3<-glmer(srhs~1 +  education + race  + education:age + (1+age |id), data=dataset3, family = 'binomial')
    summary(model3)})}
  
  {output$grafico1 <- renderPlot({
    ag <- gamlss(srhs~1 + gender + education + race + age +(1|id), family = BI, data = dataset3)
    plot(ag)})}
  
  {output$grafico2 <- renderPlot({
    bg <- gamlss(glmer(srhs~1 + gender + race + age +(1 + age| id)), family = BI, data = dataset3)
    plot(bg)})}
  
  {output$grafico3 <- renderPlot({
    cg<- gamlss(srhs~1 +  education + race  + education:age + (1+age |id), family = BI, data = dataset3)
    plot(cg)})}
  
  {output$det <- renderPrint({
    pdata1 <- dataset %>% select(gender, race, education)
    table1 <- tbl_summary(pdata1,type= list(c(gender,race,education)~ "categorical"))
    table1})}
  
  {output$det1 <- renderPrint({
    pdata2 <- dataset2 %>% select(srhs, t)
    table2 <- tbl_summary(pdata2,type=list(c(srhs)~ "categorical"), by= t)
    table2})}
  
  {output$bar1 <- renderPlotly({
    p <- ggplot(dataset2) +
      aes(x = gender, fill = gender) +
      geom_bar() +
      scale_fill_manual(
        values = c(Maschio = "#4EB7BA",
                   Femmina = "#FFA8F5")
      ) +
      labs(
        x = "Genere",
        title = "Livello di educazione per genere",
        fill = "Genere"
      ) +
      theme_minimal() +
      facet_wrap(vars(education))
    print(p)})}
  
  {output$bar2 <- renderPlotly({
    p2 <- ggplot(dataset2) +
      aes(x = race, fill = race) +
      geom_bar() +
      scale_fill_viridis_d(option = "viridis", direction = 1) +
      labs(
        x = "Razza",
        title = "Livello di educazione per razza",
        fill = "Razza"
      ) +
      theme_minimal() +
      facet_wrap(vars(education))
    print(p2)})}
  
  {output$bar3 <- renderPlotly({
    p3 <- ggplot(dataset2) +
      aes(x = gender, fill = gender) +
      geom_bar() +
      scale_fill_manual(
        values = c(Maschio = "#4EB7BA",
                   Femmina = "#FFA8F5")
      ) +
      labs(
        x = "Genere",
        title = "Livello di salute per genere",
        fill = "Genere"
      ) +
      theme_minimal() +
      facet_wrap(vars(srhs))
    print(p3)})}
  
  {output$bar4 <- renderPlotly({
    p4 <- ggplot(dataset2) +
      aes(x = race, fill = race) +
      geom_bar() +
      scale_fill_viridis_d(option = "viridis", direction = 1) +
      labs(
        x = "Razza",
        title = "Livello di salute per razza",
        fill = "Razza"
      ) +
      theme_minimal() +
      facet_wrap(vars(srhs))
    print(p4)})}
  
  {output$bar5 <- renderPlotly({
    p5 <- ggplot(dataset2) +
      aes(x = education, fill = education) +
      geom_bar() +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      facet_wrap(vars(srhs))
    labs(
      title = "Livello di salute per educazione",
      fill = "Educazione"
    ) +
      theme_minimal() +
      facet_wrap(vars(srhs))
    print(p5)})}
  
  
  
  {output$bar6 <- renderPlotly({
    p6 <-  ggplot(dataset2) +
      aes(x = education, y = age, fill = education) +
      geom_col() +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      facet_wrap(vars(srhs))
    labs(
      title = "Livello di salute per età ed educazione",
      fill = "Educazione"
    ) +
      theme_minimal() +
      facet_wrap(vars(srhs))
    print(p6)})}}


#Avviamo l'app tramite il comando seguente
shinyApp(ui,server)
