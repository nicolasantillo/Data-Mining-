library(FactoMineR)
library(FactoInvestigate)
library(Factoshiny)
library(factoextra)
library(data.table)
library(DT)
library(tidyverse)
library(esquisse)
# librerie 

packs <- c("tidyverse", "magrittr", "highcharter", "leaflet", "lubridate", 'shinycssloaders', 'dslabs', 'mclust', 'cluster', 'GGally', 'dendextend', 'tidyr' , 'kableExtra', 'FactoMineR', 'NbClust', 'parameters',
           "shiny", "shinydashboard", "shinyWidgets", 'data.table','vscc','teigen', 'glasso','leaflet.extras','geojsonio','ggtext','jsonlite', "shinythemes",'DT', "shinyjs", "shinyauthr", 'plotly', 'ggplot2', 'factoextra')
lapply(packs, require, character.only = TRUE)

# dati -----------

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00373/drug_consumption.data"
drug.use <- fread(url, sep = ",", header = FALSE, showProgress = FALSE)
column.names <- c("ID", "Anni", "Sesso", "Educazione", "Paese", "Etnia", "Nevroticismo", "Estroversione", "Ape.Esperienza", "Piacevolezza", "Coscienziosità", "Impulsività", "Ric.sen", "Alcol", "Anfetamina", "Nitrito_di_amile", "Benzodiazepine", "Caffeina", "Cannabis", "Cioccolato", "Coca", "Crack", "Ecstasy", "Eroina", "Ketamine", "Legali", "LSD", "Metadone", "Funghi", "Nicotina", "Semeron", "Volatili")
setnames(drug.use, column.names)


drug.clean <- drug.use %>%
  as_tibble %>%
  mutate_at(vars(Anni:Etnia), funs(as.factor)) %>%
  mutate(Anni = factor(Anni, labels = c("18_24", "25_34", "35_44", "45_54", "55_64", "65_"))) %>%
  mutate(Sesso = factor(Sesso, labels = c("Uomo", "Donna"))) %>%
  mutate(Educazione = factor(Educazione, labels = c("Prima16", "A16", "A17", "A18", "AlcuniCollege","CetificatoProf.", "Laurea", "Master", "Dottorato"))) %>%
  mutate(Paese = factor(Paese, labels = c("USA", "Nuova Zelanda", "Altro", "Australia", "Irlanda","Canada","UK"))) %>%
  mutate(Etnia = factor(Etnia, labels = c("Nero", "Asiatico", "Bianco", "Bianco/Nero", "Altro", "Bianco/Asiatico", "Nero/Asiatico"))) %>%
  mutate_at(vars(Alcol:Volatili), funs(as.factor)) 

drug.use[drug.use == "CL0"]<- 'Mai'
drug.use[drug.use == "CL1"]<- 'Oltre un decennio fa'
drug.use[drug.use == "CL2"]<- "Nell'ultimo decennio"
drug.use[drug.use == "CL3"]<- "Nell'ultimo anno "
drug.use[drug.use == "CL4"]<- "Nell'ultimo mese"
drug.use[drug.use == "CL5"]<- "Nell'ultima settimana"
drug.use[drug.use == "CL6"]<- "Nell'ultimo giorno"

drug.rapp <- drug.use %>%
  as_tibble %>%
  mutate_at(vars(Anni:Etnia), funs(as.factor)) %>%
  mutate(Anni = factor(Anni, labels = c("18_24", "25_34", "35_44", "45_54", "55_64", "65_"))) %>%
  mutate(Sesso = factor(Sesso, labels = c("Uomo", "Donna"))) %>%
  mutate(Educazione = factor(Educazione, labels = c("Prima16", "A16", "A17", "A18", "AlcuniCollege","CetificatoProf.", "Laurea", "Master", "Dottorato"))) %>%
  mutate(Paese = factor(Paese, labels = c("USA", "Nuova Zelanda", "Altro", "Australia", "Irlanda","Canada","UK"))) %>%
  mutate(Etnia = factor(Etnia, labels = c("Nero", "Asiatico", "Bianco", "Bianco/Nero", "Altro", "Bianco/Asiatico", "Nero/Asiatico"))) %>%
  mutate_at(vars(Alcol:Volatili), funs(as.factor))

drug.rapp <- drug.rapp[-1]
drug.clean <- drug.clean[-1]

drug.emotivo <- within(drug.rapp, rm("Alcol", "Anfetamina", "Nitrito_di_amile", "Benzodiazepine", "Caffeina", "Cannabis", "Cioccolato", "Coca", "Crack", "Ecstasy", "Eroina", "Ketamine", "Legali", "LSD", "Metadone", "Funghi", "Nicotina", "Semeron", "Volatili"))
drug.sostanze.legali <- within(drug.rapp, rm("Anni", "Sesso", "Educazione", "Paese", "Etnia", "Nevroticismo", "Estroversione", "Ape.Esperienza", "Piacevolezza", "Coscienziosità", "Impulsività", "Ric.sen", "Anfetamina", "Coca", "Crack", "Ecstasy", "Eroina", "Ketamine", "LSD", "Metadone", "Funghi", "Nicotina"))
drug.sostanze.illegali <- within(drug.rapp, rm("Anni", "Sesso", "Educazione", "Paese", "Etnia", "Nevroticismo", "Estroversione", "Ape.Esperienza", "Piacevolezza", "Coscienziosità", "Impulsività", "Ric.sen", "Alcol", "Nitrito_di_amile", "Benzodiazepine", "Caffeina", "Cannabis", "Cioccolato", "Legali", "Semeron", "Volatili"))

#Creo un sottoinsieme dataset AMERICA
drug.USA <- subset(drug.clean, Paese == "USA")
drug.USA <- drug.USA[-4]

#Creo un sottoinsieme dataset REGNO UNITO
drug.UK <- subset(drug.clean, Paese == "UK")
drug.UK <- drug.UK[-4]

#Creo una mappa globale
{df <- read_delim("paesi.csv", delim = ",")
  df$longitude <- as.numeric(df$longitude)
  df$latitude <- as.numeric(df$latitude)
  df$tot <- as.numeric(df$tot)
  bins <- c(100, 150, 200, 250, 300, 400, 450, 500, Inf)
  pal <- colorBin("YlOrRd", domain = df$area, bins = bins)
  labels <- str_c("<strong>", df$area,"</strong>",
                  " totale: ", df$tot, "<br>" ) %>%
    lapply(htmltools::HTML)}

#Lancio questi 3 comandi per eseguire l'analisi MCA per osservare summary e plot
res <- MCA(drug.clean, level.ventil = 0.10, quanti.sup=6:12, quali.sup=c(1:5), method = "Burt")
res.USA <- MCA(drug.USA, quanti.sup=5:11, quali.sup=c(1:4), method = "Burt")
res.UK <- MCA(drug.UK, quanti.sup=5:11, quali.sup=c(1:4), method = "Burt")

domande <- read_delim("domande.csv", delim = ",")
genere <- read_delim("genere.csv", delim = ",")

#Comandi interfaccia utente
ui <- {dashboardPage( skin = "green",
                      dashboardHeader(title = span(tagList(tags$img(src="https://learn.eduopen.org/pluginfile.php/28/block_institution/content/157239322/Logo-Lumsa.jpg", width = "55px", height = "55px"),
                                                           "Rischio consumo di droga")), titleWidth = 600, dropdownMenu(type = "messages",
                                                                                                                        notificationItem(
                                                                                                                          text = tags$p("Sviluppato da: Fermo, Ianeri, Santillo", color = "white"),
                                                                                                                          icon("info")))),
                      dashboardSidebar(
                        sidebarMenu(id = "sidebar",
                                    #Creiamo un menu per il dataset, assegnando un identificativo e un'icona
                                    menuItem("Introduzione", tabName = "intro", icon = icon("hand-point-right", class = "faa-passing")),
                                    
                                    menuItem("Dataset", tabName = "data", icon = icon("book", class = "pulse")),
                                    
                                    menuItem("Analisi Descrittiva", tabName = "rel", icon = icon("chart-line", class = "wrench"),
                                             #Creiamo un sotto menù all'interno del menu Grafici, assegnando un identificcativo e icona
                                             menuSubItem("Crea il tuo grafico", tabName = "bar", icon = icon("palette", class = "fa-spin")),
                                             menuSubItem("Mappa", tabName = "map", icon = icon("map", class = "fa-spin"))),
                                    
                                    #Creiamo un menu per la regressione, assegnando un identificativo e un'icona
                                    menuItem("Analisi MCA", tabName = "mca", icon = icon("magnifying-glass", class = "tada"),
                                             
                                             menuItem("Tutti i paesi",icon = icon("magnifying-glass", class = "tada"),
                                                      menuSubItem("Contributo alle Dimensioni", tabName = "dim", icon = icon("chart-line")),
                                                      menuItem("Analisi delle Dimensioni", icon = icon("chart-line"),
                                                               menuSubItem("Dimesioni 1:2", tabName = "analisit12", icon = icon("chart-line")),
                                                               menuSubItem("Dimesioni 3:4", tabName = "analisit34", icon = icon("chart-line")))),
                                             
                                             menuItem("UK",icon = icon("magnifying-glass", class = "tada"),
                                                      menuSubItem("Contributo alle Dimensioni", tabName = "dimUK", icon = icon("chart-line")),
                                                      menuItem("Analisi delle Dimensioni", icon = icon("chart-line"),
                                                               menuSubItem("Dimesioni 1:2", tabName = "analisiuk12", icon = icon("chart-line")),
                                                               menuSubItem("Dimesioni 3:4", tabName = "analisiuk34", icon = icon("chart-line")))),
                                             
                                             menuItem("USA",icon = icon("magnifying-glass", class = "tada"),
                                                      menuSubItem("Contributo alle Dimensioni", tabName = "dimUSA", icon = icon("chart-line")),
                                                      menuItem("Analisi delle Dimensioni", icon = icon("chart-line"),
                                                               menuSubItem("Dimesioni 1:2", tabName = "analisiusa12", icon = icon("chart-line")),
                                                               menuSubItem("Dimesioni 3:4", tabName = "analisiusa34", icon = icon("chart-line"))))),
                                    
                                    menuItem("Conclusioni", tabName = "end", icon = icon("hand-point-right", class = "horizontal")))),
                      
                      #Tramite il seguente comando inseriamo nelle finestre i contenuti da mostrare
                      dashboardBody(
                        tabItems(
                          
                          tabItem(tabName = "intro",
                                  tabBox(id="t0", width = 12,
                                         tabPanel("Maggiori informazioni", icon = icon("binoculars"),
                                                  fluidRow(
                                                    column(width = 8, HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/oHlaz0kQlRE" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                                                  fluidPage(
                                                    column(width = 12, tags$br(),
                                                           tags$p(strong("Per scaricare il dataset originale (formato blocco note) clicca "), a(href="http://archive.ics.uci.edu/ml/machine-learning-databases/00373/drug_consumption.data",'qui',target="_blank"), style="text-align:center;color:black")))),
                                         tabPanel("Big Five", icon = icon("universal-access"),
                                                  fluidRow(
                                                    column(width = 8, tags$img(src='https://cdn.shortpixel.ai/client/to_webp,q_glossy,ret_img,w_640/https://psicoadvisor.com/wp-content/uploads/2021/12/the-big-five-personality-traits.png', width = 600, height  = 350, align = "center"))),
                                                  fluidRow(
                                                    column(width = 12, tags$br(),
                                                           tags$p(strong("Nevroticismo:"), "Gli individui che ottengono un punteggio elevato nella scala del nevroticismo hanno più probabilità della media di essere emotivamente instabili e di provare sentimenti come ansia, preoccupazione, paura, rabbia, frustrazione, invidia, gelosia, senso di colpa, umore depresso e solitudine."),
                                                           tags$p(strong("Estroversione:"), "Gli individui si rapportano al mondo privilegiando il contatto diretto e immediato con il reale, dando l'impressione di esserne immersi, soprattutto riguardo alle interazioni sociali, verso cui manifestano una grande attrazione."),
                                                           tags$p(strong("Apertura all'esperienza:"), "L'apertura mentale tende a essere normalmente distribuita, con un piccolo numero di individui che ottengono un punteggio estremamente alto o basso, e la maggior parte delle persone che ottiene un punteggio medio. Le persone che ottengono un punteggio basso in termini di apertura mentale tendono ad essere convenzionali e tradizionali nella loro visione e comportamento. Preferiscono le routine familiari alle nuove esperienze e generalmente hanno una gamma di interessi più ristretta."),
                                                           tags$p(strong("Gradevolezza:"), "Le persone che ottengono un punteggio elevato in questa dimensione sono empatiche e altruiste, mentre un punteggio basso di amicalità si traduce in comportamenti egoistici e mancanza di empatia. Coloro che ottengono un punteggio molto basso in questa dimensione mostrano segni di comportamento riferibili alla cosiddetta triade oscura, come la manipolazione e la competitività."),
                                                           tags$p(strong("Coscenziosità:"), "Gli individui che ottengono punteggi molto alti in questa scala possono anche essere maniaci del lavoro, perfezionisti e compulsivi nel loro comportamento. Le persone che ottengono un punteggio basso sulla coscienziosità tendono a essere rilassate, meno orientate agli obiettivi e meno guidate dal successo; punteggi molto bassi indicano anche una maggiore probabilità di assumere comportamenti antisociali e criminali."),
                                                           tags$p(strong("Impulsività:"), "Le persone che manifestano un tratto impulsivo sono brillanti, veloci nelle decisioni, adottano comportamenti alternativi, sono dinamici e plastici, scattanti, originali e creativi, sono spontanei e molto spesso intuitivi, si adattano ai cambiamenti e sono flessibili alle situazioni della vita, tuttavia l’impulsività può causare numerosi inconvenienti non solo relazionali, ma anche personali."),
                                                           tags$p(strong("Ricerca di sensazioni:"), "I cercatori di sensazioni elevate tendono a impegnarsi in comportamenti sessuali ad alto rischio come avere più partner sessuali, inoltre hanno anche maggiori probabilità di apprezzare i dipinti surreali rispetto a quelli rappresentativi o forme d'arte spiacevoli (definite come presenza di contenuti violenti o aggressivi o temi di morte e disperazione).")))),
                                         tabPanel("Sostanze.1", icon = icon("pills"),
                                                  fluidRow(
                                                    column(width = 8, tags$img(src='https://www.gedistatic.it/content/gedi/img/huffingtonpost/2015/12/19/160919842-850d32b3-c2c2-496d-add1-dd18e2dc697a.jpeg?webp', width = 600, height  = 350, align = "center"))),
                                                  fluidRow(
                                                    column(width = 12, tags$br(),
                                                           tags$p(strong("Alcol:"), "Gli effetti immediati dell’intossicazione da alcol possono implicare euforia, aumentata socialità, decremento dell’ansia, problemi di memoria e problemi di controllo motorio."),
                                                           tags$p(strong("Nitrito di amile:"), "Agisce provocando il rilassamento dei vasi sanguigni e aumentando l'apporto di sangue e ossigeno al cuore, riducendo contemporaneamente il carico di lavoro per il muscolo cardiaco."),
                                                           tags$p(strong("Benzodiazepina:"), "L'uso di benzodiazepina può causare una eccessiva sedazione oltre a sonnolenza e amnesia anterograda, termine col quale si intende la difficoltà nel memorizzare informazioni nuove."),
                                                           tags$p(strong("Caffeina:"), "E' lo stimolante naturale più comunemente presente nelle piante di tè, caffè e cacao. Funziona stimolando il cervello e il sistema nervoso centrale, aiutando a rimanere vigili e a combattere la stanchezza."),
                                                           tags$p(strong("Cannabis:"), "L'effetto principale è analgesico e rilassante. Ciò avviene perché le principali sostanze contenute nella marijuana interagiscono con i recettori endocannabinoidi, particolari proteine responsabili della regolazione di dolore, appetito, umore e memoria."),
                                                           tags$p(strong("Cioccolato:"), "Abbassa la pressione sanguigna, e i semi del fondente, in particolare, aumentano la concentrazione di antiossidanti nel sangue; per le medesime ragioni il cioccolato si rivela anche un balsamo per la memoria: riattivando la circolazione sanguigna, agisce anche sul sistema nervoso favorendo le meccaniche cognitive."),
                                                           tags$p(strong("Droghe legali:"), "Si riferisce a delle sostanze psicotrope che si presume non siano in grado di indurre dipendenza fisica o quadri clinici di avvelenamento acuto a seguito di sovradosaggio. Nella maggior parte dei casi, con il termine (droga leggera) ci si riferisce alle infiorescenze della Cannabis e le sostanze psicotrope da esse ricavabili come l'hashish e le resine ad alto tenore di THC."),
                                                           tags$p(strong("Abuso di sostanze volatili:"), "Alcuni soggetti accusano capogiri, sonnolenza e stato confusionale. L’eloquio può essere disarticolato. Possono anche presentare difficoltà a restare in piedi e a deambulare, con conseguente andatura instabile. I consumatori possono manifestare eccitazione, impulsività e irritabilità."),
                                                           tags$p(strong("Semeron:"), " E' una droga fittizia che è stata introdotta per identificare i richiedenti eccessivi.")))),
                                         tabPanel("Sostanze.2", icon = icon("syringe"),
                                                  fluidRow(
                                                    column(width = 8, tags$img(src='https://best5.it/b5/wp-content/uploads/2017/04/Droghe-e-sostanze-stupefacenti-800x400.jpg', width = 600, height  = 300, align = "center"))),
                                                  fluidRow(
                                                    column(width = 12, tags$br(),
                                                           tags$p(strong("Anfetamine:"), "Viene utilizzato in alcuni Stati per il trattamento del disturbo da deficit di attenzione/iperattività nei bambini e negli adulti come antidepressivo, psicostimolante e anoressizzante."),
                                                           tags$p(strong("Cocaina:"), " E' una sostanza stupefacente che agisce come potente stimolante del sistema nervoso centrale, vasocostrittore e anestetico; crea dipendenza; è la seconda droga illegale più utilizzata a livello globale, dopo la cannabis. I sintomi principali sono perdita di contatto con la realtà e sensazioni di felicità o agitazione. I sintomi fisici possono includere battito cardiaco accelerato, sudorazione aumentata e dilatazione delle pupille."),
                                                           tags$p(strong("Crack:"), "Un consumo continuato e prolungato può provocare psicosi, stati paranoici accompagnati da deliri e allucinazioni, aggressività e alienazione."),
                                                           tags$p(strong("Ecstasy:"), "L'uso di ecstasy può causare insonnia, depressione, attacchi di panico e paranoie, che possono anche perdurare nel tempo, a distanza dall'assunzione. L'uso continuativo può inoltre condurre ad uno stato di stress psicofisico dovuto ad un eccessivo affaticamento, ipertensione e a calo di peso corporeo."),
                                                           tags$p(strong("Eroina:"), "I più importanti effetti collaterali a lungo termine riguardano principalmente la possibilitá dell'instaurarsi di una dipendenza fisica e psichica. Una volta instaurata la dipendenza, il tossicodipendente è costretto a continuare ad assumere la sostanza per evitare le crisi d'astinenza. La crisi di astinenza dura in media da 48-72 ore fino a una settimana e si presenta circa 8 ore dopo l'ultima dose"),
                                                           tags$p(strong("Ketamina:"), "E' utilizzata a scopo stupefacente per via dei suoi effetti allucinogenici di tipo dissociativo, nonché forte analgesia, e trova perciò ampio uso specie nella scena rave."),
                                                           tags$p(strong("LSD:"), "Agisce sia sul sistema nervoso centrale sia su quello periferico inducendo allucinazioni acustiche e visive e, in generale, distorsioni percettive e della consapevolezza del tempo, dello spazio e del sé."),
                                                           tags$p(strong("Metadone:"), "Si usa per alleviare il dolore grave negli individui che hanno necessità di prendere antidolorifici 24 ore su 24 e che non possono assumere altri medicinali; si usa anche per prevenire i sintomi dell’astinenza nelle persone che cercano di disintossicarsi da droghe oppiacee."),
                                                           tags$p(strong("Funghi:"), "I loro effetti sono simili a quelli dell'LSD anche se molto meno intensi."),
                                                           tags$p(strong("Nicotina:"), "Provoca in chi la consuma sensazioni di rilassamento, controllo di sé ed euforia, ma di breve durata: l’organismo umano, infatti, impiega circa due ore di tempo per dimezzare il livello di nicotina nel sangue. È questo il motivo principale per cui il fumatore diventa preda di una vera e propria dipendenza, che lo porta ad avere costantemente esigenza di accendere una nuova sigaretta.")))))),
                          
                          
                          #Creiamo una finestra per mostrare la composizione del dataset
                          tabItem(tabName = "data",
                                  tabBox(id="t1", width = 12,
                                         #Creiamo un pannello assegnando il titolo, l'identificativo per l'output, e un icona
                                         tabPanel("Identificativo & Big Five", dataTableOutput("dataT"), icon = icon("universal-access")),
                                         tabPanel("Sostanze.1", dataTableOutput("dataT2"), icon = icon("pills")),
                                         tabPanel("Sostanze.2", dataTableOutput("dataT3"), icon = icon("syringe")))),
                          
                          tabItem(tabName = "bar",
                                  tabBox(id="t2", width = 12,
                                         tabPanel(title = "Esquisse", icon = icon("palette"), esquisserUI(id = "esquisse", header = TRUE, choose_data = FALSE)))),
                          #Creiamo una tabella per studiare i grafici, ottenuti con il pacchetto GGPLOT2,GGCORRPLOT
                          tabItem(tabName = "map",
                                  tabBox(id="t3", width = 12,
                                         tabPanel("Mappa Globale", leafletOutput("mymap"), icon = icon("chart-line")),
                                  )),
                          
                          # TUTTI I DATASET
                          
                          tabItem(tabName = "analisit12",
                                  tabBox(id="t4", width = 12, height = 910,
                                         tabPanel("Corr", plotOutput("corrt12"), icon = icon("chart-line")),
                                         
                                         tabPanel("Categorie", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 7, " ",  plotOutput("sct12"), icon = icon("chart-line")),
                                                    column(width = 5,
                                                           box(" ",  dataTableOutput("datit12"), icon = icon("chart-line"), collapsible = TRUE)))))),
                          
                          tabItem(tabName = "analisit34",
                                  tabBox(id="t4", width = 12, height = 910,
                                         tabPanel("Corr", plotOutput("corrt34"), icon = icon("chart-line")),
                                         
                                         tabPanel("Categorie", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 7, " ",  plotOutput("sct34"), icon = icon("chart-line")),
                                                    column(width = 5,
                                                           box(" ",  dataTableOutput("datit34"), icon = icon("chart-line"), collapsible = TRUE)))))),                          
                          
                          
                          tabItem(tabName = "dim",
                                  tabBox(id="t4", width = 12,
                                         tabPanel("Scree Plot", plotOutput("scree1"), icon = icon("chart-line")),
                                         tabPanel("Summary",verbatimTextOutput("summaryt"), icon = icon("chart-line")),
                                         tabPanel("Dim-1:2", tabName="dimt12", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimt1"), icon = icon("chart-line")),
                                                    column(width = 6," ",  plotOutput("dimt2"), icon = icon("chart-line"))),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimt12"), icon = icon("chart-line")),
                                                    column(width = 6, " ",  dataTableOutput("datt12"), icon = icon("chart-line")))),
                                         tabPanel("Dim-3:4", tabName="dimt34", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimt3"), icon = icon("chart-line")),
                                                    column(width = 6," ",  plotOutput("dimt4"), icon = icon("chart-line"))),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimt34"), icon = icon("chart-line")),
                                                    column(width = 6, " ",  dataTableOutput("datt34"), icon = icon("chart-line")))))),
                          
                          # USA
                          
                          tabItem(tabName = "analisiusa12",
                                  tabBox(id="t4", width = 12, height = 910,
                                         tabPanel("Corr", plotOutput("corrusa12"), icon = icon("chart-line")),
                                         
                                         tabPanel("Categorie", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 7, " ",  plotOutput("scusa12"), icon = icon("chart-line")),
                                                    column(width = 5,
                                                           box(" ",  dataTableOutput("datiusa1"), icon = icon("chart-line"), collapsible = TRUE)))))),
                          
                          tabItem(tabName = "analisiusa34",
                                  tabBox(id="t4", width = 12, height = 910,
                                         tabPanel("Corr", plotOutput("corrusa34"), icon = icon("chart-line")),
                                         
                                         tabPanel("Categorie", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 7, " ",  plotOutput("scusa34"), icon = icon("chart-line")),
                                                    column(width = 5,
                                                           box(" ",  dataTableOutput("datiusa34"), icon = icon("chart-line"), collapsible = TRUE)))))),
                          
                          tabItem(tabName = "dimUSA",
                                  tabBox(id="t4", width = 12,
                                         tabPanel("Scree Plot", plotOutput("screeusa"), icon = icon("chart-line")),
                                         tabPanel("Summary",verbatimTextOutput("summaryusa"), icon = icon("chart-line")),
                                         tabPanel("Dim-1:2", tabName="dimusa12", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimusa1"), icon = icon("chart-line")),
                                                    column(width = 6," ",  plotOutput("dimusa2"), icon = icon("chart-line"))),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimusa12"), icon = icon("chart-line")),
                                                    column(width = 6, " ",  dataTableOutput("dattusa12"), icon = icon("chart-line")))),
                                         tabPanel("Dim-3:4", tabName="dimusa34", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimusa3"), icon = icon("chart-line")),
                                                    column(width = 6," ",  plotOutput("dimusa4"), icon = icon("chart-line"))),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimusa34"), icon = icon("chart-line")),
                                                    column(width = 6, " ",  dataTableOutput("dattusa34"), icon = icon("chart-line")))))),
                          
                          # UK
                          
                          tabItem(tabName = "analisiuk12",
                                  tabBox(id="t4", width = 12, height = 910,
                                         tabPanel("Corr", plotOutput("corruk12"), icon = icon("chart-line")),
                                         
                                         tabPanel("Categorie", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 7, " ",  plotOutput("scuk12"), icon = icon("chart-line")),
                                                    column(width = 5,
                                                           box(" ",  dataTableOutput("datiuk12"), icon = icon("chart-line"), collapsible = TRUE)))))),
                          
                          tabItem(tabName = "analisiuk34",
                                  tabBox(id="t4", width = 12, height = 910,
                                         tabPanel("Corr", plotOutput("corruk34"), icon = icon("chart-line")),
                                         
                                         tabPanel("Categorie", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 7, " ",  plotOutput("scuk34"), icon = icon("chart-line")),
                                                    column(width = 5,
                                                           box(" ",  dataTableOutput("datiuk34"), icon = icon("chart-line"), collapsible = TRUE)))))),                        
                          tabItem(tabName = "end",
                                  tabBox(id="t0", width = 12,
                                         tabPanel("UK", icon = icon("film"),
                                                  fluidRow(
                                                    column(width = 12,
                                                           tags$h4(strong("Donne e Uomini"),style="text-align:center; font-family: times"))),
                                                  
                                                  fluidRow(
                                                    column(width = 6, " ",
                                                           tags$p("Nel nostro dataset le donne inglesi non sono caratterizzanti; svolgeno una vita molto più sana e rigida, non hanno avuto interazioni con droghe di nessun tipo.", style="text-align:center; font-family: times"),
                                                           tags$br(),
                                                           tags$p("Gli uomini adulti, dai 40 anni in su, hanno avuto le loro esperienze ma che si stanno allontanando dalle sostanze.", style="text-align:center; font-family: times"),
                                                           tags$br(),
                                                           tags$p("In generale Inghilterra, chi lascia la scuola prima dei 18 o a 17 anni, utilizza prevalentemente cocaina (prima 18) e cannabis(17) prima di smettere.", style="text-align:center; font-family: times"), icon = icon("chart-line")),
                                                    column(width = 6,
                                                           box(" ", dataTableOutput("donne"), icon = icon("chart-line"), collapsed = TRUE)))),
                                         
                                         tabPanel("America", icon = icon("globe"),
                                                  fluidRow(
                                                    column(width = 12,
                                                           tags$h4(strong("Situazione critica in America"),style="text-align:center; font-family: times"))),
                                                  fluidRow(
                                                    column(width = 12,
                                                           tags$p("La fascia di età in cui si fa un abuso di ogni tipologia di droga è tra i 16-17 anni, e soprattutto in coloro che hanno lasciato gli studi.
                                                           Si prediligono sostanze di derivazione chimica come funghi e crack.", style="text-align:center; font-family: times"),
                                                           tags$br(),
                                                           tags$p("Collegandoci al video visto precedentemente, possiamo confermare come l’uso di droghe sintetiche/non naturali sia molto utilizzato. E che vi sia una netta differenza tra i sessi.", style="text-align:center; font-family: times"),
                                                           tags$br(),
                                                           tags$p("Gli uomini tendono ad abusarne maggiormente e i ragazzi tendono ad essere dipendenti da più sostanze, arrivando ad assumere droghe sintetiche utilizzate in ambito farmaceutico.", style="text-align:center; font-family: times"), icon = icon("chart-line"))))
                                  )),
                          
                          
                          
                          
                          tabItem(tabName = "dimUK",
                                  tabBox(id="t4", width = 12,
                                         tabPanel("Scree Plot", plotOutput("screeuk"), icon = icon("chart-line")),
                                         tabPanel("Summary",verbatimTextOutput("summaryuk"), icon = icon("chart-line")),
                                         tabPanel("Dim-1:2", tabName="dimuk112", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimuk1"), icon = icon("chart-line")),
                                                    column(width = 6," ",  plotOutput("dimuk2"), icon = icon("chart-line"))),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimuk12"), icon = icon("chart-line")),
                                                    column(width = 6, " ",  dataTableOutput("dattuk12"), icon = icon("chart-line")))),
                                         tabPanel("Dim-3:4", tabName="dimuk334", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimuk3"), icon = icon("chart-line")),
                                                    column(width = 6," ",  plotOutput("dimuk4"), icon = icon("chart-line"))),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("dimuk34"), icon = icon("chart-line")),
                                                    column(width = 6, " ",  dataTableOutput("dattuk34"), icon = icon("chart-line"))))))
                          
                          
                        ))
)}

#Comandi server
server <- function(input,output,session) {
  
  callModule(module = esquisserServer, id = "esquisse")
  #Usando il comando output avviamo il dataset positivi
  output$dataT <- renderDataTable(drug.emotivo)
  #Rendiamo operativa la selezione delle osservazioni nella barra comando grafici
  {dataset <- reactive({
    drug.emotivo[sample(nrow(drug.emotivo), input$sampleSize)]})}
  
  #Usando il comando output avviamo il dataset positivi
  output$dataT2 <- renderDataTable(drug.sostanze.legali)
  #Rendiamo operativa la selezione delle osservazioni nella barra comando grafici
  {dataset <- reactive({
    drug.sostanze.legali[sample(nrow(drug.sostanze.legali), input$sampleSize)]})}
  
  output$dataT3 <- renderDataTable(drug.sostanze.illegali)
  #Rendiamo operativa la selezione delle osservazioni nella barra comando grafici
  {dataset <- reactive({
    drug.sostanze.illegali[sample(nrow(drug.sostanze.illegali), input$sampleSize)]})}
  
  output$mymap <- renderLeaflet({
    leaflet(df) %>%
      addTiles() %>%
      setView(lng = 15, lat = 10, zoom = 1.5) %>%
      addProviderTiles("MapBox", options = providerTileOptions( id = "mapbox.light",
                                                                accessToken = Sys.getenv('pk.eyJ1IjoiZG9taW51c3Y4IiwiYSI6ImNsMXQzaG42aDE3YW8za3B0ajkyYmdwOWYifQ.0C1Ev7hRz5WEkCT3jzF5pA'))) %>%
      addCircleMarkers(data = df, lat = ~df$latitude, lng = ~df$longitude,
                       stroke = F, label=~labels, color = "green" ,
                       fillOpacity = 0.5,
                       labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                   textsize = "18px", direction = "auto"))})
  
  # tutti i dataset
  output$scree1 <- renderPlot({
    scree1 <- fviz_screeplot(res, addlabels = TRUE, ylim = c(0, 50))
    print(scree1)}, height = 700)
  
  output$dimt1 <- renderPlot({
    dimt1 <- fviz_contrib(res, choice = 'var', axes = 1, top = 10)
    print(dimt1)})
  
  output$dimt2 <- renderPlot({
    dimt2 <- fviz_contrib(res, choice = 'var', axes = 2, top = 10)
    print(dimt2)})
  
  output$datt12 <- renderDataTable(domande)
  {datasett12 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  output$datt34 <- renderDataTable(domande)
  {datasett34 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  output$dimt3 <- renderPlot({
    dimt3 <- fviz_contrib(res, choice = 'var', axes = 3, top = 10)
    print(dimt3)})
  
  output$dimt4 <- renderPlot({
    dimt4 <- fviz_contrib(res, choice = 'var', axes = 4, top = 10)
    print(dimt4)})
  
  output$datit12 <- renderDataTable(domande)
  {datt1 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  
  output$datit34 <- renderDataTable(domande)
  {datt34 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  output$dimt12 <- renderPlot({
    dimt12 <- plot.MCA(res, choix="var",xlim=c(0,0.6),ylim=c(0,0.6),invisible=c("quali.sup","quanti.sup"))
    print(dimt12)})
  
  
  output$dimt34 <- renderPlot({
    dimt34 <- plot.MCA(res, choix="var",xlim=c(0,0.6),ylim=c(0,0.6),invisible=c("quali.sup","quanti.sup"), axes = 3:4)
    print(dimt34)})
  
  
  {output$summaryt <- renderPrint({
    print(summary(res, ncp = 4))})}
  
  # analisi dim 1:2
  output$corrt12 <- renderPlot({
    corrt12 <- plot.MCA(res, choix='quanti.sup',title="Supplementary quantitatives variables")
    print(corrt12)}, height = 700)
  
  output$sct12 <- renderPlot({
    sct12 <- plot.MCA(res, invisible=c("ind"),autoLab="y",cex=0.7,title="Supplementary and Active categories")
    print(sct12)}, height = 850)
  
  
  # analisi dim 3:4
  output$corrt34 <- renderPlot({
    corrt34 <- plot.MCA(res, choix='quanti.sup',title="Supplementary quantitatives variables", axes = 3:4)
    print(corrt34)}, height = 700)
  
  output$sct34 <- renderPlot({
    sct34 <- plot.MCA(res, invisible=c("ind"),autoLab="y",cex=0.7,title="Supplementary and Active categories", axes = 3:4)
    print(sct34)}, height = 850)
  
  # USA
  output$screeusa <- renderPlot({
    screeusa <- fviz_screeplot(res.USA, addlabels = TRUE, ylim = c(0, 50))
    print(screeusa)}, height = 700)
  
  output$dimusa1 <- renderPlot({
    dimusa1 <- fviz_contrib(res.USA, choice = 'var', axes = 1, top = 10)
    print(dimusa1)})
  
  output$dimusa2 <- renderPlot({
    dimusa2 <- fviz_contrib(res.USA, choice = 'var', axes = 2, top = 10)
    print(dimusa2)})
  
  output$dimusa3 <- renderPlot({
    dimusa3 <- fviz_contrib(res.USA, choice = 'var', axes = 3, top = 10)
    print(dimusa3)})
  
  output$dimusa4 <- renderPlot({
    dimusa4 <- fviz_contrib(res.USA, choice = 'var', axes = 4, top = 10)
    print(dimusa4)})
  
  output$dimusa12 <- renderPlot({
    dimusa12 <- plot.MCA(res.USA, choix="var",xlim=c(0,0.6),ylim=c(0,0.6),invisible=c("quali.sup","quanti.sup"))
    print(dimusa12)})
  
  output$dimusa34 <- renderPlot({
    dimusa34 <- plot.MCA(res.USA, choix="var",xlim=c(0,0.6),ylim=c(0,0.6),invisible=c("quali.sup","quanti.sup"), axes = 3:4)
    print(dimusa34)})
  
  output$dattusa12 <- renderDataTable(domande)
  {datasettusa12 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  output$dattusa34 <- renderDataTable(domande)
  {datasettusa34 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  
  output$datiusa1 <- renderDataTable(domande)
  {datusa1 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  
  output$datiusa34 <- renderDataTable(domande)
  {datusa34 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  {output$summaryusa <- renderPrint({
    print(summary(res.USA, ncp = 4))})}
  
  # analisi dim 1:2
  output$corrusa12 <- renderPlot({
    corrusa12 <- plot.MCA(res.USA, choix='quanti.sup',title="Supplementary quantitatives variables")
    print(corrusa12)}, height = 700)
  
  output$scusa12 <- renderPlot({
    scusa12 <- plot.MCA(res.USA, invisible=c("ind"),autoLab="y",cex=0.7,title="Supplementary and Active categories")
    print(scusa12)}, height = 850)
  
  
  # analisi dim 3:4
  output$corrusa34 <- renderPlot({
    corrusa34 <- plot.MCA(res.USA, choix='quanti.sup',title="Supplementary quantitatives variables", axes = 3:4)
    print(corrusa34)}, height = 700)
  
  output$scusa34 <- renderPlot({
    scusa34 <- plot.MCA(res.USA, invisible=c("ind"),autoLab="y",cex=0.7,title="Supplementary and Active categories", axes = 3:4)
    print(scusa34)}, height = 850)
  
  # UK
  output$screeuk <- renderPlot({
    screeuk <- fviz_screeplot(res.UK, addlabels = TRUE, ylim = c(0, 50))
    print(screeuk)}, height = 700)
  
  output$dimuk1 <- renderPlot({
    dimuk1 <- fviz_contrib(res.UK, choice = 'var', axes = 1, top = 10)
    print(dimuk1)})
  
  output$dimuk2 <- renderPlot({
    dimuk2 <- fviz_contrib(res.UK, choice = 'var', axes = 2, top = 10)
    print(dimuk2)})
  
  output$dimuk3 <- renderPlot({
    dimuk3 <- fviz_contrib(res.UK, choice = 'var', axes = 3, top = 10)
    print(dimuk3)})
  
  output$dimuk4 <- renderPlot({
    dimuk4 <- fviz_contrib(res.UK, choice = 'var', axes = 4, top = 10)
    print(dimuk4)})
  
  output$dattuk12 <- renderDataTable(domande)
  {datasettusa12 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  output$dattuk34 <- renderDataTable(domande)
  {datasettusa34 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  output$datiuk12 <- renderDataTable(domande)
  {datuk1 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  
  output$datiuk34 <- renderDataTable(domande)
  {datuk34 <- reactive({
    domande[sample(nrow(domande), input$sampleSize)]})}
  
  output$dimuk12 <- renderPlot({
    dimuk12 <- plot.MCA(res.UK, choix="var",xlim=c(0,0.6),ylim=c(0,0.6),invisible=c("quali.sup","quanti.sup"))
    print(dimuk12)})
  
  output$dimuk34 <- renderPlot({
    dimuk34 <- plot.MCA(res.UK, choix="var",xlim=c(0,0.6),ylim=c(0,0.6),invisible=c("quali.sup","quanti.sup"), axes = 3:4)
    print(dimuk34)})
  
  
  {output$summaryuk <- renderPrint({
    print(summary(res.UK, ncp = 4))})}
  
  # analisi dim 1:2
  output$corruk12 <- renderPlot({
    corruk12 <- plot.MCA(res.UK, choix='quanti.sup',title="Supplementary quantitatives variables")
    print(corruk12)}, height = 700)
  
  output$scuk12 <- renderPlot({
    scuk12 <- plot.MCA(res.UK, invisible=c("ind"),autoLab="y",cex=0.7,title="Supplementary and Active categories")
    print(scuk12)}, height = 850)
  
  
  # analisi dim 3:4
  output$corruk34 <- renderPlot({
    corruk34 <- plot.MCA(res.UK, choix='quanti.sup',title="Supplementary quantitatives variables", axes = 3:4)
    print(corruk34)}, height = 700)
  
  output$scuk34 <- renderPlot({
    scuk34 <- plot.MCA(res.UK, invisible=c("ind"),autoLab="y",cex=0.7,title="Supplementary and Active categories", axes = 3:4)
    print(scuk34)}, height = 850)
  
  
  # conclusioni
  
  output$donne <- renderDataTable(genere)
  {gen <- reactive({
    domande[sample(nrow(genere), input$sampleSize)]})}
  
}

#Avviamo l'app tramite il comando seguente
shinyApp(ui,server)