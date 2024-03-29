# LIBRERIE

packs <- c("tidyverse", "esquisse", "ggplot2", "plotly", "GGally", "vars", "fpp2", "urca", "tseries", "forecast", "tidyverse", "magrittr", "highcharter", "leaflet", "lubridate", 'shinycssloaders', 'dslabs', 'mclust', 'cluster', 'GGally', 'dendextend', 'tidyr' , 'kableExtra', 'FactoMineR', 'NbClust', 'parameters',
           "shiny", "shinydashboard", "shinyWidgets", 'data.table','vscc','teigen', 'glasso','leaflet.extras','geojsonio','ggtext','jsonlite', "shinythemes",'DT', "shinyjs", "shinyauthr", 'plotly', 'ggplot2', 'factoextra', 'depmixS4')
lapply(packs, require, character.only = TRUE)


library(FactoMineR)
library(FactoInvestigate)
library(Factoshiny)
library(factoextra)
library(data.table)
library(DT)
library(mhsmm)
library(NHMSAR)
library(fHMM)

# DATI
load("C:/Users/nicolasantillo/Examples_L31.RData")

data("gasoline")

pollution <- as.data.frame(pollution)


# PREVISIONE STAZIONARIA

bestfit <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(diff(log((gasoline))), xreg=fourier(diff(log(gasoline)), K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}
model1 <- forecast(bestfit, xreg=fourier(diff(log(gasoline)), K=bestK, h=104))


testlj1 <- Box.test(model1$residuals,lag=104.357142857143,fitdf = 5, type="Lj")

# PREVISIONE
bestfit <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(gasoline, xreg=fourier(gasoline, K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}
model <- forecast(bestfit,xreg=fourier(gasoline, K=bestK, h=104))

testlj <- Box.test(model$residuals,lag=104.357142857143,fitdf = 5, type="Lj")


# PREVISIONI A PIU' FASI SUI DATI DI ADDESTRAMENTO 
training <- subset(gasoline, end=length(gasoline)-61)
test <- subset(gasoline, start=length(gasoline)-60)

bestfit <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(training, xreg=fourier(training, K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}

gas.train <- forecast(bestfit,xreg=fourier(training, K=bestK, h=104))

testlj.train <- Box.test(gas.train$residuals,lag=104.357142857143,fitdf = 5, type="Lj")


# PREVISIONI A PIU' FASI SUI DATI DI ADDESTRAMENTO SUL SET STAZIONARIO


training.sta <- subset(diff(log(gasoline)), end=length(diff(log(gasoline)))-61)
test.sta <- subset(diff(log(gasoline)), start=length(diff(log(gasoline)))-60)

bestfit <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(training.sta, xreg=fourier(training.sta, K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}


gas.train.sta <- forecast(bestfit,xreg=fourier(training.sta, K=bestK, h=104))

testlj.train.sta <- Box.test(gas.train.sta$residuals,lag=104.357142857143,fitdf = 5, type="Lj")



# PCA
pca <-  PCA(pollution, graph = FALSE)

# HMM k = 2

res.kmeans2 <- kmeans(pollution,2)

K <- 2
mu2 <- res.kmeans2$centers
sigma2 <- array(NA, c(9,9,9))
sigma2[,,1] <- var(pollution[res.kmeans2$cluster==1,])
sigma2[,,2] <- var(pollution[res.kmeans2$cluster==2,])

start.val2 <- hmmspec(init = rep(1/K, K), # da che cluster parto
                      trans = matrix(c(.9,.1,.1,.9),byrow=T, nrow = K, ncol = K), # matrice parto
                      parms.emis = list(mu = list(mu2[1,], mu2[2,]), #media e deviazione standard per ogni cluster
                                        sigma=list(sigma2[,,1],sigma2[,,2])),# matrice di varianza di integrit‡
                      dens.emis = dmvnorm.hsmm) # densit‡ per distribuzione normale multivariata

mod.hmm.k2 <- hmmfit(matrix(unlist(pollution),ncol=9), start.val2, mstep = mstep.mvnorm)

# HMM K = 3

res.kmeans3 <- kmeans(pollution,3)

K <- 3
mu3 <- res.kmeans3$centers
sigma3 <- array(NA, c(9,9,9))
sigma3[,,1] <- var(pollution[res.kmeans3$cluster==1,])
sigma3[,,2] <- var(pollution[res.kmeans3$cluster==2,])
sigma3[,,3] <- var(pollution[res.kmeans3$cluster==3,])

start.val3 <- hmmspec(init = rep(1/K, K),
                      trans = matrix(c(.9,.1,.1,.9),byrow=T, nrow = K, ncol = K), # matrice parto
                      parms.emis = list(mu = list(mu3[1,], mu3[2,], mu3[3,]), #media e deviazione standard per ogni cluster
                                        sigma=list(sigma3[,,1],sigma3[,,2], sigma3[,,3])),# matrice di varianza di integrit‡
                      dens.emis = dmvnorm.hsmm) # densit‡ per distribuzione normale multivariata

mod.hmm.k3 <- hmmfit(matrix(unlist(pollution),ncol=9),start.val3, mstep = mstep.mvnorm) # varianze pesate

# HMM K = 4

res.kmeans4 <- kmeans(pollution,4)

K <- 4
mu4 <- res.kmeans4$centers
sigma4 <- array(NA, c(9,9,9))
sigma4[,,1] <- var(pollution[res.kmeans4$cluster==1,])
sigma4[,,2] <- var(pollution[res.kmeans4$cluster==2,])
sigma4[,,3] <- var(pollution[res.kmeans4$cluster==3,])
sigma4[,,4] <- var(pollution[res.kmeans4$cluster==4,])

start.val4 <- hmmspec(init = rep(1/K, K),
                      trans = matrix(c(.9,.1,.1,.9),byrow=T, nrow = K, ncol = K), # matrice parto
                      parms.emis = list(mu = list(mu4[1,], mu4[2,], mu4[3,], mu4[4,]), #media e deviazione standard per ogni cluster
                                        sigma=list(sigma4[,,1],sigma4[,,2], sigma4[,,3], sigma4[,,4])),# matrice di varianza di integrit‡
                      dens.emis = dmvnorm.hsmm) # densit‡ per distribuzione normale multivariata

mod.hmm.k4 <- hmmfit(matrix(unlist(pollution),ncol=9),start.val4, mstep = mstep.mvnorm) # varianze pesate

# k = 5
res.kmeans5 <- kmeans(pollution,5)

K <- 5
mu5 <- res.kmeans5$centers
sigma5 <- array(NA, c(9,9,9))
sigma5[,,1] <- var(pollution[res.kmeans5$cluster==1,])
sigma5[,,2] <- var(pollution[res.kmeans5$cluster==2,])
sigma5[,,3] <- var(pollution[res.kmeans5$cluster==3,])
sigma5[,,4] <- var(pollution[res.kmeans5$cluster==4,])
sigma5[,,5] <- var(pollution[res.kmeans5$cluster==5,])

start.val5 <- hmmspec(init = rep(1/K, K),
                      trans = matrix(c(.9,.1,.1,.9),byrow=T, nrow = K, ncol = K), # matrice parto
                      parms.emis = list(mu = list(mu5[1,], mu5[2,], mu5[3,], mu5[4,], mu5[5,]), #media e deviazione standard per ogni cluster
                                        sigma=list(sigma5[,,1],sigma5[,,2], sigma5[,,3], sigma5[,,4], sigma5[,,5])),# matrice di varianza di integrit‡
                      dens.emis = dmvnorm.hsmm) # densit‡ per distribuzione normale multivariata

mod.hmm.k5 <- hmmfit(matrix(unlist(pollution),ncol=9),start.val5, mstep = mstep.mvnorm) # varianze pesate


# ANALISI DEL BIC

f = formula(pollution)

# BIC K = 2
aic2 = depmix(f, data = pollution, nstates = 2)
aic2fit = fit(aic2)

# BIC K = 3
aic3 = depmix(f, data = pollution, nstates = 3)
aic3fit = fit(aic3)

# BIC K = 4
aic4 = depmix(f, data = pollution, nstates = 4)
aic4fit = fit(aic4)

# BIC K = 5
aic5 = depmix(f, data = pollution, nstates = 5)
aic5fit = fit(aic5)


# LOCALE: REGIME K=2

# N02
hmmN022 <- depmix(pollution$NO2 ~ 1, family = gaussian(), nstates = 2, data=data.frame(pollution))
hmmfitN022 <- fit(hmmN022, verbose = FALSE)
post_probsN022 <- posterior(hmmfitN022)

# SO2
hmmSO22 <- depmix(pollution$SO2 ~ 1, family = gaussian(), nstates = 2, data=data.frame(pollution))
hmmfitSO22 <- fit(hmmSO22, verbose = FALSE)
post_probsS022 <- posterior(hmmfitSO22)

# PM10
hmmPM102 <- depmix(pollution$PM10 ~ 1, family = gaussian(), nstates = 2, data=data.frame(pollution))
hmmfitPM102 <- fit(hmmPM102, verbose = FALSE)
post_probsPM102 <- posterior(hmmfitPM102)

# O3
hmmO32 <- depmix(pollution$O3 ~ 1, family = gaussian(), nstates = 2, data=data.frame(pollution))
hmmfit032 <- fit(hmmO32, verbose = FALSE)
post_probs032 <- posterior(hmmfit032)

# CO
hmmCO2 <- depmix(pollution$CO ~ 1, family = gaussian(), nstates = 2, data=data.frame(pollution))
hmmfitCO2 <- fit(hmmCO2, verbose = FALSE)
post_probsCO2 <- posterior(hmmfitCO2)


# Toluene
hmmToluene2 <- depmix(pollution$Toluene ~ 1, family = gaussian(), nstates = 2, data=data.frame(pollution))
hmmfitToluene2 <- fit(hmmToluene2, verbose = FALSE)
post_probsToluene2 <- posterior(hmmfitToluene2)

# Etilbenze
hmmEtilbenze2 <- depmix(pollution$Etilbenze ~ 1, family = gaussian(), nstates = 2, data=data.frame(pollution))
hmmfitEtilbenze2 <- fit(hmmEtilbenze2, verbose = FALSE)
post_probsEtilbenze2 <- posterior(hmmfitEtilbenze2)


# CO
hmmOxylene2 <- depmix(pollution$Oxylene ~ 1, family = gaussian(), nstates = 2, data=data.frame(pollution))
hmmfitOxylene2 <- fit(hmmOxylene2, verbose = FALSE)
post_probsOxylene2 <- posterior(hmmfitOxylene2)

# PM2.5
hmmPM2.52 <- depmix(pollution$PM2.5 ~ 1, family = gaussian(), nstates = 2, data=data.frame(pollution))
hmmfitPM2.52 <- fit(hmmPM2.52, verbose = FALSE)
post_probsPM2.52 <- posterior(hmmfitPM2.52)



# LOCALE: REGIME K=3

# N02
hmmN023 <- depmix(pollution$NO2 ~ 1, family = gaussian(), nstates = 3, data=data.frame(pollution))
hmmfitN023 <- fit(hmmN023, verbose = FALSE)
post_probsN023 <- posterior(hmmfitN023)

# SO2
hmmSO23 <- depmix(pollution$SO2 ~ 1, family = gaussian(), nstates = 3, data=data.frame(pollution))
hmmfitSO23 <- fit(hmmSO23, verbose = FALSE)
post_probsS023 <- posterior(hmmfitSO23)

# PM10
hmmPM103 <- depmix(pollution$PM10 ~ 1, family = gaussian(), nstates = 3, data=data.frame(pollution))
hmmfitPM103 <- fit(hmmPM103, verbose = FALSE)
post_probsPM103 <- posterior(hmmfitPM103)

# O3
hmmO33 <- depmix(pollution$O3 ~ 1, family = gaussian(), nstates = 3, data=data.frame(pollution))
hmmfit033 <- fit(hmmO33, verbose = FALSE)
post_probs033 <- posterior(hmmfit033)

# CO
hmmCO3 <- depmix(pollution$CO ~ 1, family = gaussian(), nstates = 3, data=data.frame(pollution))
hmmfitCO3 <- fit(hmmCO3, verbose = FALSE)
post_probsCO3 <- posterior(hmmfitCO3)


# Toluene
hmmToluene3 <- depmix(pollution$Toluene ~ 1, family = gaussian(), nstates = 3, data=data.frame(pollution))
hmmfitToluene3 <- fit(hmmToluene3, verbose = FALSE)
post_probsToluene3 <- posterior(hmmfitToluene3)

# Etilbenze
hmmEtilbenze3 <- depmix(pollution$Etilbenze ~ 1, family = gaussian(), nstates = 3, data=data.frame(pollution))
hmmfitEtilbenze3 <- fit(hmmEtilbenze3, verbose = FALSE)
post_probsEtilbenze3 <- posterior(hmmfitEtilbenze3)


# CO
hmmOxylene3 <- depmix(pollution$Oxylene ~ 1, family = gaussian(), nstates = 3, data=data.frame(pollution))
hmmfitOxylene3 <- fit(hmmOxylene3, verbose = FALSE)
post_probsOxylene3 <- posterior(hmmfitOxylene3)

# PM2.5
hmmPM2.53 <- depmix(pollution$PM2.5 ~ 1, family = gaussian(), nstates = 3, data=data.frame(pollution))
hmmfitPM2.53 <- fit(hmmPM2.53, verbose = FALSE)
post_probsPM2.53 <- posterior(hmmfitPM2.53)


# LOCALE: REGIME K=4

# N02
hmmN024 <- depmix(pollution$NO2 ~ 1, family = gaussian(), nstates = 4, data=data.frame(pollution))
hmmfitN024 <- fit(hmmN024, verbose = FALSE)
post_probsN024 <- posterior(hmmfitN024)

# SO2
hmmSO24 <- depmix(pollution$SO2 ~ 1, family = gaussian(), nstates = 4, data=data.frame(pollution))
hmmfitSO24 <- fit(hmmSO24, verbose = FALSE)
post_probsS024 <- posterior(hmmfitSO24)

# PM10
hmmPM104 <- depmix(pollution$PM10 ~ 1, family = gaussian(), nstates = 4, data=data.frame(pollution))
hmmfitPM104 <- fit(hmmPM104, verbose = FALSE)
post_probsPM104 <- posterior(hmmfitPM104)

# O3
hmmO34 <- depmix(pollution$O3 ~ 1, family = gaussian(), nstates = 4, data=data.frame(pollution))
hmmfit034 <- fit(hmmO34, verbose = FALSE)
post_probs034 <- posterior(hmmfit034)

# CO
hmmCO4 <- depmix(pollution$CO ~ 1, family = gaussian(), nstates = 4, data=data.frame(pollution))
hmmfitCO4 <- fit(hmmCO4, verbose = FALSE)
post_probsCO4 <- posterior(hmmfitCO4)


# Toluene
hmmToluene4 <- depmix(pollution$Toluene ~ 1, family = gaussian(), nstates = 4, data=data.frame(pollution))
hmmfitToluene4 <- fit(hmmToluene4, verbose = FALSE)
post_probsToluene4 <- posterior(hmmfitToluene4)

# Etilbenze
hmmEtilbenze4 <- depmix(pollution$Etilbenze ~ 1, family = gaussian(), nstates = 4, data=data.frame(pollution))
hmmfitEtilbenze4 <- fit(hmmEtilbenze4, verbose = FALSE)
post_probsEtilbenze4 <- posterior(hmmfitEtilbenze4)


# CO
hmmOxylene4 <- depmix(pollution$Oxylene ~ 1, family = gaussian(), nstates = 4, data=data.frame(pollution))
hmmfitOxylene4 <- fit(hmmOxylene4, verbose = FALSE)
post_probsOxylene4 <- posterior(hmmfitOxylene4)

# PM2.5
hmmPM2.54 <- depmix(pollution$PM2.5 ~ 1, family = gaussian(), nstates = 4, data=data.frame(pollution))
hmmfitPM2.54 <- fit(hmmPM2.54, verbose = FALSE)
post_probsPM2.54 <- posterior(hmmfitPM2.54)


# LOCALE: REGIME K=5

# N02
hmmN025 <- depmix(pollution$NO2 ~ 1, family = gaussian(), nstates = 5, data=data.frame(pollution))
hmmfitN025 <- fit(hmmN025, verbose = FALSE)
post_probsN025 <- posterior(hmmfitN025)

# SO2
hmmSO25 <- depmix(pollution$SO2 ~ 1, family = gaussian(), nstates = 5, data=data.frame(pollution))
hmmfitSO25 <- fit(hmmSO25, verbose = FALSE)
post_probsS025 <- posterior(hmmfitSO25)

# PM10
hmmPM105 <- depmix(pollution$PM10 ~ 1, family = gaussian(), nstates = 5, data=data.frame(pollution))
hmmfitPM105 <- fit(hmmPM105, verbose = FALSE)
post_probsPM105 <- posterior(hmmfitPM105)

# O3
hmmO35 <- depmix(pollution$O3 ~ 1, family = gaussian(), nstates = 5, data=data.frame(pollution))
hmmfit035 <- fit(hmmO35, verbose = FALSE)
post_probs035 <- posterior(hmmfit035)

# CO
hmmCO5 <- depmix(pollution$CO ~ 1, family = gaussian(), nstates = 5, data=data.frame(pollution))
hmmfitCO5 <- fit(hmmCO5, verbose = FALSE)
post_probsCO5 <- posterior(hmmfitCO5)


# Toluene
hmmToluene5 <- depmix(pollution$Toluene ~ 1, family = gaussian(), nstates = 5, data=data.frame(pollution))
hmmfitToluene5 <- fit(hmmToluene5, verbose = FALSE)
post_probsToluene5 <- posterior(hmmfitToluene5)

# Etilbenze
hmmEtilbenze5 <- depmix(pollution$Etilbenze ~ 1, family = gaussian(), nstates = 5, data=data.frame(pollution))
hmmfitEtilbenze5 <- fit(hmmEtilbenze5, verbose = FALSE)
post_probsEtilbenze5 <- posterior(hmmfitEtilbenze5)


# CO
hmmOxylene5 <- depmix(pollution$Oxylene ~ 1, family = gaussian(), nstates = 5, data=data.frame(pollution))
hmmfitOxylene5 <- fit(hmmOxylene5, verbose = FALSE)
post_probsOxylene5 <- posterior(hmmfitOxylene5)

# PM2.5
hmmPM2.55 <- depmix(pollution$PM2.5 ~ 1, family = gaussian(), nstates = 5, data=data.frame(pollution))
hmmfitPM2.55 <- fit(hmmPM2.55, verbose = FALSE)
post_probsPM2.55 <- posterior(hmmfitPM2.55)


# HIDDEN SEMI-MARKOV MODELS

# GAMMA

# hsmm k = 2
J <- 2
init <- rep(1/J, J)
P <- matrix(c(0,.1,.1,0),nrow=J)
B <- list(mu = list(mu2[1,], mu2[2,]), sigma=list(sigma2[,,1],sigma2[,,2]))
d <- list(shape = c(10, 25), scale = c(2, 2), type = "gamma")
modelg2 <- hsmmspec(init, P, parms.emis = B, sojourn = d, dens.emis = dmvnorm.hsmm)
prog2 <- hsmmfit(matrix(unlist(pollution),ncol=9), modelg2, mstep = mstep.mvnorm)



# hsmm k = 3
J <- 3
init <- rep(1/J, J)
P <- matrix(c(0,.1,.4,.5,0,.6,.5,.9,0),nrow=J)
B <- list(mu = list(mu3[1,], mu3[2,], mu3[3,]), sigma=list(sigma3[,,1],sigma3[,,2], sigma3[,,3]))
d <- list(shape = c(10, 25, 30), scale = c(2, 2,2), type = "gamma")
modelg3 <- hsmmspec(init, P, parms.emis = B, sojourn = d, dens.emis = dmvnorm.hsmm)
prog3 <- hsmmfit(matrix(unlist(pollution),ncol=9), modelg3, mstep = mstep.mvnorm)


# POISSON

# hsmm k = 2

J <- 2
init <- rep(1/J, J)
P <- matrix(c(0,.1,.1,0),nrow=J)
B <- list(mu = list(mu2[1,], mu2[2,]), sigma=list(sigma2[,,1],sigma2[,,2]))
d <- list(lambda=c(10,30,60),shift=c(10,100,30),type='poisson')
modelp2 <- hsmmspec(init, P, parms.emis = B, sojourn = d, dens.emis = dmvnorm.hsmm)
prop2 <- hsmmfit(matrix(unlist(pollution),ncol=9), modelp2, mstep = mstep.mvnorm)




# hsmm k = 3
J <- 3
init <- rep(1/J, J)
P <- matrix(c(0,.1,.4,.5,0,.6,.5,.9,0),nrow=J)
B <- list(mu = list(mu3[1,], mu3[2,], mu3[3,]), sigma=list(sigma3[,,1],sigma3[,,2], sigma3[,,3]))
d <- list(lambda=c(10,30,60),shift=c(10,100,30),type='poisson')
modelp3 <- hsmmspec(init, P, parms.emis = B, sojourn = d, dens.emis = dmvnorm.hsmm)
prop3 <- hsmmfit(matrix(unlist(pollution),ncol=9), modelp3, mstep = mstep.mvnorm)



# shiny
ui <- {dashboardPage( skin = "green",
                      dashboardHeader(title = span(tagList(tags$img(src="https://learn.eduopen.org/pluginfile.php/28/block_institution/content/157239322/Logo-Lumsa.jpg", width = "55px", height = "55px"),
                                                           "Inquinamento")), titleWidth = 600, dropdownMenu(type = "messages",
                                                                                                            notificationItem(
                                                                                                              text = tags$p("Sviluppato da: Fermo, Ianeri, Santillo", color = "white"),
                                                                                                              icon("info")))),
                      dashboardSidebar(
                        sidebarMenu(id = "sidebar",
                                    #Creiamo un menu per il dataset, assegnando un identificativo e un'icona
                                    menuItem("Introduzione", tabName = "intro", icon = icon("hand-point-right", class = "faa-passing")),
                                    
                                    menuItem("Dataset", tabName = "data", icon = icon("book", class = "pulse")),
                                    
                                    menuItem("Analisi Descrittiva", tabName = "rel", icon = icon("chart-line", class = "wrench"),
                                             menuSubItem("Sommari", tabName = "sommari", icon = icon("chart-line")),
                                             menuSubItem("PCA", tabName = "pca", icon = icon("chart-line"))),
                                    
                                    #Creiamo un menu per la regressione, assegnando un identificativo e un'icona
                                    menuItem("Time-series",icon = icon("magnifying-glass", class = "tada"),
                                             menuItem("Grafici", icon = icon("chart-line"),
                                                      menuSubItem("Andamento", tabName = "andam", icon = icon("chart-line")),
                                                      menuSubItem("Stagionalit‡", tabName = "stagion", icon = icon("chart-line")),
                                                      menuSubItem("Ritardi, ACF e stazionariet‡", tabName = "rita", icon = icon("chart-line"))),
                                             menuItem("Previsione", icon = icon("chart-line"),
                                                      menuSubItem("Previsione", tabName = "prev", icon = icon("chart-line")))),
                                    menuItem("HMM", icon = icon("magnifying-glass", class = "tada"),
                                             menuItem("Modelli", icon = icon("chart-line"),
                                                      menuSubItem("Modelli", tabName = "hmmmod", icon = icon("chart-line")),
                                                      menuSubItem("Regime a posteriori", tabName = "local", icon = icon("chart-line")))),
                                    menuItem("HSMM", icon = icon("magnifying-glass", class = "tada"),
                                             menuItem("Modelli", icon = icon("chart-line"),
                                                      menuSubItem("Modelli", tabName = "hsmmmod", icon = icon("chart-line")))))),
                      
                      
                      #Tramite il seguente comando inseriamo nelle finestre i contenuti da mostrare
                      dashboardBody(
                        tabItems(
                          
                          # INTRODUZIONE
                          tabItem(tabName = "intro",
                                  tabBox(id="t0", width = 12,
                                         tabPanel("Una breve introduzione", icon = icon("book"),
                                                  fluidRow(
                                                    column(width = 8, tags$img(src='https://www.salute.gov.it/imgs/C_17_pagineAree_5826_0_immagine.jpg', width = 700, height  = 450, align = "center"))),
                                                  
                                                  fluidRow(
                                                    column(width = 12, tags$br(),
                                                           tags$p(strong("NO2:"), "Il biossido di azoto; si forma principalmente nei processi di combustione."),
                                                           tags$p(strong("SO2:"), "Il biossido di zolfo; le fonti di emissione principali sono legate alla produzione di energia, agli impianti termici, ai processi industriali e al traffico."),
                                                           tags$p(strong("PM10:"), "Polveri fini; particelle inquinanti invernali presenti nell'aria."),
                                                           tags$p(strong("O3:"), "Ozono; protegge la superficie dalle radiazioni ultraviolette."),
                                                           tags$p(strong("CO:"), "Monossido di carbono; la principale sorgente Ë la raffinazione del petrolio."),
                                                           tags$p(strong("Toluene:"), "Viene usato come solvente in sostituzione del benzene, Ë contenuto nella benzina."),
                                                           tags$p(strong("Etilbenze:"), "Liquido incolore infiammabile; odore simile al benzene."),
                                                           tags$p(strong("Oxylene:"), "La sua struttura Ë quella di un benzene in cui due atomi di idrogeno adiacenti sono stati sostituiti da due gruppi metile."),
                                                           tags$p(strong("PM2.5:"), "Polveri fini.")))))),
                          
                          
                          # DATASET
                          
                          tabItem(tabName = "data",
                                  tabBox(id="t1", width = 12,
                                         #Creiamo un pannello assegnando il titolo, l'identificativo per l'output, e un icona
                                         tabPanel("Dataset pollution", dataTableOutput("dataT"), icon = icon("hand-point-down")))),
                          
                          # PCA
                          
                          tabItem(tabName = "pca", 
                                  tabBox(id="t9", width = 12, height = 910,
                                         tabPanel("Bar-plot", plotOutput("pca1"), icon = icon("chart-line")),
                                         tabPanel("Contributo variabili", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ", plotOutput("pca3"), icon = icon("chart-line")),
                                                    column(width = 6," ", plotOutput("pca4"), icon = icon("chart-line"))),
                                                  fluidRow(
                                                    column(width = 6, " ", plotOutput("pca5"), icon = icon("chart-line")),
                                                    column(width = 6, " ", plotOutput("pca6"), icon = icon("chart-line")))),
                                         
                                         tabPanel("Variabili", plotOutput("pca2"), icon = icon("chart-line")),
                                         
                                  )),
                          # SOMMARIO
                          
                          tabItem(tabName = "sommari",
                                  tabBox(id= "t3", width = 12,
                                         tabPanel("Sommari", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column("Benzina", width = 4, " ",  verbatimTextOutput("summa"), icon = icon("chart-line")),
                                                    column("Pollution", width = 8, " ",  verbatimTextOutput("summapol"), icon = icon("chart-line")))))),
                          
                          # GRAFICO ANDAMENTO
                          tabItem(tabName = "andam",
                                  tabBox(id="t4", width = 12, height = 910,
                                         tabPanel("Andamento", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("anda"), icon = icon("chart-line")),
                                                    column(width = 6," ",  plotOutput("dec"), icon = icon("chart-line")))))),
                          
                          
                          # STAGIONALITA'
                          tabItem(tabName = "stagion",
                                  tabBox(id="t5", width = 12, height = 910,
                                         tabPanel("Stagionalit‡", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("stag1"), icon = icon("chart-line")),
                                                    column(width = 6, " ",  plotOutput("stag2"), icon = icon("chart-line")))))),
                          
                          tabItem(tabName = "rita",
                                  tabBox(id="t6", width = 12, height = 910,
                                         tabPanel("Ritardo e ACF", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6," ",  plotOutput("acf1"), icon = icon("chart-line")),
                                                    column(width = 6," ",  plotOutput("rita"), icon = icon("chart-line")))),
                                         tabPanel("stazionariet‡", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6," ",  plotOutput("sta"), icon = icon("chart-line")),
                                                    column(width = 6," ",  plotOutput("acf2"), icon = icon("chart-line")))))),
                          
                          tabItem(tabName = "prev",
                                  tabBox(id="t7", width = 12, height = 910,
                                         tabPanel("Previsione", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ", plotOutput("modst"), icon = icon("chart-line")),
                                                    column(width = 6, " ", verbatimTextOutput("prov"), icon = icon("chart-line")))),
                                         
                                         tabPanel("Previsione residui", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ", plotOutput("res"), icon = icon("chart-line")),
                                                    column(width = 6, " ",verbatimTextOutput("res1"), icon = icon("chart-line")))),
                                         
                                         tabPanel("Previsione stazionaria", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ", plotOutput("modst1"), icon = icon("chart-line")),
                                                    column(width = 6, " ", verbatimTextOutput("prov1"), icon = icon("chart-line")))),
                                         
                                         tabPanel("Previsione stazionaria, residui", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ", plotOutput("res2"), icon = icon("chart-line")),
                                                    column(width = 6, " ",verbatimTextOutput("res3"), icon = icon("chart-line")))),
                                         
                                         tabPanel("Previsione a pi˘ fasi sui dati di addestramento", icon = icon("chart-line"),
                                                  fluidRow(  
                                                    column(width = 6, " ", plotOutput("modad"), icon = icon("chart-line")),
                                                    column(width = 6, " ", plotOutput("dodfas"), icon = icon("chart-line")))),
                                         
                                         tabPanel("Previsione a pi˘ fasi, residui", icon = icon("chart-line"),
                                                  fluidRow(  
                                                    column(width = 6, " ", plotOutput("resad"), icon = icon("chart-line")),
                                                    column(width = 6, " ", verbatimTextOutput("test1"), verbatimTextOutput("prov2"), icon = icon("chart-line")))),
                                         
                                         tabPanel("Previsione stazionaria a pi˘ fasi", icon = icon("chart-line"),
                                                  fluidRow(  
                                                    column(width = 6, " ", plotOutput("modad1"), icon = icon("chart-line")),
                                                    column(width = 6, " ", plotOutput("dodfas1"), icon = icon("chart-line")))),
                                         
                                         tabPanel("Previsione stazionaria a pi˘ fasi, residui", icon = icon("chart-line"),
                                                  fluidRow(  
                                                    column(width = 6, " ", plotOutput("resad1"), icon = icon("chart-line")),
                                                    column(width = 6, " ", verbatimTextOutput("test11"), verbatimTextOutput("prov21"), icon = icon("chart-line")))))),
                          
                          # HMM
                          
                          tabItem(tabName = "hmmmod",
                                  tabBox(id="t8", width = 12, height = 910,
                                         tabPanel("Modello k.means con k = 2", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("hmmk2"), icon = icon("chart-line")),
                                                    column(width = 6,"Matrice di transizione stimata",  verbatimTextOutput("roundhmmk2"), verbatimTextOutput("sumhmmk2"), icon = icon("chart-line")))),
                                         tabPanel("Modello k.means con k = 3", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("hmmk3"), icon = icon("chart-line")),
                                                    column(width = 6,"Matrice di transizione stimata",  verbatimTextOutput("roundhmmk3"), verbatimTextOutput("sumhmmk3"), icon = icon("chart-line")))),
                                         tabPanel("Modello k.means con k = 4", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("hmmk4"), icon = icon("chart-line")),
                                                    column(width = 6,"Matrice di transizione stimata",  verbatimTextOutput("roundhmmk4"), verbatimTextOutput("sumhmmk4"), icon = icon("chart-line")))),
                                         tabPanel("Modello k.means con k = 5", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("hmmk5"), icon = icon("chart-line")),
                                                    column(width = 6,"Matrice di transizione stimata",  verbatimTextOutput("roundhmmk5"), verbatimTextOutput("sumhmmk5"), icon = icon("chart-line")))),
                                         tabPanel("Analisi BIC e AIC", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, "k = 2, k = 3, k = 4, K = 5", verbatimTextOutput("bic"), icon = icon("chart-line")))))),
                          
                          tabItem(tabName = "local",
                                  tabBox(id="t9", width = 12, height = 910,
                                         tabPanel("Regime posteriore per k = 2", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 4,"Biossido di zolfo", plotOutput("rp2S02"), icon = icon("chart-line")),
                                                    column(width = 4,"Polveri fini M10 - Ozono", plotOutput("rp2PM10"), plotOutput("rp203"), icon = icon("chart-line")),
                                                    column(width = 4,"Monossido di Carbonio - Toluene", plotOutput("rp2CO"), plotOutput("rp2Toluene"), icon = icon("chart-line")))),
                                         
                                         tabPanel("Regime posteriore per k = 3", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 4,"Biossido di zolfo", plotOutput("rp3S02"), icon = icon("chart-line")),
                                                    column(width = 4,"Polveri fini M10 - Ozono", plotOutput("rp3PM10"), plotOutput("rp303"), icon = icon("chart-line")),
                                                    column(width = 4,"Monossido di Carbonio - Toluene", plotOutput("rp3CO"), plotOutput("rp3Toluene"), icon = icon("chart-line")))),
                                         
                                         tabPanel("Regime posteriore per k = 4", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 4,"Biossido di zolfo", plotOutput("rp4S02"), icon = icon("chart-line")),
                                                    column(width = 4,"Polveri fini M10 - Ozono", plotOutput("rp4PM10"), plotOutput("rp403"), icon = icon("chart-line")),
                                                    column(width = 4,"Monossido di Carbonio - Toluene", plotOutput("rp4CO"), plotOutput("rp4Toluene"), icon = icon("chart-line")))),
                                         
                                         tabPanel("Regime posteriore per k = 5", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 4,"Biossido di zolfo", plotOutput("rp5S02"), icon = icon("chart-line")),
                                                    column(width = 4,"Polveri fini M10 - Ozono", plotOutput("rp5PM10"), plotOutput("rp503"), icon = icon("chart-line")),
                                                    column(width = 4,"Monossido di Carbonio - Toluene", plotOutput("rp5CO"), plotOutput("rp5Toluene"), icon = icon("chart-line")))))),
                          
                          tabItem(tabName = "hsmmmod",
                                  tabBox(id="t10", width = 12, height = 910,
                                         tabPanel("Modello gamma con k = 2", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("hsmmgam2"), icon = icon("chart-line")),
                                                    column(width = 6," ", plotOutput("hsmmgam21") , icon = icon("chart-line")))),
                                         tabPanel("Modello gamma con k = 3", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("hsmmgam3"), icon = icon("chart-line")),
                                                    column(width = 6," ",  plotOutput("hsmmgam32"), icon = icon("chart-line")))),
                                         tabPanel("Modello poisson con k = 2", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("hsmmpoi2"), icon = icon("chart-line")),
                                                    column(width = 6," ",  plotOutput("hsmmpoi21"), icon = icon("chart-line")))),
                                         tabPanel("Modello poisson con k = 3", icon = icon("chart-line"),
                                                  fluidRow(
                                                    column(width = 6, " ",  plotOutput("hsmmpoi3"), icon = icon("chart-line")),
                                                    column(width = 6," ", plotOutput("hsmmpoi31"), icon = icon("chart-line"))))))
                          
                          
                        ))
                      
                      
                      
                      
)}

#Comandi server
server <- function(input,output,session) {
  
  # DATASET
  
  output$dataT <- renderDataTable(pollution)
  {dataset <- reactive({
    pollution[sample(nrow(pollution), input$sampleSize)]})}
  
  # PCA
  {output$pca1 <- renderPlot({
    pca12 <- fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 70))
    print(pca12)}, height = 850)}
  
  {output$pca2 <- renderPlot({
    pca1 <- fviz_pca_var(pca, col.var = 'contrib', gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'), repel = TRUE) + theme_classic()
    
    print(pca1)}, height = 850)}
  
  {output$pca3 <- renderPlot({
    pca2 <- fviz_contrib(pca, choice = 'var', axes = 1, top = 10)
    print(pca2)})}
  
  {output$pca4 <- renderPlot({
    pca3 <-fviz_contrib(pca, choice = 'var', axes = 2, top = 10)
    print(pca3)})}
  
  {output$pca5 <- renderPlot({
    pca4 <- fviz_contrib(pca, choice = 'var', axes = 3, top = 10)
    print(pca4)})}
  
  {output$pca6 <- renderPlot({
    pca6 <-fviz_contrib(pca, choice = 'var', axes = 4, top = 10)
    print(pca6)})}
  
  
  # TIME SERIES
  
  
  # SUMMARY
  {output$summa <- renderPrint({
    print(summary(gasoline))})}
  
  {output$summapol <- renderPrint({
    print(summary(pollution))})}
  
  # ANDAMENTO
  
  output$anda <- renderPlot({
    and1 <- autoplot(gasoline, xlab = "Year")
    print(and1)}, height = 850)
  
  output$dec <- renderPlot({
    dec1 <- plot(decompose(gasoline))
    print(dec1)}, height = 850)
  
  # STAGIONALITA'
  
  output$stag1 <- renderPlot({
    stag1 <- ggseasonplot(gasoline)
    print(stag1)}, height = 850)
  
  output$stag2 <- renderPlot({
    stag2 <- ggseasonplot(gasoline, polar = TRUE)
    print(stag2)}, height = 850)
  
  # RITARDO
  
  output$rita <- renderPlot({
    rita1 <-  gglagplot(gasoline)
    print(rita1)}, height = 850)
  
  
  # AUTOCORRELAZIONE
  
  
  output$acf1 <- renderPlot({
    acf1 <- ggAcf(gasoline, lag.max = 80)
    print(acf1)}, height = 850)
  
  output$acf2 <- renderPlot({
    acf2 <- ggAcf(diff(log(gasoline)), lag.max = 80)
    print(acf2)}, height = 850)
  
  # STAZIONARIETA'
  
  output$sta <- renderPlot({
    acf2 <- autoplot(diff(log(gasoline)), xlab = "Year")
    print(acf2)}, height = 850)
  
  # PREVISIONE
  
  output$modst <- renderPlot({
    modst <- autoplot(model)
    print(modst)}, height = 850)
  
  output$res <- renderPlot({
    res <- checkresiduals(model)
    print(res)}, height = 850)
  
  {output$res1 <- renderPrint({
    print(testlj)})}
  
  
  {output$prov <- renderPrint({
    print(model$model)})}
  
  # PREVISIONE CON STAZIONARIETA'
  
  output$modst1 <- renderPlot({
    modst1 <- autoplot(model1)
    print(modst1)}, height = 850)
  
  output$res2 <- renderPlot({
    res2<- checkresiduals(model1)
    print(res2)}, height = 850)
  
  {output$res3 <- renderPrint({
    print(testlj1)})}
  
  {output$prov1 <- renderPrint({
    print(model1$model)})}
  
  
  # PREVISIONI A PIU' FASI SUI DATI DI ADDESTRAMENTO 
  
  output$dodfas <- renderPlot({
    dodfas <- autoplot(training, series="Training data") + autolayer(fitted(gas.train, h=12), series="12 fasi")
    print(dodfas)}, height = 850)
  
  output$modad <- renderPlot({
    modad <- gas.train %>% forecast(h=60) %>% autoplot() + autolayer(test)
    print(modad)}, height = 850)
  
  output$resad <- renderPlot({
    resad<- checkresiduals(gas.train)
    print(resad)}, height = 850)
  
  {output$test1 <- renderPrint({
    print(testlj.train)})}
  
  {output$prov2 <- renderPrint({
    print(gas.train$model)})}
  
  # PREVISIONI A PIU' FASI SUI DATI DI ADDESTRAMENTO STAZIONARI
  
  output$dodfas1 <- renderPlot({
    dodfas1 <- autoplot(training.sta, series="Training data") + autolayer(fitted(gas.train.sta, h=12), series="12 fasi")
    print(dodfas1)}, height = 850)
  
  output$modad1 <- renderPlot({
    modad1 <- gas.train.sta %>% forecast(h=60) %>% autoplot() + autolayer(test.sta)
    print(modad1)}, height = 850)
  
  output$resad1 <- renderPlot({
    resad1<- checkresiduals(gas.train.sta)
    print(resad1)}, height = 850)
  
  {output$test11 <- renderPrint({
    print(testlj.train.sta)})}
  
  {output$prov21 <- renderPrint({
    print(gas.train.sta$model)})}
  
  # HMM K = 2
  
  output$hmmk2 <- renderPlot({
    hmmk2<- plot(pollution[1:9],col=mod.hmm.k2$yhat)
    print(hmmk2)}, height = 910)
  
  {output$sumhmmk2 <- renderPrint({
    print(summary(mod.hmm.k2))})}
  
  {output$roundhmmk2 <- renderPrint({
    print(round(mod.hmm.k2$model$transition,3))})}
  
  
  # HMM K = 3
  
  output$hmmk3 <- renderPlot({
    hmmk3 <- plot(pollution[1:9],col=mod.hmm.k3$yhat)
    print(hmmk3)}, height = 910)
  
  {output$sumhmmk3 <- renderPrint({
    print(summary(mod.hmm.k3))})}
  
  {output$roundhmmk3 <- renderPrint({
    print(round(mod.hmm.k3$model$transition,3))})}
  
  # K = 4
  
  output$hmmk4 <- renderPlot({
    hmmk4 <- plot(pollution[1:9],col=mod.hmm.k4$yhat)
    print(hmmk4)}, height = 910)
  
  {output$sumhmmk4 <- renderPrint({
    print(summary(mod.hmm.k4))})}
  
  {output$roundhmmk4 <- renderPrint({
    print(round(mod.hmm.k4$model$transition,3))})}
  
  
  # HMM K = 5
  
  output$hmmk5 <- renderPlot({
    hmmk5 <- plot(pollution[1:9],col=mod.hmm.k5$yhat)
    print(hmmk5)}, height = 910)
  
  {output$sumhmmk5 <- renderPrint({
    print(summary(mod.hmm.k5))})}
  
  {output$roundhmmk5 <- renderPrint({
    print(round(mod.hmm.k5$model$transition,3))})}
  
  # BIC 
  
  {output$bic <- renderPrint({
    print(c(fit(aic2),fit(aic3), fit(aic4), fit(aic5)))}
    
  )}
  
  # REGIME A POSTERIORI K = 2
  
  
  output$rp2S02 <- renderPlot({
    rp2S02 <- matplot(post_probsS022[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability') 
    legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')
    print(rp2S02)})
  
  output$rp2PM10 <- renderPlot({
    rp2PM10 <- matplot(post_probsPM102[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability') 
    legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')
    print(rp2PM10)})
  
  output$rp203 <- renderPlot({
    rp203 <- matplot(post_probs032[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')
    print(rp203)})
  
  output$rp2CO <- renderPlot({
    rp2CO <-  matplot(post_probsCO2[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')
    print(rp2CO)})
  
  output$rp2Toluene <- renderPlot({
    rp2Toluene <- matplot(post_probsToluene2[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')
    print(rp2Toluene)})
  
  
  # REGIME A POSTERIORI K = 3
  
  output$rp3S02 <- renderPlot({
    rp3S02 <- matplot(post_probsS023[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3'), fill=1:3, bty='n')
    print(rp3S02)})
  
  output$rp3PM10 <- renderPlot({
    rp3PM10 <- matplot(post_probsPM103[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3'), fill=1:3, bty='n')
    print(rp3PM10)})
  
  output$rp303 <- renderPlot({
    rp303 <- matplot(post_probs033[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3'), fill=1:3, bty='n')
    print(rp303)})
  
  output$rp3CO <- renderPlot({
    rp3CO <-  matplot(post_probsCO3[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3'), fill=1:3, bty='n')
    print(rp3CO)})
  
  output$rp3Toluene <- renderPlot({
    rp3Toluene <- matplot(post_probsToluene3[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3'), fill=1:3, bty='n')
    print(rp3Toluene)})
  
  
  # REGIME A POSTERIORI K = 4
  
  
  output$rp4S02 <- renderPlot({
    rp4S02 <- matplot(post_probsS024[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3', 'Regime 4'), fill=1:4, bty='n')
    print(rp4S02)})
  
  output$rp4PM10 <- renderPlot({
    rp4PM10 <- matplot(post_probsPM104[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3', 'Regime 4'), fill=1:4, bty='n')
    print(rp4PM10)})
  
  output$rp403 <- renderPlot({
    rp403 <- matplot(post_probs034[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3', 'Regime 4'), fill=1:4, bty='n')
    print(rp403)})
  
  output$rp4CO <- renderPlot({
    rp4CO <-  matplot(post_probsCO4[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3', 'Regime 4'), fill=1:4, bty='n')
    print(rp4CO)})
  
  output$rp4Toluene <- renderPlot({
    rp4Toluene <- matplot(post_probsToluene4[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3', 'Regime 4'), fill=1:4, bty='n')
    print(rp4Toluene)})
  
  
  # REGIME A POSTERIORI K = 5
  
  
  output$rp5S02 <- renderPlot({
    rp5S02 <- matplot(post_probsS025[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3', 'Regime 4', 'Regime 5'), fill=1:5, bty='n')
    print(rp5S02)})
  
  output$rp5PM10 <- renderPlot({
    rp5PM10 <- matplot(post_probsPM105[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3', 'Regime 4', 'Regime 5'), fill=1:5, bty='n')
    print(rp5PM10)})
  
  output$rp503 <- renderPlot({
    rp503 <- matplot(post_probs035[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3', 'Regime 4', 'Regime 5'), fill=1:5, bty='n')
    print(rp503)})
  
  output$rp5CO <- renderPlot({
    rp5CO <-  matplot(post_probsCO5[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3', 'Regime 4', 'Regime 5'), fill=1:5, bty='n')
    print(rp5CO)})
  
  output$rp5Toluene <- renderPlot({
    rp5Toluene <- matplot(post_probsToluene5[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
    legend(x='bottomleft', c('Regime 1','Regime 2', 'Regime 3', 'Regime 4', 'Regime 5'), fill=1:5, bty='n')
    print(rp5Toluene)})
  
  
  
  # HIDDEN SEMI-MARKOV MODELS
  # K = 2 GAMMA
  output$hsmmgam2 <- renderPlot({
    hsmmgam2 <- plot(pollution[1:9],col=prog2$yhat)
    print(hsmmgam2)}, height = 850)
  
  output$hsmmgam21 <- renderPlot({
    hsmmgam21 <- plot.hsmm(prog2)
    print(hsmmgam21)}, height = 850)
  
  # K = 3 GAMMA
  
  output$hsmmgam3 <- renderPlot({
    hsmmgam3 <- plot(pollution[1:9],col=prog3$yhat)
    print(hsmmgam3)}, height = 850)
  
  output$hsmmpgam32 <- renderPlot({
    hsmmpgam32 <- plot.hsmm(prog3)
    print(hsmmpgam32)}, height = 850)
  
  # K = 2 POISSON
  
  output$hsmmpoi2 <- renderPlot({
    hsmmpoi2 <- plot(pollution[1:9],col=prop2$yhat)
    print(hsmmpoi2)}, height = 850)
  
  output$hsmmpoi21 <- renderPlot({
    hsmmpoi21 <- plot.hsmm(prop2)
    print(hsmmpoi21)}, height = 850)
  
  # K = 3 POISSON
  
  output$hsmmpoi3 <- renderPlot({
    hsmmpoi3 <- plot(pollution[1:9],col=prop3$yhat)
    print(hsmmpoi3)}, height = 850)
  
  output$hsmmpoi31 <- renderPlot({
    hsmmpoi31 <- plot.hsmm(prop3)
    print(hsmmpoi31)}, height = 850)
  
  
  
}

#Avviamo l'app tramite il comando seguente
shinyApp(ui,server)
