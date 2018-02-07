#Übungsaufgaben Tutorium: Sitzung 2 (09.02.18)

#1. Berechnen Sie die Häufigkeitsverteilung der Variablen imsmetn/imsfetn für
#andere Länder als Deutschland. Wir schauen uns mal die Unterschiede zwischen
#der Schweiz und Ungarn an.

#1. Nochmal Pakete laden und Datensatz einspeisen:

library(plyr) # Automatisierung
library(tidyverse) #Schweizer Taschenmesser des Datamanagement
library(haven) #SPSS-Daten laden
library(magrittr) #Pipes
library(psych) #Reliabilitätsanalyse
library(memisc) #Martin Elffs paket
library(sjPlot) # Schöne Regressionstabellen

# Data -------------------------------------------------------------------------
# Wir laden unsere Daten
ess <- read_spss(file = "data/Data_ESS/ESS7e02_1.sav",
                 user_na = TRUE)


#2. Welche Länder sind im Datensatz vorhanden?

table(ess$cntry)

# 2.1 Schweiz auswählen 

  data <- ess$cntry == "CH"
  table(data)
  schwitz <- ess[data, ]
  dim(schwitz)#Wir sehen: 1532 Zeilen und 601 Spalten
  
# 2.2 Nun brauchen wir nur 2 Variablen (Spalten): Imsmetn,Imdfetn
  
  data_var <- c("imsmetn","imdfetn")#Auswählen unserer Variablen
  schwitz <- schwitz[, data_var]
  dim(schwitz)#Schon besser: 1532 Zeilen und 3 Spalten
  
  table(schwitz$imsmetn) #Problematisch: Daten enthalten Missing Values
  table(schwitz$imsmetn) #Problematisch: Daten enthalten Missing Values

  
#3. Rekodiern und Daten bereinigen

# 3.1 Einmal für dieselbe Kultur:Imsmetn
  
schwitz %<>% within({
  culture_same <- imsmetn
  culture_same[imsmetn %in% c(7:9)] <- NA #Wir beseitigen nicht verwertbare Antworten

# 3.2 Einmal für andere Kulturen:imdfetn

  culture_others <- imdfetn
  culture_others[imdfetn %in% c(7:9)] <-NA
  })

#4. Häufigkeitstabelle visualisieren: Hier bietet sich der "table"-Befehl an

table(schwitz$culture_same)  #Schon besser
table(schwitz$culture_others)

#Zum Üben einfach nochmal mit Ungarn machen!

#-------------------------------------------------------------#

#2. Wieviel TV schauen die Deutschen an einem gewöhnlichen Werktag ?

#Zunächst: Selbe Prozedur wie oben

#1. Deutschland im Datensatz auswählne

dta <- ess$cntry == "DE"
ger <- ess[dta, ]

#2. Variablen Vektor erstellen

dta_var <- c("tvtot")
ger <- ess[, dta_var]

#3. Daten bereinigen

ger %<>% within({
  tv <- tvtot
  tv[tvtot %in% c(88,77,99)] <- NA
})

#4. Ergebnis bewundern: Häufigkeitstabelle

table(ger$tv)

  #4.1 Durchschnitt?

  mean(ger$tv, na.rm = T)
  
  #Ein Blick in den Fragebogen zeigt: "4" entspricht ca. 2h/Tag
  
#5. Wie sehr interessieren sich die Deutschen für Politik? Hier: poltinr
  
  #5.1 Variable in unserer Daten-Matrix auswählen
  
  ger_pol <- c("polintr")#Wichtig: "" Nicht vergessen
  ger <- ess[,ger_pol]
  
  #5.2 Rekodieren und bereinigen
  
  ger %<>% within({
    politik <- polintr
    politik[polintr %in% c(8)]<- NA
    
  })

# 6. Häufigkeitstabelle
  
table(ger$polintr)
mean(ger$polintr, na.rm = T)

# Die Deutschen interessieren sich also ziemlich bis sehr für Politik

#------------------------------------------------------------------#

#3. Links-Rechts Skala: Wie verteilen sich die Deutschen im Links-Rechts Spektrum?
# Variable: lrscale

  #3.1 Variable in Daten-Matrix auswählen

ger_spekt <- c("lrscale")
ger <- ess[ dta,ger_spekt]

  #3.2 Rekdieren und Bereinigen

ger %<>% within({
  pol.lr <- lrscale
  pol.lr[lrscale %in% c(88,77,99)] <- NA
  
})
  
#4. Häufigkeitserteilung und Durchschnitt:

table(ger$pol.lr)
mean(ger$pol.lr, na.rm = T)#4.5; Etwas mehr links als Rechts


#5.1 Bonus: Österreich

data.aut <- ess$cntry == "AT"
aut <- ess[data.aut, ]

data.aut.var <- c("lrscale")
aut <- ess[ data.aut,data.aut.var]

aut %<>% within({
  lr.aut <- lrscale
  lr.aut[lrscale %in% c(88,77,99)]<-NA
})

table(aut$lr.aut)
mean(aut$lr.aut, na.rm = T)
  