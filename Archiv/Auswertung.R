# Seminar Participation and Collaborative Services (2017)
# Institut für Informationswirtschaft und Marketing (IISM) - Karlsruher Institut für Technologie 
# 
# In diesem Skript ist die Auswertungslogik für eine Umfrage zum Thema "Reading the Mind in the Eyes" enthalten.
# Das zugehörige Experiment wurde in Limesurvey umgesetzt. 
# Die Auswertung erfolgt mit Probit-Regression. 
#   Auswertung horizontal mit probit-reg; lin-reg 
#   Aswertung vertikal mit wilcoxon-test
# Teilnehmer 
#   Alexander Haas
#   Theresa Hillemann  

###########################################################################################################
# Libraries
library("dplyr")
library("coin")

# Pfad zu den Daten setzen
path = "~/Desktop/Uni/Seminar/02_Auswertung/PACS_Seminar/"
pathKG = "AuswertungKG.csv"
pathEG = "AuswertungEG.csv"

###########################################################################################################
# Methoden

###########################################################################################################
# Setze Ergebnisbelegung
setResults <- function() {

    A2 = "lustig"
    A3 = "bestürzt"
    A4 = "begehrend"
    A5 = "darauf bestehend"
    A6 = "besorgt"
    A7 = "tagträumend"
    A8 = "unruhig"
    A9 = "verzweifelt"
    A10 = "geistesabwesend"
    A11 = "vorsichtig"
    A12 = "bedauernd"
    A13 = "skeptisch"
    A14 = "vorausahnend"
    A15 = "beschuldigend"
    A16 = "besinnlich"
    A17 = "nachdenklich"
    A18 = "bezweifelnd"
    A19 = "entschieden"
    A20 = "zögerlich"
    A21 = "freundlich"
    A22 = "tagträumend"
    A23 = "geistesabwesend"
    A24 = "aufsässig"
    A25 = "nachsinnend"
    A26 = "interessiert"
    A27 = "feindselig"
    A28 = "vorsichtig"
    A29 = "interessiert"
    A30 = "tiefsinnig"
    A31 = "kokett"
    A32 = "zuversichtlich"
    A33 = "ernst"
    A34 = "beunruhigt"
    A35 = "misstrauisch"
    A36 = "nervös"
    A37 = "argwöhnisch"
    
    a = c(A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29,A30,A31,A32,A33,A34,A35,A36,A37)

    return(a)
}

###########################################################################################################
# Schreibe für jede korrekte Antwort eine 1, ansonsten eine 0 in die Lösungsmatrix (dataM enthält Antworten aus Umfrage)
transformMatrix <- function(solData, dataM,  sex) {
  
  for(j in 2:37) {
    for(k in 1:dim(dataM)[1]) {
      solData[k,1]=dataM[k,1]
      if(sex==TRUE) {
        solData[k,38]=as.character(dataM[k,38])
      }
      if(dataM[k,j]==a[j-1]) {
        solData[k,j]=1
      } else {
        solData[k,j]=0
      }
    }
  }
  return(solData)
}


###########################################################################################################
# Summiere die Anzahl der richtigen Antworten für jede id auf
aggregateAnswers <- function(solData) {
  
  for(k in 1:dim(solData)[1]) {
    i = 0 
    for(j in 2:37) {
      if(solData[k,j]==1) {
        i = i + 1 
      }
    }
    solData[k,j+1] = i
  }
  
  return(solData)
}


###########################################################################################################
# Summiere die Anzahl der richtigen Antworten für jede Frage auf
aggregateAnswersEveryPic <- function(solData) {

  h = 0
  
  for(y in 2:(dim(solData)[2]-1)) {
    for(z in 1:(dim(solData)[1]-1)) {
      h = h + solData[z,y]
    }
    #print(h)
    solData[dim(solData)[1],y] = h
    h = 0
  }
  
  return(solData)
}


###########################################################################################################
# Anzahl der insgesamt richtigen Antworten 
calculateMean <- function(solData) {
  
  resultscount = dim(solData)[1]
  summe = 0 
  
  for(k in 1:resultscount) {
    summe = summe + as.numeric(solData[k,38])
  }
  
  #print(summe)
  mean = summe/resultscount
  
  return(mean)
}

###########################################################################################################
# Anzahl der insgesamt richtigen Antworten 
calculateMeanMatrix <- function(solData) {
  
  resultscount = dim(solData)[1]
  summe = 0 
  
  for(k in 1:resultscount) {
    summe = summe + as.numeric(solData[k,38])
  }
  #print(summe)
  mean = summe/resultscount
  
  return(mean)
}

###########################################################################################################
# Ausgabe der Lösungsdaten mit ID 
redSolMatrix <- function(solData) {
  
  solDataRed = solData[,-2:-37]
  
  return(solDataRed)
}


###########################################################################################################
# Program start

# Daten einlesen und Parameter setzen 
dataKG = read.csv(paste(path,pathKG,sep=""))
dataEG = read.csv(paste(path,pathEG,sep=""))
a = setResults()

# Matrizen mit Ergebnissen des Surveys auf wesentliche Spalten reduzieren und Sol-Matrix anlegen, jeweils für KG und EG
dataKGtmp1 = dataKG[-2:-9]
dataKGtmp2 = dataKGtmp1[-38:-89]
dataKGM = dataKGtmp2
dataKGSol = matrix(0, ncol = 38, nrow = dim(dataKG)[1]+1)

dataEGtmp1 = dataEG[-2:-9]
dataEGtmp2 = dataEGtmp1[-38:-93]
dataEGM = dataEGtmp2
dataEGSol = matrix(0, ncol = 38, nrow = dim(dataEG)[1]+1)

# Auswertung von KG
dataKGSol = transformMatrix(dataKGSol, dataKGM, FALSE)
dataKGSol = aggregateAnswers(dataKGSol)
dataKGSol = aggregateAnswersEveryPic(dataKGSol)
resultsKG = calculateMean(dataKGSol)
print(paste("Results KG: ", resultsKG))
KGres = redSolMatrix(dataKGSol)
View(KGres)

# Auswertung von EG 
dataEGSol = transformMatrix(dataEGSol, dataEGM, FALSE)
dataEGSol = aggregateAnswers(dataEGSol)
dataEGSol = aggregateAnswersEveryPic(dataEGSol)
resultsEG = calculateMean(dataEGSol)
print(paste("Results EG: ", resultsEG))
EGres = redSolMatrix(dataEGSol)
View(EGres)

# Signifikanz 
sa = dataKGSol[53,]
sb = dataEGSol[46,]
at = sa[-1]
bt = sb[-1]
att = at[-37]
btt = bt[-37]

wilcox.test(x=att,y=btt,alternative="two.sided", paired=T,exact=0)
wilcox.test(x=att,y=btt,correct=FALSE)
wilcox.test(x=att,y=btt,paired=TRUE)

# Auswertung nach Geschlecht
# dataKG_m = dataKG %>% select(Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m..) %>% filter(Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m.. %in% c("w", "W", "Weiblich", "weiblich"))
# dataKG_m = dataKG %>% filter(Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m.. %in% c("w", "W", "Weiblich", "weiblich"))
# dataKG_w = dataKG %>% select(Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m..) %>% filter(Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m.. %in% c("m", "M", "Männlich", "männlich"))
dataKGg1 = dataKG[-2:-9]
dataKGg2 = dataKGg1[-41:-97]
dataKGg3 = dataKGg2[-38:-39]
dataKGgSol = matrix(0, ncol = dim(dataKGg3)[2], nrow = dim(dataKGg3)[1])
dataKGgSolT = transformMatrix(dataKGgSol, dataKGg3, TRUE)
#dataKGgSolT[,38] = dataKGg3[,38]

dataEGg1 = dataEG[-2:-9]
dataEGg2 = dataEGg1[-43:-97]
dataEGg3 = dataEGg2[-38:-41]
dataEGgSol = matrix(0, ncol = dim(dataEGg3)[2], nrow = dim(dataEGg3)[1])
dataEGgSolT = transformMatrix(dataEGgSol, dataEGg3, TRUE)
#dataKGgSolT[,38] = dataKGg3[,38]

write.csv(dataKGgSolT, file = "~/Desktop/GenderEval.csv")
write.csv(dataEGgSolT, file = "~/Desktop/GenderEvalEG.csv")


dataKGgSolT[1,38] == "w" | dataKGgSolT[1,38] == "W" | dataKGgSolT[1,38] == "weiblich" | dataKGgSolT[1,38] == "Weiblich"
dataKGgSolT[1,38] == "m" | dataKGgSolT[1,38] == "M" | dataKGgSolT[1,38] == "männlich" | dataKGgSolT[1,38] == "Männlich"



filter(dataKGgSolT, grepl('W|w|weiblich', Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m..))

# Summary
summary = matrix(0, ncol = 2, nrow = resultscountEG+resultscountKG)
View(summary)
for(k in 1:(resultscountKG+resultscountEG)) {
  if(k<=resultscountKG) {
    summary[k,1] = "KG"
    summary[k,2] = as.numeric(KGres[k,2])
  } else {
    summary[k,1] = "EG"
    summary[k,2] = as.numeric(EGres[k-resultscountKG,2])
  }
}

su = as.data.frame(summary)
colnames(su) <- c("Test", "Ergebnis")
summaryfinal = transform(su, Ergebnis = as.numeric(as.character(Ergebnis)))
#typeof(su$Ergebnis[1])
#is.numeric(su1$Ergebnis[1])
View(summaryfinal)
wilcox_test(Ergebnis~Test, data = summaryfinal)

wilcox.test(x=KGres[,2],y=EGres[,2],
            alternative="two.sided", paired=T,exact=0)

mean(KGres[,2])
median(KGres[,2])
sd(KGres[,2])
mean(EGres[,2])
median(EGres[,2])
sd(EGres[,2])

# Plot
hist(KGres[,2], breaks=20, xlim=c(15,max(KGres[,2])), ylim = c(0,10), col="black", border="white", xlab="Eyes Test score", ylab="percent of subjects", main="")
hist(EGres[,2], breaks=19, xlim=c(16,max(EGres[,2])), ylim = c(0,9), col="black", border="white", xlab="Eyes Test score", ylab="percent of subjects", main="")

###########################################################################################################
# Select best participants for raffle
  
dataKGRaffle = as.data.frame(dataKGSol)
KGTopN  = ceiling(length(dataKGSol[,38])*0.2)

dataKGRaffleTopN_ID = dataKGRaffle %>%
  select(V1, V38) %>%
  top_n(n = KGnumber, wt = V38) %>%
  arrange(desc(V38)) %>%
  select(V1)

dataKGRaffleTopN_Mail = dataKG %>%
  select(Antwort.ID, Möchtest.Du.erfahren..wie.gut.Du.im.Vergleich.zu.den.anderen.Teilnehmern.abgeschnitten.hast.und.über.die.Ergebnisse.der.Studie.informiert.werden., Wenn.Du.am.Gewinnspiel.teilnehmen.und.einen.Gutschein.für.Goldzünglein.gewinnen..oder.die.Ergebnisse.des.Tests.von.uns.zugeschickt.bekommen.möchtest..hinterlege.bitte.Deine.Email.Adresse..so.dass.wir.Dich.kontaktieren.können.) %>%
  filter(Antwort.ID %in% dataKGRaffleTopN_ID$V1) %>%
  filter(Möchtest.Du.erfahren..wie.gut.Du.im.Vergleich.zu.den.anderen.Teilnehmern.abgeschnitten.hast.und.über.die.Ergebnisse.der.Studie.informiert.werden.!="Nein") %>%
  filter(Wenn.Du.am.Gewinnspiel.teilnehmen.und.einen.Gutschein.für.Goldzünglein.gewinnen..oder.die.Ergebnisse.des.Tests.von.uns.zugeschickt.bekommen.möchtest..hinterlege.bitte.Deine.Email.Adresse..so.dass.wir.Dich.kontaktieren.können.!="") %>%
  select(Wenn.Du.am.Gewinnspiel.teilnehmen.und.einen.Gutschein.für.Goldzünglein.gewinnen..oder.die.Ergebnisse.des.Tests.von.uns.zugeschickt.bekommen.möchtest..hinterlege.bitte.Deine.Email.Adresse..so.dass.wir.Dich.kontaktieren.können.)

# Zufällige Auswahl abzüglich der Besten