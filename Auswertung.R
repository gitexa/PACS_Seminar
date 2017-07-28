# Seminar Collaboration and Collaborative Services (2017)
# Institut für Informationswirtschaft und Marketing (IISM) - Karlsruher Institut für Technologie 
# 
# In diesem Skript ist die Auswertungslogik für eine Umfrage zum Thema "Reading the Mind in the Eyes" enthalten.
# Das zugehörige Experiment wurde in Limesurvey umgesetzt. 
# Die Auswertung erfolgt mit Probit-Regression. 
#   Auswertung horizontal mit probit-reg; lin-reg 
#   Aswertung vertikal mit wilcoxon-test
# Teilnehmer 
#   Alexander Haas (1668040)
#   Theresa Hillemann (xy)


# Libraries
library("dplyr")

# Pfad zu den Daten
path = "~/Downloads/"
nameKG = "results-survey292465 (2).csv"
nameEG = "results-survey323724.csv"

# Daten einlesen und Parameter setzen 
dataKG = read.csv(paste(path,nameKG,sep=""))
dataEG = read.csv(paste(path,nameEG,sep=""))

dataKGtmp1 = dataKG[-2:-9]
dataKGtmp2 = dataKGtmp1[-38:-89]
dataKGM = dataKGtmp2
resultscountKG = dim(dataKG)[1]
dataKGSol = matrix(0, ncol = 38, nrow = resultscountKG)

dataEGtmp1 = dataEG[-2:-9]
dataEGtmp2 = dataEGtmp1[-38:-93]
dataEGM = dataEGtmp2
resultscountEG = dim(dataEG)[1]
dataEGSol = matrix(0, ncol = 38, nrow = resultscountEG)

#data = read.csv("~/Downloads/results-survey292465 (2).csv")  # read csv file

# Richtige Werte
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

# Reduzieren der Tabelle auf die wichtigsten Einträge
# TODO(Alex): Sauber in Function
#dataRedtmp = data[-2:-9]
#dataRed = dataRedtmp[-38:-89]
#dataM = as.matrix(dataRed)

# View(dataKGM)
# View(dataEGM)
# View(dataKGSol)
#colnames(dataKGSol)[1] = "id"
#colnames(dataEGSol)[1] = "id"

# Lösungsliste 
#solData = matrix(0, ncol = 38, nrow = resultscount)
#colnames(solData)[1] = "id"

###########################################################################################################
# Schreibe für jede korrekte Antwort eine 1, ansonsten eine 0 in die Lösungsmatrix
transformMatrix <- function(solData, dataM, resultscount) {
  for(j in 1:36) {
    for(k in 1:resultscount) {
      solData[k,1]=dataM[k,1]
      if(dataM[k,j+1]==a[j]) {
        solData[k,j+1]=1
      } else {
        solData[k,j+1]=0
      }
    }
  }
  return(solData)
}


###########################################################################################################
# Summiere die Anzahl der richtigen Antworten für jede id auf
aggregateAnswers <- function(solData, resultscount) {
  for(k in 1:resultscount) {
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
# Anzahl der insgesamt richtigen Antworten 
calculateMean <- function(solData, resultscount) {
  summe = 0 
  for(k in 1:resultscount) {
    summe = summe + as.numeric(solData[k,38])
  }
  print(summe)
  mean = summe/resultscount
  
  return(mean)
}

###########################################################################################################
# Ausgabe der Lösungsdaten mit ID 
redSolMatrix <- function(solData) {
  solDataRed = solData[,-2:-37]
  
  return(solDataRed)
}

View(dataKGSol[,-2:-37])
dataEGSol[,-2:-37]

# Auswertung von KG 
dataKGSol = transformMatrix(dataKGSol, dataKGM, resultscountKG)
dataKGSol = aggregateAnswers(dataKGSol, resultscountKG)
resultsKG = calculateMean(dataKGSol, resultscountKG)
print(paste("Results KG: ", resultsKG))
dataKGSolRed = redSolMatrix(dataKGSol)
View(dataKGSolRed)

# Auswertung von EG 
dataEGSol = transformMatrix(dataEGSol, dataEGM, resultscountEG)
dataEGSol = aggregateAnswers(dataEGSol, resultscountEG)
resultsEG = calculateMean(dataEGSol, resultscountEG)
print(paste("Results EG: ", resultsEG))
dataEGSolRed = redSolMatrix(dataEGSol)
View(dataEGSolRed)
