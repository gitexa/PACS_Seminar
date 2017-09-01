# Seminar Participation and Collaborative Services (2017)
# Institut für Informationswirtschaft und Marketing (IISM) - Karlsruher Institut für Technologie 
# 
# In diesem Skript ist die Auswertungslogik für eine Umfrage zum Thema "Reading the Mind in the Eyes" enthalten.
# Das zugehörige Experiment wurde in Limesurvey umgesetzt. 
# Die Auswertung erfolgt mit Probit-Regression/Wilcoxon-Tests. 
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
path = "~/Desktop/SS17/Seminar/02_Auswertung/PACS_Seminar/"
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
transformSolutionMatrix <- function(solData, dataM, solutions, sex) {
  
  for(j in 2:37) {
    for(k in 1:dim(dataM)[1]) {
      solData[k,1]=dataM[k,1]
      if(sex==TRUE) {
        solData[k,38]=as.character(dataM[k,38])
      }
      if(dataM[k,j]==solutions[j-1]) {
        solData[k,j]=1
      } else {
        solData[k,j]=0
      }
    }
  }
  return(solData)
}

###########################################################################################################
# Summiere die Anzahl der richtigen Antworten für jede id auf (Zeilensumme)
aggregateAnswersID <- function(solData) {
  
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
# Summiere die Anzahl der richtigen Antworten für jede Frage auf (Spaltensumme)
aggregateAnswersQuestions <- function(solData) {

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
# Berechne Mittelwert der richtigen Antworten 
calculateMean <- function(solData) {
  
  resultscount = dim(solData)[1]
  summe = 0 
  
  for(k in 1:resultscount) {
    summe = summe + as.numeric(solData[k,38])
  }
  
  mean = summe/resultscount
  
  return(mean)
}

###########################################################################################################
# Reduziere Solution-Matrix auf ID und Summe der Antworten
redSolMatrix <- function(solData) {
  
  solDataRed = solData[,-2:-37]
  
  return(solDataRed)
}

###########################################################################################################
# Program start
###########################################################################################################

###########################################################################################################
# Daten für Kontrollgruppe (KG) und einlesen und Parameter setzen 
dataKG = read.csv(paste(path,pathKG,sep=""))
dataEG = read.csv(paste(path,pathEG,sep=""))
correctSolutions = setResults()

# Matrizen mit Ergebnissen des Surveys auf wesentliche Spalten reduzieren und Evaluation-Matrix anlegen, jeweils für KG und EG
dataKG_Questions = dataKG %>% 
  select(Antwort.ID, Welche.der.folgenden.vier.Begriffe.beschreibt.am.besten..was.die.abgebildete.Person.denkt..fühlt.oder.ausdrückt.:X....Welche.der.folgenden.vier.Begriffe.beschreibt.am.besten..was.die.abgebildete.Person.denkt..fühlt.oder.ausdrückt..1)
dataKG_BinaryEvaluation = matrix(0, ncol = 38, nrow = dim(dataKG)[1]+1)
dataEG_Questions = dataEG %>%
  select(Antwort.ID, Welche.der.folgenden.vier.Begriffe.beschreibt.am.besten..was.die.abgebildete.Person.denkt..fühlt.oder.ausdrückt.:X....Welche.der.folgenden.vier.Begriffe.beschreibt.am.besten..was.die.abgebildete.Person.denkt..fühlt.oder.ausdrückt..1)
dataEG_BinaryEvaluation = matrix(0, ncol = 38, nrow = dim(dataEG)[1]+1)

# Auswertung von KG: transformiere in binäre Lösungsmatrix und aggregiere jeweils für ID und Frage
dataKG_BinaryEvaluation = transformSolutionMatrix(dataKG_BinaryEvaluation, dataKG_Questions, correctSolutions, FALSE)
dataKG_BinaryEvaluation = aggregateAnswersID(dataKG_BinaryEvaluation)
dataKG_BinaryEvaluation = aggregateAnswersQuestions(dataKG_BinaryEvaluation)
dataKG_BinaryEvaluation = as.data.frame(dataKG_BinaryEvaluation)
meanPerformanceKG = calculateMean(dataKG_BinaryEvaluation)
print(paste("Results KG: ", meanPerformanceKG))

# Auswertung von EG 
dataEG_BinaryEvaluation = transformSolutionMatrix(dataEG_BinaryEvaluation, dataEG_Questions, correctSolutions, FALSE)
dataEG_BinaryEvaluation = aggregateAnswersID(dataEG_BinaryEvaluation)
dataEG_BinaryEvaluation = aggregateAnswersQuestions(dataEG_BinaryEvaluation)
dataEG_BinaryEvaluation = as.data.frame(dataEG_BinaryEvaluation)
meanPerformanceEG = calculateMean(dataEG_BinaryEvaluation)
print(paste("Results EG: ", meanPerformanceEG))

###########################################################################################################
# Gesamtauswertung

# Aggregierte Antworten für ID und Frage aus Matrix kopieren
KG_IDResult = redSolMatrix(dataKG_BinaryEvaluation)
EG_IDResult = redSolMatrix(dataEG_BinaryEvaluation)
KG_QuestionResult = as.numeric(dataKG_BinaryEvaluation[53,] %>%
  select(V2:V37))
EG_QuestionResult = as.numeric(dataEG_BinaryEvaluation[46,] %>%
  select(V2:V37))

# Tabelle, welche nacheinander Ergebnisse für EG und KG für alle Testpersonen enthält
resultsKG = dim(dataKG)[1]
resultsEG = dim(dataEG)[1]
KGEG_IDResult = matrix(0, ncol = 2, nrow = resultsKG+resultsEG)
for(k in 1:(resultsKG+resultsEG)) {
  if(k<=resultsKG) {
    KGEG_IDResult[k,1] = "KG"
    KGEG_IDResult[k,2] = as.numeric(KG_IDResult[k,2])
  } else {
    KGEG_IDResult[k,1] = "EG"
    KGEG_IDResult[k,2] = as.numeric(EG_IDResult[k-resultsKG,2])
  }
}

KGEG_IDResult = as.data.frame(KGEG_IDResult)
colnames(KGEG_IDResult) <- c("Test", "Ergebnis")
KGEG_IDResult = transform(KGEG_IDResult, Ergebnis = as.numeric(as.character(Ergebnis)))

# Deksriptive Statistik
mean(KG_IDResult[,2])
median(KG_IDResult[,2])
sd(KG_IDResult[,2])
mean(EG_IDResult[,2])
median(EG_IDResult[,2])
sd(EG_IDResult[,2])

# Plot
hist(KG_IDResult[,2], breaks=20, xlim=c(15,max(KG_IDResult[,2])), ylim = c(0,10), col="black", border="white", xlab="Eyes Test score", ylab="percent of subjects", main="")
hist(EG_IDResult[,2], breaks=19, xlim=c(16,max(EG_IDResult[,2])), ylim = c(0,9), col="black", border="white", xlab="Eyes Test score", ylab="percent of subjects", main="")

# Signifikanztests
wilcox.test(x=KG_QuestionResult, y=EG_QuestionResult, alternative="two.sided", paired=T,exact=0)
wilcox.test(x=KG_QuestionResult, y=EG_QuestionResult, correct=FALSE)
wilcox.test(x=KG_QuestionResult, y=EG_QuestionResult, paired=TRUE)
wilcox_test(Ergebnis~Test, data = KGEG_IDResult)
wilcox.test(x=KG_IDResult[,2],y=EG_IDResult[,2],
            alternative="two.sided", paired=T,exact=0)


###########################################################################################################
# Auswertung nach Geschlecht
# dataKG_m = dataKG %>% select(Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m..) %>% filter(Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m.. %in% c("w", "W", "Weiblich", "weiblich"))
# dataKG_m = dataKG %>% filter(Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m.. %in% c("w", "W", "Weiblich", "weiblich"))
# dataKG_w = dataKG %>% select(Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m..) %>% filter(Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m.. %in% c("m", "M", "Männlich", "männlich"))

# Nach Geschlecht filtern
dataKG_Questions_Sex = dataKG %>%
  select(Antwort.ID,Welche.der.folgenden.vier.Begriffe.beschreibt.am.besten..was.die.abgebildete.Person.denkt..fühlt.oder.ausdrückt.:X....Welche.der.folgenden.vier.Begriffe.beschreibt.am.besten..was.die.abgebildete.Person.denkt..fühlt.oder.ausdrückt..1,Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m..)
dataKG_BinaryEvaluation_Sex = matrix(0, ncol = dim(dataKG_Questions_Sex)[2], nrow = dim(dataKG_Questions_Sex)[1])
dataKG_BinaryEvaluation_Sex = transformSolutionMatrix(dataKG_BinaryEvaluation_Sex, dataKG_Questions_Sex, correctSolutions, TRUE)

dataEG_Questions_Sex = dataEG %>%
  select(Antwort.ID,Welche.der.folgenden.vier.Begriffe.beschreibt.am.besten..was.die.abgebildete.Person.denkt..fühlt.oder.ausdrückt.:X....Welche.der.folgenden.vier.Begriffe.beschreibt.am.besten..was.die.abgebildete.Person.denkt..fühlt.oder.ausdrückt..1,X.Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m..)
dataEG_BinaryEvaluation_Sex = matrix(0, ncol = dim(dataEG_Questions_Sex)[2], nrow = dim(dataEG_Questions_Sex)[1])
dataEG_BinaryEvaluation_Sex = transformSolutionMatrix(dataEG_BinaryEvaluation_Sex, dataEG_Questions_Sex, correctSolutions, TRUE)

# Einheitliche Schreibweise
dataKG_BinaryEvaluation_Sex[1,38] == "w" | dataKG_BinaryEvaluation_Sex[1,38] == "W" | dataKG_BinaryEvaluation_Sex[1,38] == "weiblich" | dataKG_BinaryEvaluation_Sex[1,38] == "Weiblich"
dataKG_BinaryEvaluation_Sex[1,38] == "m" | dataKG_BinaryEvaluation_Sex[1,38] == "M" | dataKG_BinaryEvaluation_Sex[1,38] == "männlich" | dataKG_BinaryEvaluation_Sex[1,38] == "Männlich"
filter(dataKGgSolT, grepl('W|w|weiblich', Zur.Auswertung.benötigen.wir.noch.einige.wenige.persönliche.Angaben.von.Dir...Geschlecht..w.m..))

# Ausgabe der Ergebnisse in Datei (bspw. zur Analyse in Excel)
write.csv(dataKG_BinaryEvaluation_Sex, file = "~/Desktop/GenderEvalKG.csv")
write.csv(dataEG_BinaryEvaluation_Sex, file = "~/Desktop/GenderEvalEG.csv")

###########################################################################################################
# Select best participants for raffle
  
topN  = ceiling(length(dataKG_BinaryEvaluation[,38])*0.2)

dataKG_Raffle_TopN_ID = dataKG_BinaryEvaluation %>%
  select(V1, V38) %>%
  top_n(n = topN, wt = V38) %>%
  arrange(desc(V38)) %>%
  select(V1)
  
dataKG_Raffle_TopN_Mail = dataKG %>%
  select(Antwort.ID, Möchtest.Du.erfahren..wie.gut.Du.im.Vergleich.zu.den.anderen.Teilnehmern.abgeschnitten.hast.und.über.die.Ergebnisse.der.Studie.informiert.werden., Wenn.Du.am.Gewinnspiel.teilnehmen.und.einen.Gutschein.für.Goldzünglein.gewinnen..oder.die.Ergebnisse.des.Tests.von.uns.zugeschickt.bekommen.möchtest..hinterlege.bitte.Deine.Email.Adresse..so.dass.wir.Dich.kontaktieren.können.) %>%
  filter(Antwort.ID %in% dataKG_Raffle_TopN_ID$V1) %>%
  filter(Möchtest.Du.erfahren..wie.gut.Du.im.Vergleich.zu.den.anderen.Teilnehmern.abgeschnitten.hast.und.über.die.Ergebnisse.der.Studie.informiert.werden.!="Nein") %>%
  filter(Wenn.Du.am.Gewinnspiel.teilnehmen.und.einen.Gutschein.für.Goldzünglein.gewinnen..oder.die.Ergebnisse.des.Tests.von.uns.zugeschickt.bekommen.möchtest..hinterlege.bitte.Deine.Email.Adresse..so.dass.wir.Dich.kontaktieren.können.!="") %>%
  select(Wenn.Du.am.Gewinnspiel.teilnehmen.und.einen.Gutschein.für.Goldzünglein.gewinnen..oder.die.Ergebnisse.des.Tests.von.uns.zugeschickt.bekommen.möchtest..hinterlege.bitte.Deine.Email.Adresse..so.dass.wir.Dich.kontaktieren.können.)

dataEG_Raffle_TopN_ID = dataEG_BinaryEvaluation %>%
  select(V1, V38) %>%
  top_n(n = topN, wt = V38) %>%
  arrange(desc(V38)) %>%
  select(V1)

dataEG_Raffle_TopN_Mail = dataEG %>%
  select(Antwort.ID, Möchtest.Du.erfahren..wie.gut.Du.im.Vergleich.zu.den.anderen.Teilnehmern.abgeschnitten.hast.und.über.die.Ergebnisse.der.Studie.informiert.werden., Wenn.Du.am.Gewinnspiel.teilnehmen.und.einen.Gutschein.für.Goldzünglein.gewinnen..oder.die.Ergebnisse.des.Tests.von.uns.zugeschickt.bekommen.möchtest..hinterlege.bitte.Deine.Email.Adresse..so.dass.wir.Dich.kontaktieren.können.) %>%
  filter(Antwort.ID %in% dataEG_Raffle_TopN_ID$V1) %>%
  filter(Möchtest.Du.erfahren..wie.gut.Du.im.Vergleich.zu.den.anderen.Teilnehmern.abgeschnitten.hast.und.über.die.Ergebnisse.der.Studie.informiert.werden.!="Nein") %>%
  filter(Wenn.Du.am.Gewinnspiel.teilnehmen.und.einen.Gutschein.für.Goldzünglein.gewinnen..oder.die.Ergebnisse.des.Tests.von.uns.zugeschickt.bekommen.möchtest..hinterlege.bitte.Deine.Email.Adresse..so.dass.wir.Dich.kontaktieren.können.!="") %>%
  select(Wenn.Du.am.Gewinnspiel.teilnehmen.und.einen.Gutschein.für.Goldzünglein.gewinnen..oder.die.Ergebnisse.des.Tests.von.uns.zugeschickt.bekommen.möchtest..hinterlege.bitte.Deine.Email.Adresse..so.dass.wir.Dich.kontaktieren.können.)

# Zufällige Auswahl der Gewinner
KG_winners = dataKG_Raffle_TopN_Mail %>%
  sample_n(size = 5, replace = FALSE)

EG_winners = dataEG_Raffle_TopN_Mail %>%
  sample_n(size = 5, replace = FALSE)
