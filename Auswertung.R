# Auswertung horizontal mit probit-reg; lin-reg 
# Aswertung vertikal mit wilcoxon-test

# Daten einlesen und Parameter setzen 
data = read.csv("~/Downloads/results-survey292465 (2).csv")  # read csv file
results-survey292465
resultscount = dim(data)[1]

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

# Reduzieren der Tabelle auf die wichtigsten Einträge 
dataRedtmp = data[-2:-9]
dataRed = dataRedtmp[-38:-89]

# Lösungsliste 
sol_data = list()

tmp = list()
for(j in 1:resultscount) {
  tmp = append(tmp,j)
  #  tmp <- list(uniform=a, normal=b, binomial=c)
}

tmpp = as.vector(tmp)

for(i in 02:37){
  tmp = 1
  name = paste('F',i,sep='')
  sol_data[[name]] <- tmp
}

data$F2
