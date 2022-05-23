#POWERANALYSE

#sæt working directory
setwd("/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Poweranalyse")

#load pakker
library(devtools)
library(cjpowR)

#Hypotese 1

#Power-analyse for effekten af udemokratisk adfærd over for Folketinget (niveau taget fra Gidengil et al. (2020))
cjpowr_amce(amce=0.05, power = 0.80, levels = 2, alpha=0.05)

#Power-analyse for effekten af udemokratisk adfærd over for journalister 
#(ingen tidligere studier med samme tilgang. Dog et studie fra Wunsch et al. (2022), 
#der finder AMCE på 0,1 for autoritære holdninger til medier. Jeg bruger et konsevativt estimat på 0,03.
cjpowr_amce(amce=0.03, power = 0.80, levels = 2, alpha=0.05)

#Hypotese 2

#UDEMOKRATISK ADFÆRD X KONGRUENS (niveauer taget fra Saikkonnen & Christensen (2021))
cjpowr_amcie(delta1 = 0, delta2 = 0, delta3 = 0.083, levels1=2, levels2=3, power= 0.80)

#Hypotese 3

#udemokratisk adfærd X Kompetence (AMCE taget fra Frederiksen (2022b))
cjpowr_amcie(delta1 = 0, delta2 = 0, delta3 = 0.041, levels1=2, levels2=2, power = 0.8)