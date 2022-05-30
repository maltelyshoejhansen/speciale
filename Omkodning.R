#Omkodning af data
#indstil working directory
setwd("/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Omkodning")

#indlæs pakker
library("readxl")
library("xlsx")
library("dplyr")

#Indlæs data
df <- read_excel("/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Rådata/CONJOINT EXCEL FINAL.xlsx")

#fjern respondenter under 3 minutter, da de sandsynligvis ikke har tænkt grundigt nok over modtaget information
df <- df %>% 
  filter(RESPONDENT_LENGTH_OF_INTERVIEW_SECONDS > 180)


#Opret nye variable fra survey variable
df <- df %>%
  mutate(køn = case_when(Q2_KØN_KVINDE==1 ~ "Kvinde", Q2_KØN_MAND==1 ~ "Mand"),
         region = case_when(Q4_REGION_NORDJYLLAND==1 ~ "Nordjylland", Q4_REGION_MIDTJYLLAND==1 ~ "Midtjylland", 
                            Q4_REGION_SYDDANMARK==1 ~ "Syddanmark",Q4_REGION_HOVEDSTADEN==1 ~ "Hovedstaden", 
                            Q4_REGION_SJÆLLAND==1 ~ "Sjælland"),
         uddannelse = case_when(Q5_UDDANNELSE_GRUNDSKOLE==1 ~ "Grundskole", Q5_UDDANNELSE_STUDENTEREKSAMEN==1 ~ "Studentereksamen",
                                Q5_UDDANNELSE_ERHVERVSFAGLIG==1 ~ "Erhvervsuddannelse", 
                                Q5_UDDANNELSE_KORT_VIDEREGÅENDE==1 ~ "Kort videregående", 
                                Q5_UDDANNELSE_MELLEMLANG_VIDEREGÅENDE==1 ~ "Mellemlang videregående",
                                Q5_UDDANNELSE_BACHELORUDDANNELSE==1 ~ "Bacheloruddannelse",
                                Q5_UDDANNELSE_LANG_VIDEREGÅENDE==1 ~ "Lang videregående"),
         parti = case_when(Q9_PARTI_SOCIALDEMOKRATIET==1 ~ "Socialdemokratiet", Q9_PARTI_RADIKALE_VENSTRE==1 ~ "Radikale", 
                           Q9_PARTI_DET_KONSERVATIVE_FOLKEPARTI==1 ~ "Konservative", Q9_PARTI_NYE_BORGERLIGE==1 ~ "Nye borgerlige", 
                           Q9_PARTI_KLAUS_RISKÆR_PEDERSEN==1 ~ "Klaus Riskær Pedersen", Q9_PARTI_SOCIALISTISK_FOLKEPARTI==1 ~ "Socialistisk Folkeparti", 
                           Q9_PARTI_LIBERAL_ALLIANCE==1 ~ "Liberal Alliance", Q9_PARTI_KRISTENDEMOKRATERNE==1 ~ "Kristendemokraterne", 
                           Q9_PARTI_DANSK_FOLKEPARTI==1 ~ "Dansk Folkeparti", Q9_PARTI_STRAM_KURS==1 ~ "Stram Kurs",
                           Q9_PARTI_VENSTRE==1 ~ "Venstre", Q9_PARTI_ENHEDSLISTEN==1 ~ "Enhedslisten", 
                           Q9_PARTI_ALTERNATIVET==1 ~ "Alternativet"),
         indvandring = case_when(Q14_INDVANDRING_DANMARK_SKAL_MODTAGE_DET_SAMME_ANTAL_INDVANDRERE_SOM_I_DAG==1 ~ "Samme antal indvandrere", 
                                 Q14_INDVANDRING_DANMARK_SKAL_MODTAGE_FLERE_INDVANDRERE==1 ~ "Flere indvandrere", 
                                 Q14_INDVANDRING_DANMARK_SKAL_MODTAGE_FÆRRE_INDVANDRERE==1 ~ "Færre indvandrere"),
         ligestilling = case_when(Q15_LIGESTILLING_LIGESTILLINGEN_MELLEM_MÆND_OG_KVINDER_ER_OPNÅET==1 ~ "Ligestilling opnået", 
                                  Q15_LIGESTILLING_DER_ER_STADIG_IKKE_LIGESTILLING_MELLEM_MÆND_OG_KVINDER==1 ~ "Ligestilling ikke opnået",
                                  Q15_LIGESTILLING_LIGESTILLINGEN_MELLEM_MÆND_OG_KVINDER_ER_GÅET_FOR_LANGT==1 ~ "Ligestilling gået for langt"))

#Omkodning af variable for fiktive kandidater
df$kand_indvandring<-factor(df$kand_indvandring, 
                            labels=c("Samme antal indvandrere",
                                     "Færre indvandrere",
                                     "Flere indvandrere"))

df$kand_ligestilling<-factor(df$kand_ligestilling, 
                             labels=c("Ligestilling ikke opnået",
                                      "Ligestilling gået for langt",
                                      "Ligestilling opnået"))

df$kand_FT<-factor(df$kand_FT, 
                   labels=c("Vil aldrig lyve for Folketinget",
                            "Villig til at lyve for Folketinget"))

df$kand_journalister<-factor(df$kand_journalister, 
                           labels=c("Deltager aldrig i interviews",
                                    "Deltager ofte i interviews"))

#tjek data
#baggrundsdata for respondenter
table(df$køn)
table(df$Q3_ALDER)
table(df$region)
table(df$iddannelse)
table(df$parti)
table(df$indvandring)
table(df$ligestilling)
table(df$Q10_DEM_TILFREDS)
table(df$Q11_TILLID_1)
table(df$Q12_TILLID_2)
table(df$Q13_TILLID_3)

#data for fiktive kandidater
table(df$kand_køn)
table(df$kand_uddannelse)
table(df$kand_beskæftigelse)
table(df$kand_indvandring)
table(df$kand_ligestilling)
table(df$kand_FT)
table(df$kand_journalister)

#tjek class for variable
#baggrunddata for respondenter
class(df$køn) #character
class(df$Q3_ALDER) #numeric
class(df$region) #character
class(df$uddannelse) #character
class(df$parti) #character
class(df$indvandring) #character
class(df$ligestilling) #character
class(df$Q10_DEM_TILFREDS) #numeric
class(df$Q11_TILLID_1) #numeric
class(df$Q12_TILLID_2) #numeric
class(df$Q13_TILLID_3) #numeric

#data for fiktive kandidater
class(df$kand_køn) #character
class(df$kand_uddannelse) #character
class(df$kand_beskæftigelse) #character
class(df$kand_indvandring) #character
class(df$kand_ligestilling) #character
class(df$kand_FT) #character
class(df$kand_journalister) #character

#omkodning af kategoriske variable til at være factor frem for character
df$køn <- factor(df$køn)
df$region <- factor(df$region)
df$uddannelse <- factor(df$uddannelse)
df$parti <- factor(df$parti)
df$indvandring <- factor(df$indvandring)
df$ligestilling <- factor(df$ligestilling)
df$kand_køn <- factor(df$kand_køn)
df$kand_uddannelse <- factor(df$kand_uddannelse)
df$kand_beskæftigelse <- factor(df$kand_beskæftigelse)
df$kand_indvandring <- factor(df$kand_indvandring)
df$kand_ligestilling <- factor(df$kand_ligestilling)
df$kand_FT <- factor(df$kand_FT)
df$kand_journalister <- factor(df$kand_journalister)

#Omkodning af niveauer i variable
#Justering af niveauer for respondet-variable
table(df$indvandring) #skal omkodes (til: samme antal, flere ind, færre ind)
table(df$ligestilling) #skal omkodes (til: opnået, ikke opnået, gået for langt)

df$indvandring <- factor(df$indvandring, levels = c("Samme antal indvandrere", "Flere indvandrere", "Færre indvandrere"))
df$ligestilling <- factor(df$ligestilling, levels = c("Ligestilling opnået", "Ligestilling ikke opnået", "Ligestilling gået for langt"))

#Justering af niveauer for kandidat-variable
table(df$kand_indvandring) #skal omkodes
table(df$kand_ligestilling) #skal omkodes
table(df$kand_journalister) #skal omkodes

df$kand_indvandring <- factor(df$kand_indvandring, levels = c("Samme antal indvandrere", "Flere indvandrere", "Færre indvandrere"))
df$kand_ligestilling <- factor(df$kand_ligestilling, levels = c("Ligestilling opnået", "Ligestilling ikke opnået", "Ligestilling gået for langt"))
df$kand_journalister <- factor(df$kand_journalister, levels = c("Deltager ofte i interviews", "Deltager aldrig i interviews"))