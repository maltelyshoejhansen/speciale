#Indstil working directory
setwd("/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse")

#load pakker
library(stargazer)
library(ggplot2)
library(gridExtra)

#Stikprøven fordeling på bagggrundvariable (forskningsdesign)

##illustrer fordeling for køn
datakøn <- ggplot(data=subset(df, !is.na(køn)), aes(x=køn)) +
  geom_bar() +  xlab("Køn") + ylab("Frekvens") + 
  scale_y_continuous(breaks = seq(0, 15000, by=2500), limits=c(0,15000)) +
  theme_classic() + 
  theme(text = element_text(family = "LM Roman 10")) 

datakøn

#gem plot
ggsave(filename = "datakøn.png",
       plot = datakøn, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5)

#illustrer fordeling for alder
alder <- df$Q3_ALDER
alder <- alder[ alder > 17 & alder < 100 ]

#plot for fordeling af alder
dataalder <- ggplot() + aes(alder)+ geom_bar() +
  xlab("Alder") + ylab("Frekvens") +
  scale_y_continuous(breaks = seq(0, 800, by=200), limits=c(0,800)) +
  scale_x_continuous(breaks = seq(18, 90, by=4), limits=c(18,90)) +
  theme_classic() +
  theme(text = element_text(family = "LM Roman 10"))

dataalder

#gem plot
ggsave(filename = "dataalder.png",
       plot = dataalder, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5)

#summary stats for alder (gennemsnit = 47 år)
summary(alder)

#illustrer fordeling for uddannelse
#forkortelse af labels for at skabe plads i plot
df <- df %>%
  mutate(uddannelse = case_when(uddannelse=="Erhvervsuddannelse" ~ "EUD",
                                uddannelse=="Grundskole" ~ "Grundskole",
                                uddannelse=="Kort videregående" ~ "KVU",
                                uddannelse=="Lang videregående" ~ "LVU",
                                uddannelse=="Mellemlang videregående" ~ "MVU",
                                uddannelse=="Studentereksamen" ~ "Studentereksamen"))

#juster levels, så korteste uddannelse vises først
df$uddannelse <- 
  factor(df$uddannelse, 
         levels = c("Grundskole", "Studentereksamen", "EUD", "KVU", "MVU", "LVU"))

#plot for fordeling for uddannelse
datauddannelse <- ggplot(data=subset(df, !is.na(uddannelse)), aes(x=uddannelse)) +
  geom_bar() +  xlab("Uddannelse") + ylab("Frekvens") + 
  theme_classic() + 
  scale_y_continuous(breaks = seq(0, 5000, by=1000), limits=c(0,5000)) +
  theme(text = element_text(family = "LM Roman 10")) 

datauddannelse

#gem plot
ggsave(filename = "datauddannelse.png",
       plot = datauddannelse, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5)

##illustrer fordeling for region

#juster rækkefølge for, hvordan regionerne præsenteres visuelt
df$region <- 
  factor(df$region, 
         levels = c("Nordjylland", "Midtjylland", "Syddanmark", "Sjælland", "Hovedstaden"))

#lav plot for fordeling på uddannelse
dataregion <- ggplot(data=subset(df, !is.na(region)), aes(x=region)) +
  geom_bar() +  xlab("Region") + ylab("Frekvens") + 
  theme_classic() + 
  scale_y_continuous(breaks = seq(0, 9000, by=1000), limits=c(0,9000)) +
  theme(text = element_text(family = "LM Roman 10")) 

#gem plot
ggsave(filename = "dataregion.png",
       plot = dataregion, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5)

#samling af plot til latex
plotdistribution <- grid.arrange(datakøn, dataalder, dataregion, datauddannelse, nrow=2, ncol=2)

#gem plot
ggsave(filename = "plotdistribution.png",
       plot = plotdistribution, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 12, 
       height = 5,
       dpi = 320)