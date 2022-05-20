#Indstil working directory
setwd("/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse")

#load pakker
library(gridExtra)
library(grid)
library(ggplot2)
library(cregg)
library(xtable)

###ROBUSTHEDSTEST###

#TEST AF RANDOMISERING

#lav ny dataframe
df_dist <- df

#specificer variable i dataframe

#køn
kand_køn <- df_dist %>%
  group_by(kand_køn) %>%
  tally() %>%
  mutate(andel = (n / nrow(df_dist))*100,
         feature = "Køn",
         level = as.factor(kand_køn))
        
#uddannelse
kand_uddannelse <- df_dist %>%
  group_by(kand_uddannelse) %>%
  tally() %>%
  mutate(andel = (n / nrow(df_dist))*100,
         feature = "Uddannelse",
         level = as.factor(kand_uddannelse))

#tidligere beskæftigelse
kand_beskæftigelse <- df_dist %>%
  group_by(kand_beskæftigelse) %>%
  tally() %>%
  mutate(andel = (n / nrow(df_dist))*100,
         feature = "Beskæftigelse",
         level = as.factor(kand_beskæftigelse))

#holdning til indvandring
kand_indvandring <- df_dist %>%
  group_by(kand_indvandring) %>%
  tally() %>%
  mutate(andel = (n / nrow(df_dist))*100,
         feature = "Indvandring",
         level = as.factor(kand_indvandring))

#holdning til ligestilling
kand_ligestilling <- df_dist %>%
  group_by(kand_ligestilling) %>%
  tally() %>%
  mutate(andel = (n / nrow(df_dist))*100,
         feature = "Ligestilling",
         level = as.factor(kand_ligestilling))

#holdning til Folketinget
kand_FT <- df_dist %>%
  group_by(kand_FT) %>%
  tally() %>%
  mutate(andel = (n / nrow(df_dist))*100,
         feature = "Folketinget",
         level = as.factor(kand_FT))

#Holdning til journalister
kand_journalister <- df %>%
  group_by(kand_journalister) %>%
  tally() %>%
  mutate(andel = (n / nrow(df_dist))*100,
         feature = "Journalister",
         level = as.factor(kand_journalister))

# Kombiner variable i en række
dist <- bind_rows(kand_FT,kand_journalister, kand_uddannelse, kand_beskæftigelse, kand_indvandring, kand_ligestilling, kand_køn)

#sæt levels og rækkefølge
dist <- dist %>%
  mutate(feature = factor(feature,
                          levels = c("Folketinget",
                                     "Journalister",
                                     "Uddannelse",
                                     "Beskæftigelse",
                                     "Indvandring",
                                     "Ligestilling",
                                     "Køn")),
         level = factor(level,
                        levels = c("Vil aldrig lyve for Folketinget",
                                   "Villig til at lyve for Folketinget",
                                   "Deltager ofte i interviews",
                                   "Deltager aldrig i interviews",
                                   "Folkeskolens 9. klasse",
                                   "Universitetsuddannet",
                                   "Ansat i en virksomhed",
                                   "Leder i en virksomhed",
                                   "Samme antal indvandrere",
                                   "Flere indvandrere",
                                   "Færre indvandrere",
                                   "Ligestilling opnået",
                                   "Ligestilling ikke opnået",
                                   "Ligestilling gået for langt",
                                   "Kvinde",
                                   "Mand")))

#visualisering af resultater
plot_dist<- dist %>%
  ggplot(., aes(x = andel,
                y = reorder(level, desc(level)),
                label = round(andel,1))) +
  geom_col(fill = "black") +
  theme(axis.text = element_text(size = 12)) +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 100)) +
  geom_text(position = position_dodge(width = .9),
            hjust = -0.2, 
            size = 3) +
  ylab("") +
  xlab("Procent") +
  labs(title = "Fordelinger") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10))

plot_dist

#gem plot
ggsave(filename = "plot_dist.png",
       plot = plot_dist, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 12, 
       height = 8,
       dpi = 320)

#TEST FOR FRAVÆR AF OVERFØRSELSEFFEKTER (opgavenummer)

#opret indikator for opgavenummer
df <- df %>% 
  mutate(conjointnr = case_when(CHOICE_SET==1 ~ "Opgave nr. 1",
                                CHOICE_SET==2 ~ "Opgave nr. 2",
                                CHOICE_SET==3 ~ "Opgave nr. 3",
                                CHOICE_SET==4 ~ "Opgave nr. 4",
                                CHOICE_SET==5 ~ "Opgave nr. 5",
                                CHOICE_SET==6 ~ "Opgave nr. 6",
                                CHOICE_SET==7 ~ "Opgave nr. 7",
                                CHOICE_SET==8 ~ "Opgave nr. 8",
                                CHOICE_SET==9 ~ "Opgave nr. 9",
                                CHOICE_SET==10 ~ "Opgave nr. 10"))


#Sæt rækkefølgen for de enkelte levels
df$conjointnr <- factor(df$conjointnr, levels = c("Opgave nr. 1", "Opgave nr. 2", "Opgave nr. 3",
                                                  "Opgave nr. 4", "Opgave nr. 5", "Opgave nr. 6",
                                                  "Opgave nr. 7", "Opgave nr. 8", "Opgave nr. 9",
                                                  "Opgave nr. 10"))

#definer som faktor
df$conjointnr <- factor(df$conjointnr)

### ANOVA
anova_conjointnr <- cj_anova(data = df,
                  formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + 
                    kand_beskæftigelse + kand_indvandring + kand_ligestilling + 
                    kand_FT + kand_journalister,
                  id = ~RESPONDENT_ID,
                  by = ~conjointnr)

anova_conjointnr

#latex output
xtable(anova_conjointnr)

#set label på de enkelte kandidatkarateristika
attr(df$kand_køn, "label") <- "Køn"
attr(df$kand_uddannelse, "label") <- "Uddannelse"
attr(df$kand_beskæftigelse, "label") <- "Beskæftigelse"
attr(df$kand_indvandring, "label") <- "Indvandring"
attr(df$kand_ligestilling, "label") <- "Ligestilling"
attr(df$kand_FT, "label") <- "Folketinget"
attr(df$kand_journalister, "label") <- "Journalister"

#Marginal means model for test af overførselseffekt
mm_conjointnr <- cj(data = df,
         formula = CHOICE_INDICATOR ~ kand_FT + kand_journalister + kand_uddannelse + kand_beskæftigelse + 
           kand_indvandring + kand_ligestilling + kand_køn,
         id = ~RESPONDENT_ID,
         by = ~conjointnr,
         estimate = "mm",
         h0 = 0.5)

#Visualisering af resultater
plot_conjointnr <- mm_conjointnr %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                colour = conjointnr)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Sandsynlighed for valg af kandidat (marginal means)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 2,
                              byrow = TRUE)) + 
  scale_shape_discrete(breaks = c("Opgave nr. 1",
                                  "Opgave nr. 2",
                                  "Opgave nr. 3",
                                  "Opgave nr. 4",
                                  "Opgave nr. 5",
                                  "Opgave nr. 6",
                                  "Opgave nr. 7",
                                  "Opgave nr. 8",
                                  "Opgave nr. 9",
                                  "Opgave nr. 10"))
plot_conjointnr

#gem plot
ggsave(filename = "plotconjointnr.png",
       plot = plot_conjointnr, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 10, 
       height = 12,
       dpi = 320)

#TEST AF PORFILPLACERINGSEFFEKTER (PROFIL TIL VENSTRE OG HØJRE)
df <- df %>% 
  mutate(profilnr = case_when(LABEL==1 ~ "Profil til venstre",
                              LABEL==2 ~ "Profil til højre"))

#ANOVA TEST
anova_profilnr <- cj_anova(data = df,
                  formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + 
                    kand_beskæftigelse + kand_indvandring + kand_ligestilling + 
                    kand_FT + kand_journalister,
                  id = ~ RESPONDENT_ID,
                  by = ~ profilnr)

anova_profilnr

#latex output
xtable(anova_profilnr)

#sæt som factor og sæt rækkefølge for levels
df$profilnr <- factor(df$profilnr, levels = c("Profil til venstre", "Profil til højre"))

#Marginal means model for test af betydning af profilnumemr
mm_profilnr <- cj(data = df,
                    formula = CHOICE_INDICATOR ~ kand_FT + kand_journalister + kand_uddannelse + kand_beskæftigelse + 
                      kand_indvandring + kand_ligestilling + kand_køn,
                    id = ~RESPONDENT_ID,
                    by = ~profilnr,
                    estimate = "mm",
                    h0 = 0.5)

#lav plot
plot_profilnr <- mm_profilnr %>% 
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                color = profilnr)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Sandsynlighed for valg af kandidat (marginal means)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 2,
                              byrow = TRUE)) + 
  scale_shape_discrete(breaks = c("Profil til venstre",
                                  "Profil til højre"))

plot_profilnr

#gem plot
ggsave(filename = "plotprofilnr.png",
       plot = plot_profilnr, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 10, 
       height = 11,
       dpi = 320)

###BALANCETEST
#lav ny dataframe til balancetest
df_balance <- df

#TEST FOR KØN

#lav indikator for køn
df_balance <- df_balance %>%
  mutate(køn = ifelse(køn=="Kvinde", 1, 0)) 

# mm model
mmbalancekøn <- cj(data = df_balance,
                  formula = køn ~ kand_FT + kand_journalister + kand_uddannelse + kand_beskæftigelse + 
                    kand_indvandring + kand_ligestilling + kand_køn,
                  id = ~RESPONDENT_ID,
                  estimate = "mm",
                  h0 = mean(df_balance$køn))

#visualisering af resultater
plotbalancekøn <- mmbalancekøn %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
                 color = "black") +
  geom_vline(xintercept = mean(df_balance$køn),
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means (kvinde)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  theme(text = element_text(family = "LM Roman 10"),
        axis.title.x = element_text(size = 10))
plotbalancekøn

#gem plot
ggsave(filename = "plotbalancekøn.png",
       plot = plotbalancekøn, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 10, 
       height = 11,
       dpi = 320)

#TEST FOR ALDER

#Marginal means model
mmbalancealder <- cj(data = df_balance,
                   formula = Q3_ALDER ~ kand_FT + kand_journalister + kand_uddannelse + kand_beskæftigelse + 
                     kand_indvandring + kand_ligestilling + kand_køn,
                   id = ~RESPONDENT_ID,
                   estimate = "mm",
                   h0 = mean(df_balance$Q3_ALDER))

#visualisering af resultater
plotbalancealder <- mmbalancealder %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
                 color = "black") +
  geom_vline(xintercept = mean(df_balance$Q3_ALDER),
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means (gennemsnitlig alder)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  theme(text = element_text(family = "LM Roman 10"),
        axis.title.x = element_text(size = 10))
plotbalancealder

#gem plot
ggsave(filename = "plotbalancealder.png",
       plot = plotbalancealder, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 10, 
       height = 11,
       dpi = 320)

#TEST FOR UDDANNELSE

#juster niveauer
df_balance$uddannelse <- factor(df_balance$uddannelse, 
                         levels = c("Grundskole", "Studentereksamen", 
                                    "Erhvervsuddannelse", "Kort videregående", "Bacheloruddannelse",
                                    "Mellemlang videregående","Lang videregående"))
#lav indikator for uddannelse
df_balance <- df_balance %>%
  mutate(uddannelse = ifelse(uddannelse=="Kort videregående" |
                               uddannelse=="Mellemlang videregående" |
                               uddannelse=="Bacheloruddannelse" |
                               uddannelse=="Lang videregående", 1, 0))

#Marginal means model
mmbalanceuddannelse <- cj(data = df_balance,
                     formula = uddannelse ~ kand_FT + kand_journalister + kand_uddannelse + kand_beskæftigelse + 
                       kand_indvandring + kand_ligestilling + kand_køn,
                     id = ~RESPONDENT_ID,
                     estimate = "mm",
                     h0 = mean(df_balance$uddannelse))

#visualisering af resultater
plotbalanceuddannelse <- mmbalanceuddannelse %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
                 color = "black") +
  geom_vline(xintercept = mean(df_balance$uddanelse),
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means (videregående uddannelse)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  theme(text = element_text(family = "LM Roman 10"),
        axis.title.x = element_text(size = 10))

plotbalanceuddannelse

#gem plot
ggsave(filename = "plotbalanceuddannelse.png",
       plot = plotbalanceuddannelse, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 10, 
       height = 11,
       dpi = 320)

#samle plots for balancetest i én figur til latex
balancetest <- grid.arrange(plotbalancekøn, plotbalancealder, plotbalanceuddannelse, ncol = 3)


#gem plot
ggsave(filename = "plotbalance.png",
       plot = balancetest, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 16, 
       height = 7,
       dpi = 320)