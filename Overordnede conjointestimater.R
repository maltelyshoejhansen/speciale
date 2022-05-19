#Set working directory
setwd("/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse")

#load pakker
library(cregg)
library(dplyr) 
library(tidyr) 
library(forcats)
library(stringr)
library(ggplot2)
library(cjoint)
library(emmeans)
library(broom)
library(ggpubr)
library(bookdown)
library(viridis)
library(estimatr)
library(grid)
library(tidyverse)
library(knitr)
font_import(pattern = "lmroman*")

#tjek data
table(df$kand_køn)
table(df$kand_uddannelse)
table(df$kand_beskæftigelse)
table(df$kand_indvandring)
table(df$kand_ligestilling)
table(df$kand_FT)
table(df$kand_journalister)

#Conjointanalyse: effekten af attributter for valg af kandidater

#set label på de enkelte kandidat-attributter
attr(df$kand_køn, "label") <- "Køn"
attr(df$kand_uddannelse, "label") <- "Uddannelse"
attr(df$kand_beskæftigelse, "label") <- "Beskæftigelse"
attr(df$kand_indvandring, "label") <- "Indvandring"
attr(df$kand_ligestilling, "label") <- "Ligestilling"
attr(df$kand_FT, "label") <- "Folketinget"
attr(df$kand_journalister, "label") <- "Journalister"

#marginal means model for den samlede conjointanalyse
conjoint_mm <- cj(data = df, 
         formula =  CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + kand_indvandring + 
             kand_ligestilling + kand_FT + kand_journalister, 
           id = ~RESPONDENT_ID,
         estimate = "mm", 
         h0=0.5)

#visualisering af de samlede conjointestimater
plot1 <- conjoint_mm %>%
       ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
       geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                height = 0.5) + 
  geom_point(fill = "black") +
  xlab("Sandsynlighed for valg af kandidat (marginal means") +
  ylab("") +
  facet_grid(feature ~., 
             scales = "free_y", space = "free_y") +
  theme(text = element_text(family = "LM Roman 10"),
        axis.title.x = element_text(size = 10))


plot1

#gem plot
ggsave(filename = "conjointplot.png",
       plot = plot1, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 8, 
       height = 10)

#latex output
latex <- conjoint_mm %>% 
  select(level, 
         estimate, 
         std.error,
         z,
         p)

kable(x = latex, 
      format = "latex",
      digits = 2)


#Opstilling af profiler for 1., 25., 50., 75. og 99. percentil
model <- lm_robust(formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + kand_indvandring + kand_ligestilling + kand_FT + kand_journalister,
                   data = df,
                   clusters = RESPONDENT_ID)

### Opstilling ad den forudsagte sandsynlighed for valg af hver profil
predicted <- bind_cols(df, 
                       as.data.frame(predict(object = model, 
                                             newdata = df,
                                             se.fit = TRUE,
                                             interval = "confidence")))

### definition af percentiler
# sammensætning af en vektor af de percentiler, der skal medtages i modellen
percentiles <- quantile(predicted$fit.fit, c(0.01, 0.25, 0.50, 0.75, 0.99))

# beregning  af afstand til percentiler 
predicted <- predicted %>% 
  mutate(dist1 = abs(percentiles[1] - fit.fit),
         dist25 = abs(percentiles[2] - fit.fit),
         dist50 = abs(percentiles[3] - fit.fit),
         dist75 = abs(percentiles[4] - fit.fit),
         dist99 = abs(percentiles[5] - fit.fit))

# valg af de profiler, der lægger tættes på de enkelte percentiler
profiles <- predicted %>% 
  filter(dist1==min(dist1) | dist25==min(dist25) | dist50==min(dist50) | dist75==min(dist75) | dist99==min(dist99)) %>% 
  select(kand_køn, kand_uddannelse, kand_beskæftigelse, kand_indvandring, kand_ligestilling, kand_FT, kand_journalister, fit.fit, fit.lwr, fit.upr, se.fit) %>% 
  distinct() %>% 
  arrange(fit.fit) %>% 
  mutate(percentiles = c(1, 25, 50, 75, 99),
         feature = "Profiltyper")

# justering af konfidensinterval således, at det ikke går under nul
profiles <- profiles %>% 
  mutate(fit.lwr = ifelse(fit.lwr<0, 0.00, fit.lwr))

### tilføjelse af labels til de enkelte profiler
# tilføj labels som variabel
profiles <- profiles %>% 
  mutate(label = paste(paste(kand_køn),
                       paste(kand_uddannelse),
                       paste(kand_beskæftigelse),
                       paste(kand_indvandring),
                       paste(kand_ligestilling),
                       paste(kand_FT),
                       paste(kand_journalister),
                       sep = "\n"))

#opstil tekst
text1 <- textGrob(label = profiles$label[1], 
                  gp = gpar(fontfamily = "LM Roman 10", fontsize = 7))

text25 <- textGrob(label = profiles$label[2], 
                   gp = gpar(fontfamily = "LM Roman 10", fontsize = 7))

text50 <- textGrob(label = profiles$label[3], 
                   gp = gpar(fontfamily = "LM Roman 10", fontsize = 7))

text75 <- textGrob(label = profiles$label[4], 
                   gp = gpar(fontfamily = "LM Roman 10", fontsize = 7))

text99 <- textGrob(label = profiles$label[5], 
                   gp = gpar(fontfamily = "LM Roman 10", fontsize = 7))


#visualisering af resultaterne
plot2 <- profiles %>% 
  ggplot(., aes(x = percentiles, 
                y = fit.fit)) +
  geom_point(fill = "black",
             size = 2) +
  geom_errorbar(aes(ymin = fit.lwr,
                    ymax = fit.upr),
                width = 2,
                color = "black") +
  scale_x_continuous("Percentiler", 
                     breaks = c(1, 25, 50, 75, 99),
                     expand = c(0.05, 0.2)) +
  scale_y_continuous("Estimeret Pr(støtte til kandidat)",
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8)) +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(text = element_text(family = "LM Roman 10")) +
  annotation_custom(text1, 
                    xmin = 1,
                    xmax = 1,
                    ymin = -0.25,
                    ymax = -0.25) + 
  annotation_custom(text25, 
                    xmin = 25,
                    xmax = 25,
                    ymin = -0.25,
                    ymax = -0.25) + 
  annotation_custom(text50,
                    xmin = 50, 
                    xmax = 50,
                    ymin = -0.25,
                    ymax = -0.25) + 
  annotation_custom(text75,
                    xmin = 75,
                    xmax = 75,
                    ymin = -0.25,
                    ymax = -0.25) + 
  annotation_custom(text99,
                    xmin = 99,
                    xmax = 99,
                    ymin = -0.25,
                    ymax = -0.25) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1, 2, 8, 1), "lines"))


plot2

#gem plot
ggsave(filename = "percentilplot.png",
       plot = plot2, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 8, 
       height = 10)

#latex output
latex <- profiles %>% 
  select(percentiles,
         kand_køn,
         kand_uddannelse,
         kand_beskæftigelse,
         kand_indvandring,
         kand_ligestilling,
         kand_FT,
         kand_journalister,
         fit.fit)

kable(x = latex, 
      format = "latex",
      digits = 2)

