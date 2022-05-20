#Indstil working directory
setwd("/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse")

#load pakker
library(gridExtra)
library(grid)
library(ggplot2)
library(cregg)

#Hypotese 2: Betydningen af kongruens i politiske præferencer for effekten af udemokratisk adfærd

#sæt labels
attr(df$kand_FT, "label") <- "Holdning til Folketinget"
attr(df$kand_journalister, "label") <- "Holdning til journalister"

#Inspicering af variable for politiske præferencer
#indvandring
table(df$kand_indvandring) #Kandidatens holdning
table(df$indvandring) #Respondentens holdning

#Ligestilling
table(df$kand_ligestilling) #Kandidatens holdning
table(df$ligestilling) #rRespondentens holdning

#KONGRUENS I HOLDNING TIL INDVANDRING

#Opret binær variabel, der indikerer, om der er kongruens i holdningen til indvandring
df$indvandring_kongruens <- NA #opret ny variabel

df$indvandring_kongruens[df$indvandring=="Samme antal indvandrere" & df$kand_indvandring=="Samme antal indvandrere"] <- "Kongruens i holdning til indvandring"
df$indvandring_kongruens[df$indvandring=="Færre indvandrere" & df$kand_indvandring=="Færre indvandrere"] <- "Kongruens i holdning til indvandring"
df$indvandring_kongruens[df$indvandring=="Flere indvandrere" & df$kand_indvandring=="Flere indvandrere"] <- "Kongruens i holdning til indvandring"

df$indvandring_kongruens[df$indvandring=="Samme antal indvandrere" & df$kand_indvandring=="Færre indvandrere"] <- "Ikke kongruens i holdning til indvandring"
df$indvandring_kongruens[df$indvandring=="Samme antal indvandrere" & df$kand_indvandring=="Flere indvandrere"] <- "Ikke kongruens i holdning til indvandring"
df$indvandring_kongruens[df$indvandring=="Færre indvandrere" & df$kand_indvandring=="Samme antal indvandrere"] <- "Ikke kongruens i holdning til indvandring"
df$indvandring_kongruens[df$indvandring=="Færre indvandrere" & df$kand_indvandring=="Flere indvandrere"] <- "Ikke kongruens i holdning til indvandring"
df$indvandring_kongruens[df$indvandring=="Flere indvandrere" & df$kand_indvandring=="Samme antal indvandrere"] <- "Ikke kongruens i holdning til indvandring"
df$indvandring_kongruens[df$indvandring=="Flere indvandrere" & df$kand_indvandring=="Færre indvandrere"] <- "Ikke kongruens i holdning til indvandring"

#inspicer variabel
table(df$indvandring_kongruens)

#sæt kongruens som første niveau
df$indvandring_kongruens <- 
  factor(df$indvandring_kongruens, 
         levels = c("Kongruens i holdning til indvandring", "Ikke kongruens i holdning til indvandring"))

#marginal means-model for interaktion mellem holdning til Folketinget og kongruens i holdning til indvandring
mm_kongruens_indvandring_FT<- cj(data = df,
         formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + kand_indvandring + 
           kand_ligestilling + kand_FT + kand_journalister,
         by = ~indvandring_kongruens,
         id = ~RESPONDENT_ID,
         estimate = "mm",
         h0 = 0.5)

# Visualisering af resultater
plot3 <- mm_kongruens_indvandring_FT %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = indvandring_kongruens)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Sansynlighed for valg af kandidat (marginal means)") +
  ylab("") +
  labs(title = "Indvandring") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  xlim(0.2, 0.9) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Kongruens i holdning til indvandring",
                                  "Ikke kongruens i holdning til indvandring"))

plot3

#gem plot
ggsave(filename = "plot_FT_kongruensindvandring.png",
       plot = plot3, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5)

#latex output
latex <- mm_kongruens_indvandring_FT %>% filter(feature=="Holdning til Folketinget") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper)

kable(x = latex, format = "latex", digits = 2)


#AMCE-model for interaktion mellem holdning til Folketinget og kongruens i holdning til indvandring
AMCE_kongruens_indvandring_FT<- cj(data = df,
                                 formula = CHOICE_INDICATOR ~  kand_FT,
                                 by = ~indvandring_kongruens,
                                 id = ~RESPONDENT_ID)


# Visualisering af resultater
plot3_AMCE <- AMCE_kongruens_indvandring_FT %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = indvandring_kongruens)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlim(-0.6, 0) +
  xlab("Sandsynlighed for valg af kandidat (AMCE)") +
  ylab("") +
  labs(title = "Indvandring") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Kongruens i holdning til indvandring",
                                  "Ikke kongruens i holdning til indvandring"))

plot3_AMCE

#gem plot
ggsave(filename = "plot3_amce.png",
       plot = plot3_AMCE, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5)

#latex output
latex <- AMCE_kongruens_indvandring_FT %>% filter(feature=="Holdning til Folketinget") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper)

kable(x = latex, format = "latex", digits = 2)


#marginal means-model for interaktion mellem holdning til journalister og kongruens i holdning til indvandring
mm_kongruens_indvandring_journalister <- cj(data = df,
                              formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + kand_indvandring + 
                                kand_ligestilling + kand_FT + kand_journalister,
                              by = ~indvandring_kongruens,
                              id = ~RESPONDENT_ID,
                              estimate = "mm",
                              h0 = 0.5)


# Visualisering af resultater
plot4 <- mm_kongruens_indvandring_journalister %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = indvandring_kongruens)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Sandsynlighed for valg af kandidat (marignal means)") +
  ylab("") +
  labs(title = "Indvandring") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  xlim(0.3, 0.8) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Kongruens i holdning til indvandring",
                                  "Ikke kongruens i holdning til indvandring"))

plot4

#gem plot
ggsave(filename = "plot_kongruensindvadring_journalister.png",
       plot = plot4, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5)

#latex output
latex <- mm_kongruens_indvandring_journalister %>% filter(feature=="Holdning til journalister") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower, 
         upper)

kable(x = latex, format = "latex", digits = 2)

#AMCE-model for interaktion mellem holdning til journalister og kongruens i holdning til indvandring
AMCE_kongruens_indvandring_journalister<- cj(data = df,
                                   formula = CHOICE_INDICATOR ~ kand_journalister,
                                   by = ~indvandring_kongruens,
                                   id = ~RESPONDENT_ID)

table(df$indvandring_kongruens)

# Visualisering af resultater
plot4_AMCE <- AMCE_kongruens_indvandring_journalister %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = indvandring_kongruens)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Sandsynlighed for valg af kandidat (AMCE)") +
  ylab("") +
  labs(title = "Indvandring") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Kongruens i holdning til indvandring",
                                  "Ikke kongruens i holdning til indvandring"))


plot4_AMCE

#gem plot
ggsave(filename = "plot4amce.png",
       plot = plot4_AMCE, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5)

#latex output
latex <- AMCE_kongruens_indvandring_journalister %>% filter(feature=="Holdning til journalister") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower, 
         upper)

kable(x = latex, format = "latex", digits = 2)

#KONGRUENS I HOLDNING TIL LIGESTILLING

#Opret variabel, der indikerer, om der er kongruens i holdningen til ligestilling
df$ligestilling_kongruens <- NA

df$ligestilling_kongruens[df$ligestilling=="Ligestilling opnået" & df$kand_ligestilling=="Ligestilling opnået"] <- "Kongruens i holdning til ligestilling"
df$ligestilling_kongruens[df$ligestilling=="Ligestilling ikke opnået" & df$kand_ligestilling=="Ligestilling ikke opnået"] <- "Kongruens i holdning til ligestilling"
df$ligestilling_kongruens[df$ligestilling=="Ligestilling gået for langt" & df$kand_ligestilling=="Ligestilling gået for langt"] <- "Kongruens i holdning til ligestilling"

df$ligestilling_kongruens[df$ligestilling=="Ligestilling opnået" & df$kand_ligestilling=="Ligestilling ikke opnået"] <- "Ikke kongruens i holdning til ligestilling"
df$ligestilling_kongruens[df$ligestilling=="Ligestilling opnået" & df$kand_ligestilling=="Ligestilling gået for langt"] <- "Ikke kongruens i holdning til ligestilling"
df$ligestilling_kongruens[df$ligestilling=="Ligestilling ikke opnået" & df$kand_ligestilling=="Ligestilling opnået"] <- "Ikke kongruens i holdning til ligestilling"
df$ligestilling_kongruens[df$ligestilling=="Ligestilling ikke opnået" & df$kand_ligestilling=="Ligestilling gået for langt"] <- "Ikke kongruens i holdning til ligestilling"
df$ligestilling_kongruens[df$ligestilling=="Ligestilling gået for langt" & df$kand_ligestilling=="Ligestilling opnået"] <- "Ikke kongruens i holdning til ligestilling"
df$ligestilling_kongruens[df$ligestilling=="Ligestilling gået for langt" & df$kand_ligestilling=="Ligestilling ikke opnået"] <- "Ikke kongruens i holdning til ligestilling"

#inspicer variabel
table(df$ligestilling_kongruens)

#sæt kongruens som første niveau
df$ligestilling_kongruens <- 
  factor(df$ligestilling_kongruens, 
         levels = c("Kongruens i holdning til ligestilling", "Ikke kongruens i holdning til ligestilling"))

#marginal means-model for interaktion mellem holdning til Folketinget og kongruens i holdning til ligestilling
mm_kongruens_ligestilling_Folketinget <- cj(data = df,
                  formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + kand_indvandring + 
                    kand_ligestilling + kand_FT + kand_journalister,
                  by = ~ligestilling_kongruens,
                  id = ~RESPONDENT_ID,
                  estimate = "mm",
                  h0 = 0.5)

# Visualisering af resultater
plot5 <- mm_kongruens_ligestilling_Folketinget %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = ligestilling_kongruens)) +
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
  labs(title = "Ligestilling") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  xlim(0.2, 0.9) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10))+
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Kongruens i holdning til ligestilling",
                                  "Ikke kongruens i holdning til ligestilling"))

plot5

#gem plot
ggsave(filename = "plot_FT_kongruensligestilling.png",
       plot = plot5, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5)

#latex output
latex <- mm_kongruens_ligestilling_Folketinget %>% filter(feature=="Holdning til Folketinget") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower, 
         upper)

kable(x = latex, format = "latex", digits = 2)


#AMCE-model for interaktion mellem holdning til Folketinget og kongruens i holdning til ligestilling
AMCE_kongruens_ligestilling_Folketinget<- cj(data = df,
                                             formula = CHOICE_INDICATOR ~  kand_FT,
                                             by = ~ligestilling_kongruens,
                                             id = ~RESPONDENT_ID)

# Visualisering af resultater
plot5_AMCE <- AMCE_kongruens_ligestilling_Folketinget %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = ligestilling_kongruens)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Sandsynlighed for valg af kandidat (AMCE)") +
  xlim(-0.5, 0) +
  ylab("") +
  labs(title = "Ligestilling") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Kongruens i holdning til ligestilling",
                                  "Ikke kongruens i holdning til ligestilling"))

plot5_AMCE

#gem plot
ggsave(filename = "plot5amce.png",
       plot = plot5_AMCE, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5)

#latex output
latex <- AMCE_kongruens_ligestilling_Folketinget %>% filter(feature=="Holdning til Folketinget") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower, 
         upper)

kable(x = latex, format = "latex", digits = 2)

#marginal means-model for holdning til journalister afhængig af kongruens i holdning til ligestilling
mm_kongruens_ligestilling_journalister <- cj(data = df,
                                            formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + kand_indvandring + 
                                              kand_ligestilling + kand_FT + kand_journalister,
                                            by = ~ligestilling_kongruens,
                                            id = ~RESPONDENT_ID,
                                            estimate = "mm",
                                            h0 = 0.5)

# Visualisering af resultater
plot6 <- mm_kongruens_ligestilling_journalister %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = ligestilling_kongruens)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Sandsynlighed for valg af kandidat (marignal means)") +
  ylab("") +
  labs(title = "Ligestilling") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  xlim(0.3, 0.8) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Kongruens i holdning til ligestilling",
                                  "Ikke kongruens i holdning til ligestilling"))

plot6

#gem plot
ggsave(filename = "plot_kongruensligestilling_journalister.png",
       plot = plot6, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#latex output
latex <- mm_kongruens_ligestilling_journalister %>% filter(feature=="Holdning til journalister") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower, 
         upper)

kable(x = latex, format = "latex", digits = 2)


#AMCE-model for interaktion mellem holdning til journalister og kongruens i holdning til ligestilling
AMCE_kongruens_ligestilling_journalister<- cj(data = df,
                                             formula = CHOICE_INDICATOR ~ kand_journalister,
                                             by = ~ligestilling_kongruens,
                                             id = ~RESPONDENT_ID)

# Visualisering af resultater
plot6_AMCE <- AMCE_kongruens_ligestilling_journalister %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = ligestilling_kongruens)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Sandsynlgihed for valg af kandidat (AMCE)") +
  ylab("") +
  labs(title = "Ligestilling") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Kongruens i holdning til ligestilling",
                                  "Ikke kongruens i holdning til ligestilling"))

plot6_AMCE

#gem plot
ggsave(filename = "plot6amce.png",
       plot = plot6_AMCE, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5)

#latex output
latex <- AMCE_kongruens_ligestilling_journalister %>% filter(feature=="Holdning til journalister") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower, 
         upper)

kable(x = latex, format = "latex", digits = 2)

#samleding af plots til visualiseringer i latex

#samlet plot for marginal means: Holdning til Folketinget for kongruens i indvandring og kongruens i ligestilling
plot_kongruensFT <- grid.arrange(plot3, plot5, nrow=2)

#gem plot
ggsave(filename = "plotkongruens_FT.png",
       plot = plot_kongruensFT, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5.5,
       dpi = 320)

#Samlet plot for AMCE: Holdning til Folketinget for kongruens i indvandring og kongruens i ligestilling
plotkongruens_FT_AMCE <- grid.arrange(plot3_AMCE, plot5_AMCE, nrow=2)

#gem plot
ggsave(filename = "plotkongruens_FT_AMCE.png",
       plot = plotkongruens_FT_AMCE, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5.5,
       dpi = 320)

#samlet plot for marginal means: Holdning til journalister for kongruens i indvandring og kongruens i ligestilling
plot_kongruensjounalist <- grid.arrange(plot4, plot6, nrow=2)

#gem plot
ggsave(filename = "plotkongruensjounalister.png",
       plot = plot_kongruensjounalist, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5.5,
       dpi = 320)

#Samlet plot for AMCE: Holdning til journalister for kongruens i indvandring og kongruens i ligestilling
plotkongruens_journalister_AMCE <- grid.arrange(plot4_AMCE, plot6_AMCE, nrow=2)

#gem plot
ggsave(filename = "plotkongruensjounalister_AMCE.png",
       plot = plotkongruens_journalister_AMCE, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5.5,
       dpi = 320)

###################################################################################################

###EKSTRA ANALYSE 1###
#Finkornet mål for politiske præferencer: betydningen af politisk distance for effekten af udemokratisk adfærd

#Distance i holdning til indvandring
df$indvandring_distance <- NA #opret ny variabel

#Distance = 0
df$indvandring_distance[df$indvandring=="Færre indvandrere" & df$kand_indvandring=="Færre indvandrere"] <- "Distance = 0"
df$indvandring_distance[df$indvandring=="Samme antal indvandrere" & df$kand_indvandring=="Samme antal indvandrere"] <- "Distance = 0"
df$indvandring_distance[df$indvandring=="Flere indvandrere" & df$kand_indvandring=="Flere indvandrere"] <- "Distance = 0"

#Distance = 1
df$indvandring_distance[df$indvandring=="Samme antal indvandrere" & df$kand_indvandring=="Flere indvandrere"] <- "Distance = 1"
df$indvandring_distance[df$indvandring=="Samme antal indvandrere" & df$kand_indvandring=="Færre indvandrere"] <- "Distance = 1"
df$indvandring_distance[df$indvandring=="Flere indvandrere" & df$kand_indvandring=="Samme antal indvandrere"] <- "Distance = 1"
df$indvandring_distance[df$indvandring=="Færre indvandrere" & df$kand_indvandring=="Samme antal indvandrere"] <- "Distance = 1"

#Distance = 2
df$indvandring_distance[df$indvandring=="Flere indvandrere" & df$kand_indvandring=="Færre indvandrere"] <- "Distance = 2"
df$indvandring_distance[df$indvandring=="Færre indvandrere" & df$kand_indvandring=="Flere indvandrere"] <- "Distance = 2"


#sæt som factor
df$indvandring_distance <- factor(df$indvandring_distance)

#marginal means-model for interaktion mellem holdning til Folketinget/journalister og distance i holdningen til indvandring
mm_distance_indvandring <- cj(data = df,
                      formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + 
                        kand_indvandring + kand_ligestilling + kand_FT + kand_journalister,
                                            by = ~indvandring_distance,
                                            id = ~RESPONDENT_ID,
                                            estimate = "mm",
                                            h0 = 0.5)

#visualisering af resultater for Folketinget
plot_distance_indvandringFT <- mm_distance_indvandring %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = indvandring_distance)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Sandsunlighed for valg af kandidat (marginal means)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Distance = 0",
                                  "Distance = 1",
                                  "Distance = 2"))
plot_distance_indvandringFT

#gem plot
ggsave(filename = "plot_distance_indvandring.png",
       plot = plot_distance_indvandring, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#AMCE-model for holdning til Folketinget/journalister og distancevariabel
AMCE_distance_indvandring <- cj(data = df,
                              formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + 
                                kand_indvandring + kand_ligestilling + kand_FT + kand_journalister,
                              by = ~indvandring_distance,
                              id = ~RESPONDENT_ID)

#visualisering af resultater for Folketinget med AMCE
AMCE_distance_indvandringFT <- AMCE_distance_indvandring %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = indvandring_distance)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(støtte til kandidat)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Distance = 0",
                                  "Distance = 1",
                                  "Distance = 2"))
AMCE_distance_indvandringFT

#gem plot
ggsave(filename = "AMCE_distance_indvandringFT.png",
       plot = AMCE_distance_indvandringFT, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#visualisering af resultater for journalister
plot_distance_indvandringjournalist <- mm_distance_indvandring %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = indvandring_distance)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(støtte til kandidat)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Distance = 0",
                                  "Distance = 1",
                                  "Distance = 2"))

plot_distance_indvandringjournalist

#gem plot
ggsave(filename = "plot_distance_indvandring.png",
       plot = plot_distance_indvandring, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#AMCE-model for holdning til journalister og distance i holdning til indvandring
AMCE_distance_indvandring <- cj(data = df,
                                formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + 
                                  kand_indvandring + kand_ligestilling + kand_FT + kand_journalister,
                                by = ~indvandring_distance,
                                id = ~RESPONDENT_ID)

#visualisering af resultater for journalister med AMCE
AMCE_distance_indvandringjournalister <- AMCE_distance_indvandring %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = indvandring_distance)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(støtte til kandidat)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Distance = 0",
                                  "Distance = 1",
                                  "Distance = 2"))
AMCE_distance_indvandringjournalister

#gem plot
ggsave(filename = "AMCE_distance_indvandringjournalister.png",
       plot = AMCE_distance_indvandringjournalister, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#DISTANCEVARIABEL FOR HOLDNING TIL LIGESTILLING
df$ligestilling_distance<- NA #opret ny variabel

#Distance = 0
df$ligestilling_distance[df$ligestilling=="Ligestilling ikke opnået" & df$kand_ligestilling=="Ligestilling ikke opnået"] <- "Distance = 0"
df$ligestilling_distance[df$ligestilling=="Ligestilling opnået" & df$kand_ligestilling=="Ligestilling opnået"] <- "Distance = 0"
df$ligestilling_distance[df$ligestilling=="Ligestilling gået for langt" & df$kand_ligestilling=="Ligestilling gået for langt"] <- "Distance = 0"

#Distance = 1
df$ligestilling_distance[df$ligestilling=="Ligestilling ikke opnået" & df$kand_ligestilling=="Ligestilling opnået"] <- "Distance = 1"
df$ligestilling_distance[df$ligestilling=="Ligestilling opnået" & df$kand_ligestilling=="Ligestilling ikke opnået"] <- "Distance = 1"
df$ligestilling_distance[df$ligestilling=="Ligestilling opnået" & df$kand_ligestilling=="Ligestilling gået for langt"] <- "Distance = 1"
df$ligestilling_distance[df$ligestilling=="Ligestilling gået for langt" & df$kand_ligestilling=="Ligestilling opnået"] <- "Distance = 1"

#Distance = 2
df$ligestilling_distance[df$ligestilling=="Ligestilling ikke opnået" & df$kand_ligestilling=="Ligestilling gået for langt"] <- "Distance = 2"
df$ligestilling_distance[df$ligestilling=="Ligestilling gået for langt" & df$kand_ligestilling=="Ligestilling ikke opnået"] <- "Distance = 2"


#sæt som factor
df$ligestilling_distance <- factor(df$ligestilling_distance)


#marginal means-model for holdning til Folketinget/journalister og distance i holdningen til ligestilling
mm_distance_ligestilling <- cj(data = df,
                      formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + kand_indvandring + 
                        kand_ligestilling + kand_FT + kand_journalister,
                      by = ~ligestilling_distance,
                      id = ~RESPONDENT_ID,
                      estimate = "mm",
                      h0 = 0.5)

#visualisering af resultater for Folketinget
plot_distance_ligestillingFT <- mm_distance_ligestilling %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = ligestilling_distance)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(støtte til kandidat)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Distance = 0",
                                  "Distance = 1",
                                  "Distance = 2"))
plot_distance_ligestillingFT

#gem plot
ggsave(filename = "plot_distance_ligestilling.png",
       plot = plot_distance_ligestilling, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#AMCE-model for interaktion mellem holdning til journalister og distance i holdningen til ligestilling
AMCE_distance_ligestilling <- cj(data = df,
                                formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + 
                                  kand_indvandring + kand_ligestilling + kand_FT + kand_journalister,
                                by = ~ligestilling_distance,
                                id = ~RESPONDENT_ID)

#visualisering af resultater for journalister med AMCE
AMCE_distance_ligestillingFT <- AMCE_distance_ligestilling %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = ligestilling_distance)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(støtte til kandidat)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Distance = 0",
                                  "Distance = 1",
                                  "Distance = 2"))

AMCE_distance_ligestillingFT

#gem plot
ggsave(filename = "AMCE_distance_ligestillingFT.png",
       plot = AMCE_distance_ligestillingFT, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#visualisering af resultater for holdning til journalister
plot_distance_ligestillingjournalist <- mm_distance_ligestilling %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = ligestilling_distance)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(støtte til kandidat)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Distance = 0",
                                  "Distance = 1",
                                  "Distance = 2"))
plot_distance_ligestillingjournalist

#gem plot
ggsave(filename = "plot_distance_ligestilling.png",
       plot = plot_distance_ligestilling, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)


#AMCE-model for interaktion mellem holdning til journalister og distance i holdning til ligestlling
AMCE_distance_ligestilling <- cj(data = df,
                                 formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + 
                                   kand_indvandring + kand_ligestilling + kand_FT + kand_journalister,
                                 by = ~ligestilling_distance,
                                 id = ~RESPONDENT_ID)

#visualisering af AMCE for journalister 
AMCE_distance_ligestillingjournalister <- AMCE_distance_ligestilling %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = ligestilling_distance)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(støtte til kandidat)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Distance = 0",
                                  "Distance = 1",
                                  "Distance = 2"))

AMCE_distance_ligestillingjournalister

#gem plot
ggsave(filename = "AMCE_distance_ligestillingjournalister.png",
       plot = AMCE_distance_ligestillingjournalister, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)