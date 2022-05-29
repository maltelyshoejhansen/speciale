#Indstil working directory
setwd("/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse")

#load pakker
library(gridExtra)
library(grid)
library(ggplot2)
library(cregg)

#HYPOTESE 3: BETYDNINGEN AF KOMPETENCE FOR EFFEKTEN AF UDEMOKRATISK ADFÆRD

#Set labels for karakteristika 
attr(df$kand_uddannelse, "label") <- "Uddannelse"
attr(df$kand_beskæftigelse, "label") <- "Beskæftigelse før politisk karriere"

#KANDIDATENS UDDANNELSESNIVEAU

#Marginal means for kandidatens holdning til Folketinget afængig kandidatens uddanelse
mm_uddannelse1 <- cj(data = df,
                  formula = CHOICE_INDICATOR ~ kand_køn + kand_beskæftigelse + kand_indvandring + 
                    kand_ligestilling + kand_FT + kand_journalister,
                  by = ~ kand_uddannelse,
                  id = ~RESPONDENT_ID,estimate = "mm",
                  h0 = 0.5)

# Visualisering af resultater
plot11 <- mm_uddannelse1 %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = kand_uddannelse)) +
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
        axis.title = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Folkeskolens 9. klasse",
                                  "Universitetsuddannet"))

plot11

#gem plot
ggsave(filename = "plot_uddannelse_FT.png",
       plot = plot11, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#latex output
latex <- mm_uddannelse1 %>% filter(feature=="Holdning til Folketinget") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper)

kable(x = latex, format = "latex", digits = 2)

#AMCE means-model for kandidatens holdning til Folketinget afhængig af kandidatens uddanelse
AMCE_uddannelse1 <- cj(data = df,
                     formula = CHOICE_INDICATOR ~  kand_FT,
                     by = ~ kand_uddannelse,
                     id = ~RESPONDENT_ID)

# Visualisering af resultater
plot11_AMCE <- AMCE_uddannelse1 %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = kand_uddannelse)) +
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
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Folkeskolens 9. klasse",
                                  "Universitetsuddannet"))


plot11_AMCE

#gem plot
ggsave(filename = "plot_uddannelse_AMCE.png",
       plot = plot11_AMCE, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#latex output
latex <- AMCE_uddannelse1 %>% filter(feature=="Holdning til Folketinget") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper)

kable(x = latex, format = "latex", digits = 2)

#marginal means for kandidatens holdning til journalister afhængig af kandidatens uddanelse
mm_uddannelse2 <- cj(data = df,
                    formula = CHOICE_INDICATOR ~ kand_køn + kand_beskæftigelse + kand_indvandring + 
                      kand_ligestilling + kand_FT + kand_journalister,
                    by = ~ kand_uddannelse,
                    id = ~RESPONDENT_ID,estimate = "mm",
                    h0 = 0.5)

# Visualisering af resultater
plot12 <- mm_uddannelse2 %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = kand_uddannelse)) +
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
        text = element_text(family = "LM Roman 10")) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Folkeskolens 9. klasse",
                                  "Universitetsuddannet"))

plot12

#gem plot
ggsave(filename = "plot_uddannelse_journalister.png",
       plot = plot12, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#latex output
latex <- mm_uddannelse2 %>% filter(feature=="Holdning til journalister") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper)

kable(x = latex, format = "latex", digits = 2)

#AMCE for kandidatens holdning til journalister afgængig af kandidatens uddanelse
AMCE_uddannelse2 <- cj(data = df,
                       formula = CHOICE_INDICATOR ~ kand_journalister,
                       by = ~ kand_uddannelse,
                       id = ~RESPONDENT_ID)

# Visualisering af resultater
plot12_AMCE <- AMCE_uddannelse2 %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = kand_uddannelse)) +
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
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        axis.title = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Folkeskolens 9. klasse",
                                  "Universitetsuddannet"))

plot12_AMCE

#gem plot
ggsave(filename = "plot_uddannelse_journalister_AMCE.png",
       plot = plot12_AMCE, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#latex output
latex <- AMCE_uddannelse2 %>% filter(feature=="Holdning til journalister") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper)

kable(x = latex, format = "latex", digits = 2)


#KANDIDATENS BESKÆFTIGELSE FØR HANS/HENDES POLITISKE KARRIERE

#Marginal means for holdning til Folketinget afhængig af beskæftigelse
mm_beskæftigelse1 <- cj(data = df,
                    formula = CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_indvandring + 
                      kand_ligestilling + kand_FT + kand_journalister,
                    by = ~ kand_beskæftigelse,
                    id = ~RESPONDENT_ID,estimate = "mm",
                    h0 = 0.5)

# Visualisering af resultater
plot13 <- mm_beskæftigelse1 %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = kand_beskæftigelse)) +
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
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Ansat i en virksomhed",
                                  "Leder i en virksomhed"))

plot13

#gem plot
ggsave(filename = "plot_beskæftigelse_FT.png",
       plot = plot13, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#latex output
latex <- mm_beskæftigelse1 %>% filter(feature=="Holdning til Folketinget") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper)

kable(x = latex, format = "latex", digits = 2)

#AMCE-model for interaktion mellem kandidatens holdning til folketinget og kandidatens beskæftigelse
AMCE_beskæftigelse1 <- cj(data = df,
                        formula = CHOICE_INDICATOR ~kand_FT,
                        by = ~ kand_beskæftigelse,
                        id = ~RESPONDENT_ID)

# Visualisering af resultater
plot13_AMCE <- AMCE_beskæftigelse1 %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = kand_beskæftigelse)) +
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
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        axis.title.x = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Ansat i en virksomhed",
                                  "Leder i en virksomhed"))

plot13_AMCE

#gem plot
ggsave(filename = "plot_beskæftigelse_FT_AMCE.png",
       plot = plot13_AMCE, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#latex output
latex <- AMCE_beskæftigelse1 %>% filter(feature=="Holdning til Folketinget") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper)

kable(x = latex, format = "latex", digits = 2)


#Model for interaktion mellem kandidatens holdning til journalister og kandidatens beskæftigelse før politisk karriere
mm_beskæftigelse2 <- cj(data = df,
                       formula = CHOICE_INDICATOR ~  kand_journalister,
                       by = ~ kand_beskæftigelse,
                       id = ~RESPONDENT_ID,estimate = "mm",
                       h0 = 0.5)

# Visualisering af resultater
plot14 <- mm_beskæftigelse2 %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = kand_beskæftigelse)) +
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
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Ansat i en virksomhed",
                                  "Leder i en virksomhed"))

plot14

#gem plot
ggsave(filename = "plot_beskæftigelse_journalister.png",
       plot = plot14, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5, 
       dpi = 320)

#latex output
latex <- mm_beskæftigelse2 %>% filter(feature=="Holdning til journalister") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper)

kable(x = latex, format = "latex", digits = 2)


#AMCE-model for interaktion mellem kandidatens holdning til journalister og kandidatens beskæftigelse
AMCE_beskæftigelse2 <- cj(data = df,
                          formula = CHOICE_INDICATOR ~ kand_journalister,
                          by = ~ kand_beskæftigelse,
                          id = ~RESPONDENT_ID,
                          estimate = "amce")

# Visualisering af resultater
plot14_AMCE <- AMCE_beskæftigelse2 %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = kand_beskæftigelse)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Sansynglighed for valg af kandidat (AMCE)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Ansat i en virksomhed",
                                  "Leder i en virksomhed"))

plot14_AMCE

#gem plot
ggsave(filename = "plot_beskæftigelse_journalister_AMCE.png",
       plot = plot14_AMCE, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)

#latex output
latex <- AMCE_beskæftigelse2 %>% filter(feature=="Holdning til journalister") %>% 
  select(BY, level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper)

kable(x = latex, format = "latex", digits = 2)