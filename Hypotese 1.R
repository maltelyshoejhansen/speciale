#Indstil working directory
setwd("/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse")

#load pakker
library(extrafont)
library(cregg)
library(ggplot2)

###HYPOTESE 1###

#Set labels for karakteristika
attr(df$kand_FT, "label") <- "Holdning til Folketinget"
attr(df$kand_journalister, "label") <- "Holdning til journalister"

#marginal means model
mm_H1 <- cj(data = df, 
         formula =  CHOICE_INDICATOR ~ kand_køn + kand_uddannelse + kand_beskæftigelse + kand_indvandring + 
           kand_ligestilling + kand_FT + kand_journalister, 
         id = ~RESPONDENT_ID, #klyngerobuste standardfejl
         estimate = "mm", 
         h0=0.5)

#plot for holdning til Folketinget
plot_FT <- mm_H1 %>%
  filter(feature=="Holdning til Folketinget") %>%
  ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.5) + 
  geom_point(fill = "black") +
  xlab("Sandsynlighed for valg af kandidat (marginal means") +
  ylab("") +
  facet_grid(feature ~., 
             scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        axis.title.x = element_text(size = 10))

plot_FT

#gem plot
ggsave(filename = "Plot_FT.png",
       plot = plot_FT, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)


#plot for holdning til journalister
plot_journalister <- mm_H1 %>%
  filter(feature=="Holdning til journalister") %>%
  ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.5) + 
  geom_point(fill = "black") +
  xlab("Sandsynlighed for valg af kandidat (marginal means)") +
  ylab("") +
  facet_grid(feature ~., 
             scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "LM Roman 10"),
        axis.title.x = element_text(size = 10))

plot_journalister

#gem plot
ggsave(filename = "plot_journalister.png",
       plot = plot_journalister, 
       path = "/Users/maltefreylyshojhansen/Desktop/Speciale/Speciale data/Kode/Analyse/Grafik",
       width = 7, 
       height = 5,
       dpi = 320)
