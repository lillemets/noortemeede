# Määra töökaust
setwd('/home/jrl/work/noortemeede')

# Laadi paketid
library('dplyr');library('plotly')

# Laadi objektid
load('/home/jrl/data/objects/funs.Rda')


# Sisesta andmed ----------

majAr <- readRDS('majandusaasta_aruanded.Rds')


# Majandusliku suuruse võrdlus ----------

## Loo tabel iga koodi aastate keskmistega
vrMaj <- aggregate(majAr[, 7:ncol(majAr)], 
                   by = list(kood = majAr$kood, 
                             osalenu = majAr$osalenu, 
                             tegevusala.laiem = majAr$tegevusala.laiem), 
                   mean, na.rm = T)

## Eemalda igalt näitajalt äärmuslikud väärtused
vrMaj[, 4:ncol(vrMaj)] <- lapply(vrMaj[, 4:ncol(vrMaj)], function(x)
                                 ifelse(x %in% boxplot.stats(x)$out, NA, x))

## Loo funktsioon joonistamiseks ja joonista
võrdleMaj <- function(x) {
  
  # Määra x skaala puktid
  skaala <- pretty(vrMaj[, x])
  
  # Määra valimid
  arvOsalenud <- length(vrMaj$kood[vrMaj$osalenu & !is.na(vrMaj[, x])])
  arvTeised <- length(vrMaj$kood[!(vrMaj$osalenu)& !is.na(vrMaj[, x])])
  
  # Joonista
  ggplot(vrMaj) + aes_string(x = 'tegevusala.laiem', y = x, color = 'tegevusala.laiem') +
    geom_jitter(width = .4, alpha = .3) + 
    geom_boxplot(width = .2, alpha = .6, outlier.colour = NA) + 
    labs(title = paste("Meetmes 1.2 osalenud ja teiste põllumajandusettevõtete", 
                       sub("\\.", " ", x), 
                       "(2007 - 2015 keskmine)"), 
         caption = "Allikas: Äriregister") + 
    scale_color_brewer(name = "Tegevusala", palette = 'Set2') + 
    scale_x_discrete(labels = NULL, name = NULL) + 
    scale_y_continuous(breaks = skaala, 
                       labels = format(skaala, big.mark = " ", scientific = F), 
                       name = Proper(sub("\\.", " ", x))) + 
    facet_grid(. ~ ifelse(osalenu, 
                      paste0("Osalenud (n=", arvOsalenud, ")"),
                      paste0("Teised (n=", arvTeised, ")"))) + 
    theme(text = element_text(family = 'Roboto Condensed', size = 12), 
          axis.text = element_text(size = 10), 
          axis.ticks = element_blank(), 
          legend.background = element_rect(fill = NA, color = NA, size = .1),
          legend.key = element_blank(), 
          panel.background = element_rect(fill = NA),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          panel.grid.major.y = element_line(color = 'gray80'),
          panel.spacing = unit(30, "pt"), 
          plot.background = element_rect(fill = 'white'),
          plot.title = element_text(size = 16), 
          plot.caption = element_text(size = 10), 
          strip.background = element_blank(), 
          strip.text = element_text(size = 12))
}
plotMaj <- lapply(names(vrMaj)[4:ncol(vrMaj)], võrdleMaj)
names(plotMaj) <- names(vrMaj)[4:ncol(vrMaj)]


# Tegevusalade võrdlus ----------

## Jäta iga koodi viimane aasta
vrTeg <- majAr[order(majAr$aasta, decreasing = T), ]
vrTeg <- vrTeg[!duplicated(vrTeg$kood), ]

vrTeg <- as.data.frame.matrix(
  prop.table(table(
    vrTeg$tegevusala, ifelse(vrTeg$osalenu, 'osalenud', 'teised')), 2))
vrTeg$tegevusala <- rownames(vrTeg)

## Määra skaala
skaala <- seq(0, round(max(vrTeg[,1:2]), digits = 1), .05)

## Jäta alles tegevusalade nimetused vaid siis, kui osakaaludes on erinevus
vrTeg$tegevusala <- ifelse(abs(vrTeg$teised - vrTeg$osalenud) > .02, vrTeg$tegevusala, NA)

## Loo funktsioon joonistamiseks ja joonista
plotTeg <- ggplot(vrTeg) + aes(x = teised, y = osalenud) + 
  geom_abline(intercept = 0, slope = 1, color = 'gray80') + 
  geom_point(size = 2) + 
  geom_text(aes(label = tegevusala), hjust = .02, vjust = -.4, 
            family = 'Roboto Condensed', alpha = .6) + 
  labs(title = paste("Meetmes 1.2 osalenud ja teiste põllumajandusettevõtete tegevusalad"), 
       subtitle = "Diagonaaljoonest kõrgemal olevad tegevusalad on osalenute seas rohkem levinud ja vastupidi. Välja on toodud tegevusalad, mille puhul erinevus oli üle 2%", 
       caption = "Allikas: Äriregister") + 
  scale_x_continuous(breaks = skaala, 
                   labels = Perc(skaala), 
                   name = paste0("Teised (n=", 
                                 length(unique(majAr$kood[!(majAr$osalenu)])), ")")) + 
  scale_y_continuous(breaks = skaala, 
                     labels = Perc(skaala), 
                     name = paste0("Osalenud (n=", 
                                   length(unique(majAr$kood[majAr$osalenu])), ")")) + 
  theme(text = element_text(family = 'Roboto Condensed', size = 12), 
        axis.text = element_text(size = 10), 
        axis.ticks = element_blank(), 
        legend.background = element_rect(fill = NA, color = NA, size = .1),
        legend.key = element_blank(), 
        panel.background = element_rect(fill = NA), 
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        panel.grid.major.x = element_line(color = 'gray80'),
        panel.grid.major.y = element_line(color = 'gray80'),
        panel.spacing = unit(30, "pt"), 
        plot.background = element_rect(fill = 'white'),
        plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        strip.background = element_blank(), 
        strip.text = element_text(size = 12))


# Salvestamine ----------
save(plotMaj, plotTeg, file = 'võrdlus.Rda')