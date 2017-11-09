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

## Loo list joonistega iga näitaja kohta

compare <- function(x) {
  
  # Määra x skaala puktid
  skaala <- pretty(vrMaj[, x])
  
  # Määra valimid
  arvOsalenud <- length(vrMaj$kood[vrMaj$osalenu & !is.na(vrMaj[, x])])
  arvTeised <- length(vrMaj$kood[!(vrMaj$osalenu)& !is.na(vrMaj[, x])])
  
  # Joonista
  ggplot(vrMaj) + aes_string(x = 'tegevusala.laiem', y = x, color = 'tegevusala.laiem') +
    geom_jitter(width = .4, alpha = .3) + 
    geom_boxplot(width = .2, alpha = .6, outlier.colour = NA) + 
    labs(title = paste("Meetmes 1.2 osalenud ja teiste põllumajandusettevõtjate", 
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
plotMaj <- lapply(names(vrMaj)[4:ncol(vrMaj)], compare)
names(plotMaj) <- names(vrMaj)[4:ncol(vrMaj)]


### Loo funktsioon, mis loob näitajate kohta joonised
plotlyMaj <- function(ind) {
  plot_ly(vrMaj, y = ~get(ind), color = ~osalenu, 
          type = 'box', boxpoints = 'all', jitter = .5, pointpos = -2) %>% 
  layout(yaxis = list(title = Proper(ind)))
}

### Rakenda funktsiooni näitajatele
plyMaj <- list()
for (näitaja in names(vrMaj)[3:ncol(vrMaj)]) {
  plyMaj[[näitaja]] <- plotlyMaj(näitaja)
}

# Tegevusalade võrdlus ----------

## Jäta iga koodi viimane aasta
vrTeg <- majAr[order(majAr$aasta, decreasing = T), ]
vrTeg <- vrTeg[!duplicated(vrTeg$kood), ]

vrTeg <- as.data.frame.matrix(
  prop.table(table(
    vrTeg$tegevusala, ifelse(vrTeg$osalenu, 'Osalenud', 'Teised')), 2))
vrTeg$tegevusala <- rownames(vrTeg)


maks <- max(vrTeg[,1:2])

## 
plyTeg <- plot_ly(vrTeg, x = ~Teised, y = ~Osalenud, text = ~paste(tegevusala),
                  type = 'scatter', mode = 'markers') %>% 
  layout(showlegend = F) %>% 
  add_trace(x = c(0, maks), y = c(0, maks), mode =  'lines', text = NULL)


# Salvestamine ----------
save(plyMaj, plyTeg, file = 'võrdlus.Rda')
