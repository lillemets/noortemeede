# Määra töökaust
setwd('/home/jrl/work/noortemeede')

# Laadi paketid
library('dplyr');library('ggplot2');library('extrafont')

# Laadi objektid
load('/home/jrl/data/objects/funs.Rda')

# Sisesta andmed ----------

majAr <- readRDS('majandusaasta_aruanded.Rds')


# Loo funktsioon tabeli puhastamiseks puuduvatest juhtumitest ----------

extTs <- function(andmed, näitaja, aastad) {
  
  # Tabel peab sisaldama järgnevaid veerge: kood, aasta ja osalenu
  
  # Jäta alles vaid olemasoleva näitaja väärtusega read
  andmed <- andmed[!is.na(andmed[, näitaja]), ]
  
  # Loo tabel, milles on kõik koodid iga aasta kohta
  andmed <- andmed[andmed$aasta %in% aastad, ]
  x <- table(andmed$kood)
  andmed <- andmed[andmed$kood %in% names(x[x >= length(aastad)]), ]
  
  # Esita tulemused
  return(andmed)

}


# Joonista iga näitaja kohta joonis ja lisa listi ----------

plotAegread <- list()

for (näitaja in names(majAr[7:ncol(majAr)])) {
  
  # Tekita sobiv aegrida
  andmed <- extTs(majAr, näitaja, 2010:2014)
  
  # Arvuta mediaanväärtused aastate ja osalemise lõikes
  keskmine <- aggregate(andmed[, näitaja], 
                      by = list(aasta = andmed$aasta, osalenu = andmed$osalenu), 
                      median, na.rm = T)
  ## Eemalda näitajalt äärmuslikud väärtused
  andmed[, näitaja] <- ifelse(andmed[, näitaja] %in% boxplot.stats(andmed[, näitaja])$out, 
                              NA, 
                              andmed[, näitaja])
  
  # Määra näitaja skaala puktid
  skaala <- pretty(andmed[, näitaja])
  
  # Joonista
  plotAegread[[näitaja]] <- 
    ggplot(andmed) + aes_string(x = 'aasta', y = näitaja) +
    geom_jitter(aes(color = substr(emtak, 1, 3)), width = .2, alpha = .5) + 
    geom_point(data = keskmine, aes(x = aasta, y = x), 
               alpha = .8, size = 2, color = 'gray10') + 
    geom_line(data = keskmine, aes(x = aasta, y = x), 
              alpha = .8, size = 1.2, color = 'gray10') + 
    labs(title = paste("Meetmes 1.2 osalenud ja teiste põllumajandusettevõtjate", 
                       sub("\\.", " ", näitaja)), 
         caption = "Allikas: Äriregister") + 
    scale_color_brewer(name = "EMTAK", palette = 'Set2') + 
    scale_x_continuous(name = NULL) + 
    scale_y_continuous(breaks = skaala, 
                       labels = format(skaala, big.mark = " ", scientific = F), 
                       name = Proper(sub("\\.", " ", näitaja))) + 
    facet_grid(ifelse(osalenu, 
                      paste0("Osalenud (n =", length(unique(andmed$kood[andmed$osalenu])), ")"),
                            paste0("Teised (n=", length(unique(andmed$kood[!(andmed$osalenu)])), ")")) ~ ., 
               scales = 'free') + 
    theme(text = element_text(family = 'Roboto Condensed', size = 12), 
          axis.text = element_text(size = 10), 
          axis.ticks = element_blank(), 
          legend.background = element_rect(fill = NA, color = NA, size = .1),
          legend.key = element_blank(), 
          panel.background = element_rect(fill = NA),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          panel.grid.major.y = element_line(color = 'gray80'),
          plot.background = element_rect(fill = 'white'),
          plot.title = element_text(size = 16), 
          plot.caption = element_text(size = 10), 
          strip.background = element_blank(), 
          strip.text = element_text(size = 12))
}


# Salvesta ----------
save(plotAegread, file = 'aegread.Rda')