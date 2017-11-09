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

teeAegrida <- function(x) {
  
  # Tekita sobiv aegrida
  andmed <- extTs(majAr, x, 2010:2014)
  
  # Arvuta mediaanväärtused aastate ja osalemise lõikes
  keskmine <- aggregate(andmed[, x], 
                      by = list(aasta = andmed$aasta, osalenu = andmed$osalenu), 
                      median, na.rm = T)
  
  ## Eemalda xlt äärmuslikud väärtused
  andmed[, x] <- ifelse(andmed[, x] %in% boxplot.stats(andmed[, x])$out, 
                              NA, 
                              andmed[, x])
  
  # Määra x skaala puktid
  skaala <- pretty(andmed[, x])
  
  # Joonista
  ggplot(andmed) + aes_string(x = 'aasta', y = x) +
    geom_jitter(aes(color = tegevusala.laiem), width = .2, alpha = .5) + 
    geom_point(data = keskmine, aes(x = aasta, y = x), 
               alpha = .8, size = 2, color = 'gray10') + 
    geom_line(data = keskmine, aes(x = aasta, y = x), 
              alpha = .8, size = 1.2, color = 'gray10') + 
    labs(title = paste("Meetmes 1.2 osalenud ja teiste põllumajandusettevõtjate", 
                       sub("\\.", " ", x)), 
         caption = "Allikas: Äriregister") + 
    scale_color_brewer(name = "Tegevusala", palette = 'Set2') + 
    scale_x_continuous(name = NULL) + 
    scale_y_continuous(breaks = skaala, 
                       labels = format(skaala, big.mark = " ", scientific = F), 
                       name = Proper(sub("\\.", " ", x))) + 
    facet_grid(ifelse(osalenu, 
                      paste0("Osalenud (n=", length(unique(andmed$kood[andmed$osalenu])), ")"),
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

plotAegread <- lapply(names(majAr[8:10]), teeAegrida)
names(plotAegread) <- names(majAr[8:10])

# Salvesta ----------
save(plotAegread, file = 'aegread.Rda')