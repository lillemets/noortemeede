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


# Loo funktsioon joonistamiseks ja joonista ----------

teeAegrida <- function(x) {
  
  # Tekita sobiv aegrida
  andmed <- extTs(majAr, x, 2010:2014)
  
  # Joonista vaid siis, kui tabel tühi ei ole
  if (nrow(andmed) > 0) {
  
  ## Eemalda xlt äärmuslikud väärtused
  andmed[, x] <- ifelse(andmed[, x] %in% boxplot.stats(andmed[, x])$out, 
                              NA, 
                              andmed[, x])
  
  # Määra x skaala puktid
  skaala <- pretty(andmed[, x])
  
  # Joonista
  ggplot(andmed) + aes_string(x = 'aasta', y = x, color = 'tegevusala.laiem') +
    #geom_jitter(width = .3, alpha = .3) + 
    #geom_line(aes(group = kood), alpha = .3) + 
    geom_line(stat = 'summary', fun.y = 'mean', 
              size = 1.2, color = 'gray30') + 
    labs(#title = paste("Meetmes 1.2 osalenud ja teiste põllumajandusettevõtete", 
         #              sub("\\.", " ", x)), 
         subtitle = "Joon esindab keskmise väärtuse muutust.",
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
  
  } else {
    return("Joonise koostamiseks puudusid andmed.")
  }
}
plotAegread <- lapply(names(majAr[which(names(majAr) == 'müügitulu'):ncol(majAr)]), teeAegrida)
names(plotAegread) <- names(majAr[which(names(majAr) == 'müügitulu'):ncol(majAr)])

# Salvesta ----------

save(plotAegread, file = 'aegread.Rda')
