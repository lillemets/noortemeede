# Määra töökaust
setwd('/home/jrl/work/noortemeede')

# Laadi paketid
library('dplyr');library('tidyr');library('ggplot2');library('extrafont')

# Laadi objektid
load('/home/jrl/data/objects/funs.Rda')

# Sisesta andmed ----------

majAr <- readRDS('majandusaasta_aruanded.Rds')


# Joonista ettevõtete eluiga ----------

majAr %>% filter(osalenu == 1) %>% dplyr::select(kood, aasta, asutamine, viimane) %>% 
  group_by(kood) %>% 
  mutate(viimane = as.Date(paste0(max(aasta) + 1, '0701'), '%Y%m%d')) %>% 
  filter(viimane <= as.Date('20170701', '%Y%m%d')) %>% 
  dplyr::select(-aasta) %>% distinct %>% 
  arrange(asutamine) %>% ungroup %>% mutate(rida = group_indices(., kood)) %>% 
  gather(key = 'näitaja', value = 'väärtus', 2:3) %>% 
  mutate(näitaja = ifelse(näitaja == 'asutamine', 
                          "Ettevõtte asutamise aeg", 
                          "Viimase majandusaasta aruande aeg")) %>% 
ggplot() + aes(x = väärtus, y = rida, color = näitaja) + 
  geom_point() + 
  scale_color_discrete(name = NULL) + 
  scale_x_date(name = NULL, date_breaks = 'year', date_labels = '%Y') + 
  scale_y_continuous(name = "Noortalunikud", labels = NULL) + 
  theme(text = element_text(family = 'Roboto Condensed', size = 12), 
        axis.text = element_text(size = 10, angle = 45), 
        axis.ticks = element_blank(), 
        legend.position = 'top', 
        legend.background = element_rect(fill = NA, color = NA, size = .1),
        legend.key = element_blank(), 
        panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = 'white'),
        plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        strip.background = element_blank(), 
        strip.text = element_text(size = 12)) -> plotEluiga

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
  
  ## Tekita sobiv aegrida
  andmed <- extTs(majAr, x, 2012:2016)
  
  ## Joonista vaid siis, kui tabel tühi ei ole
  if (nrow(andmed) > 0) {
  
  # Muuda tegevusala nii, et see kajastaks osalenute arvu valimis
  sagedus <- as.data.frame(table(andmed$tegevusala[!duplicated(andmed$kood) & andmed$osalenu]))
  andmed$tegevusala <- paste0(andmed$tegevusala, 
                              " (n=", sagedus$Freq[match(andmed$tegevusala, sagedus$Var1)], ")")
  
  ## Kohanda osalenu näitaja väärtuseid
  andmed$osalenu <- ifelse(andmed$osalenu, "Noortalunikud", "Teised")
  
  ## Joonista
  ggplot(andmed) + aes_string(x = 'aasta', y = x, color = 'osalenu') +
    expand_limits(y = 0) + 
    geom_line(stat = 'summary', fun.y = 'median', size = 1.2) + 
    labs(caption = "Allikas: Äriregister") + 
    scale_color_brewer(name = NULL, palette = 'Set2') + 
    scale_x_continuous(breaks = min(andmed$aasta):max(andmed$aasta), 
                       name = NULL) + 
    scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = F), 
                       name = Proper(sub("\\.", " ", x))) + 
    facet_wrap(~tegevusala, scales = 'free') + 
    theme(text = element_text(family = 'Roboto Condensed', size = 12), 
          axis.text.x = element_text(size = 10, angle = 45), 
          axis.text.y = element_text(size = 10), 
          axis.ticks = element_blank(), 
          legend.position = 'top', 
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

save(plotEluiga, plotAegread, file = 'aegread.Rda')
