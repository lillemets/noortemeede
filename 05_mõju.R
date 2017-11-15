# Määra töökaust
setwd('/home/jrl/work/noortemeede')

# Laadi paketid
library('ggplot2')

# Laadi objektid
load('/home/jrl/data/objects/funs.Rda')


# Sisesta andmed ----------

majAr <- readRDS('majandusaasta_aruanded.Rds')


# Arvuta aastad investeeringu tegemise aasta suhtes ----------

majAr$inv.aeg <- majAr$aasta - as.numeric(substr(majAr$makse, 1, 4))


# Joonista ----------

teeMõju <- function(x) {
  
  # Eemalda puuduvad juhtumid
  andmed <- extTs(majAr, 'kood', x, 'inv.aeg', -1:2)
  
  # Muuda tegevusala nii, et see peegeldaks sagedust
  sagedus <- as.data.frame(table(andmed$tegevusala[!duplicated(andmed$kood)]))
  andmed$tegevusala <- paste0(andmed$tegevusala, 
                               " (n=", sagedus$Freq[match(andmed$tegevusala, sagedus$Var1)], ")")
  
  ggplot(andmed) + aes_string(x = 'inv.aeg', y = x) +
    geom_line(stat = 'summary', fun.y = 'median', size = 1.2) + 
    geom_vline(xintercept = 0, color = 'red') + 
    expand_limits(y = 0) + 
    labs(caption = "Allikas: Äriregister") + 
    scale_color_brewer(name = NULL, palette = 'Set2') + 
    scale_x_continuous(name = 'Aasta investeeringu lõpetamise aasta suhtes') + 
    scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = F), 
                       name = Proper(sub("\\.", " ", x))) + 
    facet_wrap(~tegevusala, scales = 'free_y') + 
    theme(text = element_text(family = 'Roboto Condensed', size = 12), 
          axis.text = element_text(size = 10), 
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
}

plotMõju <- lapply(names(majAr[which(names(majAr) == 'müügitulu'):ncol(majAr)]), teeMõju)
names(plotMõju) <- names(majAr[which(names(majAr) == 'müügitulu'):ncol(majAr)])


# Salvesta ----------

save(plotMõju, file = 'aegread.Rda')