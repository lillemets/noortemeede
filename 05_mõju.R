# Määra töökaust
setwd('/home/jrl/work/noortemeede')

# Laadi paketid
library('ggplot2')

# Laadi objektid
load('/home/jrl/data/objects/funs.Rda')
load('/home/jrl/data/objects/ggplot_themes.Rda')

# Sisesta andmed ----------

majAr <- readRDS('majandusaasta_aruanded.Rds')


# Arvuta aastad investeeringu tegemise aasta suhtes ----------

majAr$inv.aeg <- majAr$aasta - as.numeric(substr(majAr$makse, 1, 4))


# Joonista ----------

teeMõju <- function(x) {
  
  # Eemalda puuduvad juhtumid
  andmed <- extTs(majAr, 'kood', x, 'inv.aeg', -1:3)
  
  # Muuda tegevusala nii, et see kajastaks valimi surust
  sagedus <- as.data.frame(table(andmed$tegevusala[!duplicated(andmed$kood)]))
  andmed$tegevusala <- paste0(andmed$tegevusala, 
                               " (n=", sagedus$Freq[match(andmed$tegevusala, sagedus$Var1)], ")")
  
  # Joonista
  üld <- 
      ggplot(andmed) + aes_string(x = 'inv.aeg', y = x) +
      geom_line(stat = 'summary', fun.y = 'median', size = 1.2) + 
      geom_vline(xintercept = 0, color = 'red') + 
      expand_limits(y = 0) + 
      labs(caption = "Allikas: Äriregister") + 
      scale_color_brewer(name = NULL, palette = 'Set2') + 
      scale_x_continuous(name = 'Aasta investeeringu lõpetamise aasta suhtes') + 
      scale_y_continuous(labels = function(x) format(x, digits = 0, big.mark = " ", scientific = F), 
                         name = Proper(sub("\\.", " ", x))) + 
      theme_white()
  teg <- üld + facet_wrap(~tegevusala, scales = 'free_y')
  
  # Esita
  list(üld, teg)
  
}

plotMõju <- lapply(names(majAr[which(names(majAr) == 'müügitulu'):ncol(majAr)]), teeMõju)
names(plotMõju) <- names(majAr[which(names(majAr) == 'müügitulu'):ncol(majAr)])



# Salvesta ----------

save(plotMõju, file = 'mõju.Rda')