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
vrMaj <- majAr %>% 
  mutate(osalenu = factor(ifelse(majAr$osalenu, 'Osalenud', 'Teised'))) %>% 
  group_by(kood, osalenu) %>% 
  summarise_at(c('müügitulu', 'põhivara', 'puhaskasum', 'tööjõukulud', 'kohustused', 'võlakordaja'), 
               function(x) mean(x, na.rm = T))

## Eemalda igalt näitajalt äärmuslikud väärtused
vrMaj[, 3:ncol(vrMaj)] <- lapply(vrMaj[, 3:ncol(vrMaj)], function(x)
                                 ifelse(x %in% boxplot.stats(x)$out, NA, x))

## Loo list joonistest

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
