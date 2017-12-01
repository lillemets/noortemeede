# Määra töökaust
setwd('/home/jrl/work/noortemeede')

# Laadi paketid
library('ggplot2')

# Laadi objektid
load('/home/jrl/data/objects/funs.Rda')
load('/home/jrl/data/objects/ggplot_themes.Rda')


# Sisesta andmed ----------

majAr <- readRDS('majandusaasta_aruanded.Rds')

## Arvuta tegutsemise aasta
majAr$aeg <- majAr$aasta - as.numeric(substr(majAr$asutamine, 1, 4))

## Arvuta aastad investeeringu tegemise aasta suhtes
majAr$inv.aeg <- majAr$aasta - as.numeric(substr(majAr$makse, 1, 4))

## Sisesta meetmes 1.2 osalenud ja alustajad
kogemus <- readRDS('kogemus.Rds')
majAr$osalenu <- majAr$kood %in% kogemus$kood[kogemus$toetatud]
majAr$alustaja <- majAr$kood %in% kogemus$kood[kogemus$kogemus == 'alustaja']


# Müügitulu muutus asutamise aja suhtes----------

## Loo andmestik
mtDF <- extTs(majAr, 'kood', 'müügitulu', 'aeg', 1:3)

## Määra y-skaala puktid
skaala <- seq(0, quantile(mtDF$müügitulu, probs = .9, na.rm = T), 2e4)

## Muuda tegevusala nii, et see kajastaks valimi surust
mtDF$tegevusala <- addSize(mtDF, 'kood', 'tegevusala')

## Joonista
ggplot(mtDF) + 
  aes(x = as.factor(aeg), y = müügitulu, color = factor(osalenu, levels = c(T, F))) + 
  geom_boxplot(width = .4, alpha = 0, outlier.colour = NA, position = position_dodge(.6)) + 
  coord_cartesian(ylim = c(min(skaala), max(skaala))) + 
  labs(caption = "Allikas: Äriregister", color = NULL) + 
  scale_x_discrete(labels = as.roman(1:3), name = "Tegutsemise aasta") + 
  scale_y_continuous(breaks = skaala, labels = Numb(skaala / 1e3), 
                     name = "Müügitulu, tuhat eurot") + 
  scale_color_manual(labels = c("Meetmes 1.2 osalenud ettevõtjad", 
                                "Teised investeeringutoetusi saanud ettevõtjad"), 
                     values = c('red', 'black')) + 
  facet_wrap(~tegevusala) + 
  theme_white() + theme(legend.position = 'top')


# Müügitulu protsentuaalne muutus asutamise aja suhtes----------

## Loo andmestik
mtDF <- extTs(majAr, 'kood', 'müügitulu', 'aeg', 1:3)

## Jäta alles vaid osalenud
mtDF <- mtDF[mtDF$osalenu, ]

## Muuda müügitulu nii, et see näitaks muutust võrreldes 1. aasta väärtusega
mtDF$müügitulu <- unlist(tapply(mtDF$müügitulu, mtDF$kood, function(x) (x / head(x, 1)) - 1))
mtDF <- mtDF[!is.na(mtDF$müügitulu), ] # Eemalda puuduvad
mtDF <- mtDF[!is.infinite(mtDF$müügitulu), ] # Eemalda lõputud

## Muuda tegevusala nii, et see kajastaks valimi surust
mtDF$tegevusala <- addSize(mtDF, 'kood', 'tegevusala')

## Loo andmestik mediaani koordinaatide ja tegevusalade nimetustega
lõpp <- data.frame(x = rep(max(mtDF$aeg), length(levels(mtDF$tegevusala))), 
                   y = sapply(paste(levels(mtDF$tegevusala)), 
                              function(x) median(
                                mtDF[mtDF$aeg == max(mtDF$aeg) & mtDF$tegevusala == x, 
                                     'müügitulu']), USE.NAMES = F), 
                   label = levels(mtDF$tegevusala))

## Joonista
ggplot(mtDF) + 
  aes(x = as.factor(aeg), y = müügitulu, group = tegevusala) + 
  geom_line(stat = 'summary', fun.y = 'median', size = .6) + 
  geom_text(data = lõpp, aes(x = x, y = y, label = label, group = NA), 
            hjust = 0, family = 'Roboto Condensed') + 
  coord_cartesian(xlim = c(1.5, 3)) + 
  labs(caption = "Allikas: Äriregister") + 
  scale_x_discrete(labels = as.roman(1:5), name = "Tegutsemise aasta") + 
  scale_y_continuous(breaks = seq(0, 1.2, .1), labels = Perc, 
                     name = "Müügitulu muutus I tegutsemise aasta suhtes (mediaan)") + 
  theme_white() + theme(legend.position = 'none')

# Vara muutus asutamise aja suhtes ----------

## Loo andmestik
vDF <- extTs(majAr, 'kood', 'vara', 'aeg', 1:5)

## Määra y-skaala puktid
skaala <- seq(0, quantile(vDF$vara, probs = .9, na.rm = T), 10e4)

## Muuda tegevusala nii, et see kajastaks valimi surust
vDF$tegevusala <- addSize(vDF, 'kood', 'tegevusala')

## Joonista
ggplot(vDF) + 
  aes(x = as.factor(aeg), y = vara) + 
  geom_boxplot(width = .4, alpha = 0, outlier.colour = NA) + 
  coord_cartesian(ylim = c(min(skaala), max(skaala))) + 
  labs(caption = "Allikas: Äriregister", color = NULL) + 
  scale_x_discrete(labels = as.roman(1:5), name = "Tegutsemise aasta") + 
  scale_y_continuous(breaks = skaala, labels = Numb(skaala / 1e3), 
                     name = "Vara, tuhat eurot") + 
  facet_wrap(~tegevusala, ncol = 1) + 
  theme_white() + theme(legend.position = 'bottom')



# Müügitulu muutus investeeringu aja suhtes ----------

## Loo andmestik
mtDF <- extTs(majAr, 'kood', 'müügitulu', 'inv.aeg', -1:5)

## Jäta alles vaid osalenud
mtDF <- mtDF[mtDF$osalenu, ]

## Vaata aastaid
table(mtDF$aasta, mtDF$inv.aeg)

## Määra y-skaala puktid
skaala <- seq(0, quantile(mtDF$müügitulu, probs = .9, na.rm = T), 2e4)

## Muuda tegevusala nii, et see kajastaks valimi surust
mtDF$tegevusala <- addSize(mtDF, 'kood', 'tegevusala')

## Joonista
ggplot(mtDF) + 
  aes(x = factor(inv.aeg), y = müügitulu) + 
  geom_jitter(aes(color = alustaja), width = .15, alpha = .3) +
  geom_boxplot(width = .4, alpha = 0, outlier.colour = NA) + 
  coord_cartesian(ylim = c(min(skaala), max(skaala))) + 
  labs(caption = "Allikas: Äriregister", color = NULL) + 
  scale_x_discrete(labels = c(-1, 0, paste0('+', 1:5)), 
                   name = "Aasta investeeringu tegemise aasta suhtes") + 
  scale_y_continuous(breaks = skaala, labels = Numb(skaala / 1e3), 
                     name = "Müügitulu, tuhat eurot") + 
  scale_color_manual(labels = c("Ülevõtja või jätkaja", "Alustaja"), values = c('black', 'red')) +
  theme_white() + theme(legend.position = 'top')


# Vara muutus investeeringu aja suhtes ----------

## Loo andmestik
vDF <- extTs(majAr, 'kood', 'vara', 'inv.aeg', 0:2)

## Jäta alles vaid osalenud
vDF <- vDF[vDF$osalenu, ]

## Määra y-skaala puktid
skaala <- seq(0, quantile(vDF$vara, probs = .9, na.rm = T), 2e4)

## Muuda tegevusala nii, et see kajastaks valimi surust
vDF$tegevusala <- addSize(vDF, 'kood', 'tegevusala')

## Joonista
ggplot(vDF) + 
  aes(x = factor(inv.aeg), y = vara) + 
  geom_jitter(aes(color = tegevusala), width = .2) +
  geom_boxplot(width = .4, alpha = 0, outlier.colour = NA) + 
  coord_cartesian(ylim = c(min(skaala), max(skaala))) + 
  labs(caption = "Allikas: Äriregister", color = "Tegevusala") + 
  scale_x_discrete(labels = c(0, paste0('+', 1:2)), name = "Aasta investeeringu tegemise suhtes") + 
  scale_y_continuous(breaks = skaala, labels = Numb(skaala / 1e3), 
                     name = "vara, tuhat eurot") + 
  theme_white()

# Vara ja müügitulu seos ----------

## Vii andmed laia formaati
mtV <- reshape(majAr[, c('kood', 'tegevusala', 'inv.aeg', 'müügitulu', 'vara')], 
               idvar = 'kood', timevar = 'inv.aeg', direction = 'wide')

## Loo andmestik
mtV <- mtV[complete.cases(mtV[, c(paste0('müügitulu.', 0:4), 'vara.0', 'tegevusala.0')]), ]

length(mtV[!duplicated(mtV$kood), ])

lapply(0:4, function(x) {
       ars <- lm(as.formula(paste0('müügitulu.', x, ' ~ vara.0 + tegevusala.0')), data = mtV)
       coef(ars)[grep("vara", names(coef(ars)))]
})
