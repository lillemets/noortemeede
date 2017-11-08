# Tühjenda töölaud
rm(list = ls())

# Määra töökaust
setwd('/home/jrl/work')

# Laadi pakid
library('dplyr')


# Laadi ja kohanda andmed ----------

## Sisesta majandusaasta aruanded
majAr <- readRDS('majandusandmed/majandusaasta_aruanded.Rds')

## Jäta alles vaid aasta lõpu andmed
majAr <- majAr[majAr$seisuga == 'lõpp', ]
majAr <- majAr[, -3]

## Sisesta meetmes 1.2 osalenud
majAr$osalenu <- majAr$kood %in% readRDS('noortemeede/osalenud.Rds')
## Mitme osalenu kohta on andmed olemas?
length(unique(majAr$kood[majAr$osalenu]))

## Sisesta EMTAK koodid
majEsi <- readRDS('majandusandmed/boa_4_171103.Rds')
majAr$emtak <- majEsi$emtak[match(paste(majAr$kood, majAr$aasta), paste(majEsi$kood, majEsi$aasta))] 

# Lisa tegevusala EMTAK koodi alguse järgi
emtakid <- readRDS('majandusandmed/emtak.Rds')
majAr$tegevusala <- emtakid$tegevusala[
  match(substr(majAr$emtak, 1, 5), emtakid$emtak)]

## Jätta alles vaid põllumajandusliku tegevusalaga read
majAr <- majAr[!is.na(majAr$emtak) & substr(majAr$emtak, 1, 2) == "01", ]


# Arvuta näitajate väärtused ----------

## Loo funktsioon näitaja nimetuste otsimiseks
otsi <- function(x) names(majAr)[grep(x, names(majAr), ignore.case = T)]

## Arvuta majandusnäitajad
majAr <- mutate(majAr, 
  müügitulu = X11.MÜÜGITULU.skeem.1, 
  põhivara = X120.MATERIAALNE.PÕHIVARA.KOKKU + X130.IMMATERIAALNE.PÕHIVARA.KOKKU, 
  vara = X300.A.K.T.I.V.A.VARAD.K.O.K.K.U, 
  käibevara = vara - põhivara, 
  puhaskasum = X250.ARUANDEAASTA.PUHASKASUM..KAHJUM.1.2, 
  tööjõukulud = X130.TÖÖJÕU.KULUD.KOKKU.1, 
  kohustused = X400.LüHIAJALISED.KOHUSTUSED.KOKKU + X500.PIKAAJALISED.KOHUSTUSED.KOKKU, 
  omakapital = vara - kohustused, 
  vara.rentaablus = puhaskasum / vara * 100, 
  omakapitali.rentaablus = puhaskasum / omakapital * 100, 
  müügitulu.rentaablus = puhaskasum / müügitulu * 100,
  võlakordaja = kohustused / vara, 
  maksevõimekordaja = käibevara / X400.LüHIAJALISED.KOHUSTUSED.KOKKU, 
  likviidsuskordaja = (käibevara - X70.VARUD.KOKKU. - X80.NÕUDED.JA.ETTEMAKSED.KOKKU.) / 
    X400.LüHIAJALISED.KOHUSTUSED.KOKKU
)

## Eemalda liigsed näitajad
majAr <- majAr[, !grepl('X', names(majAr))]


# Salvesta ----------
saveRDS(majAr, file = 'noortemeede/majandusaasta_aruanded.Rds')
