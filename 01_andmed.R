# Tühjenda töölaud
rm(list = ls())

# Määra töökaust
setwd('/home/jrl/work')

# Laadi pakid
library('dplyr')


# Laadi ja kohanda andmed ----------

## Sisesta tabelid
majAr <- readRDS('majandusandmed/majandusaasta_aruanded.Rds') # Majandusaasta aruanded
majStr <- readRDS('majandusandmed/boa_str_171103.Rds') # Ettevõtete struktuursed näitajad
tegevused <- readRDS('mak0713/tegevused.Rds') # Taotluste andmed

## Jäta alles vaid aasta lõpu andmed
majAr <- majAr[majAr$seisuga == 'lõpp', ]
majAr <- majAr[, -3]

## Sisesta ettevõtte viimane aasta
viimane <- aggregate(majAr$aasta, by = list(kood = majAr$kood), max)
majAr$viimane <- viimane$x[match(majAr$kood, viimane$kood)]

## Sisesta meetmes 1.2 osalenud
majAr$osalenu <- majAr$kood %in% readRDS('noortemeede/osalenud.Rds')
## Mitme osalenu kohta on andmed olemas?
length(unique(majAr$kood[majAr$osalenu]))

## Sisesta ettevõtte asutamine
majAr$asutamine <- majStr$asutamine[match(majAr$kood, majStr$kood)] 

## Sisesta EMTAK koodid
majAr$emtak <- majStr$emtak[match(paste(majAr$kood, majAr$aasta), paste(majStr$kood, majStr$aasta))] 
## Jäta alles vaid põllumajandusliku tegevusalaga read
majAr <- majAr[!is.na(majAr$emtak) & substr(majAr$emtak, 1, 2) == "01" & 
                   substr(majAr$emtak, 1, 3) != "017", ] # Eemalda ka jahindus

## Sisesta maksekuupäev
maksed <- tegevused[tegevused$meede == 1200, c('kood', 'maksekuupäev')]
maksed <- aggregate(maksed$maksekuupäev, list(kood = maksed$kood), max)
majAr$makse <- maksed$x[match(majAr$kood, maksed$kood)]


# Sisesta tegevusala ----------

## Loo tabel kõige sagedasemate koodidega
emtakid <- read.csv(textConnection('emtak,tegevusala
01111,Teraviljakasvatus
01411,Piimakarjakasvatus
01421,Veisekasvatus
01501,Segapõllumajandus
01131,Köögiviljakasvatus
01611,Taimekasvatuse abitegevused
01491,Mesindus
01461,Seakasvatus'), col.names = c('emtak', 'tegevusala'), colClasses = 'character')

## Lisa tegevusalad
majAr$tegevusala <- emtakid$tegevusala[match(majAr$emtak, emtakid$emtak)]
majAr$tegevusala <- ifelse(is.na(majAr$tegevusala) & !(majAr$emtak %in% emtakid$emtak), 
                           "Muu tegevusala", 
                           majAr$tegevusala) # Muu tegevusala, kui emtakid ei sisalda vastet


# Arvuta näitajate väärtused ----------

## Loo funktsioon näitaja nimetuste otsimiseks
otsi <- function(x) names(majAr)[grep(x, names(majAr), ignore.case = T)]

## Arvuta majandusnäitajad
majAr <- mutate(majAr, 
  müügitulu = X10.MÜÜGITULU.skeem.1, 
  põhivara = X120.MATERIAALNE.PÕHIVARA.KOKKU + X130.IMMATERIAALNE.PÕHIVARA.KOKKU, 
  vara = X300.A.K.T.I.V.A.VARAD.K.O.K.K.U, 
  käibevara = vara - põhivara, 
  puhaskasum = X250.ARUANDEAASTA.PUHASKASUM..KAHJUM.1.2, 
  tööjõukulud = abs(X130.TÖÖJÕU.KULUD.KOKKU.1), 
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
