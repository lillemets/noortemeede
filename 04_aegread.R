# Määra töökaust
setwd('/home/jrl/work/noortemeede')

# Laadi paketid
library('dplyr');library('plotly')


# Sisesta andmed ----------

majAr <- readRDS('majandusaasta_aruanded.Rds')


# Loo funktsioon tabeli puhastamiseks puuduvatest juhtumitest ----------

extTs <- function(andmed, näitaja, fun = median) { # Extract time series
  
  # Tabel peab sisaldama järgnevaid veerge: kood, aasta ja osalenu
  
  andmed <- andmed[!is.na(andmed[, näitaja]), ]
  
  # Määra, millistel aastatel on piisavalt osalenuid
  x <- as.matrix(table(andmed$aasta, andmed$osalenu))[, 2] # Vektor osalenute arvuga igal aastal
  x <- names(x)[x >= fun(x)] # Vektor aastategea, mil osalenuid oli üle mediaani/funktsiooni
  x <- as.integer(x)
  if (all(x != cummax(x))) {
    warning('Alles jäänud aastad ei ole järejestikused.')
  }
  
  # Loo tabel, milles on vaid leitud aastad ja kõik koodid iga aasta kohta
  andmed <- andmed[andmed$aasta %in% x, ]
  y <- table(andmed$kood)
  andmed <- andmed[andmed$kood %in% names(y[y >= length(x)]), ]
  
  # Esita tulemused
  return(andmed)

}

ars <- extTs(majAr, 'kohustused')
table(ars$aasta, ars$osalenu)


andmed <- extrTs(majAr, nt)
andmed <- aggregate(andmed$näitaja, list(andmed$osalenu, andmed$aasta), median, na.rm = T)
names(andmed) <- c('osalenu', 'aasta', 'näitaja')


plot_ly(andmed, x = ~aasta, y = ~näitaja, color = ~osalenu, 
        type = 'scatter', mode = 'lines') %>% 
  layout(yaxis = list(range = c(0, max(andmed$näitaja)), title = paste(nt)))
