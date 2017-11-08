# Loo funktsioon tabeli puhastamiseks puuduvatest juhtumitest

## Dplyr-põhine lahendus. Töötab vaid interaktiivselt

extrTs <- function(data, ind, min = 100) { # Extract time series
  
  # Tabel peab sisaldama järgnevaid veerge: kood, aasta ja osalenu
  
  # Loo quosure
  ind <- enquo(ind)
  
  # Määra, millistel aastatel on piisavalt osalenuid
  x <- data %>% filter(!is.na(!!ind)) %>% 
    group_by(aasta) %>% tally(osalenu) %>% filter(n > min) %>% pull(aasta)
  
  # Loo tabel, milles on vaid leitud aastad ja kõik koodid iga aasta kohta
  data %>% filter(aasta %in% x) %>% 
    group_by(kood) %>% filter(n() == length(x)) %>% ungroup %>% 
    select(osalenu, aasta, !!ind) %>% rename(näitaja = !!ind)

}

## Põhi-R lahendus

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
