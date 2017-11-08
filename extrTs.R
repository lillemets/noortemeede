# Loo funktsioon tabeli puhastamiseks puuduvatest juhtumitest
# Töötab vaid interaktiivselt

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
