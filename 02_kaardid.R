# Määra töökaust
setwd('/home/jrl/work/noortemeede')

# Laadi paketid
library('dplyr');library('ggplot2')

# Laadi tõmmis
load('kaardid.Rda')

# Sisesta andmed ----------
download.file(url = 'http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/ef_m_farmang.tsv.gz',
              destfile = 'ef_m_farmang.tsv.gz')
farmang <- read.csv('ef_m_farmang.tsv.gz', header = T) # Tähelepanu! Loob 4.2GB tabeli.

# Salvesta töölaud ----------
save.image('kaardid.Rda')