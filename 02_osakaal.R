# Määra töökaust
setwd('/home/jrl/work/noortemeede')

# Laadi paketid
library('ggplot2');library('extrafont');library('raster');library('mapproj')

# Laadi objektid
load('/home/jrl/data/objects/funs.Rda')


# Sisesta ja kohanda andmed ----------

## Loe tabel (ef_m_farmang)
vanus <- read.csv('/home/jrl/work/noortemeede/ef_m_farmang_1_Data.csv', stringsAsFactors = F)

## Kohanda tabelit
names(vanus) <- tolower(names(vanus))
vanus$value <- as.integer(gsub('\\ ', '', vanus$value))
vanus$geo[vanus$geo == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
vanus <- vanus[, c('geo', 'time', 'age', 'value')]

## Tõsta andmed laia formaati
vanusEst <- vanus[vanus$geo == "Estonia" & vanus$age != 'Total' & vanus$age != 'Not applicable', 
                  c('time', 'age', 'value')] # Jäta alles ka pikk tabel Eesti kohta
vanus <- reshape(vanus, idvar = c('geo', 'time'), timevar = "age", direction = "wide")

## Arvuta alla 35-aastate osakaal
vanus$noored <- (vanus$'value.Less than 25 years' + vanus$'value.From 25 to 34 years') / 
  vanus$value.Total
## Arvuta muutus
vanus$muutus <- vanus$noored[vanus$time == max(vanus$time)] - 
  vanus$noored[vanus$time == min(vanus$time)]

kaartTabel <- readRDS('/home/jrl/data/objects/maps/world.Rds')
kaartTabel$noored <- cut(vanus$noored[match(kaartTabel$SOVEREIGNT, vanus$geo)],
                         breaks = seq(0, .15, .03),
                         labels = c('0% - 3%', '... - 6%', '... - 9%', '... - 12%', '... - 15%'))
kaartTabel$muutus <- cut(vanus$muutus[match(kaartTabel$SOVEREIGNT, vanus$geo)],
                         breaks = seq(-.06, .06, .03),
                         labels = c('-6% - -3%', '... - 0%', '... - +3%', '... - +6%'))


# Joonista kaardid ----------

## Põhi
baseplot <- 
  ggplot(kaartTabel) + aes(long, lat, group = group) + 
  coord_map(53, 9, projection = 'lambert', 
            xlim = c(-10, 35), ylim = c(34, 66)) + 
  theme(text = element_text(family = 'Roboto Condensed', size = 10), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = 'white', color = 'gray60', size = .1),
        legend.margin = margin(4,4,4,4, 'pt'), 
        legend.key = element_rect(color = 'gray60', size = .1), 
        legend.key.size = unit(10, 'pt'), 
        legend.text = element_text(size = 8), 
        legend.title = element_text(),
        legend.position = c(.01, .99),
        legend.justification = c(0, 1),
        panel.background = element_rect(fill = '#e6f2ff'),
        panel.border = element_rect(colour = 'gray60', fill = NA, size = .1),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(size = 14), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(2,2,2,2, 'pt'))

## Noorte osakaal
plotNoored <- baseplot + 
  geom_polygon(aes(fill = noored)) + 
  geom_path(color = 'grey40', size = .1) + 
  scale_fill_brewer(name = "Alla 35-aastaste \nosakaal 2013", 
                    palette = 'RdYlGn', na.value = 'grey80') +
  labs(#title = "Alla 35-aastaste põllumajandusettevõtete juhtide osakaal", 
       caption = "Allikas: Eurostat")

## Noorte osakaalu muutus
plotMuutus <- baseplot + 
  geom_polygon(aes(fill = muutus)) + 
  geom_path(color = 'grey40', size = .1) + 
  scale_fill_brewer(name = "Alla 35-aastaste \nosakaalu \nmuutus \n2005-2013", 
                    palette = 'RdYlGn', na.value = 'grey80') +
  labs(#title = "Alla 35-aastaste põllumajandusettevõtete juhtide osakaalu muutus", 
       caption = "Allikas: Eurostat")
  
# Muutus Eestis ----------

## Kohanda vanuse kategooriaid
vanusEst$age <- factor(rev(vanusEst$age), 
                       levels = unique(vanusEst$age), 
                       labels = rev(c('< 25', '25 - 34', '35 - 44', '45 - 54', '55 - 64', '> 64')))

## Joonista
plotVanused <- 
ggplot(vanusEst) + aes(x = time, y = value, fill = age) + 
  geom_area(position = 'stack', alpha = .8) + 
  labs(#title = "Eesti põllumajandusettevõtete juhtide vanuseline jaotus", 
       caption = "Allikas: Eurostat") + 
  scale_fill_brewer(palette = 'RdYlGn', name = "Vanuse-\ngrupp") + 
  scale_x_continuous(breaks = unique(vanusEst$time), name = NULL) + 
  scale_y_continuous(breaks = seq(0, 2.5e4, 5e3), 
                     labels = Numb(seq(0, 2.5e4, 5e3)), 
                     name = "Arv") + 
  theme(text = element_text(family = 'Roboto Condensed', size = 12), 
        axis.text = element_text(size = 10), 
        axis.ticks = element_blank(), 
        legend.background = element_rect(fill = NA, color = NA, size = .1),
        legend.key = element_blank(), 
        panel.background = element_rect(fill = NA), 
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        panel.grid.major.y = element_line(color = 'gray80'),
        panel.spacing = unit(30, "pt"), 
        plot.background = element_rect(fill = 'white'),
        plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        strip.background = element_blank(), 
        strip.text = element_text(size = 12))


# Salvesta ----------

save(plotNoored, plotMuutus, plotVanused, file = 'osakaal.Rda')
