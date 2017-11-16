# Määra töökaust
setwd('/home/jrl/work/noortemeede')

# Laadi paketid
library('ggplot2')

# Laadi objektid
load('/home/jrl/data/objects/funs.Rda')


# Sisesta andmed ----------

majAr <- readRDS('majandusaasta_aruanded.Rds')


# Ettevõtte asutamisaeg ----------

plotAsutamine <- ggplot(majAr[majAr$aasta == majAr$viimane, ]) + aes(x = asutamine) + 
  geom_freqpoly() + 
  geom_vline(aes(xintercept = as.Date('2015-08-17')), color = 'red') + 
  geom_vline(aes(xintercept = as.Date('2008-10-06')), color = 'red') + 
  scale_x_date(name = "Ettevõtte asutamise aasta", date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(name = "Arv") + 
  facet_grid(ifelse(osalenu, 
                    paste0("Noortalunikud (n=", sum(osalenu), ")"),
                    paste0("Teised (n=", sum(!osalenu), ")")) ~ ., 
             scales = 'free') + 
  theme(text = element_text(family = 'Roboto Condensed', size = 12), 
        axis.text.x = element_text(size = 10, angle = 45), 
        axis.text.y = element_text(size = 10), 
        axis.ticks = element_blank(), 
        legend.background = element_rect(fill = NA, color = NA, size = .1),
        legend.key = element_blank(), 
        panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        panel.spacing = unit(30, "pt"), 
        plot.background = element_rect(fill = 'white'),
        plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        strip.background = element_blank(), 
        strip.text = element_text(size = 12))


# Majandusliku suuruse võrdlus ----------

## Loo tabel iga koodi aastate keskmistega
vrMaj <- aggregate(majAr[, which(names(majAr) == 'müügitulu'):ncol(majAr)], 
                   by = list(kood = majAr$kood, 
                             osalenu = majAr$osalenu, 
                             tegevusala = majAr$tegevusala), 
                   mean, na.rm = T)

## Loo funktsioon joonistamiseks ja joonista
võrdleMaj <- function(x) {
  
  # Määra y skaala puktid nii, et need hõlmaks vaid keskmist 80%
  skaala <- pretty(c(quantile(vrMaj[, x], probs = .1, na.rm = T), 
                     quantile(vrMaj[, x], probs = .9, na.rm = T)))
  
  # Salvesta valimite suurus
  arvNoortalunikud <- length(vrMaj$kood[vrMaj$osalenu & !is.na(vrMaj[, x])])
  arvTeised <- length(vrMaj$kood[!(vrMaj$osalenu) & !is.na(vrMaj[, x])])

  # Joonista
  ggplot(vrMaj) + aes_string(x = 'tegevusala', y = x, color = 'tegevusala') +
    geom_jitter(width = .4, alpha = .2) + 
    geom_boxplot(width = .4, alpha = 0, outlier.colour = NA) + 
    coord_cartesian(ylim = c(min(skaala), max(skaala))) + 
    labs(caption = "Allikas: Äriregister") + 
    scale_color_brewer(name = "Tegevusala", palette = 'Set1') + 
    scale_x_discrete(labels = NULL, name = NULL) + 
    scale_y_continuous(breaks = skaala, 
                       labels = format(skaala, big.mark = " ", scientific = F), 
                       name = Proper(sub("\\.", " ", x))) + 
    facet_grid(. ~ ifelse(osalenu, 
                      paste0("Noortalunikud (n=", arvNoortalunikud, ")"),
                      paste0("Teised (n=", arvTeised, ")"))) + 
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
}
plotMaj <- lapply(names(vrMaj)[which(names(vrMaj) == 'müügitulu'):ncol(vrMaj)], võrdleMaj)
names(plotMaj) <- names(vrMaj)[which(names(vrMaj) == 'müügitulu'):ncol(vrMaj)]


# Tegevusalade võrdlus ----------

## Jäta iga koodi viimane aasta
vrTeg <- majAr[order(majAr$aasta, decreasing = T), ]
vrTeg <- vrTeg[!duplicated(vrTeg$kood), ]

vrTeg <- as.data.frame.matrix(
  prop.table(table(
    vrTeg$tegevusala, ifelse(vrTeg$osalenu, 'osalenud', 'teised')), 2))
vrTeg$tegevusala <- rownames(vrTeg)

## Määra skaala
skaala <- seq(0, round(max(vrTeg[,1:2]), digits = 1), .05)

## Jäta alles tegevusalade nimetused vaid siis, kui osakaaludes on erinevus
vrTeg$tegevusala <- ifelse(abs(vrTeg$teised - vrTeg$osalenud) > .02, vrTeg$tegevusala, NA)

## Joonista
plotTeg <- ggplot(vrTeg) + aes(x = teised, y = osalenud) + 
  geom_abline(intercept = 0, slope = 1, color = 'red') + 
  geom_point(size = 2) + 
  geom_text(aes(label = tegevusala), hjust = .02, vjust = -.4, 
            family = 'Roboto Condensed', alpha = .6) + 
  expand_limits(x = 0, y = 0) + 
  labs(caption = "Allikas: Äriregister") + 
  scale_x_continuous(breaks = skaala, 
                     labels = Perc(skaala), 
                     name = paste0("Teised (n=", 
                                   length(unique(majAr$kood[!(majAr$osalenu)])), ")")) + 
  scale_y_continuous(breaks = skaala, 
                     labels = Perc(skaala), 
                     name = paste0("Noortalunikud (n=", 
                                   length(unique(majAr$kood[majAr$osalenu])), ")")) + 
  theme(text = element_text(family = 'Roboto Condensed', size = 12), 
        axis.text = element_text(size = 10), 
        axis.ticks = element_blank(), 
        legend.background = element_rect(fill = NA, color = NA, size = .1),
        legend.key = element_blank(), 
        panel.background = element_rect(fill = NA), 
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        panel.grid.major = element_line(color = 'gray80'), 
        panel.spacing = unit(30, "pt"), 
        plot.background = element_rect(fill = 'white'),
        plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        strip.background = element_blank(), 
        strip.text = element_text(size = 12))


# Salvestamine ----------

save(plotAsutamine, plotMaj, plotTeg, file = 'võrdlus.Rda')
