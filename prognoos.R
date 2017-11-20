# Määra töökaust
setwd('/home/jrl/work/noortemeede')

# Laadi paketid
library('forecast');library('zoo')


# Sisesta andmed ----------

pms204 <- setDSD(readSDMX('http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetData/PMS204/1+2.1+2+3.1+2+3.1+2+3+4+5+6+7+8+9/all?startTime=2003&endTime=2016'),
                  readSDMX('http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetDataStructure/PMS204'))
pms204 <- as.data.frame(pms204, labels = T)
pms204 <- pms204[, c(names(pms204)[grep('label.et', names(pms204))], 'obsTime', 'obsValue')]


ars <- pms204[pms204[, 1] == "Hõivatute arv" & 
                pms204[, 2] == "Kokku" & 
                pms204[, 4] != "Vanuserühmad kokku" & 
                pms204[, 4] != "35-39" & pms204[, 4] != "40-44", 
              3:6]

ars <- reshape(ars, timevar = 'obsTime', 
               idvar = c('DIM4_label.et', 'DIM5_label.et'), direction = 'wide')
nimed <- apply(ars[, 1:2], 1, paste, collapse = ' ')
nimed <- gsub(' ', '.', nimed)
ars <- data.frame(t(ars[, 3:8]))
names(ars) <- nimed
#ars <- ars[, !(grepl('kokku', names(ars)))]

ars <- as.ts(zoo(ars, c(2003, 2005, 2007, 2010, 2013, 2016))) # TS objektiks puuduvate aastate lisamisega
ars <- as.ts(zoo(na.spline(ars), 2003:2016)) # TS objektiks puuduvate aastate väärtuste tekitamisega

sta <- data.frame(rbind(ars, sapply(ars, function(x) forecast(x, h = 4)$mean)))
