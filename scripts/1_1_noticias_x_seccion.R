library(data.table)
library(jsonlite)
library(stringr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(ggExtra)
library(treemap)

# 1. General
paleta = 'Dark2'

path_data = '' # path a la carpeta 'ponencia-unl/data'

notis = fread(paste0(path_data,'/notis_filtradas.csv'))

## 1.1. noticias x diario x sección
notis_1_1 = notis[, .(n = .N), keyby = .(diario,cat)]

notis_1_1[diario == 'clarin', diario := 'Clarín']
notis_1_1[diario == 'lanacion', diario := 'La Nación']
notis_1_1[diario == 'infobae', diario := 'Infobae']
notis_1_1[cat == 'politica', cat := 'Política']
notis_1_1[cat == 'economia', cat := 'Economía']
notis_1_1[cat == 'sociedad', cat := 'Sociedad']
notis_1_1[cat == 'internacional', cat := 'Internacional']
notis_1_1[cat == 'espectaculos', cat := 'Espectáculos']
notis_1_1[cat == 'deportes', cat := 'Deportes']
notis_1_1[cat == 'cultura', cat := 'Cultura']

ggplot(notis_1_1, aes(x = reorder(cat,n), y = n), colour = 'red') +
  geom_bar(stat = 'identity', width = 0.5, fill = '#1B9E77', colour = 'black') + 
  facet_grid(~diario) + 
  theme_pander(nomargin = F) +
  scale_y_continuous(n.breaks = 5) + 
  labs(title = "¿Secciones con más menciones a ministrxs?", x = "", y = "Cantidad de noticias") +
  coord_flip()

#ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/1_1_noticias_diario_seccion.jpeg')