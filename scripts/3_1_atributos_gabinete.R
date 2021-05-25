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

oraciones = fread(paste0(path_data,'/oraciones_infobae_clarin_lanacion_clasificadas.csv'))

## 3.1. Gabinete
atributos_sustantivos = as.data.table(table(oraciones[,dsustantiva]))[, tipo := 'sustantivo']
atributos_valorativos = as.data.table(table(oraciones[,dvalorativa]))[, tipo := 'valorativo']

atributos_sustantivos[V1 == 'etica', V1 := 'Ética']
atributos_sustantivos[V1 == 'gestion', V1 := 'Gestión']
atributos_sustantivos[V1 == 'ideologia', V1 := 'Ideología']
atributos_sustantivos[V1 == 'personalidad', V1 := 'Personalidad']
atributos_sustantivos[V1 == 'otra', V1 := 'Otras']
atributos_sustantivos[, porcentaje := N * 100 / sum(N)]

atributos_valorativos[V1 == 'positiva', V1 := 'Positiva']
atributos_valorativos[V1 == 'negativa', V1 := 'Negativa']
atributos_valorativos[V1 == 'neutra', V1 := 'Neutra']
atributos_valorativos[, porcentaje := N * 100 / sum(N)]

atributos = rbind(atributos_valorativos, atributos_sustantivos)

ggbarplot(atributos_sustantivos,
          x = 'V1',
          y = 'porcentaje',
          fill = 'V1',
          sort.val = 'desc',
          sort.by.groups = F,
          ggtheme = theme_pander(nomargin = F, lp = 'none'),
          palette = paleta,
          title = 'Atributos sustantivos del Gabinete',
          xlab = "",
          ylab = "Porcentaje",
          rotate = F)
# ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/3_1_gabinete_sustantiva.jpeg', limitsize = F)

ggbarplot(atributos_valorativos,
          x = 'V1',
          y = 'porcentaje',
          fill = 'V1',
          sort.val = 'desc',
          sort.by.groups = F,
          ggtheme = theme_pander(nomargin = F,lp = 'none'),
          palette = paleta,
          title = 'Atributos valorativos del Gabinete',
          xlab = "",
          ylab = "Porcentaje",
          rotate = F)
# ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/3_1_gabinete_valorativa.jpeg', limitsize = F)