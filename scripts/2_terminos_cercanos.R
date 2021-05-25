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

# 2. Casos puntuales: adjetivos cercanos
freqs = function(oraciones, que, quien) {
  basura = c('','"','“', '”', '‘', '’', '-', ']', '[', '-', '–')
  
  frecuencias = as.data.table(
    table(
      unlist(
        str_split(
          oraciones[ministros == quien, get(que)], ','))))[!(V1 %in% basura)][order(-N)]
  
  return(setnames(frecuencias, 'V1', 'termino'))
}

ggterminos = function(terminos, nombre) {
  
  p = ggbarplot(terminos[1:15],
                x = 'termino',
                y = 'N',
                fill = 'tipo',
                width = 0.5,
                palette = paleta,
                sort.val = 'asc',
                sort.by.groups = F,
                rotate = T,
                title = paste0("Términos cercanos a ", nombre),
                ggtheme = theme_pander(nomargin = F),
                xlab = "",
                ylab = "Cantidad de menciones")
  return(p)
}

## 2.1. Martín Soria
ministro = 'Martín Soria'
adjetivos = freqs(oraciones, 'adjetivos', ministro)[,tipo := 'adjetivo']
sustantivos = freqs(oraciones, 'sustantivos', ministro)[,tipo := 'sustantivo']

terminos = rbind(adjetivos, sustantivos)[order(-N)]

ggterminos(terminos, ministro)
# ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/2_1_soria.jpeg', limitsize = F)

## 2.2. Ginés
ministro = 'Ginés González García'
adjetivos = freqs(oraciones, 'adjetivos', ministro)[,tipo := 'adjetivo']
sustantivos = freqs(oraciones, 'sustantivos', ministro)[,tipo := 'sustantivo']

terminos = rbind(adjetivos, sustantivos)[order(-N)]

ggterminos(terminos, 'Ginés G. García')
# ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/2_2_gines.jpeg', limitsize = F)

## 2.3. Trotta
ministro = 'Nicolás Trotta'
adjetivos = freqs(oraciones, 'adjetivos', ministro)[,tipo := 'adjetivo']
sustantivos = freqs(oraciones, 'sustantivos', ministro)[,tipo := 'sustantivo']

terminos = rbind(adjetivos, sustantivos)[order(-N)]

ggterminos(terminos, ministro)
# ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/2_3_trotta.jpeg', limitsize = F)

## 2.4. Cabandié
ministro = 'Juan Cabandié'
adjetivos = freqs(oraciones, 'adjetivos', ministro)[,tipo := 'adjetivo']
sustantivos = freqs(oraciones, 'sustantivos', ministro)[,tipo := 'sustantivo']

terminos = rbind(adjetivos, sustantivos)[order(-N)]

ggterminos(terminos, ministro)
# ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/2_4_cabandie.jpeg', limitsize = F)

## 2.5. Gómez Alcorta
ministro = 'Gómez Alcorta'
adjetivos = freqs(oraciones, 'adjetivos', ministro)[,tipo := 'adjetivo']
sustantivos = freqs(oraciones, 'sustantivos', ministro)[,tipo := 'sustantivo']

terminos = rbind(adjetivos, sustantivos)[order(-N)]

ggterminos(terminos, 'Elizabeth G. Alcorta')
# ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/2_5_gomezalcorta.jpeg', limitsize = F)

## 2.6. Frederic
ministro = 'Sabina Frederic'
adjetivos = freqs(oraciones, 'adjetivos', ministro)[,tipo := 'adjetivo']
sustantivos = freqs(oraciones, 'sustantivos', ministro)[,tipo := 'sustantivo']

terminos = rbind(adjetivos, sustantivos)[order(-N)]

ggterminos(terminos, ministro)
# ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/2_6_frederic.jpeg', limitsize = F)