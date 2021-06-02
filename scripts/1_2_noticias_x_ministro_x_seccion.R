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

path_data = '~/repos/ponencia-unl/data' # path a la carpeta 'ponencia-unl/data'

notis = fread(paste0(path_data,'/notis_filtradas.csv'))

## 1.2  noticias x ministrx
lministros = unique(unlist(str_split(unlist(notis[, ministros]), pattern = ',')))
ldiarios = unique(notis[,diario])
lcategorias = unique(notis[cat != 'cultura',cat])

conteo_ministros = data.table()
for(m in lministros) {
  for(d in ldiarios) {
    for(c in lcategorias){
      aux = notis[diario == d & cat == c, .(ministro = m,
                                            diario = d,
                                            categoria = c,
                                            n = sum(str_count(ministros, m)))]
      conteo_ministros = rbind(conteo_ministros, aux)
    }
    aux = notis[diario == d,
                .(ministro = m,
                  diario = d,
                  categoria = 'Total',
                  n = sum(str_count(ministros, m)))]
    conteo_ministros = rbind(conteo_ministros, aux)
  }
}

conteo_ministros[ministro == 'kulfas', ministro := 'Kulfas']
conteo_ministros[ministro == 'trotta', ministro := 'Trotta']
conteo_ministros[ministro == 'soria', ministro := 'Soria']
conteo_ministros[ministro == 'sola', ministro := 'Solá']
conteo_ministros[ministro == 'salvarezza', ministro := 'Salvarezza']
conteo_ministros[ministro == 'rossi', ministro := 'Rossi']
conteo_ministros[ministro == 'moroni', ministro := 'Moroni']
conteo_ministros[ministro == 'meoni', ministro := 'Meoni']
conteo_ministros[ministro == 'losardo', ministro := 'Losardo']
conteo_ministros[ministro == 'lammens', ministro := 'Lammens']
conteo_ministros[ministro == 'katopodis', ministro := 'Katopodis']
conteo_ministros[ministro == 'guzman', ministro := 'Guzmán']
conteo_ministros[ministro == 'gomezalcorta', ministro := 'G. Alcorta']
conteo_ministros[ministro == 'gines', ministro := 'Ginés G.G.']
conteo_ministros[ministro == 'frederic', ministro := 'Frederic']
conteo_ministros[ministro == 'ferraresi', ministro := 'Ferraresi']
conteo_ministros[ministro == 'depedro', ministro := 'De Pedro']
conteo_ministros[ministro == 'cafiero', ministro := 'Cafiero']
conteo_ministros[ministro == 'cabandie', ministro := 'Cabandié']
conteo_ministros[ministro == 'bielsa', ministro := 'M. E. Bielsa']
conteo_ministros[ministro == 'bauer', ministro := 'Bauer']
conteo_ministros[ministro == 'basterra', ministro := 'Basterra']
conteo_ministros[ministro == 'arroyo', ministro := 'Arroyo']
conteo_ministros[ministro == 'vizzotti', ministro := 'Vizzotti']
conteo_ministros[diario == 'clarin', diario := 'Clarín']
conteo_ministros[diario == 'lanacion', diario := 'La Nación']
conteo_ministros[diario == 'infobae', diario := 'Infobae']
conteo_ministros[categoria == 'politica', categoria := 'Política']
conteo_ministros[categoria == 'economia', categoria := 'Economía']
conteo_ministros[categoria == 'sociedad', categoria := 'Sociedad']
conteo_ministros[categoria == 'internacional', categoria := 'Internacional']
conteo_ministros[categoria == 'espectaculos', categoria := 'Espectáculos']
conteo_ministros[categoria == 'deportes', categoria := 'Deportes']
conteo_ministros[categoria == 'cultura', categoria := 'Cultura']

ggplot(conteo_ministros[categoria %in% c('Economía', 'Política', 'Sociedad', 'Total')], aes(x = ministro, y = n), colour = diario) +
  geom_linerange(aes(x = ministro, ymin = 0, ymax = n, colour = diario),
                 position = position_dodge(width = 0.6)) +
  geom_point(aes(colour = diario),
             position = position_dodge(width = 0.6), size=1, alpha=1) +
  coord_flip() +
  labs(title = "¿Ministrxs más mencionadxs? Septiembre 2020 ~ Marzo 2021", x = "", y = "Cantidad de noticias") + 
  theme_pander(nomargin = F) +
  scale_y_continuous(n.breaks = 3) + 
  scale_color_manual(values = c('red', 'orange', 'darkcyan')) + 
  facet_grid(~categoria)
ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/1_2_noticias_ministro_diario_seccion.jpeg')