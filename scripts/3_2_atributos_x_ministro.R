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

## 3.2. Para cada uno de lxs ministrxs
oraciones_por_ministro = oraciones[str_detect(ministros, ',', negate = T)]

oraciones_por_ministro = oraciones_por_ministro[dsustantiva == 'etica', dsustantiva := 'Ética']
oraciones_por_ministro = oraciones_por_ministro[dsustantiva == 'gestion', dsustantiva := 'Gestión']
oraciones_por_ministro = oraciones_por_ministro[dsustantiva == 'personalidad', dsustantiva := 'Personalidad']
oraciones_por_ministro = oraciones_por_ministro[dsustantiva == 'ideologia', dsustantiva := 'Ideología']
oraciones_por_ministro = oraciones_por_ministro[dsustantiva == 'otra', dsustantiva := 'Otras']

oraciones_por_ministro = oraciones_por_ministro[dvalorativa == 'positiva', dvalorativa := 'Positiva']
oraciones_por_ministro = oraciones_por_ministro[dvalorativa == 'negativa', dvalorativa := 'Negativa']
oraciones_por_ministro = oraciones_por_ministro[dvalorativa == 'neutra', dvalorativa := 'Neutral']

dsustantiva_por_ministro = oraciones_por_ministro[,
                                                  .('sustantiva' = .N,
                                                    dsustantiva),
                                                  by = .(ministros, dsustantiva)][
                                                    , .('porcentaje' = sustantiva * 100/ sum(sustantiva),
                                                        'atributo' = dsustantiva),
                                                    by = ministros
                                                    ]
dsustantiva_por_ministro = setnames(dsustantiva_por_ministro, 'ministros', 'ministro')

dvalorativa_por_ministro = oraciones_por_ministro[,
                                                  .('valorativa' = .N,
                                                    dvalorativa),
                                                  by = .(ministros, dvalorativa)][
                                                    , .('porcentaje' = valorativa * 100/ sum(valorativa),
                                                        'atributo' = dvalorativa),
                                                    by = ministros
                                                    ]
dvalorativa_por_ministro = setnames(dvalorativa_por_ministro, 'ministros', 'ministro')

ggplot(dsustantiva_por_ministro, aes(x = reorder(ministro,porcentaje), y = porcentaje)) +
  geom_bar(stat = 'identity', width = 0.5, fill = '#1B9E77', colour = 'black') + 
  facet_grid(~atributo) + 
  theme_pander(nomargin = F) +
  scale_y_continuous(n.breaks = 3) + 
  labs(title = "Asignación de atributos sustantivos por ministro", x = "", y = "%") +
  coord_flip()
# ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/3_2_sustantiva_por_ministro.jpeg')

ggplot(dvalorativa_por_ministro, aes(x = reorder(ministro,porcentaje), y = porcentaje)) +
  geom_bar(stat = 'identity', width = 0.5, fill = '#1B9E77', colour = 'black') + 
  facet_grid(~atributo) + 
  theme_pander(nomargin = F) +
  scale_y_continuous(n.breaks = 3) + 
  labs(title = "Asignación de atributos valorativos por ministro", x = "", y = "%") +
  coord_flip()
# ggsave(filename = '~/Documentos/ponencia-ateneo/dibujos/3_2_valorativa_por_ministro.jpeg')