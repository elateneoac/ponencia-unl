library(data.table)
library(stringr)

oraciones = fread('~/Documentos/ponencia-ateneo/oraciones.csv')[diario %in% c('lanacion', 'clarin', 'infobae'), ]

bolsa_etica = fread('~/Documentos/ponencia-ateneo/bolsa_etica.csv')[`bolsa?` == 'si', palabras]
regex_etica = str_c(bolsa_etica, collapse='|')

bolsa_gestion = fread('~/Documentos/ponencia-ateneo/bolsa_gestion.csv')[`bolsa?` == 'si', palabras]
regex_gestion = str_c(bolsa_gestion, collapse='|')

bolsa_ideologia = fread('~/Documentos/ponencia-ateneo/bolsa_ideologia.csv')[`bolsa?` == 'si', palabras]
regex_ideologia = str_c(bolsa_ideologia, collapse='|')

bolsa_personalidad = fread('~/Documentos/ponencia-ateneo/bolsa_personalidad.csv')[`bolsa?` == 'si', palabras]
regex_personalidad = str_c(bolsa_personalidad, collapse='|')

bolsa_positiva = fread('~/Documentos/ponencia-ateneo/bolsa_positiva.txt',header = F)[, V1]
regex_positiva = str_c(bolsa_positiva, collapse='|')

bolsa_negativa = fread('~/Documentos/ponencia-ateneo/bolsa_negativa.txt',header = F)[, V1]
regex_negativa = str_c(bolsa_negativa, collapse='|')

oraciones[, terminos := str_c(sustantivos, adjetivos, verbos, sep=',')]

oraciones[, etica := str_count(terminos, regex_etica)]
oraciones[, etica := etica / (str_count(terminos, '[^,],[^,]') + 1 + str_count(regex_etica, '\\|') + 1 - etica)]

oraciones[, gestion := str_count(terminos, regex_gestion)]
oraciones[, gestion := gestion / (str_count(terminos, '[^,],[^,]') + 1 + str_count(regex_gestion, '\\|') + 1 - gestion)]

oraciones[, ideologia := str_count(terminos, regex_ideologia)]
oraciones[, ideologia := ideologia / (str_count(terminos, '[^,],[^,]') + 1 + str_count(regex_ideologia, '\\|') + 1 - ideologia)]

oraciones[, personalidad := str_count(terminos, regex_personalidad)]
oraciones[, personalidad := personalidad / (str_count(terminos, '[^,],[^,]') + 1 + str_count(regex_personalidad, '\\|') + 1 - personalidad)]

oraciones[, positiva := str_count(terminos, regex_positiva)]
oraciones[, positiva := positiva / (str_count(terminos, '[^,],[^,]') + 1 + str_count(regex_positiva, '\\|') + 1 - positiva)]

oraciones[, negativa := str_count(terminos, regex_negativa)]
oraciones[, negativa := negativa / (str_count(terminos, '[^,],[^,]') + 1 + str_count(regex_negativa, '\\|') + 1 - negativa)]

# seteo las dimensiones de cada oración
oraciones[, dsustantiva := colnames(.SD)[max.col(.SD)], .SDcols = 10:13]
oraciones[, dvalorativa := colnames(.SD)[max.col(.SD)], .SDcols = 14:15]

# corrijo las oraciones que no son de 'otrtas' dimensiones

## calculo promedios de cada dimensión
promedios = oraciones[, .(etica = mean(etica),
                          gestion = mean(gestion),
                          ideologia = mean(ideologia),
                          personalidad = mean(personalidad),
                          negativa = mean(negativa),
                          positiva = mean(positiva))]

## seteo en 0 los valores que no superen al promedio
oraciones[etica < promedios$etica, etica := 0]
oraciones[gestion < promedios$gestion, gestion := 0]
oraciones[personalidad < promedios$personalidad, personalidad := 0]
oraciones[ideologia < promedios$ideologia, ideologia := 0]
oraciones[negativa < promedios$negativa, negativa := 0]
oraciones[positiva < promedios$positiva, positiva := 0]

## seteo en 'otra' la dimensión sustantiva que no sea de ninguna de las cuatro
oraciones[ideologia + gestion + etica + personalidad == 0, dsustantiva := 'otras']

## seteo en 'otra' la dimensión valorativa que no sea de ninguna de las cuatro
oraciones[positiva + negativa == 0, dvalorativa := 'neutra']

# guardo
fwrite(oraciones, '~/Documentos/ponencia-ateneo/oraciones_infobae_clarin_lanacion_clasificadas.csv')

# gráfico de torta de atributos de Ginés
pie(table(oraciones[ministros == 'Ginés González García', dsustantiva]), main = 'Sustantivos de Ginés')
pie(table(oraciones[ministros == 'Ginés González García', dvalorativa]), main = 'Valoraciones de Ginés')

soria = oraciones[ministros == 'Martín Soria'][, .(etica = sum(etica), gestion = sum(gestion), personalidad = sum(personalidad), ideologia = sum(ideologia), positiva = sum(positiva), negativa = sum(negativa)), keyby = ministros]
pie(unlist(soria[,.(etica, gestion, personalidad, ideologia)]), main = 'Atributos de Soria')
pie(unlist(soria[,.(positiva, negativa)]), main = 'Atributos de Soria')
