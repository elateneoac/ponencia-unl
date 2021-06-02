library(data.table)
library(jsonlite)
library(stringr)
library(spacyr)
spacy_initialize('es_core_news_sm')

notis = stream_in(file('~/Documentos/ponencia-ateneo/noticias_ministros.json'))
notis = as.data.table(notis)

textos = notis[,texto]

regex_ministros = 'Santiago Cafiero|Luis Basterra|Juan Cabandié|Roberto Salvarezza|Tristán Bauer|Agustín Rossi|Matías Kulfas|Daniel Arroyo|Jorge Ferraresi|María Eugenia Bielsa|Martín Guzmán|Nicolás Trotta|De Pedro|Martín Soria|Marcela Losardo|Gómez Alcorta|Gabriel Katopodis|Felipe Solá|Carla Vizzotti|Ginés González García|Sabina Frederic|Claudio Moroni|Mario Meoni|Matías Lammens'
doraciones = data.table(oracion = character(),
                        sustantivos = character(),
                        adjetivos = character(),
                        verbos = character(),
                        entidades = character(),
                        ministros = character(),
                        diario = character(),
                        seccion = character())
i = 1
empieza = Sys.time()

for(t in textos) {
  oraciones = spacy_tokenize(t, 'sentence', output = c('data.frame'))['token']
  for (oracion in unlist(oraciones)) {
      matcheos = str_extract_all(oracion, regex_ministros)
    if (length(unlist(matcheos))) {
      info = as.data.table(spacy_parse(oracion))
      
      sustantivos = info[pos == 'NOUN', lemma]
      adjetivos = info[pos == 'ADJ', lemma]
      verbos = info[pos == 'VERB', lemma]
      # entidades = info[pos == 'PROPN', lemma] aca sacar las entidades usando spacy_extract_entity
      entidades = spacy_extract_entity(oracion)$text # AHI VAA
      entidades = gsub('[[:punct:]]','',entidades)
      ministros = unlist(matcheos)
      entidades = entidades[!str_detect(ministros, entidades)]
      
      fila = list(oracion,
                  str_c(sustantivos, collapse = ','),
                  str_c(adjetivos, collapse = ','),
                  str_c(verbos, collapse = ','),
                  str_c(entidades, collapse = ','),
                  str_c(ministros, collapse = ','),
                  notis[i,diario],
                  notis[i,cat])
      doraciones = rbind(doraciones, fila)
    }
  }
  cat('texto ', i, '\n')
  i = i + 1
}

termina = Sys.time()
cat('tiempo ejecución: ', difftime(termina, empieza))
fwrite(doraciones, 'oraciones.csv')
# al parecer el doraciones se arma bien. encarar la corrida para todas las noticias y ver de que quede exportado bien listo para usar.