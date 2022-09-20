# funcao para encontrar as ocasioes amostrais, por sitio
# retorna uma lista de sitios, com o dia e evento de record anexados a tabela

funcao_ocasioes <- function (sites, dados) {
  
  tab_per_site <- lapply (sites, function (i) {
    # pegar cada sitio   
    subset_sitio <-  dados[which(dados$sitio == i),]
    # extrair as datas de amostragem
    datas_amostragem <- unique(subset_sitio$transDate)[order (unique(subset_sitio$transDate))]
    # criar uma seq para estas datas (que sera o dia de amostragem)
    dia_de_amostragem <- seq_along(datas_amostragem)
    # tabela com data e dia 
    data_dia <- data.frame (data=datas_amostragem,
                            dia = dia_de_amostragem)
    
    # subset sitio e data para contagem das ocasioes amostrais(apply para cada data da tabela)
    subset_sitio_data <- lapply (seq_along (data_dia$data), function (k) {
      
      # subset de sitio e data
      subset_sitio_data <- subset_sitio [which(subset_sitio$transDate == data_dia$data[k]),]
      # colar dados de dia e numero de ocasioes
      subset_sitio_data <- cbind (subset_sitio_data, 
                                  dia = data_dia$dia[k], # saber qual foi o dia de amostagem
                                  record = seq_along ((subset_sitio_data$transDate))) # numero de ocasioes
      
      # colar  a combinacao de dia e hora
      subset_sitio_data$combDiaHora <- (paste(subset_sitio_data$dia,
                                             subset_sitio_data$record,
                                             sep="-"))
      # colar a combinacao de dia e turno
      subset_sitio_data$combDiaTurno <- (paste(subset_sitio_data$dia,
                                              subset_sitio_data$HorarioAlternativo,
                                              sep="-"))
      
    
      ; # return
      subset_sitio_data
      
    }) # fechar funcao das datas
    
    # desmanchar a lista de "subset_sitio_data"
    subset_sitio_data <- do.call(rbind, subset_sitio_data)
    
  }
  )# fechar funcao dos sitios
  
  # colocar  sitios como nomes dos elementos da lista
  names(tab_per_site) <- sites
  # desmanchar a lista
  tab_per_site <- do.call(rbind, tab_per_site)
  
  # retornar
  return (tab_per_site)
  
}


# funcao closest

closest<- function (x, your.number) {
  
  which(abs(x - your.number) == min(abs(x - your.number)))
  
  }
