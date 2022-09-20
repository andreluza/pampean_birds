# pacotes necessarios
require (reshape)
require(unmarked)

# funcoes
source ("functions.R")

# ---------------------------------------------------#
# organizacao dos dados para os modelos de ocupacao

# carregar dados das covariaveis de deteccao
det_cov <- read.csv (here("data","dados_cov_detec.csv"),h=T,sep=",")

# transformar data
trans_data <- do.call (rbind,
                       strsplit (det_cov$data , "/"))

# inverter ordem de ano e data
trans_data <- lapply (seq (1,nrow (trans_data)), function (i)
  
    paste (trans_data[i,3],trans_data[i,2], trans_data[i,1],sep="-"))

# desfazer a lista, transformar em data (usando as.Date),e colando no DF
det_cov$transDate <- as.Date (unlist(trans_data))

# para voce mudar as ocasioes amostrais, isso vai funcionar

det_cov$HorarioAlternativo <- sapply(det_cov$horario, function(x) {
  if (x <= 12) {"manha"} 
  else if (x>12) {"tarde"} ## voce pode add mais else se quiser
  }
)

# transformar temperatura em numero
det_cov$temp <- as.numeric(det_cov$temp)

# transformar umidade em numero
det_cov$umid <- as.numeric(det_cov$umid)

# ------------------------------------------------
# criar eventos amostrais (sampling occasions)
# ------------------------------------------------

# obter os sitios (argumento da funcao abaixo)
sitios_amostragem <- unique(det_cov$sitio)[order(unique(det_cov$sitio))]

# rodar funcao para obter o dia e numero de ocasioes amostrais por sitio
# a funcao leva os sitios unicos de amostragem (sites) e os dados de deteccao (dados)
det_cov_occ <- funcao_ocasioes (sites = sitios_amostragem,
                                dados = det_cov)

# veja que tem o mesmo nrow do que a tabela original, mas 3 colunas a mais (ocasioes e data)
dim(det_cov_occ); dim (det_cov)

# -----------------------------------------------
# criar uma tabela de umidade e temperature no registro
# -----------------------------------------------

# umidade 
det_table_umidade <- data.matrix(cast (data=det_cov_occ, formula=sitio~combDiaTurno, 
      value = "umid",fun.aggregate = "mean",na.rm=T))[,-1]# rm a coluna com os sitios (ver "sitios_amostragem")

# corrigir a ordem dos eventos amostrais
det_table_umidade <- det_table_umidade [,match (unique(det_cov_occ$combDiaTurno),colnames(det_table_umidade))]

# padronizar
det_table_umidade_std <- (det_table_umidade-mean(det_table_umidade,na.rm=T))/sd(det_table_umidade,na.rm=T)

# temperatura
det_table_temperatura <- data.matrix(cast (data=det_cov_occ, formula=sitio~combDiaTurno, 
                                       value = "temp",fun.aggregate = "mean",na.rm=T)) [,-1]# rm a coluna com os sitios (ver "sitios_amostragem")

# corrigir a ordem dos eventos amostrais
det_table_temperatura <- det_table_temperatura [,match (unique(det_cov_occ$combDiaTurno),colnames(det_table_temperatura))]

# padronizar
det_table_temperatura_std <- (det_table_temperatura-mean(det_table_temperatura,na.rm=T))/sd(det_table_temperatura,na.rm=T)

# ----------------------------------------------------- #
# carregar dados de deteccao das aves

# list with detection data
list_files <- list.files(here("data"),pattern="dados_detec_*")

# for each file, organize data
list_det_data <- lapply (list_files, function (i) {
  
      # load
      det_sp <- read.csv (i,h=T,sep=",")
      
      # transform day
      trans_data <- do.call (rbind,
                             strsplit (det_sp$data , "/"))
      
      # invert the order of day 
      trans_data <- lapply (seq (1,nrow (trans_data)), function (i)
        
        paste (trans_data[i,3],trans_data[i,2], trans_data[i,1],sep="-"))
      
      # melt the list, bind into the df
      det_sp$transDate <- as.Date (unlist(trans_data))
      
      # create the period of day 
      
      det_sp$HorarioAlternativo <- sapply(det_sp$horario, function(x) {
        if (x <= 12) {"manha"} 
        else if (x>12) {"tarde"} ## voce pode add mais else se quiser
      }
      )
      
      # ------------------------------------------------
      # create (sampling occasions)
      # ------------------------------------------------
      
      # sites
      sitios_amostragem <- unique(det_sp$sitio)[order(unique(det_sp$sitio))]
      
      # rodar funcao para obter o dia e numero de ocasioes amostrais por sitio
      # a funcao leva os sitios unicos de amostragem (sites) e os dados de deteccao (dados)
      det_sp_occ <- funcao_ocasioes (sites = sitios_amostragem,
                                      dados = det_sp)
      
      # veja que tem o mesmo nrow do que a tabela original, mas 3 colunas a mais (ocasioes e data)
      dim(det_sp); dim (det_sp_occ)
      
      # tabela de eventos amostrais 
      tab_det_sp_occ <- data.matrix(cast (data=det_sp_occ, formula=sitio~combDiaTurno, 
                                             value = "det",fun.aggregate = "max",na.rm=T))[,-1]# rm a coluna com os sitios (ver "sitios_amostragem")
      
      # corrigir a ordem dos eventos amostrais
      tab_det_sp_occ <- tab_det_sp_occ [,match (unique(det_sp_occ$combDiaTurno),
                                                        colnames(tab_det_sp_occ))]
      tab_det_sp_occ [is.infinite (tab_det_sp_occ)] <- NA
      ; # return
      tab_det_sp_occ
      
  }

)

# -----------------------------------------------------
# site covariates
# -----------------------------------------------------

det_table_veg <- read.csv (here("data", "dados_cov_sitio2.csv"),sep=";")
# are site names properly organized?
det_table_veg$sitio == toupper(sitios_amostragem)

# correcting the order of sites
det_table_veg <- det_table_veg [match (toupper (sitios_amostragem),
                                       det_table_veg$sitio),]
# cf
det_table_veg$sitio == toupper(sitios_amostragem)

# sound detection area, standardized
std_det_area <- sqrt(det_table_veg$area)
std_det_area <- (std_det_area-mean(std_det_area))/sd(std_det_area)

# landscape characteristics (at 500 m buffer)
# agriculture
agriculture <- sqrt(det_table_veg$agriculturas )
std_agri <- (agriculture-mean(agriculture))/sd(agriculture)
# grassland
grassland <- sqrt(det_table_veg$campos)
std_grass <- (grassland-mean(grassland))/sd(grassland)
# tree
tree <- sqrt(det_table_veg$arborea)
std_tree <- (tree-mean(tree))/sd(tree)

# correlation between covariates
cor.test(std_agri,std_grass)
cor.test(std_agri,std_tree)
cor.test(std_grass,std_tree) # strong 

cor(std_grass, std_det_area)


save.image(here("output", "organized_data.RData"))
