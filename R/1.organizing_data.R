# ---------------------------------------------------- # 
# 
#                      ORGANIZAR OS DADOS
    

#       DADOS DE DETECCAO E NAO DETECCAO POR TURNO DO DIA DE AMOSTRAGEM

# ---------------------------------------------------- # 


# funcoes
source ("R/functions.R")
source ("R/packages.R")

# ---------------------------------------------------#
# organizacao dos dados para os modelos de ocupacao

# carregar dados das covariaveis de deteccao
det_cov <- read.csv (here("data","dados_cov_detec_imputed.csv"),h=T,sep=";")

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
original_temp <- (det_cov$temp)
original_temp[is.na(det_cov$temp)] # checar NAs (foram inseridos pela Patricia)
det_cov$temp <- as.numeric(det_cov$temp)


# transformar umidade em numero
det_cov$umid <- as.numeric(det_cov$umid)

# ------------------------------------------------
# criar eventos amostrais (sampling occasions)
# ------------------------------------------------

# obter os sitios (argumento da funcao abaixo)
sitios_amostragem <- unique(det_cov$sitio)

# rodar funcao para obter o dia e numero de ocasioes amostrais por sitio
# a funcao leva os sitios unicos de amostragem (sites) e os dados de deteccao (dados)
# cola tb hora redonda e sua combinacao com dia
det_cov_occ <- funcao_ocasioes (sites = sitios_amostragem,
                                dados = det_cov)

# veja que tem o mesmo nrow do que a tabela original, mas 3 colunas a mais (ocasioes e data)
dim(det_cov_occ); dim (det_cov)


# calcular temperatura media por sitio, data e hora
det_cov_occ <- det_cov_occ %>%
  right_join(x = det_cov_occ, 
             y = det_cov_occ %>%
  group_by(sitio, transDate) %>%
  summarise (temp_med = mean(temp,na.rm=T),
             umid_med = mean(umid,na.rm=T)),
  
  by = c("sitio", "transDate"))
  


det_cov_occ[which(is.na(det_cov_occ$temp))[1],]



# -----------------------------------------------
# criar uma tabela de umidade e temperature no registro
# -----------------------------------------------

# umidade 
det_table_umidade <- tapply (det_cov_occ$umid,
                             list (det_cov_occ$sitio,
                                   det_cov_occ$combDiaTurno),
                             FUN = mean,na.rm=T)
  



# corrigir a ordem dos eventos amostrais
det_table_umidade <- det_table_umidade [,match (unique(det_cov_occ$combDiaTurno),
                                                colnames(det_table_umidade))]


# padronizar
det_table_umidade_std <- (det_table_umidade-mean(det_table_umidade,na.rm=T))/sd(det_table_umidade,na.rm=T)

# temperatura
det_table_temperatura <- tapply (det_cov_occ$temp,
                                 list (det_cov_occ$sitio,
                                       det_cov_occ$combDiaTurno),
                                 FUN = mean,na.rm=T)

# corrigir a ordem dos eventos amostrais
det_table_temperatura <- det_table_temperatura [,match (unique(det_cov_occ$combDiaTurno),
                                                        colnames(det_table_temperatura))]

# padronizar
det_table_temperatura_std <- (det_table_temperatura-mean(det_table_temperatura,na.rm=T))/sd(det_table_temperatura,na.rm=T)


# ----------------------------------------------------- #
# carregar dados de deteccao das aves

# list with detection data
list_files <- list.files(here("data"),pattern="dados_detec_*")

# species
list_sp <- c("Ammodramus humeralis","Emberezoides herbicola", "Leistes superciliaris","Zonotrichia capensis")

# load data and pack in a list
# for each file, organize data
list_det_data <- lapply (list_files, function (i) {
  
      # load
      det_sp <- read.csv (here ("Data",i),h=T,sep=",")
      
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
      sitios_amostragem <- unique(det_sp$sitio)
      
      # rodar funcao para obter o dia e numero de ocasioes amostrais por sitio
      # a funcao leva os sitios unicos de amostragem (sites) e os dados de deteccao (dados)
      det_sp_occ <- funcao_ocasioes (sites = sitios_amostragem,
                                      dados = det_sp)
      
      # add species
      det_sp_occ$species <- i
      
      # change site names
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar19")] <- "Maro19"
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar20")] <- "Maro20"
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar21")] <- "Maro21"
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar22")] <- "Maro22"
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar23")] <- "Maro23"
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar24")] <- "Maro24"
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar25")] <- "Maro25"
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar26")] <- "Maro26"
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar27")] <- "Maro27"
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar28")] <- "Maro28"
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar29")] <- "Maro29"
      det_sp_occ$sitio[which(det_sp_occ$sitio == "Mar31")] <- "Maro31"
      
      
      # veja que tem o mesmo nrow do que a tabela original, mas 3 colunas a mais (ocasioes e data)
      # dim(det_sp); dim (det_sp_occ)
      ;
      
      # return
      det_sp_occ
      
      
  }

)


# melt these data
list_det_data <- do.call(rbind, list_det_data)
list_det_data$species [grep("ammo_hume",list_det_data$species)] <- list_sp[1]
list_det_data$species [grep("embe_herb",list_det_data$species)] <- list_sp[2]
list_det_data$species [grep("leis_supe",list_det_data$species)] <- list_sp[3]
list_det_data$species [grep("zono_cape",list_det_data$species)] <- list_sp[4]

# tabela de eventos amostrais 
tab_det_all_spp <- tapply (list_det_data$det,
                          list (list_det_data$sitio,
                                list_det_data$combDiaTurno),
                          FUN = max,na.rm=T)

# match colnames
tab_det_all_spp<- tab_det_all_spp[,match(colnames(det_table_temperatura),
                                         colnames(tab_det_all_spp))]


# produce detection for each species
tab_det_sp <- tapply (list_det_data$det,
                      list (list_det_data$sitio,
                            list_det_data$combDiaTurno,
                            list_det_data$species),
                      FUN = max,na.rm=T)

# match colnames
tab_det_sp<- tab_det_sp[,match(colnames(det_table_temperatura),
                                         colnames(tab_det_sp)),]


# testar a correspondencia entre as deteccoes e as covariaveis
table(is.na(det_table_temperatura_std) == is.na(tab_det_sp[,,1]))
det_table_temperatura_std[is.na(det_table_temperatura_std)]
table(is.na(tab_det_sp[,,1]))
table(is.na(det_table_umidade))
colnames(tab_det_sp[,,1]) == colnames(det_table_umidade)
tolower(rownames(tab_det_sp[,,1])) == tolower(rownames(det_table_umidade))

# testar correspondencia de deteccao de todas as spp e de cada spp
table(is.na(tab_det_all_spp) == is.na(tab_det_sp[,,4]))



# -----------------------------------------------------
# site covariates
# -----------------------------------------------------

det_table_veg <- read.csv (here("data", "dados_cov_sitio2.csv"),sep=";")

# are site names properly organized?
det_table_veg$sitio %in% toupper(rownames(tab_det_sp[,,1]))

# correcting the order of sites
det_table_veg <- det_table_veg [match (toupper(rownames(tab_det_all_spp)),
                                       det_table_veg$sitio),]
# cf
det_table_veg$sitio == toupper(rownames(tab_det_sp[,,1]))
det_table_veg$sitio == toupper(rownames(tab_det_all_spp))


# land use cover standardized by the buffer area
det_table_veg$agriculturas <- det_table_veg$agriculturas / (pi*(500^2))
det_table_veg$campo_r <- det_table_veg$campo_r / (pi*(500^2))
det_table_veg$campo_s <- det_table_veg$campo_s / (pi*(500^2))
det_table_veg$arborea <- det_table_veg$arborea / (pi*(500^2))
det_table_veg$campos <- det_table_veg$campos / (pi*(500^2))

# checar a variacao nas covs
par(mfrow=c(3,4))
lapply (seq(4,ncol(det_table_veg)-1), function (i)
  
  plot(det_table_veg[,i][order(det_table_veg[,i])],
       main = colnames(det_table_veg)[i],
       xlab = "site rank",
       ylab = "cov value",
       type="l")
  
)

# sound detection area, standardized
std_det_area <- (det_table_veg$area)
std_det_area <- (std_det_area-mean(std_det_area))/sd(std_det_area)

# landscape characteristics (at 500 m buffer)
# agriculture
agriculture <- (det_table_veg$agriculturas )
std_agri <- (agriculture-mean(agriculture))/sd(agriculture)

# dry grassland
grassland_dry <- (det_table_veg$campo_s)
std_grass_dry <- (grassland_dry-mean(grassland_dry))/sd(grassland_dry)

# regeneration grassland
grassland_reg <- (det_table_veg$campo_r)
std_grass_reg <- (grassland_reg-mean(grassland_reg))/sd(grassland_reg)

# tree
tree <- (det_table_veg$arborea)
std_tree <- (tree-mean(tree))/sd(tree)

# campos
grass <- (det_table_veg$campos)
std_grass <- (grass-mean(grass))/sd(grass)

# correlation between covariates
cbind (std_agri,std_tree,std_grass_dry,std_grass_reg) %>%
  cor()



# transform detection data and covariate data into long format


long_data <- lapply (seq(1,dim(tab_det_sp)[3]), function (i){
  
  # melt data
  long_data <- cbind(melt(tab_det_sp[,,i],as.is=TRUE),
                     melt(det_table_temperatura_std,as.is=TRUE),
                     melt(det_table_umidade_std,as.is=TRUE)
                     
  )
  # set colnames
  colnames(long_data) <- c("site", "survey", "det", "siteT", "surveyT","temp", "siteH", "surveyH","humid")
  
  # check
  # table(long_data$site == long_data$siteT)
  # table(long_data$site == long_data$siteH)
  # table(long_data$siteT == long_data$siteH)
  # table(long_data$survey == long_data$surveyT)
  # table(long_data$survey == long_data$surveyH)
  # table(long_data$surveyT == long_data$surveyH)
  # ok
  
  
  # turno/day period
  long_data$turno <- substr(as.character(long_data$survey),
                            nchar(as.character(long_data$survey))-4,
                            nchar(as.character(long_data$survey)))
  
  
  # exclude NAs
  long_data <- long_data %>%
    filter(is.na(det) !=T)# %>%
  #filter(is.na(temp) !=T)
  
  
  # new codes for surveys
  long_data$surveyNum <- as.numeric(long_data$survey)
  long_data
  
  
})


# save all  data
save.image(here("output", "organized_data.RData"))
rm(list=ls())
