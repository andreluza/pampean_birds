# interpretation
# funcoes
source ("functions.R")
# models' output
require(here)
load((here ("output", "model_output.RData")))



# --------------------------------------------
# goodness of fit

pdf(here("figures","model_selection.pdf"),width=8,height=6)

par(mfrow=c(3,4),mar=c(5,4,5,4))

# null model
lapply (seq(1,length(res_mod_null)), function (i){
  plot(res_mod_null[[i]]$sims.list$fit.actual,
       res_mod_null[[i]]$sims.list$fit.sim,
       main = paste("BPV=",round(res_mod_null[[i]]$mean$bpv,3)),
       #sub= paste ("DIC=",round(res_mod_null[[i]]$DIC,3)),
       xlab = "Actual data",
       ylab = "Simulated data",
       col=i,
       cex=0.75)
  abline(1,1)
  
})

# text
mtext("Null model",outer=F,side=4,line = 2,
      col="red")

# grassland model
lapply (seq(1,length(res_mod_grass)), function (i){
  plot(res_mod_grass[[i]]$sims.list$fit.actual,
       res_mod_grass[[i]]$sims.list$fit.sim,
       main = paste("BPV=",round(res_mod_grass[[i]]$mean$bpv,3)),
       #sub= paste ("DIC=",round(res_mod_grass[[i]]$DIC,3)),
       xlab = "Actual data",
       ylab = "Simulated data",
       col=i,
       cex=0.75)
  abline(1,1)
  
})

mtext("Grassland model",outer=F,side=4,line = 2,
      col="red")

# complete model
lapply (seq(1,length(res_mod)), function (i){
  plot(res_mod[[i]]$sims.list$fit.actual,
       res_mod[[i]]$sims.list$fit.sim,
       main = paste("BPV=",round(res_mod[[i]]$mean$bpv,3)),
       #sub= paste ("DIC=",round(res_mod[[i]]$DIC,3)),
       xlab = "Actual data",
       ylab = "Simulated data",
       col=i,
       cex=0.75)
  abline(1,1)
  
})

mtext("Complete model",outer=F,side=4,line = 2,
      col="red")

dev.off()

# -----------------------------------------------------------
# deviation information criteria
DIC_selection <- cbind (
  null=unlist(lapply (res_mod_null, function (i) i$DIC)),
  grass=unlist(lapply (res_mod_grass, function (i) i$DIC)),
  complete=unlist(lapply (res_mod, function (i) i$DIC))
)

# BPV selection
bpv_selection <- cbind (
  null=unlist(lapply (res_mod_null, function (i) i$mean$bpv)),
  grass=unlist(lapply (res_mod_grass, function (i) i$mean$bpv)),
  complete=unlist(lapply (res_mod, function (i) i$mean$bpv))
)

# change list dimensions to make it easier to select models
list_model_res <- lapply (seq(1,length(res_mod_null)), function (i) {
  
  
  res<- list (res_mod_null[[i]],
              res_mod_grass[[i]],
              res_mod[[i]])
  
  ;
  res
  
  
})

# list with detection data
list_files <- list.files(here ("data"), pattern= "dados_detec_*")


# model selection based on BPV
list_model_res <- lapply (seq(1,length(list_model_res)), function (i) 
  
  list_model_res [[i]][(closest (bpv_selection[i,],0.5))]
  
)

# list spp
names(list_model_res)<-gsub(".csv","",
                            gsub ("dados_detec_","",
                                  (list_files)))


# -----------------------------------------------
# coef best ranked models
# probabilidade de ocupacao de sitiios
# beta eh o parametro de interesse
names(list_model_res) <- list_files
summ_res<-lapply (list_model_res, function (i) 
  
  
  i[[1]]$summary [grep ("beta",rownames(i[[1]]$summary )),
                  c("mean", "2.5%","97.5%")])
# melt
summ_res <- do.call(rbind.data.frame, summ_res)
# adjust spp names
summ_res <- data.frame (summ_res,
                        Espécie= gsub(".csv","",
                                      gsub ("dados_detec_","",
                                            gsub (".beta.*","",
                                                  
                                                  rownames(summ_res)))))

# adjust spp names
summ_res$Espécie[grep("ammo_hume",summ_res$Espécie)] <- "A. humeralis"
summ_res$Espécie[grep("embe_herb",summ_res$Espécie)] <- "E. herbicola"
summ_res$Espécie[grep("leis_supe",summ_res$Espécie)] <- "L. superciliaris"
summ_res$Espécie[grep("zono_cape",summ_res$Espécie)] <- "Z. capensis"

# adjust colnames()

colnames(summ_res)[1:3]<- c("Mean","Lower","Upper")

# adjust covariate names
summ_res<-cbind (summ_res, "Covariate"=rownames(summ_res))
summ_res$Covariate[grep("beta.grass",summ_res$Covariate)]<- "Grassland"
summ_res$Covariate[grep("beta.agri",summ_res$Covariate)]<- "Crop fields"
summ_res$Covariate[grep( "beta.tree",summ_res$Covariate)]<- "Forest"
# adjust the fourth row by hand, as the grassland model (with only one parameter) was the most supported by A humeralis
summ_res$Covariate[4] <- "Grassland"

# plot
require(tidyverse)
dodge <- c(0.4,0.4)
pd <- position_dodge(dodge)
pdf_pt <- position_dodge(dodge)

occ_plot <- ggplot (summ_res,  aes  (y=Espécie, x=Mean, 
                         colour=Covariate, fill=Covariate)) + 
  
  geom_errorbar(aes(xmin=Lower,xmax=Upper),width = 0.2,size=1,
                position=pd)  + 
  
  theme_classic() + 
  
  geom_point(position=(pdf_pt), 
             size=1.5)+ 
  
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "gray50", size=0.5)+
  
  scale_color_manual(values=c("#FFC074", "#B6C867","#01937C", "#212121")) + # , "#212121"
  
  xlab("Regression coefficient") + 
  
  ylab ("Species") + 
  
  ggtitle ("A) Site occupancy probability") +
  
  theme(axis.text.x = element_text(angle = 0,size=9),
        axis.text.y = element_text(angle = 0,size=9),
        legend.position = c(0.875,0.55))


# --------------------------------------------------------------------
# deteccao
# coef plot
# alpha eh o parametro de interesse

summ_res<-lapply (list_model_res, function (i) 
  i[[1]]$summary [grep ("alpha",rownames(i[[1]]$summary )),c("mean", "2.5%","97.5%")])
summ_res <- do.call(rbind.data.frame, summ_res)
summ_res <- data.frame (summ_res,
                        Covariate = rownames(summ_res),
                        Espécie = rep(list_files,3)[order(rep(list_files,3))])
# adj spp nams
# adjust spp names
summ_res$Espécie[grep("ammo_hume",summ_res$Espécie)] <- "A. humeralis"
summ_res$Espécie[grep("embe_herb",summ_res$Espécie)] <- "E. herbicola"
summ_res$Espécie[grep("leis_supe",summ_res$Espécie)] <- "L. superciliaris"
summ_res$Espécie[grep("zono_cape",summ_res$Espécie)] <- "Z. capensis"

# adjust colnames()

colnames(summ_res)[1:3]<- c("Mean","Lower","Upper")

# adjust covariate names
summ_res$Covariate[grep("alpha.temp",summ_res$Covariate)]<- "Temperature"
summ_res$Covariate[grep("alpha.umid",summ_res$Covariate)]<- "Humidity"
summ_res$Covariate[grep( "alpha.area",summ_res$Covariate)]<- "Detection area"

# plot
require(tidyverse)
dodge <- c(0.4,0.4)
pd <- position_dodge(dodge)
pdf_pt <- position_dodge(dodge)

det_plot <- ggplot (summ_res,  aes  (y=Espécie, x=Mean, 
                         colour=Covariate, fill=Covariate)) + 
  
  geom_errorbar(aes(xmin=Lower,xmax=Upper),width = 0.2,size=1,
                position=pd)  + 
  
  theme_classic() + 
  
  geom_point(position=(pdf_pt), 
             size=1.5)+ 
  
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "gray50", size=0.5)+
  
  #facet_wrap(~Algorithm+Index,scale="free",ncol=4) + 
  
  scale_color_manual(values=c("#FFC074", "#B6C867","#01937C")) + # , "#212121"
  
  xlab("Regression coefficient") + 
  
  ylab ("") + 
  
  #xlim(-0.5,0.5) +
  
  theme(axis.text.x = element_text(angle = 0,size=10),
        legend.position = c(0.875,0.55)) + 
  
  ggtitle ("B) Detection probability")


# array
require(gridExtra)

pdf(here("figures","coeff_plot.pdf"),width=8,height=4)
grid.arrange(occ_plot,det_plot,ncol=2)
dev.off()



# mean psi
round(list_model_res$ammo_hume[[1]]$summary[grep("psi",rownames(list_model_res$ammo_hume[[1]]$summary)),],2)

# probabilities
# ammo humeralis

# posterior distribution sample size
post_samp_size <- length(list_model_res$dados_detec_ammo_hume.csv[[1]]$sims.list$beta.grass)
sum(list_model_res$dados_detec_ammo_hume.csv[[1]]$sims.list$beta.grass>0)/post_samp_size
sum(list_model_res$dados_detec_embe_herb.csv[[1]]$sims.list$beta.grass>0)/post_samp_size
sum(list_model_res$dados_detec_leis_supe.csv[[1]]$sims.list$beta.grass>0)/post_samp_size
sum(list_model_res$dados_detec_zono_cape.csv[[1]]$sims.list$beta.grass>0)/post_samp_size

# negative grass leistes
sum(list_model_res$dados_detec_leis_supe.csv[[1]]$sims.list$beta.grass<0)/post_samp_size
sum(list_model_res$dados_detec_zono_cape.csv[[1]]$sims.list$beta.grass<0)/post_samp_size


# agriculture positive
sum(list_model_res$dados_detec_ammo_hume.csv[[1]]$sims.list$beta.agri>0)/post_samp_size
sum(list_model_res$dados_detec_leis_supe.csv[[1]]$sims.list$beta.agri>0)/post_samp_size
sum(list_model_res$dados_detec_zono_cape.csv[[1]]$sims.list$beta.agri>0)/post_samp_size

# agriculture negative
sum(list_model_res$dados_detec_ammo_hume.csv[[1]]$sims.list$beta.agri<0)/post_samp_size
sum(list_model_res$dados_detec_leis_supe.csv[[1]]$sims.list$beta.agri<0)/post_samp_size
sum(list_model_res$dados_detec_zono_cape.csv[[1]]$sims.list$beta.agri<0)/post_samp_size

# postiive forest
sum(list_model_res$dados_detec_ammo_hume.csv[[1]]$sims.list$beta.tree>0)/post_samp_size
sum(list_model_res$dados_detec_leis_supe.csv[[1]]$sims.list$beta.tree>0)/post_samp_size
sum(list_model_res$dados_detec_zono_cape.csv[[1]]$sims.list$beta.tree>0)/post_samp_size

# forest negative
sum(list_model_res$dados_detec_ammo_hume.csv[[1]]$sims.list$beta.tree<0)/post_samp_size
sum(list_model_res$dados_detec_leis_supe.csv[[1]]$sims.list$beta.tree<0)/post_samp_size
sum(list_model_res$dados_detec_zono_cape.csv[[1]]$sims.list$beta.tree<0)/post_samp_size




# detection

# posterior distribution sample size
## positive
sum(list_model_res$dados_detec_ammo_hume.csv[[1]]$sims.list$alpha.temp>0)/post_samp_size
sum(list_model_res$dados_detec_embe_herb.csv[[1]]$sims.list$alpha.temp>0)/post_samp_size
sum(list_model_res$dados_detec_leis_supe.csv[[1]]$sims.list$alpha.temp>0)/post_samp_size
sum(list_model_res$dados_detec_zono_cape.csv[[1]]$sims.list$alpha.temp>0)/post_samp_size

## negative
sum(list_model_res$dados_detec_ammo_hume.csv[[1]]$sims.list$alpha.temp<0)/post_samp_size
sum(list_model_res$dados_detec_embe_herb.csv[[1]]$sims.list$alpha.temp<0)/post_samp_size
sum(list_model_res$dados_detec_leis_supe.csv[[1]]$sims.list$alpha.temp<0)/post_samp_size
sum(list_model_res$dados_detec_zono_cape.csv[[1]]$sims.list$alpha.temp<0)/post_samp_size


# humidity
## positive
sum(list_model_res$dados_detec_ammo_hume.csv[[1]]$sims.list$alpha.umid>0)/post_samp_size
sum(list_model_res$dados_detec_embe_herb.csv[[1]]$sims.list$alpha.umid>0)/post_samp_size
sum(list_model_res$dados_detec_leis_supe.csv[[1]]$sims.list$alpha.umid>0)/post_samp_size
sum(list_model_res$dados_detec_zono_cape.csv[[1]]$sims.list$alpha.umid>0)/post_samp_size


# detection area
## positive
sum(list_model_res$dados_detec_ammo_hume.csv[[1]]$sims.list$alpha.area>0)/post_samp_size
sum(list_model_res$dados_detec_embe_herb.csv[[1]]$sims.list$alpha.area>0)/post_samp_size
sum(list_model_res$dados_detec_leis_supe.csv[[1]]$sims.list$alpha.area>0)/post_samp_size
sum(list_model_res$dados_detec_zono_cape.csv[[1]]$sims.list$alpha.area>0)/post_samp_size

# negative
sum(list_model_res$dados_detec_leis_supe.csv[[1]]$sims.list$alpha.area<0)/post_samp_size






# -----------------------------------------------
# finite sample size
# fss 



summ_res_fss<-lapply (list_model_res, function (i) 
  i[[1]]$summary [grep ("fss",rownames(i[[1]]$summary )),c("mean", "2.5%","97.5%")])
do.call(rbind,summ_res_fss)



# average detection

meanP<-lapply (list_model_res, function (i) 
  i$summary [grep ("mean.p",rownames(i$summary )),c("mean", "2.5%","97.5%")])
do.call(rbind,meanP)


res_mod[[1]]$mean$mean.p
res_mod[[2]]$mean$mean.p
res_mod[[3]]$mean$mean.p
res_mod[[4]]$mean$mean.p

