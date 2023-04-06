# interpretation
# functions
source ("R/functions.R")
source ("R/packages.R")

# models' output
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

# CIs
summ_res<- lapply (seq (1,length(list_model_res)), function (i)
  rbind (
    data.frame (Mean= mean(list_model_res[[i]][[1]]$sims.list$beta.grass),
            LCI95= quantile (list_model_res[[i]][[1]]$sims.list$beta.grass, c(0.025,0.975))[1],
            HCI95=quantile (list_model_res[[i]][[1]]$sims.list$beta.grass, c(0.025,0.975))[2],
            LCI80= quantile (list_model_res[[i]][[1]]$sims.list$beta.grass, c(0.2,0.8))[1],
            HCI80=quantile (list_model_res[[i]][[1]]$sims.list$beta.grass, c(0.2,0.8))[2],
            Covariate = "Grassland"),
    data.frame (Mean= mean(list_model_res[[i]][[1]]$sims.list$beta.agri),
            LCI95= quantile (list_model_res[[i]][[1]]$sims.list$beta.agri, c(0.025,0.975))[1],
            HCI95=quantile (list_model_res[[i]][[1]]$sims.list$beta.agri, c(0.025,0.975))[2],
            LCI80= quantile (list_model_res[[i]][[1]]$sims.list$beta.agri, c(0.2,0.8))[1],
            HCI80=quantile (list_model_res[[i]][[1]]$sims.list$beta.agri, c(0.2,0.8))[2],
            Covariate = "Crop fields"),
    data.frame (Mean= mean(list_model_res[[i]][[1]]$sims.list$beta.tree),
            LCI95= quantile (list_model_res[[i]][[1]]$sims.list$beta.tree, c(0.025,0.975))[1],
            HCI95=quantile (list_model_res[[i]][[1]]$sims.list$beta.tree, c(0.025,0.975))[2],
            LCI80= quantile (list_model_res[[i]][[1]]$sims.list$beta.tree, c(0.2,0.8))[1],
            HCI80=quantile (list_model_res[[i]][[1]]$sims.list$beta.tree, c(0.2,0.8))[2],
            Covariate = "Forest"))
)

names (summ_res)<- c("A. humeralis", "E. herbicola", "L. superciliaris", "Z. capensis")

# melt
summ_res <- do.call(rbind.data.frame, summ_res)
summ_res$Species <- gsub (".2.5%", "", rownames(summ_res))
summ_res$Species <- gsub ("1", "", summ_res$Species)
summ_res$Species <- gsub ("2", "", summ_res$Species)

# plot

dodge <- c(0.4,0.4)
pd <- position_dodge(dodge)
pdf_pt <- position_dodge(dodge)

occ_plot <- ggplot (summ_res %>% 
                      filter (is.na(Mean) != T),  
                    aes  (y=Species, x=Mean, 
                         colour=Covariate, fill=Covariate)) + 
  geom_point(aes (y=Species,x=Mean),position=(pdf_pt), 
             size=5)+ 
  
  geom_errorbar(aes(xmin=LCI95,xmax=HCI95),width = 0,size=1.5,
                position=pd)  + 
  
  geom_errorbar(aes(xmin=LCI80,xmax=HCI80),width = 0,size=2.5,
                position=pd)  + 
  
  theme_classic() + 
  
  
  
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "gray50", size=0.5)+
  
  scale_color_manual(values=c("#FFC074", "#B6C867","#01937C", "#212121")) + # , "#212121"
  
  xlab("Regression coefficient") + 
  
  ylab ("Species") + 
  
  ggtitle ("A) Site occupancy probability") +
  
  theme(axis.text.x = element_text(angle = 0,size=9),
        axis.text.y = element_text(angle = 0,size=9),
        legend.position = c(0.875,0.55))

occ_plot

# --------------------------------------------------------------------
# deteccao
# coef plot
# alpha eh o parametro de interesse

# CIs
summ_res_det<- lapply (seq (1,length(list_model_res)), function (i)
  rbind (
    data.frame (Mean= mean(list_model_res[[i]][[1]]$sims.list$alpha.temp),
                LCI95= quantile (list_model_res[[i]][[1]]$sims.list$alpha.temp, c(0.025,0.975))[1],
                HCI95=quantile (list_model_res[[i]][[1]]$sims.list$alpha.temp, c(0.025,0.975))[2],
                LCI80= quantile (list_model_res[[i]][[1]]$sims.list$alpha.temp, c(0.2,0.8))[1],
                HCI80=quantile (list_model_res[[i]][[1]]$sims.list$alpha.temp, c(0.2,0.8))[2],
                Covariate = "Temperature"),
    data.frame (Mean= mean(list_model_res[[i]][[1]]$sims.list$alpha.umid),
                LCI95= quantile (list_model_res[[i]][[1]]$sims.list$alpha.umid, c(0.025,0.975))[1],
                HCI95=quantile (list_model_res[[i]][[1]]$sims.list$alpha.umid, c(0.025,0.975))[2],
                LCI80= quantile (list_model_res[[i]][[1]]$sims.list$alpha.umid, c(0.2,0.8))[1],
                HCI80=quantile (list_model_res[[i]][[1]]$sims.list$alpha.umid, c(0.2,0.8))[2],
                Covariate = "Moisture"),
    data.frame (Mean= mean(list_model_res[[i]][[1]]$sims.list$alpha.area),
                LCI95= quantile (list_model_res[[i]][[1]]$sims.list$alpha.area, c(0.025,0.975))[1],
                HCI95=quantile (list_model_res[[i]][[1]]$sims.list$alpha.area, c(0.025,0.975))[2],
                LCI80= quantile (list_model_res[[i]][[1]]$sims.list$alpha.area, c(0.2,0.8))[1],
                HCI80=quantile (list_model_res[[i]][[1]]$sims.list$alpha.area, c(0.2,0.8))[2],
                Covariate = "Detection area")
  ))

names (summ_res_det)<- c("A. humeralis", "E. herbicola", "L. superciliaris", "Z. capensis")

# melt
summ_res_det <- do.call(rbind.data.frame, summ_res_det)
summ_res_det$Species <- gsub (".5%", "", rownames(summ_res_det))
summ_res_det$Species <- gsub ("1", "", summ_res_det$Species)
summ_res_det$Species <- gsub ("2", "", summ_res_det$Species)

# plot

dodge <- c(0.4,0.4)
pd <- position_dodge(dodge)
pdf_pt <- position_dodge(dodge)

det_plot <- ggplot (summ_res_det %>% 
                      filter (is.na(Mean) != T),  
                    aes  (y=Species, x=Mean, 
                          colour=Covariate, fill=Covariate)) + 
  geom_point(aes (y=Species,x=Mean),position=(pdf_pt), 
             size=5)+ 
  
  geom_errorbar(aes(xmin=LCI95,xmax=HCI95),width = 0,size=1.5,
                position=pd)  + 
  
  geom_errorbar(aes(xmin=LCI80,xmax=HCI80),width = 0,size=2.5,
                position=pd)  + 
  
  theme_classic() + 
  
  
  
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "gray50", size=0.5)+
  
  scale_color_manual(values=c("#FFC074", "#B6C867","#01937C", "#212121")) + # , "#212121"
  
  xlab("Regression coefficient") + 
  
  ylab ("") + 
  
  #xlim(-0.5,0.5) +
  
  theme(axis.text.x = element_text(angle = 0,size=10),
        legend.position = c(0.875,0.55)) + 
  
  ggtitle ("B) Detection probability")


# array

pdf(here("figures","coeff_plot.pdf"),width=8,height=4)
grid.arrange(occ_plot,det_plot,ncol=2)
dev.off()



# mean psi
round(list_model_res$dados_detec_ammo_hume.csv[[1]]$summary[grep("psi",rownames(list_model_res$dados_detec_ammo_hume.csv[[1]]$summary)),],2)

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


# moisture
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
  i[[1]]$summary [grep ("mean.p",rownames(i[[1]]$summary )),c("mean", "2.5%","97.5%")])
do.call(rbind,meanP)



