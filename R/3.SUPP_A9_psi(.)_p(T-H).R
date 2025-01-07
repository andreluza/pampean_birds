# -----------------------------------------------------------



# The output of the 8th model


# hat(psi[i]) (.)
# p[ij] (temperature, moisture)


# ------------------------------------------------------------


# brewer colour pallete: 

#deebf7 - Am humeralis
#bdd7e7 - Em herbicola
#6baed6 - Le superciliaris
#2171b5 - Zo capensis

# ------------------------------------------------------------

# interpretation
# functions
rm(list=ls())
source ("R/functions.R")
source ("R/packages.R")

# ggplot theme
my_theme <- theme(legend.position = 'bottom', 
                  strip.text = element_text(size=12),
                  strip.text.y = element_text(color = 'black'),
                  strip.text.x = element_text(color = 'black'), 
                  text = element_text(family="serif"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
                  axis.text.y = element_text(size = 12),
                  axis.title = element_text(size=18))

# models' output
load((here ("output", "model_output.RData")))
load((here ("output", "model_output_det.RData")))
load(here("output", "organized_data.RData"))

# species
list_sp <- c("Ammodramus humeralis","Emberezoides herbicola", "Leistes superciliaris","Zonotrichia capensis")


# ------------------------------------------------

# regression plots


# create day time
long_data[[1]]$turno[grep("manha",long_data[[1]]$survey)]<-1
long_data[[1]]$turno[is.na(long_data[[1]]$turno)] <- 0

# covariate data
list_covs <- list (# site covs
  agri=std_agri,
  agri2=std_agri^2,
  grass_dry=std_grass_dry,
  grass_dry2=std_grass_dry^2,
  grass_res=std_grass_reg,
  tree=std_tree,
  
  # det covariates
  det_area=std_det_area,
  temp=long_data[[1]]$temp,
  umid=long_data[[1]]$humid,
  turno = long_data[[1]]$turno
)

#----------------------------------------------------------------------------
# plot of occupancy probability, detection probability, and FSS

df_fss<-lapply (seq(1,length(list_sp)), function (i)
  
  data.frame (species = list_sp[i],
              p=res_mod_tempHUmid[[i]]$sims.list$mean.p,
              psi=res_mod_tempHUmid[[i]]$sims.list$mean.psi
              #fss=res_mod_tempHUmid[[i]]$sims.list$fss
  ))
df_fss <- do.call(rbind,df_fss)# melt


# one plot per parameter
df_fss<-melt (df_fss,as.is=T)
# label
df_fss$label [which(df_fss$variable == "p")]<- expression(paste(hat(p[ij])))
df_fss$label [which(df_fss$variable == "psi")]<- expression(paste(hat(psi[i])))


# transform
df_fss$label<-as.character(df_fss$label)
df_fss$label <- factor(df_fss$label)

# library
library(ggridges)
library(ggplot2)

# ridgeline plot
assem_res <- ggplot(df_fss, aes(y = value, 
                                x = species,
                                fill = species)) +
  geom_violin()+
  
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_text(face = "italic",size=6)) +
  facet_wrap(~label,scales = "free",labeller = label_parsed) +
  my_theme+
  theme(legend.position = "none")+
  labs(y="Probability",x="Species")+
  scale_fill_brewer()+
  geom_point(data= df_fss %>%
               group_by(species, label) %>%
               summarize(value=mean(value)),
             aes(x=species,y=(value)),size=2)




#---------------------------------------------

# detection probability

# -------------------
# temperature

pred_data_temp <- lapply (seq (1,length(list_sp)), function (i) {
  
  # (area)
  # average relationship
  av_rel <-data.frame (x=seq(min(list_covs$temp,na.rm=T), max(list_covs$temp,na.rm=T),length.out=100), # stand value
                       x_nat = seq(min(det_cov$temp,na.rm=T), max(det_cov$temp,na.rm=T),length.out=100), # real covariate value
                       pred= plogis(res_mod_tempHUmid[[i]]$mean$int.p+
                                      res_mod_tempHUmid[[i]]$mean$alpha.temp*seq(min(list_covs$temp,na.rm=T), 
                                                                       max(list_covs$temp,na.rm=T),length.out=100)),
                       sp=list_sp[i]
                       
  )
  
  # predict
  # extract coeffs
  coeff_to_pred <- data.frame (
    int=melt (res_mod_tempHUmid[[i]]$sims.list$int.p),
    temp=melt(res_mod_tempHUmid[[i]]$sims.list$alpha.temp),
    sample=rownames(melt(res_mod_tempHUmid[[i]]$sims.list$alpha.temp)))
  colnames(coeff_to_pred) <- c("int","temp","sample") # set colnames
  
  
  # predict for each MCMC sample
  pred_data <- lapply (seq (1,length(res_mod_tempHUmid[[i]]$sims.list$alpha.temp)), function (k)
    
    data.frame (x=seq(min(list_covs$temp,na.rm=T), max(list_covs$temp,na.rm=T),length.out=100), # stand value
                x_nat = seq(min(det_cov$temp,na.rm=T), max(det_cov$temp,na.rm=T),length.out=100), # real covariate value
                pd_sample=k, # posterior dist sample
                sp=list_sp[i],
                pred= plogis(coeff_to_pred[k,"int"]+ # backtransform
                               coeff_to_pred[k,"temp"]*seq(min(list_covs$temp,na.rm=T), 
                                                           max(list_covs$temp,na.rm=T),length.out=100)
                )))
  
  # melt
  pred_data<-do.call(rbind,pred_data)
  
  output <- list("pred_data"=pred_data,
                 "av_rel"=av_rel)
  ;
  output
  
})

# melt
pred_data_temp_s<-do.call(rbind,sapply(pred_data_temp, "[","pred_data"))
av_rel<-do.call(rbind,sapply(pred_data_temp, "[","av_rel"))


# plot
temp_pred <- pred_data_temp_s %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  facet_grid(~sp)+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred,colour=sp),
            inherit.aes = F,linewidth=1.5)+
  labs(x="Temperature (ºC)",
       y= bquote(""*hat(p[ij])*""))+
  my_theme+
  scale_color_brewer()+
  theme(plot.title = element_text(face = "italic",size=22),
        legend.position = "none",
        strip.text = element_text(face="italic"))

temp_pred


# -------------------------------------------------------
# moisture

pred_data_umid <- lapply (seq (1,length(list_sp)), function (i) {
  
  # (area)
  # average relationship
  av_rel <-data.frame (x=seq(min(list_covs$umid,na.rm=T), max(list_covs$umid,na.rm=T),length.out=100), # stand value
                       x_nat = seq(min(det_cov$umid,na.rm=T), max(det_cov$umid,na.rm=T),length.out=100), # real covariate value
                       pred= plogis(res_mod_tempHUmid[[i]]$mean$int.p+
                                      res_mod_tempHUmid[[i]]$mean$alpha.umid*seq(min(list_covs$umid,na.rm=T), 
                                                                       max(list_covs$umid,na.rm=T),length.out=100)),
                       sp=list_sp[i]
                       
  )
  
  # predict
  # extract coeffs
  coeff_to_pred <- data.frame (
    int=melt (res_mod_tempHUmid[[i]]$sims.list$int.p),
    umid=melt(res_mod_tempHUmid[[i]]$sims.list$alpha.umid),
    sample=rownames(melt(res_mod_tempHUmid[[i]]$sims.list$alpha.umid)))
  colnames(coeff_to_pred) <- c("int","umid","sample") # set colnames
  
  
  # predict for each MCMC sample
  pred_data <- lapply (seq (1,length(res_mod_tempHUmid[[i]]$sims.list$alpha.umid)), function (k)
    
    data.frame (x=seq(min(list_covs$umid,na.rm=T), max(list_covs$umid,na.rm=T),length.out=100), # stand value
                x_nat = seq(min(det_cov$umid,na.rm=T), max(det_cov$umid,na.rm=T),length.out=100), # real covariate value
                pd_sample=k, # posterior dist sample
                sp=list_sp[i],
                pred= plogis(coeff_to_pred[k,"int"]+ # backtransform
                               coeff_to_pred[k,"umid"]*seq(min(list_covs$umid,na.rm=T), 
                                                           max(list_covs$umid,na.rm=T),length.out=100)
                )))
  
  # melt
  pred_data<-do.call(rbind,pred_data)
  
  output <- list("pred_data"=pred_data,
                 "av_rel"=av_rel)
  ;
  output
  
})

# melt
pred_data_umid_s<-do.call(rbind,sapply(pred_data_umid, "[","pred_data"))
av_rel<-do.call(rbind,sapply(pred_data_umid, "[","av_rel"))


# plot
umid_pred <- pred_data_umid_s %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  facet_grid(~sp)+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred,colour=sp),
            inherit.aes = F,linewidth=1.5)+
  labs(x="Moisture (%)",
       y= bquote(""*hat(p[ij])*""))+
  my_theme+
  scale_color_brewer()+
  theme(plot.title = element_text(face = "italic",size=22),
        legend.position = "none",
        strip.text = element_text(face="italic"))

umid_pred

