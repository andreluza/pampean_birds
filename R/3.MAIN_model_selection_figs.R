# -----------------------------------------------------------

#                         Model selection

#         Table of Bayesian P-values per species and model 
#                                 &
#                     figures that go in the main text

#       The output of each model can be found in the next scripts (SUPP)

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


# --------------------------------------------

# average covariate values
apply (cbind (grass, agriculture, tree),2,mean)
apply (cbind (grass, agriculture, tree),2,sd)


# temp 
mean(det_table_temperatura,na.rm=T)
sd(det_table_temperatura,na.rm=T)

# humidity 
mean(det_table_umidade,na.rm=T)
sd(det_table_umidade,na.rm=T)

# detection area 
mean(det_table_veg$area,na.rm=T)
sd(det_table_veg$area,na.rm=T)

# --------------------------------------------

# goodness of fit
# Bayesian P-value table
df_bpv <- lapply (seq(1,length(list_sp)), function (sp) {
  
  # subset spp
  species <- list_sp[sp]
  
  # data frame 
  df_bpv <-data.frame(rbind (
    
         c("psi(.)p(.)", species, res_mod_null_null[[sp]]$mean$bpv),
         c("psi(.)p(Area)", species, res_mod_det_area[[sp]]$mean$bpv),
         c("psi(.)p(Day period)", species, res_mod_det_turno[[sp]]$mean$bpv),
         c("psi(.)p(TºC,Hum)", species, res_mod_tempHUmid[[sp]]$mean$bpv),
         c("psi(.)p(TºC,Hum,Area,Day period)", species, res_mod_null[[sp]]$mean$bpv),
         c("psi(Dry,Reg,Tree,Agri)p(.)", species, res_detConstHab[[sp]]$mean$bpv),
         c("psi(Dry,Reg,Tree,Agri)p(TºC,Hum)", species, res_mod_tempHUmid_habCov[[sp]]$mean$bpv),
         c("psi(Dry,Reg,Tree,Agri)p(Area)", species, res_mod_area_habCov[[sp]]$mean$bpv),
         c("psi(Dry,Reg,Tree,Agri)p(Day period)", species, res_mod_turno_habCov[[sp]]$mean$bpv),
         c("psi(Dry,Reg)p(TºC,Hum,Area,Day period)", species, res_mod_grass[[sp]]$mean$bpv),
         c("psi(Tree,Agri)p(TºC,Hum,Area,Day period)", species, res_mod_no_grass[[sp]]$mean$bpv),
         c("psi(Dry,Reg,Tree,Agri)p(TºC,Hum,Area,Day period)", species, res_mod[[sp]]$mean$bpv)
         
         ))
  # set colnames
  colnames(df_bpv) <- c("Model", "Species", "Bpv")
  df_bpv[,"Bpv"] <-round(as.numeric(df_bpv[,"Bpv"]),3)
  df_bpv
  
})


# melt()
do.call(rbind,df_bpv) %>%
  cast(Model~Species, value = "Bpv")


# best models for each species
best_models <- list(res_detConstHab[[1]],
                    res_mod_det_area[[2]],
                    res_detConstHab[[3]],
                    res_detConstHab[[4]])


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



## ----------------------
# (LEISTES SUPERCILIARIS)

colors <- c("#deebf7", "#bdd7e7", "#6baed6", "#2171b5")
species <- list_sp

# produce one plot per spp with the same model 
sp_plots <- lapply (c(1,3), function (i) {

        
        # (GRASSLAND)
        # average relationship
        av_rel <-data.frame (x=seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100), # stand value
                             x_nat = seq(min(grassland_dry), max(grassland_dry),length.out=100), # real covariate value
                             pred= plogis(best_models[[i]]$mean$int.psi+
                                            best_models[[i]]$mean$beta.grass*seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100))
        )
        
        
        # predict
        # extract coeffs
        coeff_to_pred <- data.frame (
          int=melt (best_models[[i]]$sims.list$int.psi),
          grass=melt(best_models[[i]]$sims.list$beta.grass),
          sample=rownames(melt(best_models[[i]]$sims.list$beta.grass)))
        colnames(coeff_to_pred) <- c("int","grass","sample") # set colnames
        
        
        # predict for each MCMC sample
        pred_data <- lapply (seq (1,length(best_models[[i]]$sims.list$int.psi)), function (i)
          
          data.frame (x=seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100), # stand value
                      x_nat = seq(min(grassland_dry), max(grassland_dry),length.out=100), # real covariate value
                      pd_sample=i, # posterior dist sample
                      pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                                     coeff_to_pred[i,"grass"]*seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100)
                      )))
        
        # melt
        pred_data<-do.call(rbind,pred_data)
        
        # plot
        grass_pred_Le_sup <- pred_data %>%
          ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
          geom_line(alpha=0.02,col="gray30")+
          theme_classic()+
          geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#6baed6")+
          labs(x="Dry grassland cover",
               y= bquote("Occupancy probability, "*hat(psi[i])*""),
               title = species[i])+
          my_theme+
          theme(plot.title = element_text(face = "italic",size=22))
        
        
        # (AGRICULTURE)
        # (LEISTES SUPERCILIARIS)
        # average relationship
        av_rel <-data.frame (x=seq(min(list_covs$agri), max(list_covs$agri),length.out=100), # stand value
                             x_nat = seq(min(agriculture), max(agriculture),length.out=100), # real covariate value
                             pred= plogis(best_models[[i]]$mean$int.psi+
                                            best_models[[i]]$mean$beta.agri*seq(min(list_covs$agri), max(list_covs$agri),length.out=100)
                             )
        )
        
        # predict
        # extract coeffs
        coeff_to_pred <- data.frame (
          int=melt (best_models[[i]]$sims.list$int.psi),
          grass=melt(best_models[[i]]$sims.list$beta.agri),
          sample=rownames(melt(best_models[[i]]$sims.list$beta.agri)))
        colnames(coeff_to_pred) <- c("int","agri","sample") # set colnames
        
        
        # predict for each MCMC sample
        pred_data <- lapply (seq (1,length(best_models[[i]]$sims.list$int.psi)), function (i)
          
          data.frame (x=seq(min(list_covs$agri), max(list_covs$agri),length.out=100), # stand value
                      x_nat = seq(min(agriculture), max(agriculture),length.out=100), # real covariate value
                      pd_sample=i, # posterior dist sample
                      pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                                     coeff_to_pred[i,"agri"]*seq(min(list_covs$agri), max(list_covs$agri),length.out=100)
                      )))
        
        # melt
        pred_data<-do.call(rbind,pred_data)
        
        # plot
        agri_pred_Le_sup <- pred_data %>%
          ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
          geom_line(alpha=0.02,col="gray30")+
          theme_classic()+
          geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#6baed6")+
          labs(x="Agriculture cover",
               y= bquote("Occupancy probability, "*hat(psi[i])*""),
               title = " ")+
          my_theme+
          theme(plot.title = element_text(face = "italic",size=22))
        
        
        #ggsave(plot = agri_pred_Le_sup,filename = here("figures","weird_relationship.png"))
        
        
        # (GRASSLAND REGENERATING)
        #  (LEISTES SUPERCILIARIS)
        # average relationship
        av_rel <-data.frame (x=seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100), # stand value
                             x_nat = seq(min(grassland_reg), max(grassland_reg),length.out=100), # real covariate value
                             pred= plogis(best_models[[i]]$mean$int.psi+
                                            best_models[[i]]$mean$beta.res*seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100))
        )
        
        
        # predict
        # extract coeffs
        coeff_to_pred <- data.frame (
          int=melt (best_models[[i]]$sims.list$int.psi),
          grass=melt(best_models[[i]]$sims.list$beta.res),
          sample=rownames(melt(best_models[[i]]$sims.list$beta.res)))
        colnames(coeff_to_pred) <- c("int","grass","sample") # set colnames
        
        
        # predict for each MCMC sample
        pred_data <- lapply (seq (1,length(best_models[[i]]$sims.list$int.psi)), function (i)
          
          data.frame (x=seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100), # stand value
                      x_nat = seq(min(grassland_reg), max(grassland_reg),length.out=100), # real covariate value
                      pd_sample=i, # posterior dist sample
                      pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                                     coeff_to_pred[i,"grass"]*seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100)
                      )))
        
        # melt
        pred_data<-do.call(rbind,pred_data)
        
        
        # plot
        reg_grass_pred_Le_sup <- pred_data %>%
          ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
          geom_line(alpha=0.02,col="gray30")+
          theme_classic()+
          geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5, col="#6baed6")+
          labs(x="Regenerating grassland cover",
               y= bquote(""*hat(psi[i])*""),
               title = " ")+
          my_theme+
          theme(plot.title = element_text(face = "italic",size=22))
        
        
        
        # (TREE)
        #  (LEISTES SUPERCILIARIS)
        # average relationship
        av_rel <-data.frame (x=seq(min(list_covs$tree), max(list_covs$tree),length.out=100), # stand value
                             x_nat = seq(min(tree), max(tree),length.out=100), # real covariate value
                             pred= plogis(best_models[[i]]$mean$int.psi+
                                            best_models[[i]]$mean$beta.tree*seq(min(list_covs$tree), max(list_covs$tree),length.out=100)
                             )
        )
        
        # predict
        # extract coeffs
        coeff_to_pred <- data.frame (
          int=melt (best_models[[i]]$sims.list$int.psi),
          grass=melt(best_models[[i]]$sims.list$beta.tree),
          sample=rownames(melt(best_models[[i]]$sims.list$beta.tree)))
        colnames(coeff_to_pred) <- c("int","tree","sample") # set colnames
        
        
        # predict for each MCMC sample
        pred_data <- lapply (seq (1,length(best_models[[i]]$sims.list$int.psi)), function (i)
          
          data.frame (x=seq(min(list_covs$tree), max(list_covs$tree),length.out=100), # stand value
                      x_nat = seq(min(tree), max(tree),length.out=100), # real covariate value
                      pd_sample=i, # posterior dist sample
                      pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                                     coeff_to_pred[i,"tree"]*seq(min(list_covs$tree), max(list_covs$tree),length.out=100)
                      )))
        
        # melt
        pred_data<-do.call(rbind,pred_data)
        
        # plot
        tree_pred_Le_sup <- pred_data %>%
          ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
          geom_line(alpha=0.02,col="gray30")+
          theme_classic()+
          geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,  col="#6baed6")+
          labs(x="Tree cover",
               y= bquote(""*hat(psi[i])*""),
               title = " ")+
          my_theme+
          theme(plot.title = element_text(face = "italic",size=22))
        
        # arrange plot
        out_plot<-grid.arrange(grass_pred_Le_sup,
                     reg_grass_pred_Le_sup,
                     agri_pred_Le_sup,
                     tree_pred_Le_sup,
                     nrow=2)
        out_plot

}
)
# save plots
png (here("figures", "fig2.png"),width = 22,height = 16,units = "cm",res=300)
plot(sp_plots[[1]])
dev.off()
png (here("figures", "fig3.png"),width = 22,height = 16,units = "cm",res=300)
plot(sp_plots[[2]])
dev.off()

#----------------------------------------------------------------------------
# plot of occupancy probability, detection probability, and FSS

df_fss<-lapply (seq(1,length(list_sp)), function (i)
  
  data.frame (species = list_sp[i],
              p=best_models[[i]]$sims.list$mean.p,
              psi=best_models[[i]]$sims.list$mean.psi
              #fss=best_models[[i]]$sims.list$fss
  ))
df_fss <- do.call(rbind,df_fss)# melt


# naive occupancy
length(unique(long_data[[1]] [which(long_data[[1]]$det == 1),"site"]))/length(levels(long_data[[1]]$site))
length(unique(long_data[[2]] [which(long_data[[2]]$det == 1),"site"]))/length(levels(long_data[[1]]$site))
length(unique(long_data[[3]] [which(long_data[[3]]$det == 1),"site"]))/length(levels(long_data[[1]]$site))
length(unique(long_data[[4]] [which(long_data[[4]]$det == 1),"site"]))/length(levels(long_data[[1]]$site))


# summary
best_models[[1]]$summary[1:3,]
best_models[[2]]$summary[1:3,]
best_models[[3]]$summary[1:3,]
best_models[[4]]$summary[1:3,]

 
# one plot per parameter
df_fss<-melt (df_fss,as.is=T)
# label
df_fss$label [which(df_fss$variable == "p")]<- expression(paste(hat(p[ij])))
df_fss$label [which(df_fss$variable == "psi")]<- expression(paste(hat(psi[i])))


# transform
df_fss$label<-as.character(df_fss$label)
df_fss$label <- factor(df_fss$label)


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
               aes(x=species,y=(value)),size=2)+
  ylim(c(0,1))

png (here("figures", "fig1.png"),width = 22,height = 16,units = "cm",res=300)
  assem_res
dev.off()


# detection probability

# (area)
#  (EMBEREZOIDES HERBICOLA)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$det_area), max(list_covs$det_area),length.out=100), # stand value
                     x_nat = seq(min(det_table_veg$area), max(det_table_veg$area),length.out=100), # real covariate value
                     pred= plogis(best_models[[2]]$mean$int.p+
                                    best_models[[2]]$mean$alpha.area*seq(min(list_covs$det_area), max(list_covs$det_area),length.out=100)
                     )
)

# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (best_models[[2]]$sims.list$int.p),
  area=melt(best_models[[2]]$sims.list$alpha.area),
  sample=rownames(melt(best_models[[2]]$sims.list$alpha.area)))
colnames(coeff_to_pred) <- c("int","area","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(best_models[[2]]$sims.list$alpha.area)), function (i)
  
  data.frame (x=seq(min(list_covs$det_area), max(list_covs$det_area),length.out=100), # stand value
              x_nat = seq(min(det_table_veg$area), max(det_table_veg$area),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"area"]*seq(min(list_covs$det_area), max(list_covs$det_area),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
area_pred_Em_her <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#6baed6")+
  labs(x="Detection area",
       y= bquote(""*hat(p[ij])*""),
       title = "Emberezoides herbicola")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))+
  ylim(c(0,1))


png (here("figures", "fig4.png"),width = 13,height = 13,units = "cm",res=300)
  area_pred_Em_her
dev.off()

# -----------------------------------

# posterior exceedance probs

# day period7

# Amm hum
# grassland 
sum(best_models[[1]]$sims.list$beta.grass>0)/length(best_models[[1]]$sims.list$beta.grass)
sum(best_models[[1]]$sims.list$beta.grass<0)/length(best_models[[1]]$sims.list$beta.grass)

# regenerating grassland effect
sum(best_models[[1]]$sims.list$beta.res>0)/length(best_models[[1]]$sims.list$beta.res)
sum(best_models[[1]]$sims.list$beta.res<0)/length(best_models[[1]]$sims.list$beta.res)

# agriculture effect
sum(best_models[[1]]$sims.list$beta.agri>0)/length(best_models[[1]]$sims.list$beta.agri)
sum(best_models[[1]]$sims.list$beta.agri<0)/length(best_models[[1]]$sims.list$beta.agri)

# tree effect
sum(best_models[[1]]$sims.list$beta.tree>0)/length(best_models[[1]]$sims.list$beta.tree)
sum(best_models[[1]]$sims.list$beta.tree<0)/length(best_models[[1]]$sims.list$beta.tree)




# Lei sup
# grassland 
sum(best_models[[3]]$sims.list$beta.grass>0)/length(best_models[[3]]$sims.list$beta.grass)
sum(best_models[[3]]$sims.list$beta.grass<0)/length(best_models[[3]]$sims.list$beta.grass)

# regenerating grassland effect
sum(best_models[[3]]$sims.list$beta.res>0)/length(best_models[[3]]$sims.list$beta.res)
sum(best_models[[3]]$sims.list$beta.res<0)/length(best_models[[3]]$sims.list$beta.res)

# agriculture effect
sum(best_models[[3]]$sims.list$beta.agri>0)/length(best_models[[3]]$sims.list$beta.agri)
sum(best_models[[3]]$sims.list$beta.agri<0)/length(best_models[[3]]$sims.list$beta.agri)

# tree effect
sum(best_models[[3]]$sims.list$beta.tree>0)/length(best_models[[3]]$sims.list$beta.tree)
sum(best_models[[3]]$sims.list$beta.tree<0)/length(best_models[[3]]$sims.list$beta.tree)

# detection emberezoides
# tree effect
sum(best_models[[2]]$sims.list$alpha.area>0)/length(best_models[[2]]$sims.list$alpha.area)
sum(best_models[[2]]$sims.list$alpha.area<0)/length(best_models[[2]]$sims.list$alpha.area)


# rm(list=ls())

