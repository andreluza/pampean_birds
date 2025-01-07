# -----------------------------------------------------------


#                     The output of the 10th model


#         hat(psi[i]) (dry grassland, regenerating grassland, tree, agriculture)
#         p[ij] (area)


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

# --------------------------------

# Ammodramus humeralis
# (GRASSLAND)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100), # stand value
                     x_nat = seq(min(grassland_dry), max(grassland_dry),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[1]]$mean$int.psi+
                                    res_mod_area_habCov[[1]]$mean$beta.grass*seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100))
)


# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[1]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[1]]$sims.list$beta.grass),
  sample=rownames(melt(res_mod_area_habCov[[1]]$sims.list$beta.grass)))
colnames(coeff_to_pred) <- c("int","grass","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[1]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100), # stand value
              x_nat = seq(min(grassland_dry), max(grassland_dry),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"grass"]*seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
grass_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#deebf7")+
  labs(x="Dry grassland cover",
       y= bquote("Occupancy probability, "*hat(psi[i])*""),
       title = "Ammodramus humeralis")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))


# (AGRICULTURE)
# Ammodramus humeralis
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$agri), max(list_covs$agri),length.out=100), # stand value
                     x_nat = seq(min(agriculture), max(agriculture),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[1]]$mean$int.psi+
                                    res_mod_area_habCov[[1]]$mean$beta.agri*seq(min(list_covs$agri), max(list_covs$agri),length.out=100)
                     )
)

# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[1]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[1]]$sims.list$beta.agri),
  sample=rownames(melt(res_mod_area_habCov[[1]]$sims.list$beta.agri)))
colnames(coeff_to_pred) <- c("int","agri","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[1]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$agri), max(list_covs$agri),length.out=100), # stand value
              x_nat = seq(min(agriculture), max(agriculture),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"agri"]*seq(min(list_covs$agri), max(list_covs$agri),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
agri_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#deebf7")+
  labs(x="Agriculture cover",
       y= bquote("Occupancy probability, "*hat(psi[i])*""),
       title = " ")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))


#ggsave(plot = agri_pred,filename = here("figures","weird_relationship.png"))


# (GRASSLAND REGENERATING)
# Ammodramus humeralis
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100), # stand value
                     x_nat = seq(min(grassland_reg), max(grassland_reg),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[1]]$mean$int.psi+
                                    res_mod_area_habCov[[1]]$mean$beta.res*seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100))
)


# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[1]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[1]]$sims.list$beta.res),
  sample=rownames(melt(res_mod_area_habCov[[1]]$sims.list$beta.res)))
colnames(coeff_to_pred) <- c("int","grass","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[1]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100), # stand value
              x_nat = seq(min(grassland_reg), max(grassland_reg),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"grass"]*seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)


# plot
reg_grass_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#deebf7")+
  labs(x="Regenerating grassland cover",
       y= bquote(""*hat(psi[i])*""),
       title = " ")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))



# (TREE)
# Ammodramus humeralis
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$tree), max(list_covs$tree),length.out=100), # stand value
                     x_nat = seq(min(tree), max(tree),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[1]]$mean$int.psi+
                                    res_mod_area_habCov[[1]]$mean$beta.tree*seq(min(list_covs$tree), max(list_covs$tree),length.out=100)
                     )
)

# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[1]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[1]]$sims.list$beta.tree),
  sample=rownames(melt(res_mod_area_habCov[[1]]$sims.list$beta.tree)))
colnames(coeff_to_pred) <- c("int","tree","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[1]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$tree), max(list_covs$tree),length.out=100), # stand value
              x_nat = seq(min(tree), max(tree),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"tree"]*seq(min(list_covs$tree), max(list_covs$tree),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
tree_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#deebf7")+
  labs(x="Tree cover",
       y= bquote(""*hat(psi[i])*""),
       title = " ")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))


AMM_HUMM <- grid.arrange(grass_pred,
             reg_grass_pred,
             agri_pred,
             tree_pred,
             nrow=2)

# ----------------------
# (EMBEREZOIDES HERBICOLA)

# (GRASSLAND)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100), # stand value
                     x_nat = seq(min(grassland_dry), max(grassland_dry),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[2]]$mean$int.psi+
                                    res_mod_area_habCov[[2]]$mean$beta.grass*seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100))
)


# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[2]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[2]]$sims.list$beta.grass),
  sample=rownames(melt(res_mod_area_habCov[[2]]$sims.list$beta.grass)))
colnames(coeff_to_pred) <- c("int","grass","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[2]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100), # stand value
              x_nat = seq(min(grassland_dry), max(grassland_dry),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"grass"]*seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
grass_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#bdd7e7")+
  labs(x="Dry grassland cover",
       y= bquote("Occupancy probability, "*hat(psi[i])*""),
       title = "Emberezoides herbicola")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))


# (AGRICULTURE)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$agri), max(list_covs$agri),length.out=100), # stand value
                     x_nat = seq(min(agriculture), max(agriculture),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[2]]$mean$int.psi+
                                    res_mod_area_habCov[[2]]$mean$beta.agri*seq(min(list_covs$agri), max(list_covs$agri),length.out=100)
                     )
)

# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[2]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[2]]$sims.list$beta.agri),
  sample=rownames(melt(res_mod_area_habCov[[2]]$sims.list$beta.agri)))
colnames(coeff_to_pred) <- c("int","agri","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[2]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$agri), max(list_covs$agri),length.out=100), # stand value
              x_nat = seq(min(agriculture), max(agriculture),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"agri"]*seq(min(list_covs$agri), max(list_covs$agri),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
agri_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#bdd7e7")+
  labs(x="Agriculture cover",
       y= bquote("Occupancy probability, "*hat(hat(psi[i]))*""),
       title = " ")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))


# (GRASSLAND REGENERATING)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100), # stand value
                     x_nat = seq(min(grassland_reg), max(grassland_reg),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[2]]$mean$int.psi+
                                    res_mod_area_habCov[[2]]$mean$beta.res*seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100))
)


# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[2]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[2]]$sims.list$beta.res),
  sample=rownames(melt(res_mod_area_habCov[[2]]$sims.list$beta.res)))
colnames(coeff_to_pred) <- c("int","grass","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[2]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100), # stand value
              x_nat = seq(min(grassland_reg), max(grassland_reg),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"grass"]*seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)


# plot
reg_grass_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#bdd7e7")+
  labs(x="Regenerating grassland cover",
       y= bquote(""*hat(psi[i])*""),
       title = " ")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))



# (TREE)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$tree), max(list_covs$tree),length.out=100), # stand value
                     x_nat = seq(min(tree), max(tree),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[2]]$mean$int.psi+
                                    res_mod_area_habCov[[2]]$mean$beta.tree*seq(min(list_covs$tree), max(list_covs$tree),length.out=100)
                     )
)

# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[2]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[2]]$sims.list$beta.tree),
  sample=rownames(melt(res_mod_area_habCov[[2]]$sims.list$beta.tree)))
colnames(coeff_to_pred) <- c("int","tree","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[2]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$tree), max(list_covs$tree),length.out=100), # stand value
              x_nat = seq(min(tree), max(tree),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"tree"]*seq(min(list_covs$tree), max(list_covs$tree),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
tree_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#bdd7e7")+
  labs(x="Tree cover",
       y= bquote(""*hat(psi[i])*""),
       title = " ")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))


EM_HER <- grid.arrange(grass_pred,
             reg_grass_pred,
             agri_pred,
             tree_pred,
             nrow=2)


# ----------------------
# (LEISTES SUPERCILIARIS)



# (GRASSLAND)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100), # stand value
                     x_nat = seq(min(grassland_dry), max(grassland_dry),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[3]]$mean$int.psi+
                                    res_mod_area_habCov[[3]]$mean$beta.grass*seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100))
)


# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[3]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[3]]$sims.list$beta.grass),
  sample=rownames(melt(res_mod_area_habCov[[3]]$sims.list$beta.grass)))
colnames(coeff_to_pred) <- c("int","grass","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[3]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100), # stand value
              x_nat = seq(min(grassland_dry), max(grassland_dry),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"grass"]*seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
grass_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#6baed6")+
  labs(x="Dry grassland cover",
       y= bquote("Occupancy probability, "*hat(psi[i])*""),
       title = "Leistes superciliaris")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))


# (AGRICULTURE)
# (LEISTES SUPERCILIARIS)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$agri), max(list_covs$agri),length.out=100), # stand value
                     x_nat = seq(min(agriculture), max(agriculture),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[3]]$mean$int.psi+
                                    res_mod_area_habCov[[3]]$mean$beta.agri*seq(min(list_covs$agri), max(list_covs$agri),length.out=100)
                     )
)

# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[3]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[3]]$sims.list$beta.agri),
  sample=rownames(melt(res_mod_area_habCov[[3]]$sims.list$beta.agri)))
colnames(coeff_to_pred) <- c("int","agri","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[3]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$agri), max(list_covs$agri),length.out=100), # stand value
              x_nat = seq(min(agriculture), max(agriculture),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"agri"]*seq(min(list_covs$agri), max(list_covs$agri),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
agri_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#6baed6")+
  labs(x="Agriculture cover",
       y= bquote("Occupancy probability, "*hat(psi[i])*""),
       title = " ")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))


#ggsave(plot = agri_pred,filename = here("figures","weird_relationship.png"))


# (GRASSLAND REGENERATING)
#  (LEISTES SUPERCILIARIS)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100), # stand value
                     x_nat = seq(min(grassland_reg), max(grassland_reg),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[3]]$mean$int.psi+
                                    res_mod_area_habCov[[3]]$mean$beta.res*seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100))
)


# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[3]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[3]]$sims.list$beta.res),
  sample=rownames(melt(res_mod_area_habCov[[3]]$sims.list$beta.res)))
colnames(coeff_to_pred) <- c("int","grass","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[3]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100), # stand value
              x_nat = seq(min(grassland_reg), max(grassland_reg),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"grass"]*seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)


# plot
reg_grass_pred <- pred_data %>%
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
                     pred= plogis(res_mod_area_habCov[[3]]$mean$int.psi+
                                    res_mod_area_habCov[[3]]$mean$beta.tree*seq(min(list_covs$tree), max(list_covs$tree),length.out=100)
                     )
)

# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[3]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[3]]$sims.list$beta.tree),
  sample=rownames(melt(res_mod_area_habCov[[3]]$sims.list$beta.tree)))
colnames(coeff_to_pred) <- c("int","tree","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[3]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$tree), max(list_covs$tree),length.out=100), # stand value
              x_nat = seq(min(tree), max(tree),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"tree"]*seq(min(list_covs$tree), max(list_covs$tree),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
tree_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#6baed6")+
  labs(x="Tree cover",
       y= bquote(""*hat(psi[i])*""),
       title = " ")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))

# arrange plot
LEI_SUP <- grid.arrange(grass_pred,
             reg_grass_pred,
             agri_pred,
             tree_pred,
             nrow=2)



# ----------------------
# (ZONOTRICHIA CAPENSIS)



# (GRASSLAND)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100), # stand value
                     x_nat = seq(min(grassland_dry), max(grassland_dry),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[4]]$mean$int.psi+
                                    res_mod_area_habCov[[4]]$mean$beta.grass*seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100))
)


# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[4]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[4]]$sims.list$beta.grass),
  sample=rownames(melt(res_mod_area_habCov[[4]]$sims.list$beta.grass)))
colnames(coeff_to_pred) <- c("int","grass","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[4]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100), # stand value
              x_nat = seq(min(grassland_dry), max(grassland_dry),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"grass"]*seq(min(list_covs$grass_dry), max(list_covs$grass_dry),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
grass_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#2171b5")+
  labs(x="Dry grassland cover",
       y= bquote("Occupancy probability, "*hat(psi[i])*""),
       title = "Zonotrichia capensis")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))


# (AGRICULTURE)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$agri), max(list_covs$agri),length.out=100), # stand value
                     x_nat = seq(min(agriculture), max(agriculture),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[4]]$mean$int.psi+
                                    res_mod_area_habCov[[4]]$mean$beta.agri*seq(min(list_covs$agri), max(list_covs$agri),length.out=100)
                     )
)

# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[4]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[4]]$sims.list$beta.agri),
  sample=rownames(melt(res_mod_area_habCov[[4]]$sims.list$beta.agri)))
colnames(coeff_to_pred) <- c("int","agri","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[4]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$agri), max(list_covs$agri),length.out=100), # stand value
              x_nat = seq(min(agriculture), max(agriculture),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"agri"]*seq(min(list_covs$agri), max(list_covs$agri),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
agri_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#2171b5")+
  labs(x="Agriculture cover",
       y= bquote("Occupancy probability, "*hat(psi[i])*""),
       title = " ")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))



# (GRASSLAND REGENERATING)
#  (LEISTES SUPERCILIARIS)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100), # stand value
                     x_nat = seq(min(grassland_reg), max(grassland_reg),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[4]]$mean$int.psi+
                                    res_mod_area_habCov[[4]]$mean$beta.res*seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100))
)


# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[4]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[4]]$sims.list$beta.res),
  sample=rownames(melt(res_mod_area_habCov[[4]]$sims.list$beta.res)))
colnames(coeff_to_pred) <- c("int","grass","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[4]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100), # stand value
              x_nat = seq(min(grassland_reg), max(grassland_reg),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"grass"]*seq(min(list_covs$grass_res), max(list_covs$grass_res),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)


# plot
reg_grass_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5, col="#2171b5")+
  labs(x="Regenerating grassland cover",
       y= bquote(""*hat(psi[i])*""),
       title = " ")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))



# (TREE)
# average relationship
av_rel <-data.frame (x=seq(min(list_covs$tree), max(list_covs$tree),length.out=100), # stand value
                     x_nat = seq(min(tree), max(tree),length.out=100), # real covariate value
                     pred= plogis(res_mod_area_habCov[[4]]$mean$int.psi+
                                    res_mod_area_habCov[[4]]$mean$beta.tree*seq(min(list_covs$tree), max(list_covs$tree),length.out=100)
                     )
)

# predict
# extract coeffs
coeff_to_pred <- data.frame (
  int=melt (res_mod_area_habCov[[4]]$sims.list$int.psi),
  grass=melt(res_mod_area_habCov[[4]]$sims.list$beta.tree),
  sample=rownames(melt(res_mod_area_habCov[[4]]$sims.list$beta.tree)))
colnames(coeff_to_pred) <- c("int","tree","sample") # set colnames


# predict for each MCMC sample
pred_data <- lapply (seq (1,length(res_mod_area_habCov[[4]]$sims.list$int.psi)), function (i)
  
  data.frame (x=seq(min(list_covs$tree), max(list_covs$tree),length.out=100), # stand value
              x_nat = seq(min(tree), max(tree),length.out=100), # real covariate value
              pd_sample=i, # posterior dist sample
              pred= plogis(coeff_to_pred[i,"int"]+ # backtransform
                             coeff_to_pred[i,"tree"]*seq(min(list_covs$tree), max(list_covs$tree),length.out=100)
              )))

# melt
pred_data<-do.call(rbind,pred_data)

# plot
tree_pred <- pred_data %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred), inherit.aes = F,linewidth=1.5,             col="#2171b5")+
  labs(x="Tree cover",
       y= bquote(""*hat(psi[i])*""),
       title = " ")+
  my_theme+
  theme(plot.title = element_text(face = "italic",size=22))

# arrange plot
ZON_CAP <- grid.arrange(grass_pred,
             reg_grass_pred,
             agri_pred,
             tree_pred,
             nrow=2)


#----------------------------------------------------------------------------
# plot of occupancy probability, detection probability, and FSS

df_fss<-lapply (seq(1,length(list_sp)), function (i)
  
  data.frame (species = list_sp[i],
              p=res_mod_area_habCov[[i]]$sims.list$mean.p,
              psi=res_mod_area_habCov[[i]]$sims.list$mean.psi
              #fss=res_mod_area_habCov[[i]]$sims.list$fss
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
# area
pred_data <- lapply (seq (1,length(list_sp)), function (i) {
    
    # (area)
    # average relationship
    av_rel <-data.frame (x=seq(min(list_covs$det_area), max(list_covs$det_area),length.out=100), # stand value
                         x_nat = seq(min(det_table_veg$area), max(det_table_veg$area),length.out=100), # real covariate value
                         pred= plogis(res_mod_area_habCov[[i]]$mean$int.p+
                                        res_mod_area_habCov[[i]]$mean$alpha.area*seq(min(list_covs$det_area), max(list_covs$det_area),length.out=100)),
                         sp=list_sp[i]
                         
    )
    
    # predict
    # extract coeffs
    coeff_to_pred <- data.frame (
      int=melt (res_mod_area_habCov[[i]]$sims.list$int.p),
      area=melt(res_mod_area_habCov[[i]]$sims.list$alpha.area),
      sample=rownames(melt(res_mod_area_habCov[[i]]$sims.list$alpha.area)))
    colnames(coeff_to_pred) <- c("int","area","sample") # set colnames
    
    
    # predict for each MCMC sample
    pred_data <- lapply (seq (1,length(res_mod_area_habCov[[i]]$sims.list$alpha.area)), function (k)
      
      data.frame (x=seq(min(list_covs$det_area), max(list_covs$det_area),length.out=100), # stand value
                  x_nat = seq(min(det_table_veg$area), max(det_table_veg$area),length.out=100), # real covariate value
                  pd_sample=k, # posterior dist sample
                  sp=list_sp[i],
                  pred= plogis(coeff_to_pred[k,"int"]+ # backtransform
                                 coeff_to_pred[k,"area"]*seq(min(list_covs$det_area), max(list_covs$det_area),length.out=100)
                  )))
    
    # melt
    pred_data<-do.call(rbind,pred_data)
    
    output <- list("pred_data"=pred_data,
                   "av_rel"=av_rel)
    ;
    output

})

# melt
pred_data_s<-do.call(rbind,sapply(pred_data, "[","pred_data"))
av_rel<-do.call(rbind,sapply(pred_data, "[","av_rel"))


# plot
area_pred <- pred_data_s %>%
  ggplot(aes(x=x_nat,y=pred,group=pd_sample))+
  geom_line(alpha=0.02,col="gray30")+
  facet_grid(~sp)+
  theme_classic()+
  geom_line(data = av_rel,aes(x=x_nat,y=pred,colour=sp),
            inherit.aes = F,linewidth=1.5)+
  labs(x="Detection area",
       y= bquote(""*hat(p[ij])*""))+
  my_theme+
  scale_color_brewer()+
  theme(plot.title = element_text(face = "italic",size=22),
        legend.position = "none",
        strip.text = element_text(face="italic"))
  
area_pred



