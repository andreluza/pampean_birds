# -----------------------------------------------------------


#                     The output of the fifth model


#         hat(psi[i]) (.)
#         p[ij] (.)


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

#----------------------------------------------------------------------------
# plot of occupancy probability, detection probability, and FSS

df_fss<-lapply (seq(1,length(list_sp)), function (i)
  
  data.frame (species = list_sp[i],
              p=res_mod_null_null[[i]]$sims.list$mean.p,
              psi=res_mod_null_null[[i]]$sims.list$mean.psi
              #fss=res_mod[[i]]$sims.list$fss
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


