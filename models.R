# MODELING
require(here)
require(unmarked)
load(here("output", "organized_data.RData"))



# average covariate values
apply (cbind (grassland, agriculture, tree),2,mean)
apply (cbind (grassland, agriculture, tree),2,sd)


# temp 
mean(det_table_temperatura,na.rm=T)
sd(det_table_temperatura,na.rm=T)

# humidity 
mean(det_table_umidade,na.rm=T)
sd(det_table_umidade,na.rm=T)

# detection area 
mean(det_table_veg$area,na.rm=T)
sd(det_table_veg$area,na.rm=T)


# ----------------
# your first model on unmarked
# -----------------
# organize data
data_unmarkedFrameOccu <- unmarkedFrameOccu(y=list_det_data[[1]], 
                                            siteCovs=data.frame(grass=std_grass,
                                                                agri=std_agri,
                                                                tree=std_tree,
                                                                area=std_det_area), 
                                            obsCovs=list (temperatura = det_table_temperatura_std,
                                                          umidade = det_table_umidade_std
                                            ))
summary(data_unmarkedFrameOccu)

# rodar o modelo
# modelo com efeito da temp, umidade, e sua interacao
m0 <- occu(~temperatura*umidade+area ~1, data_unmarkedFrameOccu)
m1 <- occu(~temperatura*umidade+area ~grass, data_unmarkedFrameOccu)
m2 <- occu(~temperatura*umidade+area ~grass+tree+agri, data_unmarkedFrameOccu)

(m0);m1;m2

# have fun exploring how to organize and include site covariates, and other species

# --------------------------#
# your first model in jags
# -------------------------- #

# specify model in bugs language

sink ("model_acoustics.txt")
cat ("
     
     model {
     
      # set priors
      # site occupancy
      int.psi ~ dunif (0,1)
      beta.agri ~ dnorm (0,0.001)
      beta.grass ~ dnorm (0,0.001)
      beta.tree ~ dnorm (0,0.001)
      
      # detection
      int.p ~ dunif (0,1)
      alpha.temp ~dnorm (0,001)
      alpha.umid ~dnorm (0,001)
      alpha.area ~dnorm (0,001)
      
      # ----------------------
      # model likelihood
      # site-occupancy model (first level)
      for (i in 1:nsites) {
      
        z[i] ~ dbern (psi[i])
        logit (psi[i]) <- int.psi + beta.agri*agri[i]+
                                    beta.grass*grass[i]+
                                    beta.tree*tree[i]
        
      }
       
       # ----------------------------------- #
       # detection model (second level)
       #----------------------------------- #
       
       # likelihood 
       
        for (i in 1:nsites) {
           for (k in 1:nocc) {
          
             y[i,k] ~ dbern (z[i] * p[i,k])
             logit (p[i,k]) <- int.p + alpha.temp*temp[i,k]+
                                       alpha.umid*umid[i,k]+
                                       alpha.area*det_area[i]
            
             }
          }
        
        # goodness of fit
	# predictive posterior checks
	# chi squared discrepancy

	for (i in 1:nsites) {
	   for (k in 1:nocc) {

	       y.sim [i,k] ~ dbern (e.sim[i,k]) # simulate data under the model
	       e.sim [i,k] <- z[i]*p[i,k]  # expected data under the model
	       # chi squared discrepancy for the actual data
	       chi2.actual[i,k] <- pow((y[i,k]-e.sim[i,k]),2)/(e.sim[i,k]+e)
	       # chi squared discrepancy for the simulated data
	       chi2.sim[i,k] <- pow((y.sim[i,k]-e.sim[i,k]),2)/(e.sim[i,k]+e)
		     # 'e' to avoid division by zero


	   }

	}
	        
  # derived params
	# chi 2 discrepancy across data
	fit.actual <- sum ( chi2.actual[,])       
	fit.sim <- sum ( chi2.sim[,])
	# Bayesian p-value, compare actual and simulated data
	bpv <- step (fit.sim - fit.actual)

	# site occupancy probability
	mean.psi <- mean (psi[])
  fss <- sum (z[])
  mean.p <- mean(p[,])
      
      }
     ", fill=T)
sink()

# params to monitor
params <- c("mean.psi","fss","mean.p",
            "beta.grass","beta.agri","beta.tree",
            "alpha.temp","alpha.umid","alpha.area",
            "fit.actual","fit.sim","bpv")

# MCMC settings
ni <- 50000 # total number of iterations of MCMC
nb <- 40000 # adaptive phase
na <- 30000 # burn-in phase
nt <- 20 # thinning each
nc <- 3 # nchains

# bound data

res_mod <- lapply (list_det_data, function (i) { # for each species, run the model
  
  jags.data <- list (y=i,
                     # site covs
                     agri=std_agri,
                     grass=std_grass,
                     tree=std_tree,
                     # det covariates
                     det_area=std_det_area,
                     temp=det_table_temperatura_std,
                     umid=det_table_umidade_std,
                     # data
                     nsites = nrow(i),
                     nocc = ncol(i),
                     e= 0.0000001)
  
  # initial values
  zst <- apply (i,1,max,na.rm=T)
  inits <- function () {list(z=zst)}
  
  # call jags and run the model
  require(jagsUI)
  m1 <- jags (jags.data,
              inits,
              params,
              "model_acoustics.txt",
              n.chains=nc,
              n.thin=nt,
              n.iter=ni,
              n.adapt=na,
              n.burnin=nb,
              parallel=T,
              n.cores=nc)
  ;# return
  
  m1
})

# --------------------------------
# grassland model

# specify model in bugs language

sink ("model_acoustics_grass.txt")
cat ("
     
     model {
     
      # set priors
      # site occupancy
      int.psi ~ dunif (0,1)
      #beta.agri ~ dnorm (0,0.001)
      beta.grass ~ dnorm (0,0.001)
      #beta.tree ~ dnorm (0,0.001)
      
      # detection
      int.p ~ dunif (0,1)
      alpha.temp ~dnorm (0,001)
      alpha.umid ~dnorm (0,001)
      alpha.area ~dnorm (0,001)
      
      # ----------------------
      # model likelihood
      # site-occupancy model (first level)
      for (i in 1:nsites) {
      
        z[i] ~ dbern (psi[i])
        logit (psi[i]) <- int.psi + beta.grass*grass[i]
        
      }
       
       # ----------------------------------- #
       # detection model (second level)
       #----------------------------------- #
       
       # likelihood 
       
        for (i in 1:nsites) {
           for (k in 1:nocc) {
          
             y[i,k] ~ dbern (z[i] * p[i,k])
             logit (p[i,k]) <- int.p + alpha.temp*temp[i,k]+
                                       alpha.umid*umid[i,k]+
                                       alpha.area*det_area[i]
            
             }
          }
        
        # goodness of fit
	# predictive posterior checks
	# chi squared discrepancy

	for (i in 1:nsites) {
	   for (k in 1:nocc) {

	       y.sim [i,k] ~ dbern (e.sim[i,k]) # simulate data under the model
	       e.sim [i,k] <- z[i]*p[i,k]  # expected data under the model
	       # chi squared discrepancy for the actual data
	       chi2.actual[i,k] <- pow((y[i,k]-e.sim[i,k]),2)/(e.sim[i,k]+e)
	       # chi squared discrepancy for the simulated data
	       chi2.sim[i,k] <- pow((y.sim[i,k]-e.sim[i,k]),2)/(e.sim[i,k]+e)
		     # 'e' to avoid division by zero


	   }

	}
	        
  # derived params
	# chi 2 discrepancy across data
	fit.actual <- sum ( chi2.actual[,])       
	fit.sim <- sum ( chi2.sim[,])
	# Bayesian p-value, compare actual and simulated data
	bpv <- step (fit.sim - fit.actual)

	# site occupancy probability
	mean.psi <- mean (psi[])
  fss <- sum (z[])
      mean.p <- mean(p[,])
      }
     ", fill=T)
sink()

# params to monitor
params <- c("mean.psi","fss","beta.grass","mean.p",
            "alpha.temp","alpha.umid","alpha.area",
            "fit.actual","fit.sim","bpv")

# MCMC settings
ni <- 50000 # total number of iterations of MCMC
nb <- 40000 # adaptive phase
na <- 30000 # burn-in phase
nt <- 20 # thinning each
nc <- 3 # nchains

# bound data

res_mod_grass <- lapply (list_det_data, function (i) { # for each species, run the model
  
  jags.data <- list (y=i,
                     # site covs
                     grass=std_grass,
                     # det covariates
                     det_area=std_det_area,
                     temp=det_table_temperatura_std,
                     umid=det_table_umidade_std,
                     # data
                     nsites = nrow(i),
                     nocc = ncol(i),
                     e= 0.0000001)
  
  # initial values
  zst <- apply (i,1,max,na.rm=T)
  inits <- function () {list(z=zst)}
  
  # call jags and run the model
  require(jagsUI)
  m1 <- jags (jags.data,
              inits,
              params,
              "model_acoustics_grass.txt",
              n.chains=nc,
              n.thin=nt,
              n.iter=ni,
              n.adapt=na,
              n.burnin=nb,
              parallel=T,
              n.cores=nc)
  ;# return
  
  m1
})

# -----------------------------------------

# --------------------------------
# grassland model

# specify model in bugs language

sink ("model_acoustics_null.txt")
cat ("
     
    model {
       
        # set priors
        # site occupancy
        psi ~ dunif (0,1)
        
        # detection
        int.p ~ dunif (0,1)
        alpha.temp ~dnorm (0,001)
        alpha.umid ~dnorm (0,001)
        alpha.area ~dnorm (0,001)
        
        # ----------------------
        # model likelihood
        # site-occupancy model (first level)
        for (i in 1:nsites) {
        
          z[i] ~ dbern (psi)
          
        }
         
         # ----------------------------------- #
         # detection model (second level)
         #----------------------------------- #
         
         # likelihood 
         
          for (i in 1:nsites) {
             for (k in 1:nocc) {
            
               y[i,k] ~ dbern (z[i] * p[i,k])
               logit (p[i,k]) <- int.p + alpha.temp*temp[i,k]+
                                         alpha.umid*umid[i,k]+
                                         alpha.area*det_area[i]
              
               }
            }
          
          # goodness of fit
  	# predictive posterior checks
  	# chi squared discrepancy
  
  	for (i in 1:nsites) {
  	   for (k in 1:nocc) {
  
  	       y.sim [i,k] ~ dbern (e.sim[i,k]) # simulate data under the model
  	       e.sim [i,k] <- z[i]*p[i,k]  # expected data under the model
  	       # chi squared discrepancy for the actual data
  	       chi2.actual[i,k] <- pow((y[i,k]-e.sim[i,k]),2)/(e.sim[i,k]+e)
  	       # chi squared discrepancy for the simulated data
  	       chi2.sim[i,k] <- pow((y.sim[i,k]-e.sim[i,k]),2)/(e.sim[i,k]+e)
  		     # 'e' to avoid division by zero
  
  
  	   }
  
  	}
  	        
    # derived params
  	# chi 2 discrepancy across data
  	fit.actual <- sum ( chi2.actual[,])       
  	fit.sim <- sum ( chi2.sim[,])
  	# Bayesian p-value, compare actual and simulated data
  	bpv <- step (fit.sim - fit.actual)
  
  	# site occupancy probability
  	mean.psi <- mean (psi[])
    fss <- sum(z[])
     mean.p <- mean(p[,])   
      }
     ", fill=T)
sink()

# params to monitor
params <- c("mean.psi","fss","mean.p",
            "alpha.temp","alpha.umid","alpha.area",
            "fit.actual","fit.sim","bpv")

# MCMC settings
ni <- 50000 # total number of iterations of MCMC
nb <- 40000 # adaptive phase
na <- 30000 # burn-in phase
nt <- 20 # thinning each
nc <- 3 # nchains

# bound data

res_mod_null <- lapply (list_det_data, function (i) { # for each species, run the model
  
  jags.data <- list (y=i,
                     # site covs
                     # det covariates
                     det_area=std_det_area,
                     temp=det_table_temperatura_std,
                     umid=det_table_umidade_std,
                     # data
                     nsites = nrow(i),
                     nocc = ncol(i),
                     e= 0.0000001)
  
  # initial values
  zst <- apply (i,1,max,na.rm=T)
  inits <- function () {list(z=zst)}
  
  # call jags and run the model
  require(jagsUI)
  m1 <- jags (jags.data,
              inits,
              params,
              "model_acoustics_null.txt",
              n.chains=nc,
              n.thin=nt,
              n.iter=ni,
              n.adapt=na,
              n.burnin=nb,
              parallel=T,
              n.cores=nc)
  ;# return
  
  m1
})


# save model output
save (res_mod_null,
      res_mod,
      res_mod_grass,
      file=here("output", "model_output.RData"))

