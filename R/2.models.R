
# -----------------------------------
# 
#           SITE OCCUPANCY MODELS 
#       FITTED TO DATA OF EACH SPECIES
#
# COVARIATE CODES IN THE NAMES OF EACH CODE

# 
#   psi(D-R-A-T)_p(T-H-A-Tu) # the combination of covariates in the complete model(others will be a subset)
#
# Site-level covariates
# D = Dry grassland
# R = Regenerating grassland
# A = Agriculture
# T = Trees
#
# Observation-level covariates
# T = Temperature
# H = Humidity
# A = Detection area (vary only across sites)
# Tu = Hour of the day (vary only across surveys)

# -----------------------------------


source ("R/packages.R")
load(here("output", "organized_data.RData"))


# create a directory to host BUGS codes
dir.create ("BUGS_code")

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


# ----------------
# your first model on unmarked
# -----------------
# organize data
data_unmarkedFrameOccu <- unmarkedFrameOccu(y=tab_det_sp[,,1], 
                                            siteCovs=data.frame(grass_dry=std_grass_dry,
                                                                grass_reg=std_grass_reg,
                                                                agri=std_agri,
                                                                tree=std_tree,
                                                                area=std_det_area), 
                                            obsCovs=list (temperatura = det_table_temperatura_std,
                                                          umidade = det_table_umidade_std
                                            ))
summary(data_unmarkedFrameOccu)

# rodar o modelo
# modelo com efeito da temp, umidade, e sua interacao
m0 <- occu(~temperatura+umidade+area ~1, data_unmarkedFrameOccu)
m1 <- occu(~temperatura+umidade+area ~grass_dry, data_unmarkedFrameOccu)
m2 <- occu(~temperatura+umidade+area ~grass_reg, data_unmarkedFrameOccu)
m3 <- occu(~temperatura+umidade+area ~grass_dry+grass_reg, data_unmarkedFrameOccu)
m4 <- occu(~temperatura+umidade+area ~grass_dry+tree+agri, data_unmarkedFrameOccu)
m5 <- occu(~temperatura+umidade+area ~grass_dry+grass_reg+tree+agri, data_unmarkedFrameOccu)

(m0);m1;m2;m3;m4;m5

# have fun exploring how to organize and include site covariates, and other species

# --------------------------#
#       complete model
# -------------------------- #


# MCMC settings
ni <- 50000 # total number of iterations of MCMC
nb <- 40000 # adaptive phase
na <- 30000 # burn-in phase
nt <- 20 # thinning each
nc <- 3 # nchains


# specify model in bugs language

sink (here ("BUGS_code","psi(D-R-A-T)_p(T-H-A-Tu).txt"))
cat ("
     
     model {
     
      # set priors
      # site occupancy
      psi.u ~ dunif (0,1)
      int.psi <- logit (psi.u)
      beta.agri ~ dnorm (0,0.1)
      beta.grass ~ dnorm (0,0.1)
      beta.res ~ dnorm (0,0.1)
      beta.tree ~ dnorm (0,0.1)
      
      # detection
      p.u ~ dunif (0,1)
      int.p <- logit (p.u)
      alpha.temp ~dnorm (0,0.1)
      alpha.umid ~dnorm (0,0.1)
      alpha.area ~dnorm (0,0.1)
      alpha.turno~dnorm (0,0.1)
      
      # ----------------------
      # model likelihood
      # site-occupancy model (first level)
      for (i in 1:nsites) {
      
        z[i] ~ dbern (psi[i])
        logit (psi[i]) <- int.psi + beta.agri*agri[i]+
                                    beta.grass*grass_dry[i]+
                                    beta.res*grass_res[i]+
                                    beta.tree*tree[i]
        
      }
       
       
       # ----------------------------------- #
       # detection model (second level)
       #----------------------------------- #
       
       # likelihood 
       
        for (k in 1:nobs) {
          
             y[k] ~ dbern (z[site[k]] * p[k])
             logit (p[k]) <- int.p + alpha.temp*temp[k]+
                                     alpha.umid*umid[k]+
                                     alpha.area*det_area[site[k]]+
                                     alpha.turno*turno[k]
            
             
          }
        
  # goodness of fit
	# predictive posterior checks
	# chi squared discrepancy

	for (k in 1:nobs) {

	       y.sim [k] ~ dbern (z[site[k]]*p[k]) # simulate data under the model
	       e.sim [k] <- z[site[k]]*p[k]  # expected data under the model
	       # chi squared discrepancy for the actual data
	       chi2.actual[k] <- pow((y[k]-e.sim[k]),2)/(e.sim[k]+e)
	       # chi squared discrepancy for the simulated data
	       chi2.sim[k] <- pow((y.sim[k]-e.sim[k]),2)/(e.sim[k]+e)
		     # 'e' to avoid division by zero


	   }

	
    	        
      # derived params
    	# chi 2 discrepancy across data
    	fit.actual <- sum ( chi2.actual)       
    	fit.sim <- sum ( chi2.sim)
    	# Bayesian p-value, compare actual and simulated data
    	bpv <- step (fit.sim - fit.actual)
    
    	# site occupancy probability
    	mean.psi <- mean(psi[])
      fss <-sum(z[])
      mean.p <-mean(p)
  
  
      
      }
     ", fill=T)
sink()

# params to monitor
params <- c("mean.psi",
            "fss",
            "mean.p",
            "psi.u",
            "p.u",
            "int.psi",
            "int.p",
            "beta.grass",
            "beta.res",
            "beta.agri",
            "beta.tree",
            "alpha.temp",
            "alpha.umid",
            "alpha.area",
            "alpha.turno",
            "fit.actual",
            "fit.sim",
            "bpv")


# bound data

res_mod <- lapply (long_data, function (i) { # for each species, run the model

  
  # bundle data  
  jags.data <- list (y=i$det,
                     # site covs
                     agri=std_agri,
                     grass_dry=std_grass_dry,
                     grass_res=std_grass_reg,
                     tree=std_tree,
                     # det covariates
                     det_area=std_det_area,
                     temp=i$temp,
                     umid=i$humid,
                     turno = as.numeric(as.factor(i$turno)),
                     
                     # data
                     site = as.numeric(i$site),
                     survey=i$surveyNum,
                     nobs = nrow(i),
                     nsites = length(unique(i$site)),
                     nocc =length(unique(i$survey)),
                     e= 0.0000001)
  
  # initial values
  zst <- tapply(i$det,i$site, FUN = max)
  inits <- function () {list(z=zst)}
  
  # call jags and run the model
  
  m1 <- jags (jags.data,
              inits,
              params,
              here ("BUGS_code","psi(D-R-A-T)_p(T-H-A-Tu).txt"),
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
# --------------------------------


sink (here ("BUGS_code","psi(D-R)_p(T-H-A-Tu).txt"))
cat ("
     
     
     model {
     
      # set priors
      # site occupancy
      psi.u ~ dunif (0,1)
      int.psi <- logit (psi.u)
      beta.grass ~ dnorm (0,0.1)
      beta.res ~ dnorm (0,0.1)
      
      # detection
      p.u ~ dunif (0,1)
      int.p <- logit (p.u)
      alpha.temp ~dnorm (0,0.1)
      alpha.umid ~dnorm (0,0.1)
      alpha.area ~dnorm (0,0.1)
      alpha.turno~dnorm (0,0.1)
      
      # ----------------------
      # model likelihood
      # site-occupancy model (first level)
      for (i in 1:nsites) {
      
        z[i] ~ dbern (psi[i])
        logit (psi[i]) <- int.psi + beta.grass*grass_dry[i]+
                                    beta.res*grass_res[i]
        
      }
       
       # ----------------------------------- #
       # detection model (second level)
       #----------------------------------- #
       
       # likelihood 
       
        for (k in 1:nobs) {
          
             y[k] ~ dbern (z[site[k]] * p[k])
             logit (p[k]) <- int.p + alpha.temp*temp[k]+
                                     alpha.umid*umid[k]+
                                     alpha.area*det_area[site[k]]+
                                     alpha.turno*turno[k]
            
             
          }
        
  # goodness of fit
	# predictive posterior checks
	# chi squared discrepancy

	for (k in 1:nobs) {

	       y.sim [k] ~ dbern (z[site[k]]*p[k]) # simulate data under the model
	       e.sim [k] <- z[site[k]]*p[k]  # expected data under the model
	       # chi squared discrepancy for the actual data
	       chi2.actual[k] <- pow((y[k]-e.sim[k]),2)/(e.sim[k]+e)
	       # chi squared discrepancy for the simulated data
	       chi2.sim[k] <- pow((y.sim[k]-e.sim[k]),2)/(e.sim[k]+e)
		     # 'e' to avoid division by zero


	   }

	
	        
  # derived params
	# chi 2 discrepancy across data
	fit.actual <- sum ( chi2.actual)       
	fit.sim <- sum ( chi2.sim)
	# Bayesian p-value, compare actual and simulated data
	bpv <- step (fit.sim - fit.actual)

	# site occupancy probability
	mean.psi <- mean (psi)
  fss <- sum (z)
  mean.p <- mean(p)
  
  
     }
      
      
     ", fill=T)
sink()

# params to monitor
params <- c("mean.psi",
            "fss",
            "psi.u",
            "p.u",
            "int.psi",
            "int.p",
            "beta.grass",
            "beta.res",
            "mean.p",
            "alpha.temp",
            "alpha.umid",
            "alpha.area",
            "alpha.turno",
            "fit.actual",
            "fit.sim",
            "bpv")


# bound data
res_mod_grass <- lapply (long_data, function (i) { # for each species, run the model
  
  
  # bundle data  
  jags.data <- list (y=i$det,
                     # site covs
                     grass_dry=std_grass_dry,
                     grass_res=std_grass_reg,
                     # det covariates
                     det_area=std_det_area,
                     temp=i$temp,
                     umid=i$humid,
                     turno = as.numeric(as.factor(i$turno)),
                     
                     # data
                     site = as.numeric(i$site),
                     survey=i$surveyNum,
                     nobs = nrow(i),
                     nsites = length(unique(i$site)),
                     nocc =length(unique(i$survey)),
                     e= 0.0000001)
  
  
  # initial values
  zst <- tapply(i$det,i$site, FUN = max)  # initial values
  inits <- function () {list(z=zst)}
  
  # call jags and run the model

  m1 <- jags (jags.data,
              inits,
              params,
              here ("BUGS_code","psi(D-R)_p(T-H-A-Tu).txt"),
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


# -----------------------------------
# model without grassland cover
# -----------------------------------


sink (here ("BUGS_code","psi(A-T)_p(T-H-A-Tu).txt"))
cat ("
     
      model {
     
      # set priors
      # site occupancy
      psi.u ~ dunif (0,1)
      int.psi <- logit (psi.u)
      beta.agri ~ dnorm (0,0.1)
      beta.tree ~ dnorm (0,0.1)
      
      # detection
      p.u ~ dunif (0,1)
      int.p <- logit (p.u)
      alpha.temp ~dnorm (0,0.1)
      alpha.umid ~dnorm (0,0.1)
      alpha.area ~dnorm (0,0.1)
      alpha.turno~dnorm (0,0.1)
      
      # ----------------------
      # model likelihood
      # site-occupancy model (first level)
      for (i in 1:nsites) {
      
        z[i] ~ dbern (psi[i])
        logit (psi[i]) <- int.psi + beta.agri*agri[i]+
                                    beta.tree*tree[i]
        
      }
       
       # ----------------------------------- #
       # detection model (second level)
       #----------------------------------- #
       
       # likelihood 
       
        for (k in 1:nobs) {
          
             y[k] ~ dbern (z[site[k]] * p[k])
             logit (p[k]) <- int.p + alpha.temp*temp[k]+
                                     alpha.umid*umid[k]+
                                     alpha.area*det_area[site[k]]+
                                     alpha.turno*turno[k]
            
             
          }
        
  # goodness of fit
	# predictive posterior checks
	# chi squared discrepancy

	for (k in 1:nobs) {

	       y.sim [k] ~ dbern (z[site[k]]*p[k]) # simulate data under the model
	       e.sim [k] <- z[site[k]]*p[k]  # expected data under the model
	       # chi squared discrepancy for the actual data
	       chi2.actual[k] <- pow((y[k]-e.sim[k]),2)/(e.sim[k]+e)
	       # chi squared discrepancy for the simulated data
	       chi2.sim[k] <- pow((y.sim[k]-e.sim[k]),2)/(e.sim[k]+e)
		     # 'e' to avoid division by zero


	   }

	
	        
  # derived params
	# chi 2 discrepancy across data
	fit.actual <- sum ( chi2.actual)       
	fit.sim <- sum ( chi2.sim)
	# Bayesian p-value, compare actual and simulated data
	bpv <- step (fit.sim - fit.actual)

	# site occupancy probability
	mean.psi <- mean (psi)
  fss <- sum (z)
  mean.p <- mean(p)
      
      }
     ", fill=T)
sink()

# params to monitor
params <- c("mean.psi",
            "fss",
            "mean.p",
            "psi.u",
            "p.u",
            "int.psi",
            "int.p",
            "beta.agri",
            "beta.tree",
            "alpha.temp",
            "alpha.umid",
            "alpha.area",
            "alpha.turno",
            "fit.actual",
            "fit.sim",
            "bpv")

# bound data
res_mod_no_grass <- lapply (long_data, function (i) { # for each species, run the model
  
  
  # bundle data  
  jags.data <- list (y=i$det,
                     # site covs
                     agri=std_agri,
                     tree=std_tree,
                     # det covariates
                     det_area=std_det_area,
                     temp=i$temp,
                     umid=i$humid,
                     turno = as.numeric(as.factor(i$turno)),
                     
                     # data
                     site = as.numeric(i$site),
                     survey=i$surveyNum,
                     nobs = nrow(i),
                     nsites = length(unique(i$site)),
                     nocc =length(unique(i$survey)),
                     e= 0.0000001)
  
  # initial values
  zst <- tapply(i$det,i$site, FUN = max)
  inits <- function () {list(z=zst)}
  
  # call jags and run the model
  
  m1 <- jags (jags.data,
              inits,
              params,
              here ("BUGS_code","psi(A-T)_p(T-H-A-Tu).txt"),
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
# null model of psi
# --------------------------------

sink (here ("BUGS_code","psi(.)_p(T-H-A-Tu).txt"))
cat ("
     
       model {
     
      # set priors
      # site occupancy
      psi.u ~ dunif (0,1)
      
      # detection
      p.u ~ dunif (0,1)
      int.p <- logit (p.u)
      alpha.temp ~dnorm (0,0.1)
      alpha.umid ~dnorm (0,0.1)
      alpha.area ~dnorm (0,0.1)
      alpha.turno~dnorm (0,0.1)
      
      # ----------------------
      # model likelihood
      # site-occupancy model (first level)
      for (i in 1:nsites) {
      
        z[i] ~ dbern (psi.u)
        
      }
       
       # ----------------------------------- #
       # detection model (second level)
       #----------------------------------- #
       
       # likelihood 
       
        for (k in 1:nobs) {
          
             y[k] ~ dbern (z[site[k]] * p[k])
             logit (p[k]) <- int.p + alpha.temp*temp[k]+
                                     alpha.umid*umid[k]+
                                     alpha.area*det_area[site[k]]+
                                     alpha.turno*turno[k]
            
             
          }
        
  # goodness of fit
	# predictive posterior checks
	# chi squared discrepancy

	for (k in 1:nobs) {

	       y.sim [k] ~ dbern (z[site[k]]*p[k]) # simulate data under the model
	       e.sim [k] <- z[site[k]]*p[k]  # expected data under the model
	       # chi squared discrepancy for the actual data
	       chi2.actual[k] <- pow((y[k]-e.sim[k]),2)/(e.sim[k]+e)
	       # chi squared discrepancy for the simulated data
	       chi2.sim[k] <- pow((y.sim[k]-e.sim[k]),2)/(e.sim[k]+e)
		     # 'e' to avoid division by zero


	   }

	
	        
  # derived params
	# chi 2 discrepancy across data
	fit.actual <- sum ( chi2.actual)       
	fit.sim <- sum ( chi2.sim)
	# Bayesian p-value, compare actual and simulated data
	bpv <- step (fit.sim - fit.actual)

	# site occupancy probability
	mean.psi <- mean (psi.u)
  fss <- sum (z)
  mean.p <- mean(p)
      
      }
      
     ", fill=T)
sink()

# params to monitor
params <- c("mean.psi",
            "psi.u",
            "p.u",
            "int.p",
            "fss",
            "mean.p",
            "alpha.temp",
            "alpha.umid",
            "alpha.area",
            "alpha.turno",
            "fit.actual",
            "fit.sim",
            "bpv")

# bound data
res_mod_null<-lapply (long_data, function (i) { # for each species, run the model
  
  
  # bundle data  
  jags.data <- list (y=i$det,
                     # site covs
                     agri=std_agri,
                     tree=std_tree,
                     # det covariates
                     det_area=std_det_area,
                     temp=i$temp,
                     umid=i$humid,
                     turno = as.numeric(as.factor(i$turno)),
                     
                     # data
                     site = as.numeric(i$site),
                     survey=i$surveyNum,
                     nobs = nrow(i),
                     nsites = length(unique(i$site)),
                     nocc =length(unique(i$survey)),
                     e= 0.0000001)
  
  # initial values
  zst <- tapply(i$det,i$site, FUN = max)
  inits <- function () {list(z=zst)}
  
  
  # call jags and run the model

  m1 <- jags (jags.data,
              inits,
              params,
              here ("BUGS_code","psi(.)_p(T-H-A-Tu).txt"),
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
      res_mod_no_grass,
      file=here("output", "model_output.RData"))

# end