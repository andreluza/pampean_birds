
# -----------------------------------
# 
#           MODELING
#
# -----------------------------------
rm(list=ls())


source ("R/packages.R")
load(here("output", "organized_data.RData"))


# ----------------
# your first model on unmarked
# -----------------
# organize data
data_unmarkedFrameOccu <- unmarkedFrameOccu(y=list_det_data[[1]], 
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
m0 <- occu(~1 ~1, data_unmarkedFrameOccu)
m1 <- occu(~temperatura ~grass_dry, data_unmarkedFrameOccu)
m2 <- occu(~temperatura+umidade ~grass_dry, data_unmarkedFrameOccu)
m3 <- occu(~temperatura+umidade+area ~grass_dry, data_unmarkedFrameOccu)
m4 <- occu(~temperatura+area ~grass_dry, data_unmarkedFrameOccu)
m5 <- occu(~umidade+area ~grass_dry, data_unmarkedFrameOccu)
m6 <- occu(~umidade ~grass_dry, data_unmarkedFrameOccu)
m7 <- occu(~area ~grass_dry, data_unmarkedFrameOccu)

(m0);m1;m2;m3;m4;m5;m6;m7


# --------------------------------
# null model no det no occ

# specify model in bugs language

sink (here ("BUGS_code","psi(.)_p(.).txt"))
cat ("
     
    model {
       
        # set priors
        # site occupancy
        psi ~ dunif (0,1)
        p  ~ dunif (0,1)
        
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
       
        for (k in 1:nobs) {
          
             y[k] ~ dbern (z[site[k]] * p)
             
          }
        
  # goodness of fit
	# predictive posterior checks
	# chi squared discrepancy

	for (k in 1:nobs) {

	       y.sim [k] ~ dbern (z[site[k]]*p) # simulate data under the model
	       e.sim [k] <- z[site[k]]*p  # expected data under the model
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
            "psi",
            "p",
            "fit.actual",
            "fit.sim",
            "bpv")

# MCMC settings
ni <- 50000 # total number of iterations of MCMC
nb <- 40000 # adaptive phase
na <- 30000 # burn-in phase
nt <- 20 # thinning each
nc <- 3 # nchains

# bound data

res_mod_null_null<-lapply (long_data, function (i) { # for each species, run the model
  
  jags.data <- list (y=i$det,
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
              here ("BUGS_code","psi(.)_p(.).txt"),
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

# -----------------------------------------------
# detection area


# specify model in bugs language

sink (here ("BUGS_code","psi(.)_p(A).txt"))
cat ("
     
     model {
     
      # set priors
      # site occupancy
      psi.u ~ dunif (0,1)
      
      # detection
      p.u ~ dunif (0,1)
      int.p <- logit (p.u)
      alpha.area ~dnorm (0,0.1)
      
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
             logit (p[k]) <- int.p + alpha.area*det_area[site[k]]
            
             
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
            "fss",
            "mean.p",
            "psi.u",
            "p.u",
            "int.p",
            "alpha.area",
            "fit.actual",
            "fit.sim",
            "bpv")


# bound data
res_mod_det_area <- lapply (long_data, function (i) { # for each species, run the model
  
  jags.data <- list (y=i$det,
                     # det covariates
                     det_area=std_det_area,
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
              here ("BUGS_code","psi(.)_p(A).txt"),
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


# ------------------------------------------------------------------
# turno


# specify model in bugs language

sink (here ("BUGS_code","psi(.)_p(Tu).txt"))
cat ("
     
     model {
     
      # set priors
      # site occupancy
      psi.u ~ dunif (0,1)
      
      # detection
      p.u ~ dunif (0,1)
      int.p <- logit (p.u)
      alpha.turno ~dnorm (0,0.1)
      
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
             logit (p[k]) <- int.p + alpha.turno*turno[k]
            
             
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
            "fss",
            "mean.p",
            "psi.u",
            "p.u",
            "int.p",
            "alpha.turno",
            "fit.actual",
            "fit.sim",
            "bpv")


# bound data
res_mod_det_turno <- lapply (long_data, function (i) { # for each species, run the model
  
  
  jags.data <- list (y=i$det,
                     # det covariates
                     turno=as.numeric(as.factor(i$turno)),
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
              here ("BUGS_code","psi(.)_p(Tu).txt"),
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

# ---------------------------------------------------------------
# temperature and humidity

# specify model in bugs language

sink (here ("BUGS_code","psi(.)_p(T-H).txt"))
cat ("
     
     model {
     
      # set priors
      # site occupancy
      psi ~ dunif (0,1)
      
      # detection
      p.u ~ dunif (0,1)
      int.p <- logit (p.u)
      alpha.temp ~dnorm (0,0.1)
      alpha.umid ~dnorm (0,0.1)
      
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
       
        for (k in 1:nobs) {
          
             y[k] ~ dbern (z[site[k]] * p[k])
             logit (p[k]) <- int.p + alpha.temp*temp[k]+
                                     alpha.umid*umid[k]
             
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
            "psi",
            "p.u",
            "int.p",
            "alpha.temp",
            "alpha.umid",
            "fit.actual",
            "fit.sim",
            "bpv")

# bound data

res_mod_tempHUmid <- lapply (long_data, function (i) { # for each species, run the model
  
  jags.data <- list (y=i$det,
                     # det covariates
                     temp=i$temp,
                     umid=i$humid,
                     
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
              here ("BUGS_code","psi(.)_p(T-H).txt"),
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


# -----------------------------------------------
# complete just without area

# specify model in bugs language

sink (here ("BUGS_code","psi(D-R-A-T)_p(T-H).txt"))
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
                                     alpha.umid*umid[k]
            
             
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
            "beta.grass",
            "beta.res",
            "beta.agri",
            "beta.tree",
            "alpha.temp",
            "alpha.umid",
            "fit.actual",
            "fit.sim",
            "bpv")

# bound data
res_mod_tempHUmid_habCov <- lapply (long_data, function (i) { # for each species, run the model
  
  jags.data <- list (y=i$det,
                     # site covs
                     agri=std_agri,
                     grass_dry=std_grass_dry,
                     grass_res=std_grass_reg,
                     tree=std_tree,
                     # det covariates
                     temp=i$temp,
                     umid=i$humid,
                     
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
              here ("BUGS_code","psi(D-R-A-T)_p(T-H).txt"),
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

# -------------------------------------------

# area detection, habitat covs

sink (here ("BUGS_code","psi(D-R-A-T)_p(A).txt"))
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
      alpha.area ~dnorm (0,0.1)
      
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
             logit (p[k]) <- int.p + alpha.area*det_area[site[k]]
            
             
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
            "beta.grass",
            "beta.res",
            "beta.agri",
            "beta.tree",
            "alpha.area",
            "fit.actual",
            "fit.sim",
            "bpv")

# bound data
res_mod_area_habCov <- lapply (long_data, function (i) { # for each species, run the model
  
  jags.data <- list (y=i$det,
                     # site covs
                     agri=std_agri,
                     grass_dry=std_grass_dry,
                     grass_res=std_grass_reg,
                     tree=std_tree,
                     
                     # det covariates
                     det_area=std_det_area,
                     
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
              here ("BUGS_code","psi(D-R-A-T)_p(A).txt"),
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


# -------------------------------------------

# turno detection, habitat covs

sink (here ("BUGS_code","psi(D-R-A-T)_p(Tu).txt"))
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
      alpha.turno ~dnorm (0,0.1)
      
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
             logit (p[k]) <- int.p +  alpha.turno*turno[k]
            
             
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
            "beta.grass",
            "beta.grass2",
            "beta.res",
            "beta.agri",
            "beta.agri2",
            "beta.tree",
            "alpha.turno",
            "fit.actual",
            "fit.sim",
            "bpv")

# bound data
res_mod_turno_habCov <- lapply (long_data, function (i) { # for each species, run the model
  
  # bundle data
  jags.data <- list (y=i$det,
                     # site covs
                     agri=std_agri,
                     grass_dry=std_grass_dry,
                     grass_res=std_grass_reg,
                     tree=std_tree,
                     
                     # det covariates
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
              here ("BUGS_code","psi(D-R-A-T)_p(Tu).txt"),
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


# --------------------------------------------

# habitat - constant detection

# --------------------------------------------


sink (here ("BUGS_code","psi(D-R-A-T)_p(.).txt"))
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
      p ~ dunif (0,1)
      
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
          
             y[k] ~ dbern (z[site[k]] * p)
             
          }
        
  # goodness of fit
	# predictive posterior checks
	# chi squared discrepancy

	for (k in 1:nobs) {

	       y.sim [k] ~ dbern (z[site[k]]*p) # simulate data under the model
	       e.sim [k] <- z[site[k]]*p  # expected data under the model
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
            "p",
            "int.psi",
            "beta.grass",
            "beta.grass2",
            "beta.res",
            "beta.agri",
            "beta.agri2",
            "beta.tree",
            "fit.actual",
            "fit.sim",
            "bpv")


# bound data
res_detConstHab <- lapply (long_data, function (i) { # for each species, run the model
  
  jags.data <- list (y=i$det,
                     # site covs
                     agri=std_agri,
                     grass_dry=std_grass_dry,
                     grass_res=std_grass_reg,
                     tree=std_tree,
                     
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
              here ("BUGS_code","psi(D-R-A-T)_p(.).txt"),
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


# save
save (res_mod_null_null,
      res_mod_det_area,
      res_mod_tempHUmid,
      res_mod_det_turno,
      res_mod_tempHUmid_habCov,
      res_mod_turno_habCov ,
      res_detConstHab,
      res_mod_area_habCov,
      file=here("output", "model_output_det.RData"))
