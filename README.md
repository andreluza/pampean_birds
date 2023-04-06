Landscape and the occurrence of Pampean birds: modeling site occupancy
through acoustic detection
================
P. Paludo, A.L. Luza, M.J.R. Pereira
2023-04-06

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

## The models’ design looks like:

### Global model


         
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
         

### Grassland model


         
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
         

### Null model


         
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
         

## This paper was produced using the following software and associated packages:

    ## R version 4.2.2 (2022-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19044)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
    ## [3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.utf8    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] gridExtra_2.3     forcats_0.5.2     stringr_1.5.0     dplyr_1.1.0      
    ##  [5] purrr_1.0.1       readr_2.1.3       tidyr_1.3.0       tibble_3.1.8     
    ##  [9] ggplot2_3.4.0     tidyverse_1.3.2   jagsUI_1.5.2.9002 here_1.0.1       
    ## [13] unmarked_1.2.5    reshape_0.8.9    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.10         lubridate_1.9.1     lattice_0.20-45    
    ##  [4] assertthat_0.2.1    rprojroot_2.0.3     digest_0.6.31      
    ##  [7] utf8_1.2.2          R6_2.5.1            cellranger_1.1.0   
    ## [10] plyr_1.8.8          backports_1.4.1     reprex_2.0.2       
    ## [13] evaluate_0.20       coda_0.19-4         httr_1.4.4         
    ## [16] pillar_1.8.1        rlang_1.0.6         googlesheets4_1.0.1
    ## [19] readxl_1.4.1        rstudioapi_0.14     rjags_4-13         
    ## [22] rmarkdown_2.20      googledrive_2.0.0   munsell_0.5.0      
    ## [25] broom_1.0.3         compiler_4.2.2      modelr_0.1.10      
    ## [28] xfun_0.36           pkgconfig_2.0.3     htmltools_0.5.4    
    ## [31] tidyselect_1.2.0    fansi_1.0.4         crayon_1.5.2       
    ## [34] tzdb_0.3.0          dbplyr_2.3.0        withr_2.5.0        
    ## [37] MASS_7.3-58.1       grid_4.2.2          jsonlite_1.8.4     
    ## [40] gtable_0.3.1        lifecycle_1.0.3     DBI_1.1.3          
    ## [43] magrittr_2.0.3      scales_1.2.1        stringi_1.7.12     
    ## [46] cli_3.6.0           pbapply_1.7-0       fs_1.6.0           
    ## [49] xml2_1.3.3          ellipsis_0.3.2      generics_0.1.3     
    ## [52] vctrs_0.5.2         tools_4.2.2         glue_1.6.2         
    ## [55] hms_1.1.2           parallel_4.2.2      fastmap_1.1.0      
    ## [58] yaml_2.3.7          timechange_0.2.0    colorspace_2.1-0   
    ## [61] gargle_1.2.1        rvest_1.0.3         knitr_1.42         
    ## [64] haven_2.5.1
