Landscape and the occurrence of Pampean birds: modeling site occupancy
through acoustic detection
================
P. Paludo, A.L. Luza, M.J.R. Pereira
2024-01-07

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

# List of folders

Root  
\|  
\|- BUGS_code: The 12 models were written in BUGS language and are
stored within this folder;  
\|  
\|- data: Data used in the analyzes. We used an imputed version of data
in which missing observation covariates (temperature, moisture) were
imputed by hand based on neighbor sites and sampling days (made by P.
Paludo)  
\|  
\|- figures: main figures;  
\|  
\|- output: processed data and model output;  
\|—— model_output.RData: output produced by running the code
“2.models.R”  
\|—— model_output_det.RData: output produced by running the code
“2a.modelsDetection.R”  
\|—— organized_data.RData: output (occupancy data) produced by running
the code “1.organizing_data.R”  
\|  
\|- R: R code as follows:  
\|—— “functions.R”: useful functions to formatting the occupancy data  
\|—— “packages.R”: required R packages  
\|—— “1.organizing_data.R”: organize occupancy data (processed data)  
\|—— “2.models.R”: BUGS models and analyzes - change occupancy
covariates  
\|—— “2a.modelsDetection.R”: BUGS models and analyzes - mostly changing
the combination of detection covariates  
\|—— “3.MAIN_model_selection_figs.R”: code to produce results shown in
the main text  
\|—— “3.SUPP_A10_psi(D-R-A-T)\_p(T-H).R”: Code for appendix A10  
\|—— “3.SUPP_A11_psi(D-R-A-T)\_p(A).R”: Code for appendix A11  
\|—— “3.SUPP_A12_psi(D-R-A-T)\_p(Tu).R”: Code for appendix A12  
\|—— “3.SUPP_A13_psi(D-R-A-T)\_p(.).R”: Code for appendix A13  
\|—— “3.SUPP_A2_psi(D-R-A-T)\_p(T-H-A-Tu).R”: Code for appendix A2  
\|—— “3.SUPP_A3_psi(D-R)\_p(T-H-A-Tu).R”: Code for appendix A3  
\|—— “3.SUPP_A4_psi(A-T)\_p(T-H-A-Tu).R”: Code for appendix A4  
\|—— “3.SUPP_A5_psi(.)\_p(T-H-A-Tu).R”: Code for appendix A5  
\|—— “3.SUPP_A6_psi(.)\_p(.).R”: Code for appendix A6  
\|—— “3.SUPP_A7_psi(.)\_p(A).R”: Code for appendix A7  
\|—— “3.SUPP_A8_psi(.)\_p(Tu).R”: Code for appendix A8  
\|—— “3.SUPP_A9_psi(.)\_p(T-H).R”: Code for appendix A9  
\|  
\|- supporting_info: Rmarkdown and docx files showing the supporting
information associated to the article  

## This paper was produced using the following software and associated packages:

    ## R version 4.4.1 (2024-06-14 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
    ## [3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.utf8    
    ## 
    ## time zone: Europe/Paris
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] ggridges_0.5.6  gridExtra_2.3   lubridate_1.9.3 forcats_1.0.0  
    ##  [5] stringr_1.5.1   dplyr_1.1.4     purrr_1.0.2     readr_2.1.5    
    ##  [9] tidyr_1.3.1     tibble_3.2.1    ggplot2_3.5.1   tidyverse_2.0.0
    ## [13] jagsUI_1.6.2    here_1.0.1      unmarked_1.4.1  reshape_0.8.9  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] utf8_1.2.4        generics_0.1.3    stringi_1.8.4     lattice_0.22-6   
    ##  [5] hms_1.1.3         digest_0.6.35     magrittr_2.0.3    evaluate_1.0.1   
    ##  [9] grid_4.4.1        timechange_0.3.0  fastmap_1.2.0     rprojroot_2.0.4  
    ## [13] plyr_1.8.9        fansi_1.0.6       scales_1.3.0      cli_3.6.2        
    ## [17] rlang_1.1.3       munsell_0.5.1     withr_3.0.0       yaml_2.3.8       
    ## [21] tools_4.4.1       parallel_4.4.1    tzdb_0.4.0        colorspace_2.1-1 
    ## [25] vctrs_0.6.5       R6_2.5.1          lifecycle_1.0.4   MASS_7.3-60.2    
    ## [29] pkgconfig_2.0.3   pillar_1.9.0      gtable_0.3.5      glue_1.7.0       
    ## [33] Rcpp_1.0.13       xfun_0.44         tidyselect_1.2.1  rstudioapi_0.16.0
    ## [37] knitr_1.48        htmltools_0.5.8.1 rmarkdown_2.28    compiler_4.4.1
