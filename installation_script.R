pckg_install_ICCI()
pckg_install()
pckg_load()

IHRC::run_ana_setup_file("/scratch/project_2007428/users/detrois/scripts/onset_pred/standard_setup_prs_phers.tsv")

#########################################################################################
######## PACKAGES #######################################################################
#########################################################################################
#' Installs the ICCI package along with its dependency comorbidity
#'
#' @param target_dir R directory for installing the package
pckg_install_ICCI <- function(target_dir="/scratch/project_2007428/users/detrois/rpckgs/") {
  .libPaths(c("/scratch/project_2007428/users/detrois/rpckgs/", .libPaths()))  
  libpath <- .libPaths()[1]
  install.packages("/scratch/project_2007428/users/detrois/rpckgs/comorbidity_1.0.7.tar.gz",
                   target_dir, 
                   repos=NULL, 
                   type="source")
  library(comorbidity)
  install.packages("/scratch/project_2007428/users/detrois/rpckgs/ICCI_2.3.0.tar.gz",
                   target_dir, 
                   repos=NULL, 
                   type="source")
  library(ICCI)
}
#' Installs personal packages in the sandbox
pckg_install <- function() {
  .libPaths(c("/scratch/project_2007428/users/detrois/rpckgs/", .libPaths()))  
  libpath <- .libPaths()[1]
  install.packages("/scratch/project_2007428/users/detrois/rpckgs/Istudy_3.3.0.tar.gz",
                   "/scratch/project_2007428/users/detrois/rpckgs/", 
                   repos=NULL, 
                   type="source")
  library(Istudy)
  
  install.packages("/scratch/project_2007428/users/detrois/rpckgs/IUtils_2.1.0.tar.gz",
                   "/scratch/project_2007428/users/detrois/rpckgs/", 
                   repos=NULL, 
                   type="source")
  library(IUtils)


  install.packages("/scratch/project_2007428/users/detrois/rpckgs/IHRC_4.0.0.tar.gz",
                   "/scratch/project_2007428/users/detrois/rpckgs/", 
                   repos=NULL, 
                   type="source")
  library(IHRC)
}

#' Loads personal packages in the sandbox
pckg_load <- function() {
  #rm(list = ls())
  .libPaths(c("/scratch/project_2007428/users/detrois/rpckgs/", .libPaths()))  
  library(Istudy)
  library(IUtils)
  library(ICCI)
  library(IHRC)
}
