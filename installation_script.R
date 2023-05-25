pckg_install_ICCI()
pckg_install()

IHRC::run_ana_setup_file("standard_setup_ukbb.tsv")

#########################################################################################
######## PACKAGES #######################################################################
#########################################################################################
#' Installs the ICCI package along with its dependency comorbidity
#'
#' @param target_dir R directory for installing the package
pckg_install_ICCI <- function(target_dir="/home/ivm/R/x86_64-pc-linux-gnu-library/4.2/") {
  .libPaths(target_dir, .libPaths())
  
  install.packages("/finngen/green/detrois_up/pckg_share/comorbidity_1.0.7.tar.gz",
                   target_dir, 
                   repos=NULL, 
                   type="source")
  library(comorbidity)
  install.packages("/finngen/green/detrois_up/pckg_share/ICCI_2.2.2.tar.gz",
                   target_dir, 
                   repos=NULL, 
                   type="source")
  library(ICCI)
}
#' Installs personal packages in the sandbox
pckg_install <- function(target_dir="/home/ivm/R/x86_64-pc-linux-gnu-library/4.2/") {
  .libPaths(target_dir, .libPaths())

  install.packages("/home/ivm/R/pckg_uploads/Istudy_3.3.0.tar.gz",
                   target_dir, 
                   repos=NULL, 
                   type="source")
  library(Istudy)
  
  install.packages("/home/ivm/R/pckg_uploads/IUtils_2.1.0.tar.gz",
                   target_dir, 
                   repos=NULL, 
                   type="source")
  library(IUtils)


  install.packages("/home/ivm/R/pckg_uploads/IHRC_4.0.0.tar.gz",
                   target_dir, 
                   repos=NULL, 
                   type="source")
  library(IHRC)
}

#' Loads personal packages in the sandbox
pckg_load <- function(target_dir="/home/ivm/R/x86_64-pc-linux-gnu-library/4.2/") {
  #rm(list = ls())
  .libPaths(target_dir, .libPaths())
  library(Istudy)
  library(IUtils)
  library(ICCI)
  library(IHRC)
}
