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
  assign(".lib.loc", c("/home/ivm/R/x86_64-pc-linux-gnu-library/4.2", "/usr/local/lib/R/site-library","/usr/lib/R/site-library", "/usr/lib/R/library") , envir=environment(.libPaths))
  
  install.packages("/finngen/green/detrois_up/pckg_share/comorbidity_1.0.5.tar.gz",
                   target_dir, 
                   repos=NULL, 
                   type="source")
  library(comorbidity)
  install.packages("/finngen/green/detrois_up/pckg_share/ICCI_2.2.2.tar.xz",
                   target_dir, 
                   repos=NULL, 
                   type="source")
  library(ICCI)
}
#' Installs personal packages in the sandbox
pckg_install <- function() {
  assign(".lib.loc", c("/home/ivm/R/x86_64-pc-linux-gnu-library/4.2", "/usr/local/lib/R/site-library","/usr/lib/R/site-library", "/usr/lib/R/library") , envir=environment(.libPaths))
  install.packages("/home/ivm/R/pckg_uploads/Istudy.tar.xz",
                   "/home/ivm/R/x86_64-pc-linux-gnu-library/4.2/", 
                   repos=NULL, 
                   type="source")
  library(Istudy)
  
  install.packages("/home/ivm/R/pckg_uploads/IUtils.tar.xz",
                   "/home/ivm/R/x86_64-pc-linux-gnu-library/4.2/", 
                   repos=NULL, 
                   type="source")
  library(IUtils)


  install.packages("/home/ivm/R/pckg_uploads/IHRC.tar.xz",
                   "/home/ivm/R/x86_64-pc-linux-gnu-library/4.2/", 
                   repos=NULL, 
                   type="source")
  library(IHRC)
}

#' Loads personal packages in the sandbox
pckg_load <- function() {
  #rm(list = ls())
  #assign(".lib.loc", c("/home/ivm/R/x86_64-pc-linux-gnu-library/4.2", "/usr/local/lib/R/site-library","/usr/lib/R/site-library", "/usr/lib/R/library") , envir=environment(.libPaths))
  library(Istudy)
  library(IUtils)
  library(ICCI)
  library(IHRC)
}
