# https://github.com/grantmcdermott/renv-rspm
# renv::init()    ## Automatically run if you cloned/opened the repo as an RStudio project
renv::restore()   ## Enter "y" when prompted
options(repos = c(RSPM = "https://packagemanager.rstudio.com/all/latest"))
