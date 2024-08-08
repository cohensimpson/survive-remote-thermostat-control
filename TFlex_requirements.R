## Install Necessary Modules Not Available Via Conda-Forge ---------------------
## https://remotes.r-lib.org
## https://remotes.r-lib.org/reference/install_version.html
## https://remotes.r-lib.org/reference/install_git.html
## https://stackoverflow.com/questions/76915654/why-use-devtoolsinstall-github-instead-of-remotesinstall-github






## How should package dependencies be handled?  --------------------------------
dependency_handling = c("Depends", "Imports", "LinkingTo")






## Install Experimental Version of "rstanarm" ----------------------------------
## Install *experimental* version of rstanarm which allows usage of the function
## "rstanarm::stan_surv" to estimate bayesian multilevel survival models
## https://github.com/stan-dev/rstanarm/tree/feature/survival
## https://github.com/stan-dev/rstanarm/issues/614#issuecomment-1965469560
## https://discourse.mc-stan.org/t/stan-surv-in-rstanarm/26812
## Change "2" in "-j2" to however many cores you can/want to use to install with parallelisation
Sys.setenv(MAKEFLAGS = "-j6")
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")

## https://mc-stan.org/cmdstanr/
# remotes::install_git(
#     url = "https://github.com/stan-dev/cmdstanr/", 
#     ref = "v0.7.1", 
#     dependencies = dependency_handling, 
#     upgrade = "never", 
#     force = FALSE
#     )

## http://mc-stan.org/rstanarm/
remotes::install_git(
    url = "https://github.com/stan-dev/rstanarm/", 
    ref = "feature/survival",
    dependencies = dependency_handling, 
    upgrade = "never", 
    force = FALSE
    )

## https://gianluca.statistica.it/software/survhe/tutorial
## survHE (annoyingly) requires rJava and (at least for my mac) Java 8
remotes::install_github(
    repo = "https://github.com/giabaio/survHE@v2.0-1",  
    dependencies = dependency_handling, 
    upgrade = "never", 
    force = FALSE
    )

remotes::install_github(
    repo = "https://github.com/giabaio/survHEhmc", 
    dependencies = dependency_handling, 
    upgrade = "never", 
    force = FALSE
    )

## https://paul-buerkner.github.io/brms/
remotes::install_github(
    repo = "https://github.com/paul-buerkner/brms@v2.21.0",  
    dependencies = dependency_handling, 
    upgrade = "never", 
    force = FALSE
    )

## https://github.com/teunbrand/ggarrow
remotes::install_github(
    repo = "https://github.com/teunbrand/ggarrow@v0.1.0",  
    dependencies = dependency_handling, 
    upgrade = "never", 
    force = FALSE
    )

## https://github.com/davidsjoberg/ggsankey
remotes::install_github(
    repo = "https://github.com/davidsjoberg/ggsankey",  
    dependencies = dependency_handling, 
    upgrade = "never", 
    force = FALSE
    )

## https://github.com/hughjonesd/ggmagnify
remotes::install_github(
    repo = "https://github.com/hughjonesd/ggmagnify@v0.4.0",  
    dependencies = dependency_handling, 
    upgrade = "never", 
    force = FALSE
    )

## https://patchwork.data-imaginist.com
remotes::install_github(
    repo = "https://github.com/thomasp85/patchwork@v1.2.0",  
    dependencies = dependency_handling, 
    upgrade = "never", 
    force = FALSE
    )

## https://github.com/VPetukhov/ggrastr
remotes::install_github(
    repo = "https://github.com/VPetukhov/ggrastr@v1.0.2",  
    dependencies = dependency_handling, 
    upgrade = "never", 
    force = FALSE
    )

## https://github.com/ErikKusch/KrigR
remotes::install_github(
    repo = "https://github.com/ErikKusch/KrigR",  
    dependencies = dependency_handling, 
    upgrade = "never", 
    force = FALSE
    )

## https://github.com/caijun/humidity
remotes::install_github(
    repo = "https://github.com/caijun/humidity@v0.1.3",  
    dependencies = dependency_handling, 
    upgrade = "never", 
    force = FALSE
    )






## Easy Installs From CRAN -----------------------------------------------------
## https://glmmtmb.github.io/glmmTMB/
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "blockTools", 
    version = "0.6.4",
    dependencies = dependency_handling, 
    upgrade = "never"
    )
    
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "cobalt", 
    version = "4.5.4",
    dependencies = dependency_handling, 
    upgrade = "never"
    )
    
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "corx", 
    version = "1.0.7.2", 
    dependencies = dependency_handling, 
    upgrade = "never"
    )
    
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "DeclareDesign",
    version = "1.0.8", 
    dependencies = dependency_handling, 
    upgrade = "never"
    )

## https://cran.r-project.org/web/packages/DHARMa/
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "DHARMa", 
    version = "0.4.6",
    dependencies = dependency_handling, 
    upgrade = "never"
    )

## https://cran.r-project.org/web/packages/export/index.html
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "export", 
    version = "0.3.0",
    dependencies = dependency_handling, 
    upgrade = "never"
    )
    
## https://lbbe-software.github.io/fitdistrplus/articles/fitdistrplus_vignette.html
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "fitdistrplus", 
    version = "1.1-11",
    dependencies = dependency_handling, 
    upgrade = "never"
    )
    
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "glmmTMB", 
    version = "1.1.8", 
    dependencies = dependency_handling, 
    upgrade = "never"
    )
    
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "simstudy", 
    version = "0.7.1", 
    dependencies = dependency_handling, 
    upgrade = "never"
    )

## https://masurp.github.io/specr/index.html
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "specr", 
    version = "1.0.0", 
    dependencies = dependency_handling, 
    upgrade = "never"
    )

## https://github.com/kolesarm/multe
## https://cran.r-project.org/web/packages/multe/index.html
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "multe", 
    version = "1.0.0", 
    dependencies = dependency_handling, 
    upgrade = "never"
    )
    
## https://github.com/jacobkap/predictrace/
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "predictrace", 
    version = "2.0.1",
    dependencies = dependency_handling, 
    upgrade = "never"
    )

## https://cran.r-project.org/web/packages/psych/index.html
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "psych", 
    version = "2.4.3", 
    dependencies = dependency_handling, 
    upgrade = "never"
    )

## https://github.com/r-lib/ragg
## https://cran.r-project.org/web//packages//ragg/index.html
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "ragg", 
    version = "1.3.2",
    dependencies = dependency_handling, 
    upgrade = "never"
    )

## https://cran.r-project.org/web/packages/RSQLite/index.html
remotes::install_version(
    repos = "https://cran.r-project.org/", 
    package = "RSQLite", 
    version = "2.3.6",
    dependencies = dependency_handling, 
    upgrade = "never"
    )





## R-Universe Installs ---------------------------------------------------------
## https://vincentarelbundock.r-universe.dev/modelsummary
## https://github.com/r-lib/remotes/issues/618#issuecomment-1265911110
remotes::install_cran(
      pkgs = "modelsummary", ## Development version.
      repos = c("https://vincentarelbundock.r-universe.dev", "https://easystats.r-universe.dev"),
      dependencies = dependency_handling, 
      upgrade = "never"
    )






## Reconfigured CMDSTANR  ------------------------------------------------------
## cmdstanr installed from conda-forge via "_setup_environment.yml"
## https://discourse.mc-stan.org/t/stan-compile-with-apple-accelerate-blas-lapack/32164
## https://mc-stan.org/cmdstanr/reference/install_cmdstan.html

# cmdstanr::install_cmdstan(cores = 8)
# 
# cmdstanr::cmdstan_make_local(
#     cpp_options = base::list(
#     STAN_THREADS = TRUE,
#     STAN_NO_RANGE_CHECKS = TRUE,
#     LDLIBS = "-lblas -llapack -llapacke",
#     CXXFLAGS = "-mcpu=native -DEIGEN_USE_BLAS -DEIGEN_USE_LAPACKE",
#     LDFLAGS = "-framework Accelerate -L/opt/homebrew/opt/lapack/lib",
#     CXXFLAGS_OPTIM= "-mcpu=native",
#     CXXFLAGS_OPTIM_TBB= "-mcpu=native",
#     CXXFLAGS_OPTIM_SUNDIALS= "-mcpu=native"
#     ),
#     append = FALSE
#     )
# 
# cmdstanr::rebuild_cmdstan(cores = 8)
