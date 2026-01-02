statistical_analysis_template <- function(path, ...){

  dots <- list(...)

  # Create the project directory on your local machine
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # Add the following directories to the project
  directories <- c('R','reports','assets','data')
  for(dir in directories){
    dir.create(path=file.path(path, dir))
  }


  # Within the project directory
  withr::with_dir(path, {

    report_template()

    if(dots$use_renv){

      # Initialize an renv if you like
      renv::init(bare=T, force = FALSE, load = FALSE, restart = FALSE)

      usethis::use_template('install_packages.R',
                            save_as = 'R/install_packages.R',
                            package = 'dmisc')

    }

  })

}
