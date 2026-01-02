report_template <- function(report_name = 'report'){

  if(!dir.exists('reports')){
    dir.create('reports')
  }

  withr::with_dir('reports', {

    rmarkdown::draft(stringr::str_glue("{report_name}.Rmd"),
                     template = "statistical-analysis",
                     package = "dmisc",
                     edit = FALSE)
  })
}
