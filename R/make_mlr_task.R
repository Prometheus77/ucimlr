#' Creates an mlr task from a data list
#'
#' @param data A data list as returned by \code{read_ucimlr} with option \code{read_as = 'list'}
#' @param ... Additional parameters to be passed to \code{mlr::makeRegrTask} or \code{mlr::makeClassifTask}
#'
#' @export
make_mlr_task <- function(data, ...)
{
  if (!requireNamespace("mlr", quietly = TRUE))
  {
    stop("Package 'mlr' must be installed to read as task")
  }

  if(data$properties$default_task == "regr")
  {
    task <- mlr::makeRegrTask(id = data$properties$short_name, data=data$data, target=data$properties$default_target, ...)
  } else if(data$properties$default_task=="classif")
  {
    task <- mlr::makeClassifTask(id = data$properties$short_name, data=data$data, target=data$properties$default_target, ...)
  } else {
    stop("Error: Task type '",data$properties$default_task,"' not supported")
  }
  return(task)
}
