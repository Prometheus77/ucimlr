#' Read a dataset from the UCI Machine Learning Repository into a data frame
#'
#' @param dataset The name of the dataset that you want to read
#' @param col_names Should column names be included? Default is TRUE
#' @param read_as The format that should be returned. Default is \code{'df'}
#' \itemize{
#'   \item \code{'df'} Return a data frame
#'   \item \code{'tbl'} Return a tbl (see package \code{tibble})
#'   \item \code{'task'} Return a task (see package \code{mlr})
#'   \item \code{'list'} Return a data list containing elements \code{data} and \code{properties}
#' }
#'
#' @examples
#' blog <- read_ucimlr("blog")
#' @export

read_ucimlr <- function(dataset, col_names = TRUE, read_as = "df")
{
  # make sure everything is in order
  assert_choice(read_as, c("df", "tbl", "task", "list"))
  if (!requireNamespace("mlr", quietly = TRUE) && read_as == "task")
  {
    stop("Package 'mlr' must be installed to read as task")
  }
  dl <- list_datasets()
  short_names <- unlist(lapply(dl, function(x) x$short_name))
  if (!(dataset %in% short_names)) { stop("Error: dataset '", dataset, "' not in list \nPlease use one of: ",
                                         paste(short_names, collapse = ", ")) }

  # get stuff
  idx <- which(short_names == dataset)
  output <- download_data(url = dl[[idx]]$url,
                          file = dl[[idx]]$file,
                          zipfile = dl[[idx]]$zipfile,
                          col_names = ifelse(length(dl[[idx]]$col_names) == 1, dl[[idx]]$col_names == TRUE, FALSE),
                          delim = dl[[idx]]$delim)
  output <- as.data.frame(output)
  output <- fix_coltypes(output)

  if (length(dl[[idx]]$col_names) != 1)
  {
    if (length(dl[[idx]]$col_names) != ncol(output))
    {
      stop("Error: length of col_names doesn't match number of columns of dataset '", dl[[idx]]$short_name, "'")
    } else {
      names(output) <- dl[[idx]]$col_names
    }
  } else {
    if (!(dl[[idx]]$col_names %in% c(TRUE, FALSE)))
    {
      names(output) <- dl[[idx]]$col_names
    }
  }

  output <- fix_names(output)
  output <- fix_nulls(output, dl[[idx]]$null_char)

  if (exists("exclude_cols", where=dl[[idx]]))
  {
    output <- output[, -which(names(output) %in% dl[[idx]]$exclude_cols)]
  }

  if (col_names == FALSE)
  {
    names(output) <- paste("X", seq_along(names(output)), sep = "")
  }

  # return object based on read_as
  if (read_as == 'df')
  {
    return(output)
  } else if (read_as == 'tbl')
  {
    return(as.tbl(output))
  } else if (read_as == 'task')
  {
    output <- remove_target_na(output, target = dl[[idx]]$default_target)
    return(make_mlr_task(list(data=output, properties = dl[[idx]])))
  } else if (read_as == 'list') {
    return(list(data = output, properties = dl[[idx]]))
  }
}

