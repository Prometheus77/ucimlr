# generic function for downloading, unzipping, and reading in datasets from the web
download_data <- function(url,file,zipfile=NULL,col_names=TRUE,delim="csv")
{
  if(file.exists("dataset.dat")) { file.remove("dataset.dat") }
  downloader::download(paste0(url,file),"dataset.dat")
  if(delim=="csv")
  {
    if(!is.null(zipfile))
    {
      output <- readr::read_csv(unz("dataset.dat",zipfile),col_names=col_names)
    } else {
      output <- readr::read_csv("dataset.dat",col_names=col_names)
    }
  } else if(delim=="tsv") {
    if(!is.null(zipfile))
    {
      output <- readr::read_tsv(unz("dataset.dat",zipfile),col_names=col_names)
    } else {
      output <- readr::read_tsv("dataset.dat",col_names=col_names)
    }
  } else {
    if(!is.null(zipfile))
    {
      output <- readr::read_delim(unz("dataset.dat",zipfile),delim=delim,col_names=col_names)
    } else {
      output <- readr::read_delim("dataset.dat",delim=delim,col_names=col_names)
    }
  }
  file.remove("dataset.dat")
  return(output)
}

# forces all column types in a tbl to be usable by mlr
fix_coltypes <- function(data)
{
  for(i in 1:ncol(data))
  {
    if(length(class(data[[i]]))!=1)
    {
      data[[i]] <- as.factor(data[[i]])
    } else {
      if(!(class(data[[i]]) %in% c("integer","numeric","factor")))
      {
        data[[i]] <- as.factor(data[[i]])
      }
    }
  }
  return(data)
}

# simple function, adds new sequential element to existing list
add_list <- function(source,new)
{
  if(!is.list(source)) { stop("source must be a list") }
  if(length(source)==0)
  {
    source[[1]] <- new
  } else {
    source[[length(source)+1]] <- new
  }
  return(source)
}

# takes unusable characters out of column names for mlr
fix_names <- function(dataset)
{
  for(i in c("\\(","\\)"))
  {
    names(dataset) <- gsub(i,"_",names(dataset))
    names(dataset) <- gsub(i,"_",names(dataset))
  }
  return(dataset)
}

fix_nulls <- function(dataset,null_char=NULL)
{
  if(!is.null(null_char))
  {
    for(i in null_char)
    {
      dataset[dataset==i] <- NA
    }
    for(j in which(sapply(dataset,class)=="factor"))
    {
      vect <- collect(select(dataset,eval(as.symbol(names(dataset)[j]))))[[1]]
      vect_no_na <- vect[!is.na(vect)]
      if(!anyNA(as.numeric(as.character(vect_no_na))))
      {
        dataset[,j] <- as.numeric(as.character(vect))
      }
    }
  }
  return(dataset)
}

remove_target_na <- function(dataset, target = NULL)
{
  if (!(target %in% names(dataset))) { stop("Error: target '",target,"' doesn't exist in dataset") }
  target_vector <- dataset[, which(names(dataset) == target)]
  if(anyNA(target_vector)) { dataset <- dataset[-which(is.na(target_vector)), ] }
  return(dataset)
}
