#' Read a dataset from the UCI Machine Learning Repository into a data frame
#'
#' @param dataset The name of the dataset that you want to read
#' @param row.names Should row names be included? Default is FALSE
#' @param col.names Should column names be included? Default is TRUE
#'
#' @examples
#' blog <- read_ucimlr("blog")
#' @export
read_ucimlr <- function(dataset, row.names = FALSE, col.names = TRUE)
{
  show_table <- FALSE
  dl <- list()
  dl <- add_list(dl,list(dataset_name="Abalone",short_name="abalone",url="https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/",
                         file="abalone.data",zipfile=NULL,delim="csv",col_names=c("sex","length","diameter","height","whole_weight","shucked_weight","viscera_weight","shell_weight","rings"),
                         default_target="rings",null_char=NULL,default_task="classif"))
  dl <- add_list(dl,list(dataset_name="Adult",short_name="adult",url="https://archive.ics.uci.edu/ml/machine-learning-databases/adult/",
                         file="adult.data",zipfile=NULL,delim="csv",col_names=c("age","workclass","fnlwgt","education","education_num","marital_status","occupation","realtionship","race","sex","capital_gain","capital_loss","hours_per_week","native_country","income"),
                         default_target="income",null_char=NULL,default_task="classif"))
  dl <- add_list(dl,list(dataset_name="BlogFeedback",short_name="blog",url="https://archive.ics.uci.edu/ml/machine-learning-databases/00304/",
                         file="BlogFeedback.zip",zipfile="blogData_train.csv",delim="csv",col_names=FALSE,default_target=NULL,null_char=NULL,default_task="regr"))
  dl <- add_list(dl,list(dataset_name="Facebook comment volume",short_name="facebook",url="https://archive.ics.uci.edu/ml/machine-learning-databases/00363/",
                         file="Dataset.zip",zipfile="Dataset/Training/Features_Variant_1.csv",delim="csv",
                         col_names=c("likes","checkins","talking_about","category","derived1","derived2","derived3","derived4","derived5","derived6",
                                     "derived7","derived8","derived9","derived10","derived11","derived12","derived13","derived14","derived15","derived16",
                                     "derived17","derived18","derived19","derived20","derived21","derived22","derived23","derived24","derived25","cc1",
                                     "cc2","cc3","cc4","cc5","base_time","post_length","post_shares","post_promo_status","h_local","post_pub_sun",
                                     "post_pub_mon","post_pub_tue","post_pub_wed","post_pub_thu","post_pub_fri","post_pub_sat","base_date_sun",
                                     "base_date_mon","base_date_tue","base_date_wed","base_date_thu","base_date_fri","base_date_sat","target"),
                         default_target="target",null_char=NULL,default_task="regr"))
  dl <- add_list(dl,list(dataset_name="Communities and Crime",short_name="crime",url="https://archive.ics.uci.edu/ml/machine-learning-databases/communities/",
                         file="communities.data",zipfile=NULL,delim="csv",col_names=FALSE,default_target=NULL,null_char="?",exclude_cols="X4",default_task="regr"))
  dl <- add_list(dl,list(dataset_name="3D Road Network (North Jutland, Denmark)",short_name="network3d",url="https://archive.ics.uci.edu/ml/machine-learning-databases/00246/",
                         file="3D_spatial_network.txt",zipfile=NULL,delim="csv",col_names=FALSE,default_target=NULL,null_char=NULL,default_task="regr"))
  dl <- add_list(dl,list(dataset_name="Airfoil Self-Noise",short_name="airfoil",url="https://archive.ics.uci.edu/ml/machine-learning-databases/00291/",
                         file="airfoil_self_noise.dat",zipfile=NULL,delim="tsv",col_names=c("frequency","angle","chord_length","velocity","thickness","sound_pressure"),
                         default_target="sound_pressure",null_char=NULL,default_task="regr"))
  dl <- add_list(dl,list(dataset_name="Air Quality",short_name="airquality",url="https://archive.ics.uci.edu/ml/machine-learning-databases/00360/",
                         file="AirQualityUCI.zip",zipfile="AirQualityUCI.csv",delim=";",col_names=TRUE,default_target="T",null_char=NULL,default_task="regr"))
  dl <- add_list(dl,list(dataset_name="Relative location of CT slices on axial axis",short_name="ct",url="https://archive.ics.uci.edu/ml/machine-learning-databases/00206/",
                         file="slice_localization_data.zip",zipfile="slice_localization_data.csv",delim="csv",col_names=TRUE,default_target=NULL,null_char=NULL,default_task="regr"))
  dl <- add_list(dl,list(dataset_name="Appliances energy prediction",short_name="appliances",url="https://archive.ics.uci.edu/ml/machine-learning-databases/00374/",
                         file="energydata_complete.csv",zipfile=NULL,delim="csv",col_names=TRUE,default_target=NULL,null_char=NULL,default_task="regr"))

  if(show_table==TRUE)
  {
    output <- dataset_tbl
  } else {
    short_names <- unlist(lapply(dl,function(x) x$short_name))
    if(!(dataset %in% short_names)) { stop("Error: dataset '",dataset,"' not in list \nPlease use one of: ",
                                           paste(short_names,collapse=", ")) }
    idx <- which(short_names==dataset)
    output <- download_data(url=dl[[idx]]$url,
                            file=dl[[idx]]$file,
                            zipfile=dl[[idx]]$zipfile,
                            col_names=ifelse(length(dl[[idx]]$col_names)==1,dl[[idx]]$col_names==TRUE,FALSE),
                            delim=dl[[idx]]$delim)
  }
  output <- fix_coltypes(output)
  if(length(dl[[idx]]$col_names) != 1)
  {
    if(length(dl[[idx]]$col_names) != ncol(output))
    {
      stop("Error: length of col_names doesn't match number of columns of dataset '",dl[[idx]]$short_name,"'")
    } else {
      names(output) <- dl[[idx]]$col_names
    }
  } else {
    if(!(dl[[idx]]$col_names %in% c(TRUE,FALSE)))
    {
      names(output) <- dl[[idx]]$col_names
    }
  }
  output <- fix_names(output)
  output <- fix_nulls(output,dl[[idx]]$null_char)
  if(exists("exclude_cols",where=dl[[idx]]))
  {
    output <- output[,-which(names(output) %in% dl[[idx]]$exclude_cols)]
  }
  return(list(data=output,properties=dl[[idx]]))
}

