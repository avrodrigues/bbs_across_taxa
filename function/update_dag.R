# Update the DAG when new paths are added to the original DAG model


update_dag <- function(dag, new_path) {
  require(stringi)

  n_char <- nchar(dag[[1]])

  stri_sub(dag[[1]],n_char-1, n_char-2) <- new_path

  dag
}
