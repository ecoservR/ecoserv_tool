#######################################
### Cleaning functions              ###
### for EcoservR                    ###
### Sandra Angers-Blondin           ###
### 08 Oct 2020                     ###
#######################################


#' Clean Up and Free Memory
#'
#' This function deletes the contents of the scratch folder and uses garbage collection to return memory to the OS.

#' @param scratch The scratch (temporary) folder where outputs are saved
#' @return Nothing; deletes contents of the scratch folder.
#' @export
#'
cleanUp <- function(scratch = scratch_path){
# scratch is the path to the temporary folder

   if (grepl("ecoservR", scratch)){ # ONLY delete scratch folder if it has ecoservR in the name (which it should automatically). Extra stopgap to prevent accidentally deleting an important folder

invisible(  # this just prevents message being printed to the console
   file.remove(dir(    # delete all files
      file.path(scratch),
      full.names = TRUE,
      recursive = TRUE
   ))
)
      }

invisible(  # remove folders too
   unlink(list.dirs(file.path(scratch), recursive = FALSE),  # FALSE here essential otherwise would delete scratch folder itself
          recursive = TRUE)  # but true here to delete empty folders within scratch
)

invisible(gc())  # probably not doing much but forces R to return memory to system if available
}



#' Turn list-column into character column
#'
#' Turns a list into character format: used in EcoservR to fix some MasterMap attributes that import wrongly from certain format (eg GML).

#' @param x A problematic column
#' @return A vector of character data. Where the original list element was character(0), this is change to NA. Where the original list element had a vector of multiple terms, these are concatenated with a comma.
#' @export
#'
list_to_char <- function(x) {
   if (inherits(x, "list")){
      tochar <- sapply(x, function(y) paste0(y, collapse = ","))
      tochar <- ifelse(tochar == "", NA, tochar)
   }
}
