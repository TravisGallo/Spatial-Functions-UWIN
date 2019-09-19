check_path <- function(path){
  if( file.exists( path ) ){
    cat(
      paste(
        cli::col_green(
          cli::symbol$tick
        ),
        "Raster layer exists")
    )
  }else{
    # The error (warning to provide to the user.)
    err <- paste0(
      "File does not exist.\n",
      "  You have either entered the wrong path or\n",
      "  you need to download the appropriate raster."
    )
    cat(
      paste(
        cli::col_red(
          cli::symbol$cross
        ),
        err
      )
    )
  }
}

# a hidden function for some command line reporting
.cli_post <- function(my_text, pass = FALSE){
  if(pass){
    cat(
      paste(
        cli::col_green(
          cli::symbol$tick
        ),
        "\n"
      )
    )
    
  } else {
    cat(
      paste(
        cli::symbol$bullet,
        my_text,
        " "
      )
    )
  }
}
