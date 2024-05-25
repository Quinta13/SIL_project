
load_requirements <- function(requirements) {

    for (req in requirements){

        # If the package is not installed, install it
        if (!require(req, character.only = TRUE)){
            print(paste("Package", req, "not found. Installing..."))
            install.packages(req)
        }
        # Load the package
        library(req, character.only = TRUE)

    }
}

get_palette_color <- function(i) {

    colors <- palette()

    j <- i %% length(palette())
    
    return(colors[j])

}
