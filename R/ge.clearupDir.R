ge.cleaupDir  <- function(dir=getwd()){
        setwd(dir)
        files  <- list.files()
        exts  <- tools::file_ext(file)
        extsu  <- unique(exts)
        for (i in extsu){
                 if (i != ""){
                        dir.create(i)        
                }
                for (j in files) {
                        if(tools::file_ext(j) == i){
                                file.copy(j, i)
                                file.remove(j)
                        }    
                }
        }
}
