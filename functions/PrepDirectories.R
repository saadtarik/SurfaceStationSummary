prepDirectories <- function(data.dir,projectID){

  # make the directory we will use for the project
  dir.create(file.path(data.dir), recursive = TRUE, showWarnings = FALSE)
  
  # make the directory for the data
  dir.create(file.path(data.dir, "data"), showWarnings = FALSE)
  dir.create(file.path(data.dir, "data", "raw"), showWarnings = FALSE)
  dir.create(file.path(data.dir, "data", "csv"), showWarnings = FALSE)  
  
  # make the directory for the project
  dir.create(file.path(data.dir,projectID), showWarnings = FALSE)  
  dir.create(file.path(data.dir,projectID,"figures"), showWarnings = FALSE)  
  dir.create(file.path(data.dir,projectID,"maps"), showWarnings = FALSE)  
  dir.create(file.path(data.dir,projectID,"tex"), showWarnings = FALSE)
}