
# make the directory we will use for the project
dir.create(file.path(data.dir), recursive = TRUE, showWarnings = FALSE)

# make the directory for the data
dir.create(file.path(data.dir, "data"), showWarnings = FALSE)
dir.create(file.path(data.dir, "data", "raw"), showWarnings = FALSE)
dir.create(file.path(data.dir, "data", "csv"), showWarnings = FALSE)

# make the directory for the figures
dir.create(file.path(data.dir, "figures"), showWarnings = FALSE)
dir.create(file.path(data.dir, "figures","StationImages"), showWarnings = FALSE)

# make a directory for tex documents
dir.create(file.path(data.dir, "tex"), showWarnings = FALSE)


