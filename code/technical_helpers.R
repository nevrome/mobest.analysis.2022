basepath <- "/mnt/archgen/users/schmid/mobest.analysis.2020"

cluster_script <- function(path) {
  cluster_run(paste0("cd ", basepath, " && ./", path))
}

cluster_qsub_script <- function(path) {
  cluster_run(paste0("cd ", basepath, " && qsub ", path))
}

cluster_run <- function(command) {
  rstudioapi::terminalExecute(paste0(
    "ssh -oProxyJump=clemens_schmid@sshgw.eva.mpg.de ", 
    "clemens_schmid@daghead1.eva.mpg.de -t ",
    "\'bash -l -c \"", command, "\"\'"
  ))
}

cluster_up <- function(path) {
  if (dir.exists(path)) { 
    scp <- "scp -r"
    to_copy <- file.path(basepath, dirname(path))
  } else { 
    scp <- "scp"
    to_copy <- file.path(basepath, path)
  }
  rstudioapi::terminalExecute(paste0(
    scp, " -oProxyJump=clemens_schmid@sshgw.eva.mpg.de ", 
    path, 
    " clemens_schmid@daghead1.eva.mpg.de:", 
    to_copy
  ))
}

cluster_down <- function(path) {
  if (dir.exists(path)) {
    scp <- "scp -r"
    to_copy <- dirname(path)
  } else { 
    scp <- "scp"
    to_copy <- path
  }
  rstudioapi::terminalExecute(paste0(
    scp, " -oProxyJump=clemens_schmid@sshgw.eva.mpg.de ", 
    "clemens_schmid@daghead1.eva.mpg.de:",
    file.path(basepath, path), " ",
    to_copy
  ))
}

