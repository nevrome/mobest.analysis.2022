basepath <- "/mnt/archgen/users/schmid/mobest.analysis.2020"

pw <- readLines("pw.txt")

submit_pw_once <- function(termId) { rstudioapi::terminalSend(termId, paste0(pw, "\n")) }

login <- function(termId) {
  Sys.sleep(1)
  submit_pw_once(termId)
  Sys.sleep(1)
  submit_pw_once(termId)
}

local_script <- function(path) {
  termId <- rstudioapi::terminalCreate()
  rstudioapi::terminalSend(
    termId,
    paste0(
      "cd ~/agora/mobest.analysis.2020 && ./",
      path,
      "\n"
    )
  )
}

cluster_script <- function(path) {
  cluster_run(paste0("cd ", basepath, " && ./", path))
}

cluster_qsub_script <- function(path) {
  cluster_run(paste0("cd ", basepath, " && qsub ", path))
}

cluster_singularity_script <- function(path, cores = 16, memory = 50) {
  cluster_run(paste0(
    "cd ", basepath, " && ",
    "/mnt/archgen/users/schmid/singularity/sge_nevrome_mobest.sh ", 
    cores, " ", memory, " ",
    path
  ))
}

cluster_run <- function(command) {
  termId <- rstudioapi::terminalCreate()
  rstudioapi::terminalSend(
    termId,
    paste0(
      "ssh -oProxyJump=clemens_schmid@sshgw.eva.mpg.de ", 
      "clemens_schmid@daghead1.eva.mpg.de -t ",
      "\'bash -l -c \"", command, "\"\'",
      "\n"
    )
  )
  login(termId)
}

cluster_up_one <- function(path) {
  if (dir.exists(path)) { 
    scp <- "scp -r"
    to_copy <- file.path(basepath, dirname(path))
  } else { 
    scp <- "scp"
    to_copy <- file.path(basepath, path)
  }
  termId <- rstudioapi::terminalCreate()
  rstudioapi::terminalSend(
    termId,
    paste0(
      scp, " -oProxyJump=clemens_schmid@sshgw.eva.mpg.de ", 
      path, 
      " clemens_schmid@daghead1.eva.mpg.de:", 
      to_copy,
      "\n"
    )
  )
  login(termId)
}

cluster_up <- function(...) {
  paths <- list(...)
  Map(cluster_up_one, paths)
}

cluster_down_one <- function(path) {
  if (dir.exists(path)) {
    scp <- "scp -r"
    to_copy <- dirname(path)
  } else { 
    scp <- "scp"
    to_copy <- path
  }
  termId <- rstudioapi::terminalCreate()
  rstudioapi::terminalSend(
    termId,
    paste0(
      scp, " -oProxyJump=clemens_schmid@sshgw.eva.mpg.de ", 
      "clemens_schmid@daghead1.eva.mpg.de:",
      file.path(basepath, path), " ",
      to_copy,
      "\n"
    )
  )
  login(termId)
}

cluster_down <- function(...) {
  paths <- list(...)
  print(paths)
  Map(cluster_down_one, paths)
}
