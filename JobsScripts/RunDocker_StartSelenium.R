`%>%` <- magrittr::`%>%`
is_docker_live <- function () {
  shell_out <- suppressWarnings(shell("docker ps", intern = T))
  .out <- try({isTRUE(stringr::str_detect(shell_out[1], "CONTAINER ID"))})
  .out
}
docker_containers <- function () {
  
  if (!is_docker_live()) {
    message("Docker is not running. Start docker to check containers.")
    .out <- NULL
  } else {
    shell_out <- shell("docker ps", intern = T)
    if (length(shell_out) == 1) {
      .out <- NULL
    } else {
      .split <- strsplit(shell_out, "\\s{2,}")
      .out <- setNames(purrr::map(.split[-1], ~{setNames(.x, .split[[1]])}), lapply(.split[-1], `[[`, 7))
    }
  }
  return(.out)
}  

docker_container_ip <- function () {
  dc <- docker_containers()
  .out <- shell(paste0("docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' ", dc[[1]][1]), intern = TRUE)
  .out <- gsub("\\'", "", .out)
  cat(.out)
  return(.out)
}
rD_timeout <- function (remDr, milliseconds)
{
  qpath <- sprintf("%s/session/%s/timeouts", remDr$serverURL,
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, method = "POST", qdata = jsonlite::toJSON(list(type = "implicit", ms = milliseconds), 
                                                                 auto_unbox = TRUE))
}
# readBin(utils::readRegistry(file.path("SOFTWARE", "TightVNC", "Server", fsep = "\\"), hive = "HLM")$Password, "character", n = 15)

startSelenium <- function(){
  # If docker is not yet started start it
  if (!is_docker_live()) {
    docker_start <- shell.exec(shortPathName("file:///C:/Program Files/Docker/Docker/Docker Desktop.exe"))
    wait <- 0
    while (!is_docker_live()) {
      Sys.sleep(20)
      wait <- wait + 20
      message("Waiting for Docker to launch: ", round(wait/60,1) ,"min elapsed")
    }
  }
# if no container is active start one
  dc <- docker_containers()
  if (is.null(dc)) {
    message("Starting container...")
    shell_out <- shell("docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-chrome-debug", intern = T)
    Sys.sleep(10)
    dc <- docker_containers()
    wait <- 0
    while (is.null(dc)) {
      Sys.sleep(20)
      dc <- docker_containers()
      wait <- wait + 20
      message("Waiting for Container to launch: ", round(wait/60,1), "min elapsed")
    }
  } 
  
  message("Active containers: ")
  print(dc)
  
  message("Starting RSelenium")
  rD <- try({
      RSelenium::remoteDriver(remoteServerAddr = "localhost",
                              port = 4445L,
                              browserName = "chrome")
  })
  .status <- rD$getStatus()
  if (!.status$ready) {
    Sys.sleep(5)
  }
  sess <- rD$getSessions()
  if (length(sess) > 0) rD$closeall()
  sess <- try({rD$open(silent = TRUE)})
  rD_timeout(rD, 25000)
  return(rD)
}
