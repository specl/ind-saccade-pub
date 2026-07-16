require(httr)
require(jsonlite)


loadAS20=function(repo){
  fname="as20Local"
  
  get_github_files <- function(owner, repo, path, branch = "main", pattern = NULL) {
    url <- sprintf(
      "https://api.github.com/repos/%s/%s/contents/%s?ref=%s",
      owner, repo, path, branch)
    resp <- GET(url, add_headers(Accept = "application/vnd.github.v3+json"))
    stop_for_status(resp)
    files <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
    files <- files[files$type == "file", ]
    if (!is.null(pattern)) {
      files <- files[grepl(pattern, files$name), ]}
    urls <- as.character(files$download_url)
    urls[!is.na(urls) & nchar(urls) > 0]
  }
  
  if (repo) {
    owner   <- "PerceptionCognitionLab"
    repo    <- "data6"
    path    <- "antisaccade/as20"
    pattern <- "\\.dat$"          # anything ending in .dat — adjust as needed
    urls <- get_github_files(owner, repo, path, pattern = pattern)
    message(sprintf("Found %d files matching '%s'", length(urls), pattern))
    read_subjs <- lapply(urls, function(url) {
      tryCatch(
        read.csv(url, header = FALSE),
        error = function(e) {
          if (grepl("no lines available in input", e$message)) {
            message("Skipping empty file (no-show): ", url)
            return(NULL)
          }
          stop(e)
        }
      )
    })
    read_subjs <- Filter(Negate(is.null), read_subjs)
    message(sprintf("Successfully loaded %d of %d files", length(read_subjs), length(urls)))
    dat <- do.call(rbind, read_subjs)
    write.table(dat,file = fname,quote=F,row.names = F,col.names = F)}
  dat=read.table(fname,header=F)  
  return(dat)}


cleanAS20=function(repo=F){
  dat=loadAS20(repo)
  colnames(dat) = c("pid","sid","blk","trl","cond","targ","dur","resp","rt")
  sidLabs=unique(dat$sid)
  names=tapply(dat$sid,dat$sid,mean)
  badSession=names[tapply(dat$sid,dat$sid,length)!=450]
  badSub=36
  badBlock=0:5
  remove=dat$sid %in% badSession |dat$pid==badSub | dat$blk %in% badBlock
  dat=dat[!(remove),]
  dat$sub=as.integer(as.factor(dat$pid))
  return(dat)}

#dat=cleanAS20(repo=T)
