# sparse lookup matrix for subject-item effects
siMat <- function(dat){
  idx <- dat %>% count(subject, item)
  nsi <- nrow(idx)
  si.lookup = matrix(0, 
                     length(unique(dat$subject)),
                     length(unique(dat$item)))
  for(i in 1:nsi){
    si.lookup[idx$subject[i], idx$item[i]] <- i
  }
  si.lookup
}
