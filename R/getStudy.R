# Robert Ernst
# getStudy
# get study based on code (unique)

require(GSCFClient)

getStudy <- function
### get study based on code (unique)
(studyCode ##<< Give a study code
){
  studies = getStudies() # get all studies avaible for user
  #grep study from studies list based on study code
  study = studies[[grep(studyCode, sapply(studies, function(x) x$title))]]
  study
  ### Returns the study information
}
