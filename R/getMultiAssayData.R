# Robert Ernst
# getMultiAssayData
# Get data from multiple assays from multiple studies and combine them in one data.frame

require(GSCFClient)
require(stringr)

getMultiAssayData <-function
### Get data from multiple assays from multiple studies and combine them in one data.frame
(studyCodes, ##<< Give a list with study codes, one for each assa.
 assayNames, ##<< Give a list of assays, same order as studyCodes
 subjectMetadata ##<< List of subject metadata
)
{
  studies = getStudies()
  multiAssayData = data.frame()
  
  for(i in 1:length(studyCodes)){
    #Get study
    study = studies[[grep(studyCodes[i], sapply(studies, function(x) x$code))]]
    
    #Get assay data
    studyAssays = namedListToDataFrame(getAssaysForStudy(study['token']),"name")
    assayToken = studyAssays[[assayNames[i]]][2]
    assayData = assayDataAsMatrix(assayToken)$data
    assayData$Features = str_split_fixed(assayData$measurementName,".v",2)[,1]
    assayData$Study = rep(studyCodes[i],nrow(assayData))
    
    # Get subject information based on subjectMetadata list
    studySubjects = namedListToDataFrame(getSubjectsForStudy(study$token))
    assayData = cbind(assayData, t(data.frame(studySubjects[assayData$subject][subjectMetadata,])))
    
    #Get event data.
    studyEvents = getSamplingEventsForStudy(study['token'])
    relativeTimes = unlist(lapply(assayData$samplingEvent, function(x) studyEvents[[x]]$relativeTimeInRelatedEvent))
    eventDay = unlist(lapply(assayData$samplingEvent, function(x) studyEvents[[x]]$startTime))
    assayData$relatedEvent = unlist(lapply(assayData$samplingEvent, function(x) studyEvents[[x]]$'relatedEvent/Chall.'))
    assayData$eventDay = floor(as.numeric(as.character(eventDay)) / 60 / 60 / 24)
    assayData$relativeTime = as.numeric(as.character(relativeTimes)) / 60
    
    #Get group names.
    studyGroups = namedListToDataFrame(getEventGroupsForStudy(study['token']))
    assayData$groupName = t(studyGroups[assayData$eventGroup]["name",])
    
    multiAssayData = rbind(multiAssayData, assayData)
  }
  multiAssayData 
  ### Returns a data.frame, content is based on the study, assay and metadata selection.
}
