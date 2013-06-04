#   Copyright 2013 Robert Ernst
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

#require(GSCFClient)
require(stringr)

namedListToDataFrame <- function
### Transform a named list to table.
(list, ##<< Give a named list
 colName = "token" ##<< Number of row containing column names.
){
  # Transform nested list to data.frame
  dataFrame = data.frame(do.call(cbind, list))
  dataFrame = data.frame(sapply(dataFrame, unlist), stringsAsFactors=FALSE)

  colnames(dataFrame) = dataFrame[colName,]  # set column names
  
  return(dataFrame)
}

getStudy <- function
### get study based on code (unique)
(studyCode ##<< Give a study code
){
  studies = getStudies()
  study = studies[[grep(studyCode, sapply(studies, function(x) x$title))]]
  study
}

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