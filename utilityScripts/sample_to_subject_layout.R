# Simple script to transform sample layout to subject layout
# Robert Ernst

# Copyright 2013 Robert Ernst
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#Read in data
filename = "sample_filename.txt"
data = read.table(filename, sep="\t", header=TRUE)
  ## Example sample layout file 
  ## ID  Visit	Time	Glucose
  ## Sample1	1	000	1.2
  ## Sample1	1	010	3.19
  ## Sample1	1	020	4.15
  ## Sample1	1	030	7.82

sampleIds = levels(data$ID)
features = names(data)[4:14] 

#Setup subjectLayout
subjectLayout = data.frame(c("Feature","time","visit","week_dbnpTime",sampleIds))
colnames(subjectLayout) = ""
rownames(subjectLayout) = subjectLayout[,1]

#Visit to week switch.
visitToWeek <- function(type) {
  switch(type,
         "1" = "2w",
         "2" = "4w")
}

#Format times to dbnp time layout.
timeToDbnpTime <- function(type) {
  switch(type,
         "0" = "",
         "10" = "10m",
         "20" = "20m",
         "30" = "30m",
         "60" = "1h",
         "90" = "1h 30m",
         "120" = "2h",
         "180" = "3h",
         "240" = "4h",
         "300" = "5h")
}

#Transform data
for(feature in features){
  featureMeasurement = data[c("ID","Visit","Time",feature)]
  visits = unique(featureMeasurement$Visit)
  for(visit in visits){
    visitMeasurement = featureMeasurement[featureMeasurement$Visit == visit,]
    times = unique(visitMeasurement$Time)
    for(time in times){
      timeMeasurement = visitMeasurement[visitMeasurement$Time == time,]
      rownames(timeMeasurement) = timeMeasurement$ID
      subjectLayout = cbind(subjectLayout, timeMeasurement[,4][
        match(rownames(subjectLayout), rownames(timeMeasurement))])
      
      subjectLayout[1,ncol(subjectLayout)] = feature
      subjectLayout[2,ncol(subjectLayout)] = timeToDbnpTime(as.character(time))
      subjectLayout[3,ncol(subjectLayout)] = visitToWeek(as.character(visit))
      subjectLayout[4,ncol(subjectLayout)] = paste(visitToWeek(as.character(visit)), timeToDbnpTime(as.character(time)))
    }
  }
}

# Write data
outputFilename = "testSubjectLayout.txt"
write.table(subjectLayout, file=outputFilename, sep="\t", row.names=FALSE, col.name=FALSE)