# Robert Ernst
# Transpose transcriptomics files to subject layout

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

## Example input file 
## Feature  Time  Subject_1 Subject_2
## 58812    5w    2.34      3.12
## 500012   5w    5.123     12.12

## Example output file 
## Feature    58812   500012
## Time       5w      5w
## Subject_1  2.34    5.123
## Subject_2  3.12    12.12

require(gdata)

transposeToSubjectLayout <- function
### transpose transcriptomics files to subject layout.
(filename, ##<< excel filename
 dataPath ##<< path to excel filename
){
  data = read.xls(paste0(dataPath,filename), method='tab', check.names=FALSE) #read excel file, sheet 1
  dataTransposed = t(data) #tranpose data
  
  #Change row/col names for opencpu use.
  dataTransposed = cbind(rownames(dataTransposed),dataTransposed)
  colnames(dataTransposed) = dataTransposed[1,]
  dataTransposed = dataTransposed[2:nrow(dataTransposed),]
  
  write.table(dataTransposed, file=paste0(dataPath,'transposeddata.txt'), quote = FALSE, sep='\t')
  
  dataTransposedpath=paste0(dataPath,'transposeddata.txt')
  
  replacespace= paste0("sed -i 's/ //g' ",dataTransposedpath)
    
  system(replacespace)
  
  dataTransposedpath
  
  ### Return transposed data.matrix
}
