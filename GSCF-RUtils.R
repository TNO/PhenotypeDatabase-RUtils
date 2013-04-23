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

namedListToDataFrame <- function
### Transform a named list to table.
(list, ##<< Give a named list
 colName = "name" ##<< Name of row containing column names.
){
  # Transform nested list to data.frame
  dataFrame = data.frame(do.call(cbind, list))
  dataFrame = data.frame(sapply(dataFrame, unlist), stringsAsFactors=FALSE)

  colnames(dataFrame) = dataFrame[colName,]  # set column names
  
  return(dataFrame)
}