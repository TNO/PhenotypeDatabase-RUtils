# Robert Ernst
# getStudy
# get study based on code (unique)

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
