# Robert Ernst
# AUC
# Function to calculate AUC values for response curves using Trapezoidal integration.

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

require(pracma)

AUC <- function
### Function to calculate AUC values for response curves using Trapezoidal integration.
( x, ##<< the x coordinates of points in the curve
  y, ##<< the y coordinates of points in the curve
  baseline = 0 ##<< baseline value of y axis
)
{
  AUCp = 0 #positive AUC
  AUCn = 0 #negative AUC
  y = y-baseline
  
  for(i in 2:length(y)){
    #Calculate per block of two points the AUCp and AUCn
    yPoints = y[c(i-1,i)]
    xPoints = x[c(i-1,i)]
    
    #Sort to the decrease number of posibilities to calculate AUC.
    yPoints = sort(yPoints, decreasing=T)
    
    #Both yPoints above baseline -> AUCp
    if(yPoints[1] >= 0 && yPoints[2] >= 0 ){ 
      pointsAUCp = trapz(xPoints,yPoints)
      AUCp = AUCp + abs(pointsAUCp)
    }
    
    #Both yPoints bewlow baseline -> AUCn
    else if(yPoints[1] <= 0 && yPoints[2] <= 0 ){
      pointsAUCn = trapz(xPoints,yPoints)
      AUCn = AUCn + abs(pointsAUCn)
    }
    
    #yPoints cross the axis -> AUCn and AUCp
    else if(yPoints[1] > 0 && yPoints[2] < 0 ){
      yPointsDiff = yPoints[1] + abs(yPoints[2])
      coefficient = yPointsDiff / abs(diff(xPoints))
      crossX = yPoints[1] / coefficient
      
      pointsAUCp = trapz(c(xPoints[1], xPoints[1]+crossX),c(yPoints[1], 0))
      AUCp = AUCp + abs(pointsAUCp)
      
      pointsAUCn = trapz(c(xPoints[1]+crossX, xPoints[2]),c(0,yPoints[2]))
      AUCn = AUCn + abs(pointsAUCn)
    }
    AUCt <- AUCp + AUCn
  }
  data.frame(AUCp, AUCn, AUCt)
  ### Returns a data.frame containing all AUC's calculated
}  
