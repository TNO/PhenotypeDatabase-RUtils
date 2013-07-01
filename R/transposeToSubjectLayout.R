# Robert Ernst
# Transpose transcriptomics files to subject layout

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
(filePath ##<< filepath of the excel file
){
  data = read.xls(filePath, method="tab") #read excel file, sheet 1
  dataTransposed = t(data) #tranpose data
  dataTransposed
  ### Return transposed data.matrix
}
