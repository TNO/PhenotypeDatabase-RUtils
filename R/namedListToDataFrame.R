# Robert Ernst
# namedListToDataFrame
# Transform a named list to table.

require(GSCFClient)

namedListToDataFrame <- function
### Transform a named list to data.frame
(list, ##<< Give a named list
 colName = "token" ##<< Number of row containing column names.
){
  # Transform nested list to data.frame
  dataFrame = data.frame(do.call(cbind, list))
  dataFrame = data.frame(sapply(dataFrame, unlist), stringsAsFactors=FALSE)

  colnames(dataFrame) = dataFrame[colName,]  # set column names
  
  return(dataFrame)
  ### Returns the transformed namedList as a data.frame
}
