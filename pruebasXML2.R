library(XML)


url <- "reporteMonedas.do.xml"

xmldoc <- xmlParse(url)
rootNode <- xmlRoot(xmldoc)
rootNode[1]

data <- xmlSApply(rootNode,function(x) xmlSApply(x,xmlValue))

cd.catalog <- data.frame(t(data), row.names = NULL)

cd.catalog[1:2,]

View(cd.catalog)



library("downloader")  # Para descargar archivos
library(XML)  # Para leer archivos XML
library(dplyr)  # Para data frames
library(utils)

# Parse XML To DataFrame starting from xmlDoc and the node type of the target
xml2DataFrame <- function(xmldoc, nodeType) {
  # Fetch rootNode
  rootNode <- xmlRoot(xmldoc)
  
  # Get all nodes of specified type
  targetNodes <- xmlElementsByTagName(rootNode, nodeType, recursive = FALSE)
  
  # Examine first row in targetNodes to build dataFrame
  sample <- first(targetNodes)
  attrs = xmlAttrs(sample)
  dfLength <- length(names(attrs))
  dfColNames <- names(attrs)
  
  # Create dataFrame from sample attrs names as columnNames
  df <- data.frame(matrix(ncol = dfLength, nrow = 0))
  colnames(df) <-dfColNames
  
  # For each targetNode extract attributes and create new row in dataframe
  for (node in targetNodes)
  {
    attrs <- xmlAttrs(node)
    df[nrow(df) + 1,] = attrs[1:dfLength] # %>% select(1:dfLength)
  }
  
  return(df)
}


datasetFilename = "reporteIncisos.do.xml"

# Parse xml file 
xmldoc <- xmlParse(datasetFilename, encoding="latin1") # ISO-8859-1

nodeName <- "inciso"

dfReporteEstadosCompra <- xml2DataFrame(xmldoc, nodeName)

dfReporteEstadosCompra
