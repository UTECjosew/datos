#Instalamos el paquete necesario
install.packages("XML")
install.packages("downloader")
library(XML)

library(downloader)  # Para descargar archivos
library(XML)  # Para leer archivos XML
library(dplyr)  # Para data frames


#Cargamos la base de datos en un string llamado url
url <- "reporteMonedas.do.xml"


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



#Localizamos el archivo
XML <- xmlParse(url, encoding="latin1") #XMLInternalDocument
nodeName <- "moneda"


dfMonedas <- xml2DataFrame(XML,nodeName)

