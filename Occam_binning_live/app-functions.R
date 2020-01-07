# Write formatted Occam input file
writeOccamInputFile <- function(itId, binnedId, outputId) {
  file.create(outputId)
  cat(":action", "search", "", ":nominal", sep = "\n",
      file = outputId, append = TRUE)
  write.table(itId, file = outputId, append = TRUE, quote = FALSE,
              sep = ",", eol = '\r\n', row.names = FALSE,
              col.names = FALSE)
  cat("",":no-frequency","",":data","", sep = "\n",
      file = outputId, append = TRUE)
  cat("#", file = outputId, append = TRUE)
  write.table(itId[,4], file = outputId, append = TRUE,
              quote = FALSE, eol = '\t', row.names = FALSE,
              col.names = FALSE)
  cat("\n", file = outputId, append = TRUE)
  write.table(binnedId, file = outputId, append = TRUE,
              quote = FALSE, sep = "\t", eol = '\r\n',
              row.names = FALSE, col.names = FALSE)
}

# Perform binning of data and generate table
dataBinning <- function(inputId, binType, binTarget, naOmit) {
  binM <- matrix(data = NA, nrow = nrow(inputId), ncol = ncol(inputId))
  colnames(binM) <- colnames(inputId)
  
  for (i in seq(ncol(inputId))) {
    if (is.character(inputId[,i])) {
      binM[,i] <- as.numeric(as.character(
                    factor(inputId[,i],
                           labels = seq(2))
                    ))
    } else {
      switch (binType[i],
              interval = {
                binM[,i] <- as.numeric(as.character(
                              bin(inputId[,i],
                                  nbins = binTarget[i],
                                  labels = seq(binTarget[i]),
                                  method = "length",
                                  na.omit = FALSE)
                            ))
              },
              distro = { 
                binM[,i] <- as.numeric(as.character(
                              bin(inputId[,i],
                                  nbins = binTarget[i],
                                  labels = seq(binTarget[i]),
                                  method = "content",
                                  na.omit = FALSE)
                            ))
              },
              clusters = {
                binM[,i] <- as.numeric(as.character(
                              bin(inputId[,i],
                                  nbins = binTarget[i],
                                  labels = seq(binTarget[i]),
                                  method = "clusters",
                                  na.omit = FALSE)
                            ))
              }
      )
    }
  }
  binM <- binM - 1
  for (i in seq(ncol(inputId))) {
    binM[,i][is.na(binM[,i])] <- naOmit[i]
  }
  return(binM)
}