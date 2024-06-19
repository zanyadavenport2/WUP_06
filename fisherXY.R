fisherXY <- function(x, y, confL = 0, printAll=FALSE){
  # Inputs: 
  #   x: factor variable, 2 levels
  #   y: factor variable, n levels
  # confL: confidence level, default = 0.95
  #        print only those tables, results if p-value < (1-confL)
  # printAll: overide the 'print only if p-value < (1-confL), and
  #           print all tables and fisher test results
  #
  # Constraint:
  #   x and y must be same length
  #
  # Outputs:
  #   All 2x2 Fisher exact tests
  
  if(confL<0 || confL> 1){
    print('error: confL must be between 0 and 1')
    return(999)
  }
  
  if(!is.factor(x)) x=as.factor(x)
  if(!is.factor(y)) y=as.factor(y)
  
  fisherTable = as.matrix(table(x, y))
  cat('\ncontingency table:\n')
  print(fisherTable)
  cat('\n***All 2X2 tests****\n\n')
  
  # all 2x2 tests
  levelsY = levels(y)
  levelsX = levels(x)
  
  m = length(levelsX)
  n = length(levelsY)
  
  confLevel = 0.95
  if (confL !=0 ) confLevel=confL
  
  for (i in 1:(m-1))
    for(j in ((i+1):m))
      for (k in 1:(n-1))
        for (l in ((k+1):n)) {
          
          fisherIndexX = c(i,j)
          fisherIndexY = c(k,l)
          
          fTempTable = as.matrix(fisherTable[fisherIndexX,
                                             fisherIndexY])
          
          fisherTest=fisher.test(fTempTable, conf.int = TRUE, 
                                 conf.level = confLevel)
          
          if((fisherTest$p.value < (1-confLevel) ) || printAll){
            
            print(c(levelsX[i], levelsX[j]))
            print(c(levelsY[k], levelsY[l]))
            print(fTempTable)
            print(fisherTest)
            cat('\n')
          }
          
        }
}