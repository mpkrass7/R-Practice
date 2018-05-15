library(MASS)
?Boston
#Linear model function to make and plot models of everything on one value
#Basically the scrub version of pairs :-) 
RegressOnRest = function(df, num){
  
  #This part will determine our plot size
 # dims <- ceiling(sqrt(length(names(df))-1))
  #par(mfrow = c(dims, dims))
  
  #This part is to show all regression coefficients at the end
  coefficient <- c()
  
  #This part takes out the thing we're regressing on
  df2 <- df[,-num]
  
  #This part filters for only numeric columns
  numberCols <- sapply(df2, is.numeric)
  df2 <- df2[,numberCols]
  
  #This part loops through and regresses the new df on the value of the df specified
  #It will print a summary and plot each linear regression
  for (i in seq(1:ncol(df2)))
  {
    imodel <- lm(df[,num] ~ df2[,i])
    regressor <- names(df2)[i]
    print(sprintf("Regressing %s on %s", names(df[num]),regressor))
    print(summary(imodel))
    plot(df[,num] ~ df2[,i], xlab = names(df2)[i], ylab = names(df)[num])
    abline(imodel)
    coefficient <- c(coefficient, coefficients(imodel)[2])
  }
  #Puts some names on the coefficient values for the end
  names(coefficient) <- names(df2)
  return(coefficient)
}

RegressOnRest(Boston,5)

pairs(Boston)
