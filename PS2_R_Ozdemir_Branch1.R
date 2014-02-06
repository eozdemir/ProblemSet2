#Branch1-distribution is calculated incorrectly for dataset1
rm(list = ls()) #clear the workspace
results<- sample(1:100, 10, replace=TRUE)#generate random election results 
my.fnc1<- function(results,stat){ #function to derive the statistics
  i<- as.numeric(substr(results, start=1, stop=2)) #takes the first integers and stores
  #to calculate distribution incorrectly, changed one of the options (stop=2)
  if(stat=="m"){        #if stat option is specified as "m", returns m statistics
    for(n in 1:9){     #applies the formula for m statistics for every integer stored
      X<- (sum(results[i==n]))/(sum(results)-log(1+1/i))
      m<- sqrt(length(results))*max(X)
      return(m)
    }
  }  
  else if(stat=="d"){    #if stat option is specified as "d", returns d statistics
    for(n in 1:9){      #applies the formula for d statistics for every integer stored
      Y<- sum((sum(results[i==n]))/(sum(results)-log(1+1/i))^2)
      d<- sqrt(length(results))*sqrt(Y)
      return(d)
    }
  }
  else{     #if stat option is not specified as "m" or "d", it returns both statistics
    for(n in 1:9){  #derive m statistics, same as above
      X<- (sum(results[i==n]))/(sum(results)-log(1+1/i))
      m<- sqrt(length(results))*max(X)
      Y<- sum((sum(results[i==n]))/(sum(results)-log(1+1/i))^2)
      d<- sqrt(length(results))*sqrt(Y)
      output<-c(m,d)  
      return(output) #return two values in the function
    }   
}
}  

dataset1<- c(10,10,10,10,10)

unit.test1<- function(){
  x1<- c(0.851<my.fnc1(results=dataset1, stat="m"))
  x2<- c(1.212<my.fnc1(results=dataset1, stat="d"))
  logical.x<- c(x1,x2)
  return(all(logical.x))
}
unit.test1()
