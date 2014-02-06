##Branch 2-either of the statistics (m statistics in this case) are wrong for dataset1 
rm(list = ls()) #clear the workspace
results<- sample(1:100, 10, replace=TRUE)#generate random election results 
my.fnc2<- function(results,stat){ #function to derive the statistics
  i<- as.numeric(substr(results, start=1, stop=1)) #takes the first integers and stores
  if(stat=="m"){        #if stat option is specified as "m", returns m statistics
    for(n in 1:9){     #applies the formula for m statistics for every integer stored
      X<- (sum(results[i==n]))/(sum(results)-log(1+1/i))
      m<- sqrt(length(results))*min(X) #instead of max, this function incorrectly uses minimum in m statistics formula
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
      m<- sqrt(length(results))*min(X)
      Y<- sum((sum(results[i==n]))/(sum(results)-log(1+1/i))^2)
      d<- sqrt(length(results))*sqrt(Y)
      output<-c(m,d)  
      return(output) #return two values in the function
    }   
  }
}  
dataset1<- c(10,10,10,10,10) #dataset1 generated for unit test

unit.test2<- function(){
  x1<- c(0.851<my.fnc2(results=dataset1, stat="m"))
  x2<- c(1.212<my.fnc2(results=dataset1, stat="d"))
  logical.x<- c(x1,x2)
  return(all(logical.x)) #returns true if statistics confirm no fraud
}
unit.test2() #we will know if this is false