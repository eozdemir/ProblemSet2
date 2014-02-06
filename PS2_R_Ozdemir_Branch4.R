##Branch 4-either of the statistics (d statistics in this case) are wrong for dataset2
rm(list = ls()) #clear the workspace
results<- sample(1:100, 10, replace=TRUE)#generate random election results 
my.fnc4<- function(results,stat){ #function to derive the statistics
  i<- as.numeric(substr(results, start=1, stop=1)) #takes the first integers and stores
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
      d<- sqrt(length(results))*sqrt(Y+100) #added 100 to Y to get a wrong d stats
      return(d)
    }
  }
  else{     #if stat option is not specified as "m" or "d", it returns both statistics
    for(n in 1:9){  #derive m statistics, same as above
      X<- (sum(results[i==n]))/(sum(results)-log(1+1/i))
      m<- sqrt(length(results))*max(X)
      Y<- sum((sum(results[i==n]))/(sum(results)-log(1+1/i))^2)
      d<- sqrt(length(results))*sqrt(Y+100)
      output<-c(m,d)  
      return(output) #return two values in the function
    }   
  }
}  
dataset2<- c(20,50,60,70,80) #dataset2 generated for unit test

unit.test4<- function(){
  x1<- c(0.851<my.fnc4(results=dataset2, stat="m"))
  x2<- c(1.212<my.fnc4(results=dataset2, stat="d"))
  logical.x<- c(x1,x2)
  return(all(logical.x)) #returns true if statistics confirm no fraud
}
unit.test4() #we will know if this is false