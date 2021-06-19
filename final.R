# USER DEFINED PARAMETERS  -----------------------
define <-function()
{
  x_ee$w_size<-2 # cnt represents no of seconds per window. i.e =3 seconds
  x_ee$no_of_rules<-10 # no of rules --> buffer size
  x_ee$MST<-0.57   # minmum support threshold
  x_ee$Max_level_complexity<-3 # maximum level of rule complexity
  x_ee$sample <-30  # sample size (in seconds)
  
  x_ee$results<- as.data.frame(matrix(0,nrow=5,ncol=4))  # Data Frame for recording results 
  x_ee$resultsIndex<-0  # row number in results DF
}
#---------------------------------------------
#  Varible reset 
reset_vars <- function()
{
  #============================================================
  for(q in 1:x_ee$no_of_rules)
  {
    x_ee$a[q]<-""
    x_ee$b[q]<-0
    x_ee$acc_A[q]<-""
    x_ee$acc_B[q]<-0.0
   # rules_weight<<- matrix(x_ee$no_of_rules,10) # rules weight 
  #  x_ee$results<- as.data.frame(matrix(0,nrow=5,ncol=4))
    
  }
}
# ---------  Printing 
printOut <- function()
{
    sum1<-0
  sum2<-0
 
  
 # cat("index......................",x_ee$resultsIndex,"K:",k,"\n")
  
   for (q in 1:x_ee$no_of_rules){sum1<-sum1+x_ee$rules_weight[q]}
 
  for (q in 1:x_ee$no_of_rules){x_ee$rules_weight[q]<-x_ee$rules_weight[q]/sum1;sum2<-sum2+x_ee$rules_weight[q];
  if(x_ee$rules_weight[q]>0.0001)
  {
    cat(x_ee$rules[q],x_ee$rules_weight[q],"\n")
   
    # ===== Writing results in results DF 
    x_ee$resultsIndex=x_ee$resultsIndex+1   # increase row number in DF
    
     x_ee$results[x_ee$resultsIndex,1]=k  # Fold number
     x_ee$results[x_ee$resultsIndex,2]=x_ee$rules[q]   # Rule
    x_ee$results[x_ee$resultsIndex,3]=x_ee$rules_weight[q]  # Rule weight
   
    
      #==   End writing 
  
  }
  }
}

#=======   
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}


#============
migrate <- function(x_ee,i) 
{
  
  x_ee$va<-cbind(x_ee$va,as.vector(x_ee$a))
  x_ee$vb<-cbind(x_ee$vb,as.vector(x_ee$b))
  update_rules(x_ee,i)
  #   for (q in 1:x_ee$no_of_rules){x_ee$a[q]<-" ";x_ee$b[q]<-0}
}

update_rules<-function(x_ee,i)
{
  rule_sum<-0
  select_rules(x_ee,i)
}


select_rules<-function(x_ee,i)
{
  cr<-""     # current rule
  cr_weight<-0.0   # current weight for the rule
  
  cat("----------------","\n")
  
  cat("current window:",win,"   current weight:",x_ee$current_weight,"\n")
  
  for (q in 1:x_ee$Max_level_complexity)
  {
    cat("list of rules:",x_ee$a[q],"   events:",x_ee$b[q],"\n")  
    s<-1
    flag<-0
    while(s<=x_ee$no_of_rules ) 
    {
      if(x_ee$a[q]==x_ee$rules[s])
      {
        x_ee$rules_weight[s]<- x_ee$rules_weight[s]+(x_ee$b[q]*x_ee$current_weight) # weight rebalancing (past & current)
        s<-x_ee$no_of_rules
        flag<-1
      }
      if((x_ee$rules[s]=="") && (flag==0))
      {
        x_ee$rules[s]<-x_ee$a[q];
        x_ee$rules_weight[s]<- x_ee$b[q]*x_ee$current_weight
        s<-x_ee$no_of_rules
      }
      s<-s+1
      flag<-0
    }
  }
}

#============
modify <- function(x_ee,i,m) {
  j <- 1
  flag <- 0
  s <- " ";
  # to update events table 
  while (j < x_ee$no_of_rules)
  {
    if (x_ee$a[j] == m)
    {
      flag <- 1
      x_ee$b[j] <- x_ee$b[j] + 1
    }
    if (x_ee$a[j] == "")
    {
      s <- toString(data[i, 1, 1])
      x_ee$a[j] <- s
      x_ee$b[j] <- x_ee$b[j] + 1
      flag <- 2
    }
    
    if (flag > 0)   # means an update exist
    {
      #===========CHECK SWAP ===============================
      if (x_ee$b[j] > x_ee$b[j - 1] && j > 1)
      {
        t <- x_ee$b[j]
        x_ee$b[j] <- x_ee$b[j - 1]
        x_ee$b[j - 1] <- t
        t1 <- x_ee$a[j]
        x_ee$a[j] <- x_ee$a[j - 1]
        x_ee$a[j - 1] <- t1
      }
      #========== END SWAP  ================================
      break
    }
    j <- j + 1
  }#- end of events table update
}

eventDelay<-function(x)
{
  m<-0
  while(m<x)
  {
   t1<-as.integer(second(Sys.time()))
    m=m+1
  }
}

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#---------------------------------  MAIN Program --------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

library(lubridate)
dev.off(dev.list()["RStudioGD"])
cat("\014")
setwd("/Users/alaaalakari/Desktop/work/data/R")
x_ee<- new.env()


#-------------- Parameters 

assign('a', NULL,envir=x_ee)
assign('b',NULL,envir=x_ee)

assign('va', NULL,envir=x_ee)
assign('vb',NULL,envir=x_ee)
assign('w_size',NULL,envir=x_ee)
assign('no_of_rules',NULL,envir=x_ee)
assign('Max_level_complexity',0,envir=x_ee)
assign('sample',0,envir=x_ee)
assign('MST',0,envir=x_ee)
assign('total_window',0,envir=x_ee)
assign('current_weight',0,envir=x_ee)
assign('rules',NULL,envir=x_ee)
assign('rules_weight',NULL,envir=x_ee)



define()  # ---   USER DEFINED PARAMETERS ---------------

#x_ee$total_window<-0   # total window size 
#x_ee$current_weight<-0 


#====   intilize events table ===========
a <<- matrix(x_ee$no_of_rules) # event array
b <<- matrix(x_ee$no_of_rules) # counting array


rules<<- matrix(x_ee$no_of_rules,10) # rules 
rules_weight<<- matrix(x_ee$no_of_rules,10) # rules weight 


#======= READ input data   =================================

data <- read.csv("tmpJunk.csv",  strip.white = TRUE) # read stream input
#data <- read.csv("111111.csv",  strip.white = TRUE) # read stream input

#data <- read.csv("power_cons.csv",  strip.white = TRUE) # read stream input

#===============================================================

reset_vars()  # reset all varibles
for (q in 1:x_ee$no_of_rules){x_ee$rules[q]<-"";x_ee$rules_weight[q]<-0.0}
#-------------------------------------------------------
for(k in 1:1)   # k represents number of Folds
{
  for (q in 1:x_ee$no_of_rules){x_ee$rules[q]<-"";x_ee$rules_weight[q]<-0.0}

  x_ee$total_window<-0   # total window size 
  x_ee$current_weight<-0 
   
i<-1    # i represents starting row in dataset

i=(x_ee$sample*(k-1))+1

t1<-data[i,3]
m_row<-nrow(data)

cat(k,":New Fold^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",i,"\n")
while(data[i,3]<=x_ee$sample)
{
  
  reset_vars()  # reset all varibles
  
  cnt<-1
  win<-0

  # ----- start of new window 
  
  while(cnt<=x_ee$w_size) # cnt represents no of second per window
  {
    
    
    
    
t2=t1
      while(t1==t2)
    {
      if(i==m_row){break} # end of stream detected 

      #......................... Firing event  ...................................................
#   cat(k,":","fire event : ",toString(data[i,1])," #:",i,"--",data[i,3],"cnt:",cnt,"t1:",t1,"win:",win,"\n") 
      modify(x_ee,i,data[i,1,1])
 
      #----- delay function
     # eventDelay(500)#x_ee$sample*30/m_row)   
 
     #=================================
      i<-i+1
      win<-win+1
      t1<-data[i,3]
    }
    
    if(i==m_row){break}
    t1<-data[i,3]
    cnt<-cnt+1  # Seconds increment within time window
  }
  cat("cnt:",cnt,"data:",data[i,3],"data previos:",data[i-1,3],"\n")
  if(i==m_row){break}
  
  # ................................  End of window .................................
  
  
  x_ee$total_window<-x_ee$total_window+win  # calculating total window 
  x_ee$current_weight<-win/x_ee$total_window  # calculating current window weight ( W = win/total   )
  
  
  cat("no of events per window:",win,"\n")
  cat("total window size so far:",x_ee$total_window,"\n")
  cat("current window weight:",x_ee$current_weight,"\n")
  cat("past weight:",(1-x_ee$current_weight),"\n")
  cat("====:",i,"\n")
  migrate(x_ee,i)
}





printOut()
#results
write.csv(x_ee$results,'results.csv')
}
#===================================
