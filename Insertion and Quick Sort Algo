While there are large number of sorting algorithms in which Insertion Sorting is widely used for small data sets so let us start with Insertion sort Algorithm.

1. Insertion-Sort Algo:
Below function for Insertion sort is used to sort the given vector/array which will return sorted vector of integer with step by step process of insertion sorting algorithm.

let us suppose an array arr : array of integers to sort, and n: size of an array , an integer.

InterimResult <- 0 
InsertionSort <- function(arr,n){
  for(num in 2:n){
    # num <- 5
    CurrentNum <- arr[num]
    LeftNum <- num-1
    while (LeftNum > 0 && arr[LeftNum] > CurrentNum){
      arr[(LeftNum + 1)] = arr[LeftNum]
      
      if(any(InterimResult == 0)){
        InterimResult <- arr
      }else{
        InterimResult <- data.frame(rbind(InterimResult,arr))
        rownames(InterimResult) <- NULL
      }
      
      LeftNum = LeftNum - 1 
    }
    arr[(LeftNum + 1)] <- CurrentNum
  }
  InterimResult <- rbind(InterimResult,arr)
  return(InterimResult)
}

# e.g.
# arr <- c(2,4,6,8,3)
# n <- 5
# Call Insertion Sort function written above, InsertionSort(arr,n) 
# Result : 
 X1 X2 X3 X4 X5
1  2  4  6  8  8
2  2  4  6  6  8
3  2  4  4  6  8
4  2  3  4  6  8



2. Quick-Sort Algo (Generally used to sort large data sets).

quick_sortAlgo <-function(Vector){

  # if length of input/given vector is equal to or less than one, return the same Vector.
  if(length(Vector)<=1) return(Vector)
  
  PivotValue<-Vector[1]
  RestValue<-Vector[-1]
  pivot_less<-quick_sortAlgo(RestValue[RestValue<PivotValue])
  pivot_greater<-quick_sortAlgo(Vector = RestValue[RestValue>=PivotValue])
  return(c(pivot_less,PivotValue,pivot_greater))
}

# e.g. 
# let us suppose a Vector <- c(5,4,12,13,3,8,6,88)
# Call Quick sort function written above, quick_sortAlgo(Vector)
# Result :
[1]  3  4  5  6  8 12 13 88

