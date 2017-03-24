# read node annotations
gf <- read.csv('nodes.csv')
# read path/node weights
wf <- read.csv('type_weights.csv')
#merge data frame on type
df <- merge(gf,wf)
ind <- order(df$row,df$column)
df <- df[ind,]
df$index <- 1:nrow(df)
reward = df$reward

#profit <- df$reward
#profit[is.na(profit)] <- -Inf
#cost <- 1-(1/(1+exp(-w)))
#df$cost <- cost 


# turn row,col index to linear index
# board size is 9 x 9
rc2ind <- function(rind,cind,sz=9)
{
  if( rind<1 | cind <1 | rind >sz | cind > sz )
  {
    ind <- NA
  } else
  {
    ind <- ((rind-1)*sz)+cind
  }
  return(ind)
}

ind2rc <- function(ind,sz=9)
{
  rind <- as.vector(floor(ind/sz))
  cind <- as.vector(ind-(sz*rind))
  if(cind==0)
  {
    cind = sz
  } else {
    rind <- rind+1  
  }
  
  return(rbind(rind,cind))
}


getMoveCost<- function(pathIndex1,pathIndex2,currPos,nextPos)
{
  # to make rewrd symmetric
  kk = length(pathIndex1)
  offset = (reward[currPos]-reward[nextPos])/2
  a = sum(reward[pathIndex1])+offset
  b = sum(reward[pathIndex2])+offset
  
  if (sum(is.na(c(a,b)))==2){
    return(NA)
  } else
  {
    return(max(a,b,na.rm=T))
  }
}

getKnightPath <- function(currPos,nextMove)
{
  i=currPos[1]
  j=currPos[2]
  
  m <- length(nextMove)
  path <- rep(NA,m)
  for (kk in 1:m) {
    if(nextMove[kk]=="N") {
      i <- i-1
    } else if (nextMove[kk]=="S") {
      i <- i+1
    } else if (nextMove[kk]=="W") {
      j <- j-1
    } else {
      # E
      j <- j+1
    }
    path[kk] <- rc2ind(i,j)
  }
  return(path)
}

move2stringWithMaxReward <-function(currPos,nextPos)
{
  a1 <- ind2rc(currPos)
  b1 <- ind2rc(nextPos)
  dff <- b1-a1
  i <- dff[1]
  j <- dff[2]
  
  m1 <- rep("S",abs(i))
  if(i<0){
    m1 <- rep("N",abs(i))
  }
  
  if(j<0){
    m1 <- c(m1,rep("W",abs(j)))
  } else {
    m1 <- c(m1,rep("E",abs(j)))
  }
  
  p1 <- getKnightPath(ind2rc(currPos),m1)
  p2 <- getKnightPath(ind2rc(currPos),rev(m1))
  r1 <- sum(reward[p1])
  r2 <- sum(reward[p2])
  
  move <- m1
  
  if(is.na(r1)){
    move <- rev(m1)
  } else if(is.na(r2)){
    move <- m1 
  } else{
    if(r1 < r2){
      move <- m1
    }
  }
  return(move)
}

getKnightPathMaxReward <- function(currPos,nextMove)
{
  kk <- length(nextMove)
  p1 <- getKnightPath(currPos,nextMove)
  p2 <- getKnightPath(currPos,rev(nextMove))
  
  nextPos = p1[kk]
  rw <- getMoveCost(p1,p2,rc2ind(currPos[1],currPos[2]),nextPos)
  return( list(index=p1[length(p1)],reward=rw))
}

getMovesAndRewards <-function(fromNode) {
  
  moves <- matrix(NA,8,3)
  mdf <- data.frame(to=rep(NA,8),reward=rep(NA,8))
  
  moves[1,] <- c("N","N","E")
  moves[2,] <- c("N","N","W")
  moves[3,] <- c("S","S","E")
  moves[4,] <- c("S","S","W")
  
  moves[5,] <- c("E","E","N")
  moves[6,] <- c("E","E","S")
  moves[7,] <- c("W","W","N")
  moves[8,] <- c("W","W","S")
  
  for (kk in 1:8){
    mv <- getKnightPathMaxReward(fromNode,as.vector(moves[kk,]))
    mdf$to[kk] <- mv$index
    mdf$reward[kk] <- mv$reward
  }
  na.ind <- is.na(mdf$to)
  mdf <- mdf[!na.ind,]
  return(mdf)
}

# remove all nodes which are NA
df.sub <- df[!is.na(df$reward),]

n <- nrow(df)
adj <- matrix(NA,n,n)
node.id <- df$index
rownames(adj) <- node.id
colnames(adj) <- node.id

# calculate path lengths from every node to every node
# only consider the rewarding paths as there are two navigable paths

for (ii in 1:9){
  print(ii)
  for (jj in 1:9)
  {
    currPos <- c(ii,jj)
    currIndex <- rc2ind(ii,jj)
    
    if(!is.na(reward[currIndex])){
      mdf <- getMovesAndRewards(currPos)
      index <- mdf$to
      rw <- mdf$reward
      adj[currIndex,index] <- rw 
    }
    
  }
}

# get a node which is all NA
# this should be crevice
all.na <- apply(adj,1,FUN=function(tmp) sum(is.na(tmp)))
ind <- which(all.na==81)
sum(df$type[ind]=="crevice")
# yes, all rows with NAs are crevice nodes. they can be eliminated

adj.sub <- adj[-ind,]
adj.sub <- adj.sub[,-ind]

write.table(adj.sub,"AdjReward.csv",col.names = FALSE,row.names=FALSE,sep=",")
write.table(colnames(adj.sub),"AdjRewardIndexes.csv",col.names = FALSE,row.names=FALSE,sep=",")


checkAndGetValidPath <- function(soln)
{
  k <- length(soln)
  origin <- soln[1]
  
  originNN <- c(22,24,58,60,52,30,48)
  currIndex <-origin
  ind = 1
  valid = TRUE
  while(valid & (ind<k))
  {
    ind = ind+1
    nextIndex = soln[ind]
    valid = checkMove(currIndex,nextIndex)
    if(!valid){
      print("Failed at:")
      print(c(currIndex,nextIndex))
    }
    currIndex = nextIndex
  }
  if(ind==k){
    print("tour is valid");
    link <- list(valid=valid,path=path)
    # assume last index is always the orgin
  } else {
    # try recovering
    ind = ind-1
    # check if a path exists between the latest-in-the-valid-path and origin
    valid_path = soln[1:ind]
    link <- checkMoveWithNN(valid_path)
  }
  return(link)
}

checkMove<-function(currIndex,nextIndex)
{
  a1 <- ind2rc(currIndex)
  b1 <- ind2rc(nextIndex)
  d = sum((b1-a1)^2)
  return( ifelse(abs(d-5)<1e-1,TRUE,FALSE))
}

checkMoveWithNN <- function(valid_path)
{
  
  originNN <- c(22,24,58,60,52,30,48)
  nn <- length(valid_path)
  mm <- length(originNN)
  valid =  FALSE
  ind=nn
  while( (!valid) & ind>1)
  {
    nextIndex = valid_path[ind]
    if( (nextIndex %in% originNN ) )
    {
      # found match
      valid = TRUE
      valid_path <- c(valid_path[1:(ind-1)],nextIndex,41)
    }
    ind <- ind-1
  }
  
  # if above fails, see if orgin can be directly linked
  if(!valid)
  {
    ind = nn
    while ((!valid) & ind>1)
    {
      nextIndex = valid_path[ind]
      valid = checkMove(nextIndex,41)
      ind = ind-1
    }
    if(valid){
      valid_path <- c(valid_path[1:ind],41)
    }
  }
  return(list(valid=valid,path=valid_path))
}

scoreTour <- function(path)
{
  mm <- length(path)
  score = 0
  for(ii in 2:mm){
    score = score + adj[path[ii-1],path[ii]]
  }
  return(score)
}


path2string <-function(path)
{
  mm <- length(path)
  msg <- c()
  for (kk in 1:(mm-2))
  {
    move <- paste(move2stringWithMaxReward(path[kk],path[kk+1]),sep="",collapse=",")
    msg <- paste(c(msg,move),sep="",collapse="|")
  }
  move <- paste(move2stringWithMaxReward(path[mm-1],path[mm]),sep="",collapse=",")
  msg <- paste(c(msg,move),sep="",collapse="|")
}


soln <- c(38,44,29,11,4,22,12,19,34,52,68,61,45,63,70,59,53,35,51,69,62,46,40,30,36,54,47,41,31,21,2,9,20,26,10,28,18,1,3,14,7,25,15,8,24,42,48,64,71,60,43,27,37,55,39,49,33,16,5,23,13,6,17,32,50,66,72,57,67,73,56,38)
soln <- as.numeric(colnames(adj.sub)[soln])


res <- readLines("TourSampling-01.csv")
mm <- length(res)

bestScore = 0
bestPath = c()
bestMsg = 0
for (ii in 1:mm)
{
  cat("Iternation: ",ii)
  soln <- as.numeric(unlist(strsplit(gsub("\\]","",gsub("\\[","",res[[ii]])),",")))
  soln <- as.numeric(colnames(adj.sub)[soln])
  link <- checkAndGetValidPath(soln)
  if(link$valid)
  {
    path <- link$path
    score <- scoreTour(path)
    msg <- path2string(path)
    cat("score: ",score,"\n")
    if(score > bestScore)
    {
      bestScore = score
      bestPath = path
      bestMsg = msg
      bestInd = ii
  
    }
  }
}

write.table(noquote(bestMsg),"BestOutPut-02.txt",row.names = F,col.names = F,sep=",",quote=F)

soln <- readLines("BestOutPut-01.txt")
soln <- unlist(strsplit(soln,"|"))
path = c(41)
i=5;j=5;
mm = length(soln)
for (ii in 1:mm)
{
  nextPos = soln[ii]
  if(nextPos == ","){
    print("skip")
  } else if (nextPos == "E") {
    j=j+1
  } else if(nextPos=="W"){
    j = j-1
  } else if(nextPos=="N") {
    i = i-1
  } else if(nextPos=="S"){
    i = i+1
  } else if (nextPos=="|"){
    nextMove = rc2ind(i,j)
    path = c(path,nextMove)
  } else {
      cat("error")
  }
}
nextMove = rc2ind(i,j)
path = c(path,nextMove)
cat(path)
             


