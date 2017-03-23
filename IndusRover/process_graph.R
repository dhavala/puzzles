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
  rind <- rind+1
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


             


