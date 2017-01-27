library(plyr)
library(dplyr)

weather <- read.csv(file = "weather.csv")
flightCost <- read.csv(file = "flightCost.csv")
hotelCost <- read.csv(file = "hotelCost.csv")
cities <- read.csv(file = "cities.csv")

isTargetDay <- function(state, dayNum) {
  return(state$dayNum == dayNum)
}

getFlightCost <- function(cityA, cityB) {
  return(flightCost %>% 
           filter((cityStart == cityA & cityDest == cityB) | (cityStart == cityB & cityDest == cityA)) %>% 
           select(flightCost))
}

getHotelCost <- function(cityH) {
  return(hotelCost %>% 
           filter(city == cityH) %>% 
           select(hotelCost))
}

getWeather <- function(cityW, dayNum) {
  return(weather %>% 
           filter(city == cityW, DayNum == dayNum) %>% 
           select(weather))
}

getTotalCost <- function(cityStart, cityDest) {
  return(getFlightCost(cityStart, cityDest) + getHotelCost(cityDest))
}

computeTarget <- function(cStart, cDest, dayNr, weatherInfluence) {
  action <- getTotalCost(cStart, cDest)
  weatherForCity <- getWeather(cDest, dayNr)
  return(action*(1/weatherForCity)*weatherInfluence)
}

prepareNodes <- function() {
  return(ddply(weather, .(city, DayNum), function(x) {
    data.frame(
      city = x$city,
      dayNum = x$DayNum
    )
  }) %>% select(city,dayNum) %>% arrange(dayNum))
}

constructGraph <- function(nodeSet, weatherInfluence) {
  return(ddply(nodeSet, .(city, dayNum), function(x) {
    ddply((nodeSet %>% filter(dayNum == x$dayNum + 1)), .(city, dayNum), function(y) {
      data.frame(
        nodeStart = x,
        nodeDest = y,
        gScore = computeTarget(x$city, y$city, y$dayNum, weatherInfluence)$flightCost
      )
    })
  })) %>% select(nodeStart.city, nodeStart.dayNum, nodeDest.city, nodeDest.dayNum, gScore)
}

getFirstN <- function(set, n) {
  return(set %>% arrange(gScore) %>% slice(1:n))
}

startState <- function(nodes) {
  return(ddply((nodes %>% filter(city == "start", dayNum == 0)), .(city, dayNum, hScore), function(x) {
    data.frame(
      prevCity = "start",
      gScore = 0 
    )
  }))
}

findBest <- function(set) {
  return(set %>%
           slice(which.min(gScore + hScore)))
}

getAvailableEdges <- function(node, edges) {
  return(edges %>%
           filter(nodeStart.dayNum > node$dayNum | nodeStart.dayNum == node$dayNum & nodeStart.city == node$city)
  )
}

addhScore <- function(nodes, edges, targetDayNum) {
  return(ddply(nodes, .(city, dayNum), function(x){
    available <- getAvailableEdges(x, edges)
    data.frame(
      hScore = sum(getFirstN(available, targetDayNum - x$dayNum)$gScore)
    )
  }))
}

addStates <- function(set, state) {
  return(rbind(set, state))
}

removeState <- function(set, state) {
  return(set[!(set$city == state$city
               & set$prevCity == state$prevCity
               & set$dayNum == state$dayNum
               & set$hScore == state$hScore
               & set$gScore == state$gScore),])
}

existsByCityAndDayNum <- function(set, stateR) {
  return(set %>% filter(city == stateR$city & dayNum == stateR$dayNum))
}

createNewState <- function(currState, nextCity, gScore, nodes) {
  return(
    data.frame(
      city = nextCity,
      prevCity = currState$city,
      dayNum = currState$dayNum + 1,
      gScore = currState$gScore + gScore,
      hScore = 0
    )
  )
}

expandState <- function(curr, nodes, edges) {
  neighbours <- edges %>% 
    filter(nodeStart.city == curr$city, nodeStart.dayNum == curr$dayNum)
  if(nrow(neighbours) == 0) {
    return(NULL)
  }
  return(ddply(neighbours, .(nodeStart.city, nodeStart.dayNum, nodeDest.city, nodeDest.dayNum, gScore), function(x) {
    data.frame(
      city = x$nodeDest.city,
      prevCity = curr$city,
      dayNum = x$nodeDest.dayNum,
      gScore = curr$gScore + x$gScore,
      hScore = (nodes %>% filter(city == x$nodeDest.city, dayNum == x$nodeDest.dayNum))$hScore
    )
  }) %>% select(city, prevCity, dayNum, gScore, hScore))
}

findOpenStates <- function(openSet, closedSet, newStates) {
  return(ddply(newStates, .(city, prevCity, dayNum, gScore, hScore), function(x) {
    if(nrow(existsByCityAndDayNum(closedSet, x)) == 0) {
      return(x)
    }
  }))
}

updateSet <- function(set, states) {
  return(ddply(set, .(city, prevCity, dayNum, gScore, hScore), function(x){
    occurence <- existsByCityAndDayNum(states, x)
    if(nrow(occurence) == 0) {
      return(x)
    } else {
      if((x$gScore + x$hScore) > (occurence$gScore + occurence$hScore)) {
        return(occurence)
      }
    }
  }))
}

getNewStates <- function(set, states) {
  return(ddply(states, .(city, prevCity, dayNum, gScore, hScore), function(x) {
    occurence <- existsByCityAndDayNum(set, x)
    if(nrow(occurence) == 0) {
      return(x)
  }}))
}

mergeStates <- function(set, states) {
  if(is.null(states)) {
    return(set)
  }
  #update
  set <- updateSet(set, states)
  #add new
  return(addStates(set, getNewStates(set, states)))
}

getResult <- function(set, targetDayNum) {
  return(set %>% 
           filter(city == "start", dayNum == targetDayNum) %>%
           slice(which.min(gScore)))
}

reconstructPath <- function(set, state) {
  prevState <- set %>%  slice(which(city == state$prevCity & dayNum == (state$dayNum - 1)))
  if(nrow(prevState) == 0) {
    return(state)
  }
  return(addStates(state, reconstructPath(set, prevState)))
}

runAStar <- function(targetDayNum, weatherInfluence) {
  nodes <- prepareNodes()
  edges <- constructGraph(nodes, weatherInfluence)
  nodes <- addhScore(nodes, edges, targetDayNum)
  
  curr <- NULL
  closedSet <- NULL
  openSet <- startState(nodes)
  
  while(nrow(openSet) != 0) {
    curr <- findBest(openSet)
    if(isTargetDay(curr, targetDayNum)){
      return(reconstructPath(closedSet, curr))
    }

    closedSet <- addStates(closedSet, curr)
    openSet <- removeState(openSet, curr)

    newStates <- expandState(curr, nodes, edges)
    openStates <- findOpenStates(openSet, closedSet, newStates)
    openSet <- mergeStates(openSet, openStates)
  }

  stop("Failure - path has not been found")
}