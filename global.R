library(shinydashboard)
library(shiny)
library(DT)
library(stringr)
library(ggplot2)
library(lpSolve)
library(httr)
library(jsonlite)
library(tidyr)
library(RCurl)
library(curl)

options(stringsAsFactors = FALSE)
set_config( config( ssl_verifypeer = 0L ) )

load("data/results.Rdata")
load("data/char_lookup.Rdata")

source("herostats.R")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
heroes <- unique(unlist(lapply(results, function(x) names(x$us$heroes$stats$competitive))))
heroes2 <- unique(unlist(lapply(results, function(x) names(x$us$heroes$stats$quickplay))))
heroes <- unique(c(heroes, heroes2))
g <- lapply(results, function(x) parseHeroStats(x, mode='comp'))
gq <- lapply(results, function(x) parseHeroStats(x, mode='qp'))

calculateScores <- function(hero, aLoP){
  print(hero)
  y <- lapply(aLoP, function(x) x[[hero]])
  r <- do.call('rbind', y)
  if(!is.null(r)){
    r[which(r == 'NULL')] <- 0
    r <- data.frame(r)
    r <- as.data.frame(lapply(r, unlist))
    #non_per_min <- grep("time|win", colnames(r))
    #i <- 4:ncol(r)
    #j <- i[-match(non_per_min, i)]
    per_min <- r[, 4:ncol(r)] / (r$tp * 60)
    colnames(per_min) <- paste0(colnames(per_min), "_per_min")
    x <- cbind(r, per_min)
    m<-x[,grep("per_min", colnames(x))]
    if(!is.null(m$deaths_per_min)){
      m$deaths_per_min <- m$deaths_per_min * -10
    } 
    if(!is.null(m$environmental_deaths_per_min)){
      m$environmental_deaths_per_min <- m$environmental_deaths_per_min * -20
    }
    
    if(hero == 'zenyatta'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(2,2,2,2,2,3,3,3,3,2,3,1,1,2,1,1,2,2,2,2,1,2,1,3,3)))
    } else if (hero == 'reinhardt'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(2,3,3,3,2,1,1,1,2,2,3,2,2,1,1,2,2,1,2,3,2,1)))
    } else if (hero == 'lucio'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,2,1,1,1,1,2,3,1,3,1,3,3,1,2,2,1,1,2,2,1,2,2,2,1,3)))
    } else if (hero == 'roadhog'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,1,3,3,3,3,3,1,1,1,2,2,1,3,2,1,2,2,1,1,2,2,1,2,2,2,1)))
    } else if (hero == 'pharah'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,3,3,2,2,2,3,2,3,1,3,3,1,2,3,1,1,2,2,1,2,1,2,1)))
    } else if (hero == 'mercy'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,1,1,1,1,1,1,1,1,1,3,1,1,3,1,3,2,1,1,1,1,1,2,1,2,1,1,3,3)))
    } else if (hero == 'soldier76'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(2,2,1,2,3,2,2,2,3,2,1,1,3,3,1,2,1,2,1,1,2,2,1,2,1,2,1)))
    } else if (hero == 'dva'){
      m$mech_deaths <- m$mech_deaths * -20
      score <- apply(m, 1, function(x) weighted.mean(x, c(2,1,2,1,1,1,2,1,2,1,2,3,1,3,3,2,1,1,2,2,1,2,2,3,2,1,3)))
    } else if (hero == 'mei'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,3,2,2,3,2,1,1,2,1,3,1,2,3,2,1,2,2,1,1,2,2,2,2,3,2,1)))
    } else if (hero == 'reaper'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(2,3,3,3,3,3,1,2,2,3,2,1,2,1,1,2,2,1,2,1,2,1)))
    } else if (hero == 'genji'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,2,3,3,3,2,1,2,2,3,2,3,2,1,1,1,2,1,2,1,2,1)))
    } else if (hero == 'tracer'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,3,3,3,3,2,1,1,1,2,3,2,1,2,1,1,2,2,1,2,1,2,1)))
    } else if (hero == 'winston'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,2,1,1,2,2,2,2,3,2,1,1,2,2,1,2,1,3,3,2,1)))
    } else if (hero == 'zarya'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,3,3,3,2,2,1,1,2,2,3,2,3,2,1,1,2,2,1,2,3,2,1)))
    } else if (hero == 'ana'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,3,3,3,1,1,1,2,2,3,3,3,3,1,2,2,1,1,2,2,1,2,1,2,1,3,3)))
    } else if (hero == 'bastion'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(2,1,1,1,1,3,2,1,3,1,1,1,1,2,1,1,1,2,2,1,2,2,1)))
    } else if (hero == 'symmetra'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,3,3,3,1,2,2,2,3,2,1,1,2,2,1,2,1,2,2,1)))
    } else if (hero == 'junkrat'){
      score <- apply(m, 1, function(x) weighted.mean(x, c(3,2,2,3,2,1,2,3,2,1,1,1,1,2,1,2,1,1,2,2)))
    }
    x$score <- score
    return(x)
  }
}

all_results <- sapply(heroes, function(x) calculateScores(x, g))
needed_fields <- lapply(all_results, function(x) data.frame(hero=x$hero, user=x$username, score=x$score, games_played=x$games_played))
scaled_scores <- lapply(needed_fields, function(y) range01(y$score))
final <- mapply(cbind, needed_fields, scaled=scaled_scores)

final <-do.call('rbind', final)
final$type <- char_lookup[match(final$hero, char_lookup$hero),]$type
if(any(final$score == 'NaN')){
  final <- final[-which(final$score == 'NaN'),]
}

all_results_q <- sapply(heroes, function(x) calculateScores(x, gq))
needed_fields_q <- lapply(all_results_q, function(x) data.frame(hero=x$hero, user=x$username, score=x$score, games_played=x$tp))
if(any(unlist(lapply(needed_fields_q, function(x) any(is.na(x)))))){
  i <- which(needed_fields_q[which(unlist(lapply(needed_fields_q, function(x) any(is.na(x)))))][[1]]$score == 'NaN')
  needed_fields_q[which(unlist(lapply(needed_fields_q, function(x) any(is.na(x)))))][[1]] <- needed_fields_q[which(unlist(lapply(needed_fields_q, function(x) any(is.na(x)))))][[1]][-i,]
}
scaled_scores_q <- lapply(needed_fields_q, function(y) range01(y$score))
final_q <- mapply(cbind, needed_fields_q, scaled=scaled_scores_q)

final_q <-do.call('rbind', final_q)
final_q$type <- char_lookup[match(final_q$hero, char_lookup$hero),]$type
if(any(final_q$score == 'NaN')){
  final_q <- final_q[-which(final_q$score == 'NaN'),]
}
