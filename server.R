


range01 <- function(x){(x-min(x))/(max(x)-min(x))}

base_url <- 'https://owapi.net/api/v3/u/'
server <- function(input, output) {
  players <- reactiveValues()
  players$list <- unique(final$user)
  new_players <- reactiveValues()
  new_players$list <- list()
  the_stats <- reactiveValues(stats = final)
  table_results <- reactiveValues(results = all_results)
  
  observeEvent(input$reset2, {
    the_stats$stats <- final
  })
  
  observeEvent(input$add_player, {
    withProgress(message = 'Fetching Battle.net Stats...', value = 0, {
        incProgress(amount = 1 / length(heroes), message = paste0('Getting ', input$newPlayer, ' stats'))
        user <- input$newPlayer
        user <- gsub("#", "-", user)
        players$list[user] <- user
        the_url <- paste0(base_url, user, '/heroes')
        r <- GET(the_url)
        stats <- fromJSON(content(r, 'text'))
        i <- length(results) + 1
        results[[i]] <- stats
        hs <- lapply(results, function(x) parseHeroStats(x))
        all_results <- sapply(heroes, function(x) calculateScores(x, hs))
        table_results$results <- all_results
        
        needed_fields <- lapply(all_results, function(x) data.frame(hero=x$hero, user=x$username, score=x$score, games_played=x$games_played))
        scaled_scores <- lapply(needed_fields, function(y) range01(y$score))
        updated <- mapply(cbind, needed_fields, scaled=scaled_scores)
        updated <-do.call('rbind', updated)
        updated$type <- char_type[match(updated$hero, char_type$heroes),]$type
        the_stats$stats <- updated
    })
  })
  
  showPlayers <- reactive({
    the_users <- players$list
    the_text <- 'Showing results for users: <br><ul>'
    for(u in the_users){
      the_text <- paste0(the_text, paste0('<li>', u, '</li>'))
    }
    the_text <- paste0(the_text, '</ul>')
    HTML(the_text)
  })
  
  output$about <- renderText(HTML("<br>Optimal hero score calculations: <a href = 'https://docs.google.com/spreadsheets/d/1IA_lxxftfDh6_eEH70xPqxopIFRosIkOOe8F9IZPHtQ/edit#gid=0', target='_blank'>Here</a>
                                  <br>Note: Scores not available yet for McCree, Widowmaker, Orisa, Torb, Sombra, Hanzo. <p>
                                  Data from: <a href = 'https://github.com/SunDwarf/OWAPI/blob/master/api.md'>OWAPI</a>"))
  output$players <- renderText({ showPlayers()})
  
  parseStats <- reactive({
    if(length(players$list) >= 6){
      some_stats <- subset(the_stats$stats, games_played >= as.numeric(input$min_gp))
      some_stats$id <- 1:nrow(some_stats)
      return(some_stats)
    }
  })
  
  output$table1 <- DT::renderDataTable({
    player_stats <- parseStats()
    if(length(unique(player_stats$user)) < 6){
      return()
    }
    player_stats$hero <- as.character(player_stats$hero)
    player_stats$user <- as.character(player_stats$user)
    con <- rbind(t(model.matrix(~ type + 0, player_stats)), t(model.matrix(~ hero + 0, player_stats)), t(model.matrix(~ user + 0, player_stats)), rep(1, nrow(player_stats)))
    dir <- c("=", "=", "=", "=", rep("<=", length(unique(player_stats$hero))), rep("<=", length(unique(player_stats$user))), "=")
    rhs <- c(input$damage,input$defense,input$support,input$tank, rep(1, length(unique(player_stats$hero))), rep(1, length(unique(player_stats$user))), 6)
    # 
    # if(input$objective == 'Medals Per Game'){
    #   obj <- player_stats$mpg
    # } else if(input$objective == 'On Fire Per Game'){
    #   obj <- player_stats$fire
    # } else if(input$objective == 'Gold Medals Per Game'){
    #   obj <- player_stats$gmpg
    # } else if(input$objective == 'Healing + Damage'){
    #   obj <- player_stats$key_metric
    # } else if (input$objective == 'Healing + Elims'){
    #   obj <- player_stats$key_metric2
    # } else if (input$objective == 'Objective Time'){
    #   obj <- player_stats$obj_avg
    # }
    
    obj <- player_stats$scaled
    opt <- lp("max", obj, con, dir, rhs, all.bin=TRUE)
    optcomp <- player_stats[which(opt$solution == 1),]
    optcomp$exclude <- paste0("<button id='button_", optcomp$id, "' type=\'button\' class=\'btn btn-default action-button\' onclick=\'Shiny.onInputChange(&quot;select_button&quot;, this.id)\'>Exclude</button>")
    optcomp[,c(2,1,4,5,6,8)]
  }, rownames= FALSE, selection = 'single', options=list(bPaginate=FALSE, bFilter=FALSE), caption='Optimal Team Comp', escape = FALSE)
  
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    player_stats <- parseStats()
    user <- player_stats[match(selectedRow, player_stats$id),]$user
    
    the_stats$stats <- the_stats$stats[-which(the_stats$stats$user %in% user),]
    
  })
  
   output$table2 <- DT::renderDataTable({
  #   player_stats <- parseStats()
  #   y_value <- switch(input$objective, 
  #                     
  #                     'Medals Per Game'= player_stats$mpg,
  #                     'On Fire Per Game'= player_stats$fire,
  #                     'Gold Medals Per Game'= player_stats$gmpg,
  #                     'Healing + Damage'= player_stats$key_metric,
  #                     'Healing + Elims'= player_stats$key_metric2,
  #                     'Objective Time' = player_stats$obj_avg)
  #   player_stats$y_value <- y_value
  #   x <- subset(player_stats, hero == input$hero)
  #  ggplot(x, aes(x=reorder(user, -y_value), y=y_value, fill=games_played)) + geom_bar(stat='identity') + labs(y = input$objective, title=input$hero) + theme_gray(base_size = 16)
  
     x <- table_results$results[[which(names(table_results$results) == input$hero)]]
     x <- subset(x, games_played >= as.numeric(input$min_gp))
     y<-x[, grep("per_min", colnames(x))]
     remove <- c("win_percentage_per_min", "eliminations_per_life_per_min", "critical_hit_accuracy_per_min", "weapon_accuracy_per_min", "games_played_per_min")
     i <- match(remove, colnames(y), 0)
     y <- y[,-i]
     
     y <- cbind(x$user, range01(x$score), x[,gsub("_per_min", "", remove[which(i != 0)])],  y)
     colnames(y)[1:2] <- c("User", "Score")
     y <- y[order(y$Score, decreasing=TRUE),]
     datatable(y, caption=paste0('User stats for ', input$hero), options = list(scrollX = TRUE, order = list(2, 'desc')))
      })
}
