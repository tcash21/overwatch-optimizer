
parseHeroStats <- function(user, mode='comp'){
  if(mode == 'comp'){
    heroes <- names(user$us$heroes$stats$competitive)
    username <- unlist(str_extract_all(user$`_request`$route, "\\w+-\\d+"))
    user <- user$us$heroes$stats$competitive
  } else if (mode == 'qp'){
    heroes <- names(user$us$heroes$stats$quickplay)
    username <- unlist(str_extract_all(user$`_request`$route, "\\w+-\\d+"))
    user <- user$us$heroes$stats$quickplay
  }
 
  stats <- list()
  for(hero in heroes){
    if(hero == 'reaper'){
      if(!is.null(user$reaper$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$reaper$general_stats$time_played,
            souls_consumed=user$reaper$hero_stats$souls_consumed,
            death_blossom_kills=user$reaper$hero_stats$death_blossom_kills,
            eliminations=user$reaper$general_stats$eliminations,
            final_blows=user$reaper$general_stats$final_blows,
            solo_kills=user$reaper$general_stats$solo_kills,
            critical_hits=user$reaper$general_stats$critical_hits,
            damage_done=user$reaper$general_stats$damage_done,
            objective_kills=user$reaper$general_stats$objective_kills,
            critical_hit_accuracy=user$reaper$general_stats$critical_hit_accuracy,
            eliminations_per_life=user$reaper$general_stats$eliminations_per_life,
            weapon_accuracy=user$reaper$general_stats$weapon_accuracy,
            self_healing=user$reaper$general_stats$self_healing,
            environmental_deaths=user$reaper$general_stats$environmental_deaths,
            medals_bronze=user$reaper$general_stats$medals_bronze,
            medals_silver=user$reaper$general_stats$medals_silver,
            medals_gold=user$reaper$general_stats$medals_gold,
            medals=user$reaper$general_stats$medals,
            cards=user$reaper$general_stats$cards,
            games_played=user$reaper$general_stats$games_played,
            objective_time=user$reaper$general_stats$objective_time,
            time_spent_on_fire=user$reaper$general_stats$time_spent_on_fire,
            win_percentage=user$reaper$general_stats$win_percentage)
      }
    } else if (hero == 'reinhardt'){
      if(!is.null(user$reinhardt$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$reinhardt$general_stats$time_played,
            damage_blocked=user$reinhardt$hero_stats$damage_blocked,
            charge_kills=user$reinhardt$hero_stats$charge_kills,
            fire_strike_kills=user$reinhardt$hero_stats$fire_strike_kills,
            earth_shatter_kills=user$reinhardt$hero_stats$earthshatter_kills,
            eliminations=user$reinhardt$general_stats$eliminations,
            final_blows=user$reinhardt$general_stats$final_blows,
            solo_kills=user$reinhardt$general_stats$solo_kills,
            damage_done=user$reinhardt$general_stats$damage_done,
            objective_kills=user$reinhardt$general_stats$objective_kills,
            multi_kills=user$reinhardt$general_stats$multikills,
            eliminations_per_life=user$reinhardt$general_stats$eliminations_per_life,
            deaths=user$reinhardt$general_stats$deaths,
            environmental_deaths=user$reinhardt$general_stats$environmental_deaths,
            medals_bronze=user$reinhardt$general_stats$medals_bronze,
            medals_silver=user$reinhardt$general_stats$medals_silver,
            medals_gold=user$reinhardt$general_stats$medals_gold,
            medals=user$reinhardt$general_stats$medals,
            cards=user$reinhardt$general_stats$cards,
            games_played=user$reinhardt$general_stats$games_played,
            objective_time=user$reinhardt$general_stats$objective_time,
            time_spent_on_fire=user$reinhardt$general_stats$time_spent_on_fire,
            win_percentage=user$reinhardt$general_stats$win_percentage)
      }
    } else if (hero == 'zenyatta'){
      if(!is.null(user$zenyatta$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$zenyatta$general_stats$time_played,
            eliminations=user$zenyatta$general_stats$eliminations,
            final_blows=user$zenyatta$general_stats$final_blows,
            solo_kills=user$zenyatta$general_stats$solo_kills,
            damage_done=user$zenyatta$general_stats$damage_done,
            objective_kills=user$zenyatta$general_stats$objective_kills,
            critical_hit_accuracy=user$zenyatta$general_stats$critical_hit_accuracy,
            eliminations_per_life=user$zenyatta$general_stats$eliminations_per_life,
            weapon_accuracy=user$zenyatta$general_stats$weapon_accuracy,
            healing_done=user$zenyatta$general_stats$healing_done,
            turrets_destroyed=user$zenyatta$general_stats$turrets_destroyed,
            offensive_assists=user$zenyatta$general_stats$offensive_assists,
            self_healing=user$zenyatta$general_stats$self_healing,
            deaths=user$zenyatta$general_stats$deaths,
            environmental_deaths=user$zenyatta$general_stats$environmental_deaths,
            medals_bronze=user$zenyatta$general_stats$medals_bronze,
            medals_silver=user$zenyatta$general_stats$medals_silver,
            medals_gold=user$zenyatta$general_stats$medals_gold,
            medals=user$zenyatta$general_stats$medals,
            cards=user$zenyatta$general_stats$cards,
            games_played=user$zenyatta$general_stats$games_played,
            objective_time=user$zenyatta$general_stats$objective_time,
            time_spent_on_fire=user$zenyatta$general_stats$time_spent_on_fire,
            win_percentage=user$zenyatta$general_stats$win_percentage,
            transcendance_healing=user$zenyatta$general_stats$transcendence_healing,
            defensive_assists=user$zenyatta$general_stats$defensive_assists
          )
      }
    } else if (hero == 'lucio'){
      if(!is.null(user$lucio$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$lucio$general_stats$time_played,
            sound_barriers_provided = user$lucio$hero_stats$sound_barriers_provided,
            eliminations=user$lucio$general_stats$eliminations,
            final_blows=user$lucio$general_stats$final_blows,
            solo_kills=user$lucio$general_stats$solo_kills,
            critical_hits=user$lucio$general_stats$critical_hits,
            damage_done=user$lucio$general_stats$damage_done,
            objective_kills=user$lucio$general_stats$objective_kills,
            environmental_kills=user$lucio$general_stats$environmental_kills,
            critical_hit_accuracy=user$lucio$general_stats$critical_hit_accuracy,
            eliminations_per_life=user$lucio$general_stats$eliminations_per_life,
            weapon_accuracy=user$lucio$general_stats$weapon_accuracy,
            healing_done=user$lucio$general_stats$healing_done,
            offensive_assist=user$lucio$general_stats$offensive_assist,
            self_healing=user$lucio$general_stats$self_healing,
            deaths=user$lucio$general_stats$deaths,
            environmental_deaths=user$lucio$general_stats$environmental_deaths,
            medals_bronze=user$lucio$general_stats$medals_bronze,
            medals_silver=user$lucio$general_stats$medals_silver,
            medals_gold=user$lucio$general_stats$medals_gold,
            medals=user$lucio$general_stats$medals,
            cards=user$lucio$general_stats$cards,
            games_played=user$lucio$general_stats$games_played,
            objective_time=user$lucio$general_stats$objective_time,
            time_spent_on_fire=user$lucio$general_stats$time_spent_on_fire,
            win_percentage=user$lucio$general_stats$win_percentage,
            defensive_assists=user$lucio$general_stats$defensive_assists
          )
      }
    } else if (hero == 'roadhog') {
      if(!is.null(user$roadhog$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$roadhog$general_stats$time_played,
            enemies_hooked = user$roadhog$hero_stats$enemies_hooked,
            whole_hog_kills = user$roadhog$hero_stats$whole_hog_kills,
            hook_accuracy = user$roadhog$hero_stats$hook_accuracy,
            eliminations=user$roadhog$general_stats$eliminations,
            final_blows=user$roadhog$general_stats$final_blows,
            solo_kills=user$roadhog$general_stats$solo_kills,
            critical_hits=user$roadhog$general_stats$critical_hits,
            damage_done=user$roadhog$general_stats$damage_done,
            objective_kills=user$roadhog$general_stats$objective_kills,
            multikills=user$roadhog$general_stats$multikills,
            environmental_kills=user$roadhog$general_stats$environmental_kills,
            melee_final_blows=user$roadhog$general_stats$melee_final_blows,
            critical_hit_accuracy=user$roadhog$general_stats$critical_hit_accuracy,
            eliminations_per_life=user$roadhog$general_stats$eliminations_per_life,
            weapon_accuracy=user$roadhog$general_stats$weapon_accuracy,
            turrets_destroyed=user$roadhog$general_stats$turrets_destroyed,
            self_healing=user$roadhog$general_stats$self_healing,
            environmental_deaths=user$roadhog$general_stats$environmental_deaths,
            medals_bronze=user$roadhog$general_stats$medals_bronze,
            medals_silver=user$roadhog$general_stats$medals_silver,
            medals_gold=user$roadhog$general_stats$medals_gold,
            medals=user$roadhog$general_stats$medals,
            cards=user$roadhog$general_stats$cards,
            games_played=user$roadhog$general_stats$games_played,
            objective_time=user$roadhog$general_stats$objective_time,
            time_spent_on_fire=user$roadhog$general_stats$time_spent_on_fire,
            win_percentage=user$roadhog$general_stats$win_percentage
          )
      }
    } else if (hero == 'pharah') {
      if(!is.null(user$pharah$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$pharah$general_stats$time_played,
            rocket_direct_hits = user$pharah$hero_stats$rocket_direct_hits,
            barrage_kills = user$pharah$hero_stats$barrage_kills,
            eliminations=user$pharah$general_stats$eliminations,
            final_blows=user$pharah$general_stats$final_blows,
            solo_kills=user$pharah$general_stats$solo_kills,
            damage_done=user$pharah$general_stats$damage_done,
            objective_kills=user$pharah$general_stats$objective_kills,
            multikills=user$pharah$general_stats$multikills,
            environmental_kills=user$pharah$general_stats$environmental_kills,
            melee_final_blows=user$pharah$general_stats$melee_final_blows,
            eliminations_per_life=user$pharah$general_stats$eliminations_per_life,
            weapon_accuracy=user$pharah$general_stats$weapon_accuracy,
            teleporter_pads_destroyed=user$pharah$general_stats$teleporter_pads_destroyed,
            turrets_destroyed=user$pharah$general_stats$turrets_destroyed,
            environmental_deaths=user$pharah$general_stats$environmental_deaths,
            medals_bronze=user$pharah$general_stats$medals_bronze,
            medals_silver=user$pharah$general_stats$medals_silver,
            medals_gold=user$pharah$general_stats$medals_gold,
            medals=user$pharah$general_stats$medals,
            cards=user$pharah$general_stats$cards,
            games_played=user$pharah$general_stats$games_played,
            objective_time=user$pharah$general_stats$objective_time,
            time_spent_on_fire=user$pharah$general_stats$time_spent_on_fire,
            win_percentage=user$pharah$general_stats$win_percentage
          )
      }
    } else if (hero == 'mercy') {
      if(!is.null(user$mercy$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$mercy$general_stats$time_played,
            players_resurrected = user$mercy$hero_stats$players_resurrected,
            eliminations=user$mercy$general_stats$eliminations,
            final_blows=user$mercy$general_stats$final_blows,
            solo_kills=user$mercy$general_stats$solo_kills,
            critical_hits=user$mercy$general_stats$critical_hits,
            damage_done=user$mercy$general_stats$damage_done,
            objective_kills=user$mercy$general_stats$objective_kills,
            critical_hit_accuracy=user$mercy$general_stats$critical_hit_accuracy,
            eliminations_per_life=user$mercy$general_stats$eliminations_per_life,
            weapon_accuracy=user$mercy$general_stats$weapon_accuracy,
            healing_done=user$mercy$general_stats$healing_done,
            teleporter_pads_destroyed=user$mercy$general_stats$teleporter_pads_destroyed,
            turrets_destroyed=user$mercy$general_stats$turrets_destroyed,
            offensive_assists=user$mercy$general_stats$offensive_assists,
            self_healing=user$mercy$general_stats$self_healing,
            deaths=user$mercy$general_stats$deaths,
            environmental_deaths=user$mercy$general_stats$environmental_deaths,
            medals_bronze=user$mercy$general_stats$medals_bronze,
            medals_silver=user$mercy$general_stats$medals_silver,
            medals_gold=user$mercy$general_stats$medals_gold,
            medals=user$mercy$general_stats$medals,
            cards=user$mercy$general_stats$cards,
            games_played=user$mercy$general_stats$games_played,
            objective_time=user$mercy$general_stats$objective_time,
            time_spent_on_fire=user$mercy$general_stats$time_spent_on_fire,
            win_percentage=user$mercy$general_stats$win_percentage,
            blaster_kills=user$mercy$general_stats$blaster_kills,
            defensive_assists=user$mercy$general_stats$defensive_assists,
            damage_amplified=user$mercy$general_stats$damage_amplified
            
          )
      }
    }  else if (hero == 'soldier76') {
      if(!is.null(user$mercy$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$soldier76$general_stats$time_played,
            helix_rockets_kills = user$soldier76$hero_stats$helix_rockets_kills,
            tactical_visor_kills = user$soldier76$hero_stats$tactical_visor_kills,
            biotic_fields_deployed = user$soldier76$hero_stats$biotic_fields_deployed,
            biotic_field_healing_done = user$soldier76$hero_stats$biotic_field_healing_done,
            eliminations=user$soldier76$general_stats$eliminations,
            final_blows=user$soldier76$general_stats$final_blows,
            solo_kills=user$soldier76$general_stats$solo_kills,
            critical_hits=user$soldier76$general_stats$critical_hits,
            damage_done=user$soldier76$general_stats$damage_done,
            objective_kills=user$soldier76$general_stats$objective_kills,
            multikills=user$soldier76$general_stats$multikills,
            critical_hit_accuracy=user$soldier76$general_stats$critical_hit_accuracy,
            eliminations_per_life=user$soldier76$general_stats$eliminations_per_life,
            weapon_accuracy=user$soldier76$general_stats$weapon_accuracy,
            teleporter_pads_destroyed=user$soldier76$general_stats$teleporter_pads_destroyed,
            turrets_destroyed=user$soldier76$general_stats$turrets_destroyed,
            self_healing=user$soldier76$general_stats$self_healing,
            environmental_deaths=user$soldier76$general_stats$environmental_deaths,
            medals_bronze=user$soldier76$general_stats$medals_bronze,
            medals_silver=user$soldier76$general_stats$medals_silver,
            medals_gold=user$soldier76$general_stats$medals_gold,
            medals=user$soldier76$general_stats$medals,
            cards=user$soldier76$general_stats$cards,
            games_played=user$soldier76$general_stats$games_played,
            objective_time=user$soldier76$general_stats$objective_time,
            time_spent_on_fire=user$soldier76$general_stats$time_spent_on_fire,
            win_percentage=user$soldier76$general_stats$win_percentage
          )
      }
    } else if (hero == 'dva') {
      if(!is.null(user$dva$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$dva$general_stats$time_played,
            damage_blocked=user$dva$hero_stats$damage_blocked,
            mech_deaths=user$dva$hero_stats$mech_deaths,
            eliminations=user$dva$general_stats$eliminations,
            final_blows=user$dva$general_stats$final_blows,
            solo_kills=user$dva$general_stats$solo_kills,
            shots_fired=user$dva$general_stats$shots_fired,
            critical_hits=user$dva$general_stats$critical_hits,
            damage_done=user$dva$general_stats$damage_done,
            objective_kills=user$dva$general_stats$objective_kills,
            multikills=user$dva$general_stats$multikills,
            critical_hit_accuracy=user$dva$general_stats$critical_hit_accuracy,
            eliminations_per_life=user$dva$general_stats$eliminations_per_life,
            weapon_accuracy=user$dva$general_stats$weapon_accuracy,
            teleporter_pads_destroyed=user$dva$general_stats$teleporter_pads_destroyed,
            turrets_destroyed=user$dva$general_stats$turrets_destroyed,
            deaths=user$dva$general_stats$deaths,
            medals_bronze=user$dva$general_stats$medals_bronze,
            medals_silver=user$dva$general_stats$medals_silver,
            medals_gold=user$dva$general_stats$medals_gold,
            medals=user$dva$general_stats$medals,
            cards=user$dva$general_stats$cards,
            games_played=user$dva$general_stats$games_played,
            objective_time=user$dva$general_stats$objective_time,
            time_spent_on_fire=user$dva$general_stats$time_spent_on_fire,
            win_percentage=user$dva$general_stats$win_percentage,
            self_destruct_kills=user$dva$general_stats$self_destruct_kills
          )
      }
    } else if (hero == 'mei') {
      if(!is.null(user$mei$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$mei$general_stats$time_played,
            enemies_frozen=user$mei$hero_stats$enemies_frozen,
            blizzard_kills=user$mei$hero_stats$blizzard_kills,
            damage_blocked=user$mei$hero_stats$damage_blocked,
            eliminations=user$mei$general_stats$eliminations,
            final_blows=user$mei$general_stats$final_blows,
            solo_kills=user$mei$general_stats$solo_kills,
            shots_fired=user$mei$general_stats$shots_fired,
            shots_hit=user$mei$general_stats$shots_hit,
            critical_hits=user$mei$general_stats$critical_hits,
            damage_done=user$mei$general_stats$damage_done,
            objective_kills=user$mei$general_stats$objective_kills,
            multikill=user$mei$general_stats$multikill,
            critical_hit_accuracy=user$mei$general_stats$critical_hit_accuracy,
            eliminations_per_life=user$mei$general_stats$eliminations_per_life,
            weapon_accuracy=user$mei$general_stats$weapon_accuracy,
            turrets_destroyed=user$mei$general_stats$turrets_destroyed,
            self_healing=user$mei$general_stats$self_healing,
            environmental_deaths=user$mei$general_stats$environmental_deaths,
            medals_bronze=user$mei$general_stats$medals_bronze,
            medals_silver=user$mei$general_stats$medals_silver,
            medals_gold=user$mei$general_stats$medals_gold,
            medals=user$mei$general_stats$medals,
            cards=user$mei$general_stats$cards,
            games_played=user$mei$general_stats$games_played,
            objective_time=user$mei$general_stats$objective_time,
            time_spent_on_fire=user$mei$general_stats$time_spent_on_fire,
            win_percentage=user$mei$general_stats$win_percentage
          )
      }
    } else if (hero == 'genji') {
      if(!is.null(user$genji$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$genji$general_stats$time_played,
            dragonblade_kill=user$genji$hero_stats$dragonblade_kill,
            damage_reflected=user$genji$hero_stats$damage_reflected,
            dragonblades=user$genji$hero_stats$dragonblades,
            eliminations=user$genji$general_stats$eliminations,
            final_blows=user$genji$general_stats$final_blows,
            critical_hits=user$genji$general_stats$critical_hits,
            damage_done=user$genji$general_stats$damage_done,
            objective_kills=user$genji$general_stats$objective_kills,
            critical_hit_accuracy=user$genji$general_stats$critical_hit_accuracy,
            eliminations_per_life=user$genji$general_stats$eliminations_per_life,
            weapon_accuracy=user$genji$general_stats$weapon_accuracy,
            turrets_destroyed=user$genji$general_stats$turrets_destroyed,
            environmental_deaths=user$genji$general_stats$environmental_deaths,
            medals_bronze=user$genji$general_stats$medals_bronze,
            medals_silver=user$genji$general_stats$medals_silver,
            medals_gold=user$genji$general_stats$medals_gold,
            medals=user$genji$general_stats$medals,
            cards=user$genji$general_stats$cards,
            games_played=user$genji$general_stats$games_played,
            objective_time=user$genji$general_stats$objective_time,
            time_spent_on_fire=user$genji$general_stats$time_spent_on_fire,
            win_percentage=user$genji$general_stats$win_percentage
          )
      }
    } else if (hero == 'tracer') {
      if(!is.null(user$tracer$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$tracer$general_stats$time_played,
            pulse_bomb_kills=user$tracer$hero_stats$pulse_bomb_kills,
            pulse_bombs_attached=user$tracer$hero_stats$pulse_bombs_attached,
            eliminations=user$tracer$general_stats$eliminations,
            final_blows=user$tracer$general_stats$final_blows,
            solo_kills=user$tracer$general_stats$solo_kills,
            critical_hits=user$tracer$general_stats$critical_hits,
            damage_done=user$tracer$general_stats$damage_done,
            objective_kills=user$tracer$general_stats$objective_kills,
            multikills=user$tracer$general_stats$multikills,
            critical_hit_accuracy=user$tracer$general_stats$critical_hit_accuracy,
            eliminations_per_life=user$tracer$general_stats$eliminations_per_life,
            weapon_accuracy=user$tracer$general_stats$weapon_accuracy,
            self_healing=user$tracer$general_stats$self_healing,
            environmental_deaths=user$tracer$general_stats$environmental_deaths,
            medals_bronze=user$tracer$general_stats$medals_bronze,
            medals_silver=user$tracer$general_stats$medals_silver,
            medals_gold=user$tracer$general_stats$medals_gold,
            medals=user$tracer$general_stats$medals,
            cards=user$tracer$general_stats$cards,
            games_played=user$tracer$general_stats$games_played,
            objective_time=user$tracer$general_stats$objective_time,
            time_spent_on_fire=user$tracer$general_stats$time_spent_on_fire,
            win_percentage=user$tracer$general_stats$win_percentage
          )
      }
    } else if (hero == 'winston') {
      if(!is.null(user$winston$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$winston$general_stats$time_played,
            players_knocked_back=user$winston$hero_stats$players_knocked_back,
            damage_blocked=user$winston$hero_stats$damage_blocked,
            melee_kill=user$winston$hero_stats$melee_kill,
            jump_pack_kills=user$winston$hero_stats$jump_pack_kills,
            eliminations=user$winston$general_stats$eliminations,
            final_blows=user$winston$general_stats$final_blows,
            damage_done=user$winston$general_stats$damage_done,
            objective_kills=user$winston$general_stats$objective_kills,
            eliminations_per_life=user$winston$general_stats$eliminations_per_life,
            environmental_deaths=user$winston$general_stats$environmental_deaths,
            medals_bronze=user$winston$general_stats$medals_bronze,
            medals_silver=user$winston$general_stats$medals_silver,
            medals_gold=user$winston$general_stats$medals_gold,
            medals=user$winston$general_stats$medals,
            cards=user$winston$general_stats$cards,
            games_played=user$winston$general_stats$games_played,
            objective_time=user$winston$general_stats$objective_time,
            primal_rage_kill=user$winston$general_stats$primal_rage_kills,
            solo_kills=user$winston$general_stats$solo_kills,
            time_spent_on_fire=user$winston$general_stats$time_spent_on_fire,
            win_percentage=user$winston$general_stats$win_percentage
          )
      }
    } else if (hero == 'zarya') {
      if(!is.null(user$zarya$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$zarya$general_stats$time_played,
            damage_blocked=user$zarya$hero_stats$damage_blocked,
            high_energy_kills=user$zarya$hero_stats$high_energy_kills,
            protected_barriers_applied=user$zarya$hero_stats$protected_barriers_applied,
            lifetime_energy_accumulation=user$zarya$hero_stats$lifetime_energy_accumulation,
            eliminations=user$zarya$general_stats$eliminations,
            final_blows=user$zarya$general_stats$final_blows,
            solo_kills=user$zarya$general_stats$solo_kills,
            damage_done=user$zarya$general_stats$damage_done,
            objective_kills=user$zarya$general_stats$objective_kills,
            multikills=user$zarya$general_stats$multikills,
            eliminations_per_life=user$zarya$general_stats$eliminations_per_life,
            weapon_accuracy=user$zarya$general_stats$weapon_accuracy,
            turrets_destroyed=user$zarya$general_stats$turrets_destroyed,
            environmental_deaths=user$zarya$general_stats$environmental_deaths,
            medals_bronze=user$zarya$general_stats$medals_bronze,
            medals_silver=user$zarya$general_stats$medals_silver,
            medals_gold=user$zarya$general_stats$medals_gold,
            medals=user$zarya$general_stats$medals,
            cards=user$zarya$general_stats$cards,
            games_played=user$zarya$general_stats$games_played,
            objective_time=user$zarya$general_stats$objective_time,
            time_spent_on_fire=user$zarya$general_stats$time_spent_on_fire,
            win_percentage=user$zarya$general_stats$win_percentage
          )
      }
    } else if (hero == 'ana') {
      if(!is.null(user$ana$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$ana$general_stats$time_played,
            nano_boosts_applied=user$ana$hero_stats$nano_boosts_applied,
            nano_boost_assists=user$ana$hero_stats$nano_boost_assists,
            unscoped_accuracy=user$ana$hero_stats$unscoped_accuracy,
            scoped_accuracy=user$ana$hero_stats$scoped_accuracy,
            eliminations=user$ana$general_stats$eliminations,
            final_blows=user$ana$general_stats$final_blows,
            solo_kill=user$ana$general_stats$solo_kill,
            damage_done=user$ana$general_stats$damage_done,
            objective_kills=user$ana$general_stats$objective_kills,
            eliminations_per_life=user$ana$general_stats$eliminations_per_life,
            weapon_accuracy=user$ana$general_stats$weapon_accuracy,
            healing_done=user$ana$general_stats$healing_done,
            offensive_assists=user$ana$general_stats$offensive_assists,
            self_healing=user$ana$general_stats$self_healing,
            deaths=user$ana$general_stats$deaths,
            environmental_deaths=user$ana$general_stats$environmental_deaths,
            medals_bronze=user$ana$general_stats$medals_bronze,
            medals_silver=user$ana$general_stats$medals_silver,
            medals_gold=user$ana$general_stats$medals_gold,
            medals=user$ana$general_stats$medals,
            cards=user$ana$general_stats$cards,
            games_played=user$ana$general_stats$games_played,
            objective_time=user$ana$general_stats$objective_time,
            time_spent_on_fire=user$ana$general_stats$time_spent_on_fire,
            win_percentage=user$ana$general_stats$win_percentage,
            defensive_assists=user$ana$general_stats$defensive_assists,
            enemies_slept=user$ana$general_stats$enemies_slept
          )
      }
    } else if (hero == 'bastion') {
      if(!is.null(user$bastion$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$bastion$general_stats$time_played,
            eliminations=user$bastion$general_stats$eliminations,
            final_blows=user$bastion$general_stats$final_blows,
            shots_fired=user$bastion$general_stats$shots_fired,
            shots_hit=user$bastion$general_stats$shots_hit,
            critical_hits=user$bastion$general_stats$critical_hits,
            damage_done=user$bastion$general_stats$damage_done,
            objective_kills=user$bastion$general_stats$objective_kills,
            critical_hit_accuracy=user$bastion$general_stats$critical_hit_accuracy,
            eliminations_per_life=user$bastion$general_stats$eliminations_per_life,
            weapon_accuracy=user$bastion$general_stats$weapon_accuracy,
            healing_done=user$bastion$general_stats$healing_done,
            turrets_destroyed=user$bastion$general_stats$turrets_destroyed,
            self_healing=user$bastion$general_stats$self_healing,
            environmental_deaths=user$bastion$general_stats$environmental_deaths,
            deaths=user$bastion$general_stats$deaths,
            medals_bronze=user$bastion$general_stats$medals_bronze,
            medals_silver=user$bastion$general_stats$medals_silver,
            medals_gold=user$bastion$general_stats$medals_gold,
            medals=user$bastion$general_stats$medals,
            cards=user$bastion$general_stats$cards,
            games_played=user$bastion$general_stats$games_played,
            time_spent_on_fire=user$bastion$general_stats$time_spent_on_fire,
            win_percentage=user$bastion$general_stats$win_percentage
          )
      }
    } else if (hero == 'symmetra') {
      if(!is.null(user$symmetra$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$symmetra$general_stats$time_played,
            sentry_turret_kills=user$symmetra$hero_stats$sentry_turret_kills,
            players_teleported=user$symmetra$hero_stats$players_teleported,
            teleporter_uptime=user$symmetra$hero_stats$teleporter_uptime,
            eliminations=user$symmetra$general_stats$eliminations,
            final_blows=user$symmetra$general_stats$final_blows,
            solo_kills=user$symmetra$general_stats$solo_kills,
            damage_done=user$symmetra$general_stats$damage_done,
            objective_kills=user$symmetra$general_stats$objective_kills,
            eliminations_per_life=user$symmetra$general_stats$eliminations_per_life,
            environmental_deaths=user$symmetra$general_stats$environmental_deaths,
            medals_bronze=user$symmetra$general_stats$medals_bronze,
            medals_silver=user$symmetra$general_stats$medals_silver,
            medals_gold=user$symmetra$general_stats$medals_gold,
            medals=user$symmetra$general_stats$medals,
            cards=user$symmetra$general_stats$cards,
            games_played=user$symmetra$general_stats$games_played,
            objective_time=user$symmetra$general_stats$objective_time,
            damage_blocked=user$symmetra$general_stats$damage_blocked,
            time_spent_on_fire=user$symmetra$general_stats$time_spent_on_fire,
            win_percentage=user$symmetra$general_stats$win_percentage
            
          )
      }
    } else if (hero == 'junkrat') {
      if(!is.null(user$junkrat$general_stats$time_played)){
        stats[[hero]] <- 
          list(
            hero = hero,
            username = username,
            tp = user$junkrat$general_stats$time_played,
            enemies_trapped=user$junkrat$hero_stats$enemies_trapped,
            rip_tire_kills=user$junkrat$hero_stats$rip_tire_kills,
            games_played=user$junkrat$general_stats$games_played,
            shots_hit=user$junkrat$general_stats$shots_hit,
            time_spent_on_fire=user$junkrat$general_stats$time_spent_on_fire,
            cards=user$junkrat$general_stats$cards,
            medals_gold=user$junkrat$general_stats$medals_gold,
            damage_done=user$junkrat$general_stats$damage_done,
            eliminations=user$junkrat$general_stats$eliminations,
            medals_silver=user$junkrat$general_stats$medals_silver,
            final_blows=user$junkrat$general_stats$final_blows,
            medals_bronze=user$junkrat$general_stats$medals_bronze,
            objective_time=user$junkrat$general_stats$objective_time,
            objective_kills=user$junkrat$general_stats$objective_kills,
            solo_kills=user$junkrat$general_stats$solo_kills,
            shots_fired=user$junkrat$general_stats$shots_fired,
            medals=user$junkrat$general_stats$medals,
            win_percentage=user$junkrat$general_stats$win_percentage,
            multi_kills=user$junkrat$general_stats$multikills,
            environmental_deaths=user$junkrat$general_stats$environmental_deaths
            
          )
      }
    }
  }
  return(stats)
}