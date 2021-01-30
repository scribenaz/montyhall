#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Contestant Selects A Door
#'
#' @description
#'   This block of code will randomly select ONE of the three doors
#'   and return the result as the selection.
#'
#' @details
#'   Since the contestant will not know the position of the car when
#'   they select a door, we do not need to share the information about the
#'   game set-up before the selection is made. We will create a vector 
#'   containing three integers to represent a door number.
#'
#' @param 
#'   The select_door function calls upon a generic function with no arguments.
#'   The doors function uses logical vectors 1-3.
#'   The a.pick function uses the sample function to randomly generate a logical vector 
#'   taken from the doors function.
#'
#' @return 
#'   The function returns a length three character vector that will represent
#'   the door number. The vectors can only be : 1, 2, or 3
#'
#' @examples
#'   1
#'   2 
#'   3
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host Opens Goat Door
#'
#' @description
#'   This part of the game setup deals with assigning variables to the door number
#'   opened. The variables are two goats and a car.
#'
#' @details
#'   In this portion of the game, the host will always open a door with a goat behind
#'   it. But it cannot be a door the contestant has already selected. So it must be a
#'   door that is not a car and not a current contestant selection.
#'
#'   Note: If the contestant selects the car on the first guess, the host can open either
#'   door left remaining. But if the contestant selects a goat, the host only has one option,
#'   to open the remaining goat door since the host will always open a goat door.
#'
#' @param 
#'   The open_goat_door function calls upon the arguments game and a.pick.
#'   The door function takes in the logical vectors 1-3.
#'   If statements evaluate the game and a.pick arguments and assign
#'   the output to functions goat.doors and opened.door
#'
#' @return 
#'   The return would be a logical vector, an integer, between 1 and 3.
#'
#' @examples
#'   1
#'   2
#'   3
#'  
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change Doors
#'
#' @description
#'   The game continue with adding more code to allow the contestant the option of selecting
#'   another door that is still closed with a change door function.
#'
#' @details
#'   The contestant is given the option to change from their initial selection to the other 
#'   door that is still closed. The function will represent the game-playing strategy as the
#'   argument stay=TRUE or stay=FALSE.
#'
#' @param 
#'   The change_door function calls upon the arguments stay, opened.door and a.pick.
#'   The door function takes in the logical vectors 1-3.
#'   If statements evaluate the stay argument and assigns
#'   the output to function final.pick.  
#'
#' @return 
#'   The return would be a logical vector, an integer, between 1 and 3.
#'
#' @examples
#'   1
#'   2
#'   3
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine If Contestant Has Won
#' @description
#'   If statements are used to compare results of previous step to determine if the contestant has won
#'   or lost the game.
#'
#' @details
#'   The final.pick is called from a previous step and put through if statements to return the game
#'   results. If the final pick was a goat, the result is LOSE. If the contestant picks a car, then the 
#'   result is a WIN.
#'
#' @param 
#'   The determine_winner function calls upon the final.pick and game arguments.
#'   The IF statements evaluate the game function based upon final.pick function.
#'
#' @return 
#'   The return is an atomic vector
#'
#' @examples
#'   WIN  LOSE
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Simulation Set-Up
#'
#' @description
#'   Take the packages from the previous steps and combine them into one package.
#'
#' @details
#'   After completing the process of creating small packages to handle specific functions of the 
#'   game, the time has come to combine them all to run together as one complete package.
#'  
#' @parameter
#'   The play_game function calls the previous functions created:
#'      new.game
#'      first.pick
#'      opened.door
#'   New functions below are created which call upon other functions as arguments.
#'      final.pick.stay
#'      final.pick.switch
#'      outcome.stay
#'      outcome.switch
#'      strategy
#'      outcome
#'      game.results
#'
#' @return 
#'   The game will return the vector of the strategy function as well as 
#'   the vector of the outcome function in a table with the appropriate headers.
#'  
#'
#' @examples
#'   Strategy     Outcome
#'   stay          WIN
#'   switch        LOSE
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Adding The Game To A Loop
#'
#' @description
#'   We want to run simulations to gather data for inferential statistics.
#'
#' @details
#'   By creating a loop to run the program 10,000 tiems, we will gather enough
#'   data to make inferences upon the game strategy for each case, stay or switch.
#'
#' @param 
#'   The play_n_games function calles upon the argument of a numeric vector. 
#'   The library dplyr calls upon the package.
#'   The results.list function will act as a collector for atomic vectors from the loop.
#'   The loop.count will act as a counter
#'   The for loop runs the functions play.game and creates new functions to gather data 
#'   and increment the counter when complete.
#'   Results are stored in results.df and put into a table and formatted with the round 
#'   function.
#'
#' @return 
#'   A formatted table with headers and stategy results formatted to show proportion.
#'   
#'
#' @examples
#'       outcome
#'   strategy   LOSE  WIN
#'    stay      .67   .33
#'    switch    .33   .67
#'
#' @export
play_n_games <- function( n=10000 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
