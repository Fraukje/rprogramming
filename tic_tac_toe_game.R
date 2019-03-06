get_player_move <- function (player, board) {
        prompt_message = paste("Player", player, ", choose your move: ", sep = " " )
        n <- as.integer(readline(prompt=prompt_message))
        while(board[n] != 0){
                prompt_message = paste("Position is already taken, please choose again:")
                n <- as.integer(readline(prompt=prompt_message))
        }
        return(n)
}

check_winner <- function(board, player){
        if(any(rowSums(board) == player * 3) | any(colSums(board) == player * 3)) {
                  return(paste("Player", player, "has three in a row and wins!"))
        }      
        else{ 
                  return(NULL)}
}

tic_tac_toe <- function() {
        
        library(raster) ##Visualizing the board needs raster package
  
        print("Welcome to Tic, Tac, Toe game!")
  
        # Initiate board, set turn and choose random starting player
        board <- matrix(0, 3, 3)
        turn <- 1
        player <- sample(1:2, 1)
        print(paste("Player", player, "may start", sep = " "))
        
        ## Start playing: get player move, update and show board.
        while(turn < 10) {
                n <- get_player_move(player, board)
                board[n] <- player
                plot(raster(board), legend = "false")
                turn <- turn + 1

                ## Check if game is done
                result <- check_winner(board, player)
                if (!is.null(result)) {
                          print(result)
                          break
                }
                ## Swap player
                player <- if(player == 1){2}
                else {1}
        }
}


  
  