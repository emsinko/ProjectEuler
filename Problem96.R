
#################################
### BACKPROPAGATION ALGORITHM ###
#################################

library(magrittr)


grids <- read.delim("https://projecteuler.net/project/resources/p096_sudoku.txt", header = FALSE, stringsAsFactors = FALSE)


get_sector <- function(row_index, col_index){
  return(3 * ( (row_index - 1) %/% 3) + (col_index - 1) %/% 3 + 1)
}


get_row_column_index <- function(index){
  return(list(row = (index-1) %% 9 + 1, column = floor((index -1) / 9) + 1))
}

get_non_unique_numbers_from_sector <- function(sudoku_grid, row_index, col_index){
  return(sudoku_grid[sector_lookuptab == get_sector(row_index, col_index)])
}


check_rules <- function(sudoku_grid, row_index, column_index){
  return(
    all(
      sum(sudoku_grid[row_index , ] == sudoku_grid[row_index,column_index]) == 1,
      sum(sudoku_grid[,column_index] == sudoku_grid[row_index,column_index]) == 1,
      sum(get_non_unique_numbers_from_sector(sudoku_grid, row_index,column_index) == sudoku_grid[row_index,column_index]) == 1 
    )
  )
}

sector_lookuptab <- 
  matrix(c( 
    rep(c(rep(1,3), rep(4,3), rep(7,3)),3), 
    rep(c(rep(2,3), rep(5,3), rep(8,3)),3),
    rep(c(rep(3,3), rep(6,3), rep(9,3)),3)
  ), nrow = 9, ncol = 9, byrow = FALSE)

solve_sudoku_backpropagation <- function(sudoku, num_iter = 1000){
  
  time <- Sys.time()
  filled_matrix <- sudoku != 0
  sudoku_copy <- sudoku
  
  filled_indexes <- which(filled_matrix == TRUE)
  unfilled_indexes <- which(filled_matrix == FALSE)
  
  index <- min(unfilled_indexes)
  
  sudoku_copy[index] <- 1
  
  #if(sudoku_copy[index] == 9){
  #  return("CHYBA !! NEDA SA VYPLNIT POLICKO")
  #}
  
  ###############
  ### FOR CYKLUS / WHILE CYKLUS ! 
  
  for(i in 1:num_iter){
    #print(paste("Iteration:",i,"           Position of sudoku:",index,"/ 81"))
    
    # Ak cislo v policku splna pravidla:
    if(check_rules(sudoku_copy,get_row_column_index(index)$row, get_row_column_index(index)$column)){
      if(index == max(unfilled_indexes)){
        print(paste("Sudoku solved in", Sys.time() - time, "secs. with",i,"iterations"))
        return(sudoku_copy)
      }
      index <- index + 1 # posunieme sa o 1 policko dopredu
      helper <- 1  # zapamatame si smer 
      #print(paste("index",index))
      # Ak este mozeme zvysovat cislo:
    } else if(sudoku_copy[index] != 9){  
      sudoku_copy[index] <- sudoku_copy[index] + 1  # zvysime hodnotu policka o 1
      helper <- 0
      #print(sudoku_copy[index])
      # Ak uz je aktualne cislo = 9      
    } else {                          
      sudoku_copy[index] <- 0 # vynuluj mi aktualne policko
      index <- index - 1      # vrat sa o jedno policko spät
      helper <- -1            # zapamatanie smeru (ak by bolo predvyplnene policko vzadu, musime ho preskocit) 
    }
    
    #print(index)
    while(index %in% filled_indexes){
      index <- index + 1 * helper
    }
    
    # Ak sme sa posuvali dopredu: 
    if(helper == 1){ 
      sudoku_copy[index] <- 1  # inicializujeme policko 
      
      # Ak sme sa posunuli dozadu:
    } else if( (helper == -1) & (sudoku_copy[index] != 9) ){
      sudoku_copy[index] <- sudoku_copy[index] + 1 
    } else if( (helper == -1) & (sudoku_copy[index] == 9) ){
      ## CHYBA: ak je helper -1 a presiahli sme  # mozu byt 3x 9 v rade - problem
      sudoku_copy[index] <- 0
      index <- index - 1 # A ZASE MUSIME CHCECKNUT CI NEJDE O CISLO ZO ZADANIA .. rekurzia
      
      while(index %in% filled_indexes){
        index <- index + 1 * helper
      }
      
      if(sudoku_copy[index] == 9){
        sudoku_copy[index] <- 0
        index <- index - 1 # A ZASE MUSIME CHCECKNUT CI NEJDE O CISLO ZO ZADANIA .. rekurzia
        
        while(index %in% filled_indexes){
          index <- index + 1 * helper
        }
      }
      sudoku_copy[index] <- sudoku_copy[index] + 1 
    }
    
    # if(helper == 0){
    #  next()
    # }
    
    #} else if(sudoku_copy[index] != 9){     ## NAVYS AK SA DA, ALEBO SA VRAT 
    #  sudoku_copy[index] <- sudoku_copy[index] + 1  # zvysime hodnotu policka o 1
    
    # Ak uz je aktualne cislo = 9      
    #} else {                          
    #  sudoku_copy[index] <- 0 # vynuluj mi aktualne policko
    #  index <- index - 1      # vrat sa o jedno policko spät
    #  helper <- -1            # zapamatanie smeru (ak by bolo predvyplnene policko vzadu, musime ho preskocit) 
  }
  #############
  #############
  print("Unsolved sudoku:")
  return(sudoku_copy)
}

#####
### Project Euler - Problem 96:
#####

grids <- read.delim("https://projecteuler.net/project/resources/p096_sudoku.txt", header = FALSE, stringsAsFactors = FALSE)

euler_sum <- 0 

for(i in seq(from = 2, to = 492, by = 10)){
  
  sudoku <- 
    grids[i:(i+8), ] %>%
    strsplit("") %>%
    unlist() %>%
    as.integer() %>%
    matrix(ncol = 9, nrow = 9, byrow = T)
  
  solved_sudoku <- solve_sudoku_backpropagation(sudoku, num_iter = 10000000)
  euler_sum <- euler_sum + as.numeric(paste(solved_sudoku[1,1:3], collapse = ""))
  print(paste("Solution of",(i+8)/10,"/ 50","is",ifelse(check_solution(solved_sudoku),"good.", "WRONG!!!!!!"),"Increase sum by:", as.numeric(paste(solved_sudoku[1,1:3], collapse = "")),"to :",euler_sum ))
print(paste("Solution of project-Euler problem 96 is:",euler_sum))
}  
