library(MonteCarlo)


### ENter Values

Runs<-1000  # number of sims


#Payoff Matrix A  (High and Lows)

UL_High_A<- 10
UL_Low_A<-  1

LL_High_A<- 10
LL_Low_A<-  1

UR_High_A<- 10
UR_Low_A<-  1

LR_High_A<-10
LR_Low_A<-  1

###Payoff Matrix for Player B Relative to Player A

### Upper Left B
Ratio_UL<- -1.25

### Lower Right B
Ratio_LL<- -1.25

### Upper Right B
Ratio_UR<- 1

### Lower Right R

Ratio_LR<- 1


##############################################################   Start Functions ##########################################################
####   No Equib Function

get_no_equib<-function(no_equib_vector) { 
  
  no_equilibrium <-no_equib_vector
  no_equilibrium<-no_eq #TEST
  ### No Equib
  no_equilibrium[is.na(no_equilibrium)] <-0
  sum_no_equib<-sum(no_equilibrium)  ###  Number no equilibrium
  nr<-nrow(no_equilibrium)
  percent_no_equib<-sum_no_equib/nr   ##  Percentage no equilibrium
  
  ### A Values
  no_eq_A <-t(no_equilibrium) * df_test_A  #get payoffs
  #no_eq_A<-dplyr::filter_all(no_eq_A, any_vars(. != 0))  # filter
  
  ### B Values
  no_eq_B <-t(no_equilibrium) * df_test_B  #get payoffs
  #no_eq_B<-dplyr::filter_all(no_eq_B, any_vars(. != 0))  # filter
  
  ### A and B
  ZERO_EQ_A_B <-data.frame(no_eq_A, no_eq_B)  #all combined DF 
  
  row_sum_zero_eq<-ZERO_EQ_A_B %>%
    rowwise() %>%
    filter_all(any_vars(. != 0)) %>%
    mutate(row_sum_max=max(A_Yes_B_Yes.1, A_No_B_Yes.1, A_Yes_B_No.1, A_No_B_No.1, 
                           A_Yes_B_Yes, A_No_B_Yes, A_Yes_B_No, A_No_B_No)) %>%
    mutate(row_sum_max_Player_A =max(A_Yes_B_Yes, A_No_B_Yes, A_Yes_B_No, A_No_B_No)) %>%
    mutate(row_sum_max_Player_B =max(A_Yes_B_Yes.1, A_No_B_Yes.1, A_Yes_B_No.1, A_No_B_No.1)) %>%
    mutate(row_sum_win_points= row_sum_max_Player_A - row_sum_max_Player_B)
  
  row_sum_zero_eq <-ifelse( nrow(row_sum_zero_eq) > 0, list(row_sum_zero_eq), list(ZERO_EQ_A_B))
  row_sum_zero_eq <-data.frame(row_sum_zero_eq) #convert to df
  
  ### Player A
  A_Points<- row_sum_single_eq %>%
    filter(row_sum_win_points > 0) %>% #Win A
    mutate(A_Yes_B_Yes_P = ifelse(A_Yes_B_Yes == row_sum_max_Player_A, 1,0 )* row_sum_win_points) %>%
    mutate(A_No_B_Yes_P = ifelse(A_No_B_Yes == row_sum_max_Player_A, 1,0 ) * row_sum_win_points) %>%
    mutate(A_Yes_B_No_P = ifelse(A_Yes_B_No == row_sum_max_Player_A, 1,0 ) * row_sum_win_points) %>%
    mutate(A_No_B_No_P = ifelse(A_No_B_No == row_sum_max_Player_A, 1,0 ) * row_sum_win_points) %>%
    summarise_all(sum)
  
  A_Points<- A_Points[,13:16]  #Add
  
  A_Wins<- row_sum_single_eq %>%
    filter(row_sum_win_points > 0) %>% #Win A 
    tally()
  
  ### Player B
  B_Points<- row_sum_single_eq %>%
    filter(row_sum_win_points < 0) %>% #Win B
    mutate(A_Yes_B_Yes_P= ifelse(A_Yes_B_Yes.1 == row_sum_max_Player_B, 1,0 )* row_sum_win_points) %>%
    mutate(A_No_B_Yes_P= ifelse(A_No_B_Yes.1 == row_sum_max_Player_B, 1,0 ) * row_sum_win_points) %>%
    mutate(A_Yes_B_No_P= ifelse(A_Yes_B_No.1 == row_sum_max_Player_B, 1,0 ) * row_sum_win_points) %>%
    mutate(A_No_B_No_P= ifelse(A_No_B_No.1 == row_sum_max_Player_B, 1,0 ) * row_sum_win_points) %>%
    summarise_all(sum)
  
  B_Points<- B_Points[,13:16]  #Add
  
  B_Wins<- row_sum_zero_eq %>%
    filter(row_sum_win_points < 0) %>% #Win B 
    tally()
  
  
  
  T_PLY_No_EQ <-row_sum_zero_eq %>%
    filter(row_sum_win_points == 0) %>% #Win A
    tally() 
  
  Points_No_EQ <-A_Points + B_Points  #combine
  colnames(Points_No_EQ)<-namesB
  
  tally_DF <-cbind(A_Wins,B_Wins,T_PLY_No_EQ)  #Combine
  colnames(tally_DF)<-c("Player_A_Wins","Player_B_Wins","Ties")
  tally_DF <-data.frame(tally_DF)  #as df
  
  
  ### Final Values
  Final_Value<-data.frame(tally_DF, Points_No_EQ)  #Final
  
  return(Final_Value) 
}




############ Single Nash Equib  Function  ######

get_single_equib<-function(single_equib_vector) { 
  #single_eq<-single_eq
  single_eq <-single_equib_vector
  
  ### A Values
  single_eq_A <-t(single_eq) * df_test_A  #get payoffs
  
  ### B Values
  single_eq_B <-t(single_eq) * df_test_B  #get payoffs
  
  ### A and B
  SINGLE_EQ_A_B <-data.frame(single_eq_A, single_eq_B)  #all combined DF
  
  
  row_sum_single_eq<-SINGLE_EQ_A_B %>%
    rowwise() %>%
    filter_all(any_vars(. != 0)) %>%
    mutate(row_sum_max=max(A_Yes_B_Yes.1, A_No_B_Yes.1, A_Yes_B_No.1, A_No_B_No.1, 
                           A_Yes_B_Yes, A_No_B_Yes, A_Yes_B_No, A_No_B_No)) %>%
    mutate(row_sum_max_Player_A =max(A_Yes_B_Yes, A_No_B_Yes, A_Yes_B_No, A_No_B_No)) %>%
    mutate(row_sum_max_Player_B =max(A_Yes_B_Yes.1, A_No_B_Yes.1, A_Yes_B_No.1, A_No_B_No.1)) %>%
    mutate(row_sum_win_points= row_sum_max_Player_A - row_sum_max_Player_B)
  
  row_sum_single_eq <-ifelse(nrow(row_sum_single_eq) > 0, list(row_sum_single_eq), 
                             list(SINGLE_EQ_A_B))
  row_sum_single_eq <-data.frame(row_sum_single_eq) #convert to df
  

  ### Player A
  A_Points<- row_sum_single_eq %>%
    filter(row_sum_win_points > 0) %>% #Win A
    mutate(A_Yes_B_Yes_P = ifelse(A_Yes_B_Yes == row_sum_max_Player_A, 1,0 )* row_sum_win_points) %>%
    mutate(A_No_B_Yes_P = ifelse(A_No_B_Yes == row_sum_max_Player_A, 1,0 ) * row_sum_win_points) %>%
    mutate(A_Yes_B_No_P = ifelse(A_Yes_B_No == row_sum_max_Player_A, 1,0 ) * row_sum_win_points) %>%
    mutate(A_No_B_No_P = ifelse(A_No_B_No == row_sum_max_Player_A, 1,0 ) * row_sum_win_points) %>%
    summarise_all(sum)
  
  A_Points<- A_Points[,13:16]  #Add
  
  A_Wins<- row_sum_single_eq %>%
    filter(row_sum_win_points > 0) %>% #Win A 
    tally()
  
  ### Player B
  B_Points<- row_sum_single_eq %>%
    filter(row_sum_win_points < 0) %>% #Win B
    mutate(A_Yes_B_Yes_P= ifelse(A_Yes_B_Yes.1 == row_sum_max_Player_B, 1,0 )* row_sum_win_points) %>%
    mutate(A_No_B_Yes_P= ifelse(A_No_B_Yes.1 == row_sum_max_Player_B, 1,0 ) * row_sum_win_points) %>%
    mutate(A_Yes_B_No_P= ifelse(A_Yes_B_No.1 == row_sum_max_Player_B, 1,0 ) * row_sum_win_points) %>%
    mutate(A_No_B_No_P= ifelse(A_No_B_No.1 == row_sum_max_Player_B, 1,0 ) * row_sum_win_points) %>%
    summarise_all(sum)
  
  B_Points<- B_Points[,13:16]  #Add
  
  B_Wins<- row_sum_single_eq %>%
    filter(row_sum_win_points < 0) %>% #Win B 
    tally()
  
  
  
  T_PLY_Single_EQ <-row_sum_single_eq %>%
    filter(row_sum_win_points == 0) %>% #Win A
    tally() 
  
  Points_No_EQ <-A_Points + B_Points  #combine
  colnames(Points_No_EQ)<-namesB
  
  tally_DF <-cbind(A_Wins,B_Wins,T_PLY_Single_EQ)  #Combine
  colnames(tally_DF)<-c("Player_A_Wins","Player_B_Wins","Ties")
  tally_DF <-data.frame(tally_DF)  #as df
  
  
  ### Final Values
  Final_Value<-data.frame(tally_DF, Points_No_EQ)  #Final
  
  return(Final_Value) 
}

######################   Double Function ##########

get_double_equib<-function(double_equib_vector) { 
  
  two_equilibrium <-double_equib_vector
  
  
  #head(two_equilibrium)
  two_equilibrium[is.na(two_equilibrium)] <-0
  sum_two_equib<-sum(two_equilibrium)  ###  Number no equilibrium
  nr<-nrow(two_equilibrium)
  percent_two_equib<-sum_two_equib/nr   ##  Percentage no equilibrium
  
  
  ### A Values
  two_eq_A <-t(two_equilibrium) * df_test_A  #get payoffs
  
  ### B Values
  two_eq_B <-t(two_equilibrium) * df_test_B  #get payoffs
  ### A and B
  
  TWO_EQ_A_B <-data.frame(two_eq_A, two_eq_B)  #all combined DF 
  
  row_sum_two_eq<-TWO_EQ_A_B %>%
    rowwise() %>%
    filter_all(any_vars(. != 0)) %>%
    mutate(row_sum_max=max(A_Yes_B_Yes.1, A_No_B_Yes.1, A_Yes_B_No.1, A_No_B_No.1, 
                           A_Yes_B_Yes, A_No_B_Yes, A_Yes_B_No, A_No_B_No)) %>%
    mutate(row_sum_max_Player_A =max(A_Yes_B_Yes, A_No_B_Yes, A_Yes_B_No, A_No_B_No)) %>%
    mutate(row_sum_max_Player_B =max(A_Yes_B_Yes.1, A_No_B_Yes.1, A_Yes_B_No.1, A_No_B_No.1)) %>%
    mutate(row_sum_win_points= row_sum_max_Player_A - row_sum_max_Player_B)
  
  
  row_sum_two_eq <-ifelse(nrow(row_sum_two_eq) > 0, list(row_sum_two_eq), 
                          list(TWO_EQ_A_B))
  row_sum_two_eq <-data.frame(row_sum_two_eq) #convert to df
  
  ### Player A
  A_Points<- row_sum_single_eq %>%
    filter(row_sum_win_points > 0) %>% #Win A
    mutate(A_Yes_B_Yes_P = ifelse(A_Yes_B_Yes == row_sum_max_Player_A, 1,0 )* row_sum_win_points) %>%
    mutate(A_No_B_Yes_P = ifelse(A_No_B_Yes == row_sum_max_Player_A, 1,0 ) * row_sum_win_points) %>%
    mutate(A_Yes_B_No_P = ifelse(A_Yes_B_No == row_sum_max_Player_A, 1,0 ) * row_sum_win_points) %>%
    mutate(A_No_B_No_P = ifelse(A_No_B_No == row_sum_max_Player_A, 1,0 ) * row_sum_win_points) %>%
    summarise_all(sum)
  
  A_Points<- A_Points[,13:16]  #Add
  
  A_Wins<- row_sum_single_eq %>%
    filter(row_sum_win_points > 0) %>% #Win A 
    tally()
  
  ### Player B
  B_Points<- row_sum_single_eq %>%
    filter(row_sum_win_points < 0) %>% #Win B
    mutate(A_Yes_B_Yes_P= ifelse(A_Yes_B_Yes.1 == row_sum_max_Player_B, 1,0 )* row_sum_win_points) %>%
    mutate(A_No_B_Yes_P= ifelse(A_No_B_Yes.1 == row_sum_max_Player_B, 1,0 ) * row_sum_win_points) %>%
    mutate(A_Yes_B_No_P= ifelse(A_Yes_B_No.1 == row_sum_max_Player_B, 1,0 ) * row_sum_win_points) %>%
    mutate(A_No_B_No_P= ifelse(A_No_B_No.1 == row_sum_max_Player_B, 1,0 ) * row_sum_win_points) %>%
    summarise_all(sum)
  
  B_Points<- B_Points[,13:16]  #Add
  
  B_Wins<- row_sum_two_eq %>%
    filter(row_sum_win_points < 0) %>% #Win B 
    tally()
  
  
  T_PLY_Two_EQ <-row_sum_two_eq %>%
    filter(row_sum_win_points == 0) %>% #Win A
    tally() 
  
  Points_Two_EQ <-A_Points + B_Points  #combine
  colnames(Points_Two_EQ)<-namesB
  
  tally_DF <-cbind(A_Wins,B_Win, T_PLY_Two_EQ)  #Combine
  colnames(tally_DF)<-c("Player_A_Wins","Player_B_Wins","Ties")
  tally_DF <-data.frame(tally_DF)  #as df
  
  ### Combine
  Final_Value<-data.frame(tally_DF, Points_Two_EQ)  #Final
  
  return(Final_Value) 
}

###

get_game_results<-function(Runs,
                           Upper_Left_Low_A,
                           Upper_Left_High_A,
                           Upper_Right_Low_A,
                           Upper_Right_High_A,
                           Lower_Left_Low_A,
                           Lower_Left_High_A,
                           Lower_Right_Low_A,
                           Lower_Right_High_A,
                           Upper_Left_Ratio_B,
                           Upper_Right_Ratio_B,
                           Lower_Left_Ratio_B,
                           Lower_Right_Ratio_B) {   #UL/UR/LL/LR_UL/UR/LL/LR
  
  Runs<-Runs  # number of sims
  
  Runs<-1000
  Upper_Left_Low_A<-1
  Upper_Left_High_A<-10
  Upper_Right_Low_A<-1
  Upper_Right_High_A<-10
  Lower_Left_Low_A<-1
  Lower_Left_High_A<-10
  Lower_Right_Low_A<-1
  Lower_Right_High_A<-10
  Upper_Left_Ratio_B<-1
  Upper_Right_Ratio_B<-1
  Lower_Left_Ratio_B<-1
  Lower_Right_Ratio_B<-1
  
  blank_matrix<-data.frame(matrix(0, nrow = Runs, ncol = 4))  #for use
  
  #Payoff Matrix A
  
  UL_High_A<- Upper_Left_High_A
  UL_Low_A<-  Upper_Left_Low_A
  
  LL_High_A<- Lower_Left_High_A
  LL_Low_A<-  Lower_Left_Low_A
  
  UR_High_A<- Upper_Right_High_A
  UR_Low_A<-  Upper_Right_Low_A
  
  LR_High_A<- Lower_Right_High_A
  LR_Low_A<-  Lower_Right_Low_A
  
  ###Payoff Matrix for Player B Relative to Player A
  
  Ratio_UL<- Upper_Left_Ratio_B
  UL_High_B <-ifelse(Ratio_UL >= 0, UL_High_A, UL_Low_A) #Flip High for Low when Negative
  UL_Low_B  <-ifelse(Ratio_UL >= 0, UL_Low_A, UL_High_A) #Flip Low for High when Negative
  
  UL_High_B<-UL_High_B * Ratio_UL
  UL_Low_B<-UL_Low_B * Ratio_UL
  
  ### Lower Right B
  Ratio_LL<- Lower_Left_Ratio_B
  LL_High_B <-ifelse(Ratio_LL >= 0, LL_High_A, LL_Low_A)  #Flip High for Low when Negative
  LL_Low_B  <-ifelse(Ratio_LL >= 0, LL_Low_A, LL_High_A)  #Flip Low for High when Negative
  
  LL_High_B<-LL_High_B * Ratio_LL  
  LL_Low_B<-LL_Low_B * Ratio_LL
  
  ### Upper Right B
  Ratio_UR<- Upper_Right_Ratio_B
  
  UR_High_B <-ifelse(Ratio_UR >= 0, UR_High_A, UR_Low_A)  #Flip High for Low when Negative
  UR_Low_B  <-ifelse(Ratio_UR >= 0, UR_Low_A, UR_High_A)  #Flip Low for High when Negative
  
  UR_High_B<-UR_High_B * Ratio_UR  
  UR_Low_B<-UR_Low_B * Ratio_UR
  
  
  ### Lower Right R
  
  Ratio_LR<- Lower_Right_Ratio_B 
  
  LR_High_B <-ifelse(Ratio_LR >= 0, LR_High_A, LR_Low_A)  #Flip High for Low when Negative
  LR_Low_B  <-ifelse(Ratio_LR >= 0, LR_Low_A, LR_High_A)  #Flip Low for High when Negative
  
  LR_High_B<-LR_High_B  * Ratio_LR  
  LR_Low_B<-LR_Low_B * Ratio_LR
  ###  Player A Payoffs
  #set.seed(96)  # Seed repeats 
  
  A_Yes_B_Yes<-runif(Runs,UL_Low_A,UL_High_A)  # Random number generator
  A_No_B_Yes<-runif(Runs,LL_Low_A,LL_High_A)   # Random number generator
  A_Yes_B_No<-runif(Runs,UR_Low_A,UR_High_A)  # Random number generator
  A_No_B_No<-runif(Runs,LR_Low_A,LR_High_A)  # Random number generator
  
  
  
  #  Note convention A_Y is Player A's choice when B select's 'Yes'
  #  Note convention A_N is Player A's choice when B select's 'No'
  
  
  ###  Player B Payoffs
  #set.seed(1996)  #Since even, set different random start
  B_Yes_A_Yes<-runif(Runs,UL_Low_B,UL_High_B)  # Random 
  B_No_A_Yes<-runif(Runs,UR_Low_B,UR_High_B)  # Random 
  B_Yes_A_No<-runif(Runs,LL_Low_B,LL_High_B)  # Random 
  B_No_A_No<-runif(Runs,LR_Low_B,LR_High_B)  # Random 
  
  #UL,LL,UR,LR
  df_test_B <-data.frame(B_Yes_A_Yes,B_No_A_Yes,B_Yes_A_No,B_No_A_No)
  #head(df_test_B)
  df_choice_B_Y<-data.frame(ifelse(df_test_B[,1] > df_test_B[,2], 1, 0))   #When A is Yes; 1 B Yes
  df_choice_B_N<-data.frame(ifelse(df_test_B[,4] > df_test_B[,3], 1, 0))   #When A is No; 1 B No
  #head(df_test_B)
  namesB<-c("A_Yes_B_Yes","A_Yes_B_No",  "A_No_B_Yes", "A_No_B_No")
  colnames(df_test_B)<-namesB
  df_test_B<-abs(df_test_B)  #make abs value
  
  
  #  Note convention B_Y is Player B's choice when A select's 'Yes'
  #  Note convention B_N is Player B's choice when A select's 'No'
  
  
  #Choices DF
  
  choices_A_B_df<-data.frame(df_choice_A_Y, df_choice_A_N, df_choice_B_Y, df_choice_B_N)
  colnames(choices_A_B_df)<-c("df_choice_A_Y", "df_choice_A_N", "df_choice_B_Y", "df_choice_B_N")
  
  ###  Choices Builds the matrix to find nash equibs by matching AY,AN,BY,BN in appropriate quadrants
  
  choices <- choices_A_B_df  %>% #Checks Nash Equiblibrium 
    mutate(A_Yes_B_Yes= ifelse((df_choice_A_Y == df_choice_B_Y) & df_choice_A_Y == 1, 1,0)) %>%
    mutate(A_Yes_B_Yes= ifelse((df_choice_A_Y == df_choice_B_Y) & df_choice_A_Y == 1, 1,0)) %>%
    mutate(A_No_B_Yes= ifelse((df_choice_A_Y == df_choice_B_N) & df_choice_A_Y == 0, 1,0)) %>%
    mutate(A_Yes_B_No= ifelse((df_choice_A_N == df_choice_B_Y) & df_choice_A_N == 0, 1,0)) %>% 
    mutate(A_No_B_No = ifelse((df_choice_A_N == df_choice_B_N) & df_choice_A_N == 1, 1,0)) %>% 
    dplyr::select(A_Yes_B_Yes, A_No_B_No, A_Yes_B_No,A_No_B_Yes)
  
  
  #################################  Detect Equilibriums  ########################
  equib_check <-data.frame(apply(choices,1,sum))  # Sum by row
  single_eq <-ifelse(equib_check == 1, 1,0)  ###  Opposites so 1 becomes zero, zero 1 to blank 
  double_eq <-ifelse(equib_check >= 2, 1,0)
  no_eq<-ifelse(equib_check == 0, 1,0)
  check_sum<-sum(single_eq) + sum(double_eq) + sum(no_eq)
  
  ### Code for Zero Equib Test
  no_nash_zero<-data.frame(0,0,0,0,0,0,0)  #null results
  colnames(no_nash_zero)<-c("Player_A_Wins",
                            "Player_B_Wins", "Ties","A_Yes_B_Yes", "A_Yes_B_No", "A_No_B_Yes", "A_No_B_No")
  
  
  ################         No Equib Test Area #####
  
  final_values_No_Nash_EQ_value<-ifelse(sum(no_eq) == 0, list(no_nash_zero), 
                                        list(get_no_equib(no_eq)))
  final_values_No_Nash_EQ_value<-data.frame(final_values_No_Nash_EQ_value)  #DF
  
  ################         Single Equib Test Area #####
  
  final_values_Single_Nash_EQ_value<-ifelse(sum(single_eq) == 0, list(no_nash_zero), 
                                            list(get_single_equib(single_eq)))
  final_values_Single_Nash_EQ_value<-data.frame(final_values_Single_Nash_EQ_value)  #DF
  
  
  ########################   Two Nash Test Area ######
  
  final_values_Two_Nash_EQ_values<-ifelse(sum(double_eq) == 0, list(no_nash_zero), 
                                          list(get_double_equib(double_eq)))
  final_values_Two_Nash_EQ_values<-data.frame(final_values_Two_Nash_EQ_values)  #DF
  
  
  ############## Summarize Single, Double and Zero Nash Equib ######
  
  ###  Convert to Data Tables 
  Final_values_Nash_EQ_values_DT1 <-datatable(final_values_Single_Nash_EQ_value)
  Final_values_No_EQ_values_DT1<-datatable(final_values_No_Nash_EQ_value )
  Final_values_Two_EQ_values_DT1 <-datatable(final_values_Two_Nash_EQ_values)
  
  ### Totals
  Sub_Total <-rbind(final_values_Single_Nash_EQ_value, 
                    final_values_Two_Nash_EQ_values,
                    final_values_No_Nash_EQ_value)
  
  Sub_Total <-data.frame(Sub_Total)#DF
  
  
  ###  Sum
  Total<-apply(Sub_Total,2,sum)  #roll up Note:  if not equal to N-Sims then "Tie"
  Summary_game_A<-rbind(Sub_Total, Total)
  Summary_game_A <-data.frame(Summary_game_A)
  row.names(Summary_game_A) <-c("Single Nash Eq.","Two or More Nash Eq.","No Nash Eq.","Totals")
  games_won<-Summary_game_A$Player_A_Wins + Summary_game_A$Player_B_Wins  #add up player a and b wins
  Summary_game1 <-round(Summary_game_A, digits = 1) #Round
  
  
  Summary_game <- Summary_game1 %>% 
    rownames_to_column('Game_Type') %>%
    dplyr::mutate(Game_Sum =Player_A_Wins + Player_B_Wins + Ties) %>%
    dplyr::mutate(Points_Sum =A_Yes_B_Yes + A_No_B_Yes  + A_Yes_B_No + A_No_B_No) %>%
    dplyr::select_all()  # Sum Games total
  
  return(Summary_game) 
}


###################################################      Game Theory Start   ##################################################################



blank_matrix<-data.frame(matrix(0, nrow = Runs, ncol = 4))  #for use


###Payoff Matrix for Player B Relative to Player A

UL_High_B <-ifelse(Ratio_UL >= 0, UL_High_A, UL_Low_A) #Flip High for Low when Negative
UL_Low_B  <-ifelse(Ratio_UL >= 0, UL_Low_A, UL_High_A) #Flip Low for High when Negative

UL_High_B<-UL_High_B * Ratio_UL
UL_Low_B<-UL_Low_B * Ratio_UL

### Lower Right B

LL_High_B <-ifelse(Ratio_LL >= 0, LL_High_A, LL_Low_A)  #Flip High for Low when Negative
LL_Low_B  <-ifelse(Ratio_LL >= 0, LL_Low_A, LL_High_A)  #Flip Low for High when Negative

LL_High_B<-LL_High_B * Ratio_LL  
LL_Low_B<-LL_Low_B * Ratio_LL

### Upper Right B


UR_High_B <-ifelse(Ratio_UR >= 0, UR_High_A, UR_Low_A)  #Flip High for Low when Negative
UR_Low_B  <-ifelse(Ratio_UR >= 0, UR_Low_A, UR_High_A)  #Flip Low for High when Negative

UR_High_B<-UR_High_B * Ratio_UR  
UR_Low_B<-UR_Low_B * Ratio_UR


### Lower Right R


LR_High_B <-ifelse(Ratio_LR >= 0, LR_High_A, LR_Low_A)  #Flip High for Low when Negative
LR_Low_B  <-ifelse(Ratio_LR >= 0, LR_Low_A, LR_High_A)  #Flip Low for High when Negative

LR_High_B<-LR_High_B  * Ratio_LR  
LR_Low_B<-LR_Low_B * Ratio_LR

###  Player A Payoffs
#set.seed(96)  # Seed repeats 

A_Yes_B_Yes<-runif(Runs,UL_Low_A,UL_High_A)  # Random number generator
A_No_B_Yes<-runif(Runs,LL_Low_A,LL_High_A)   # Random number generator
A_Yes_B_No<-runif(Runs,UR_Low_A,UR_High_A)  # Random number generator
A_No_B_No<-runif(Runs,LR_Low_A,LR_High_A)  # Random number generator

#UL,LL,UR,LR Populate data frame with radom values
df_test_A <-data.frame(A_Yes_B_Yes, A_No_B_Yes,A_Yes_B_No,A_No_B_No)
df_choice_A_Y<-data.frame(ifelse(df_test_A[,1] > df_test_A[,2], 1, 0))  #UL minus LL when B is "yes" = 1
df_choice_A_N<-data.frame(ifelse(df_test_A[,4] > df_test_A[,3], 1, 0))  #LR minus UR when B is "no" = 1
#head(df_test_A)
names<-names(df_test_A)

#  Note convention A_Y is Player A's choice when B select's 'Yes'
#  Note convention A_N is Player A's choice when B select's 'No'


###  Player B Payoffs
#set.seed(1996)  #Since even, set different random start
B_Yes_A_Yes<-runif(Runs,UL_Low_B,UL_High_B)  # Random 
B_No_A_Yes<-runif(Runs,UR_Low_B,UR_High_B)  # Random 
B_Yes_A_No<-runif(Runs,LL_Low_B,LL_High_B)  # Random 
B_No_A_No<-runif(Runs,LR_Low_B,LR_High_B)  # Random 

#UL,LL,UR,LR
df_test_B <-data.frame(B_Yes_A_Yes,B_No_A_Yes,B_Yes_A_No,B_No_A_No)
#head(df_test_B)
df_choice_B_Y<-data.frame(ifelse(df_test_B[,1] > df_test_B[,2], 1, 0))   #When A is Yes; 1 B Yes
df_choice_B_N<-data.frame(ifelse(df_test_B[,4] > df_test_B[,3], 1, 0))   #When A is No; 1 B No
#head(df_test_B)
namesB<-c("A_Yes_B_Yes","A_Yes_B_No",  "A_No_B_Yes", "A_No_B_No")
colnames(df_test_B)<-namesB
df_test_B<-abs(df_test_B)  #make abs value


#  Note convention B_Y is Player B's choice when A select's 'Yes'
#  Note convention B_N is Player B's choice when A select's 'No'


#Choices DF

choices_A_B_df<-data.frame(df_choice_A_Y, df_choice_A_N, df_choice_B_Y, df_choice_B_N)
colnames(choices_A_B_df)<-c("df_choice_A_Y", "df_choice_A_N", "df_choice_B_Y", "df_choice_B_N")

###  Choices Builds the matrix to find nash equibs by matching AY,AN,BY,BN in appropriate quadrants

choices <- choices_A_B_df  %>% #Checks Nash Equiblibrium 
  mutate(A_Yes_B_Yes= ifelse((df_choice_A_Y == df_choice_B_Y) & df_choice_A_Y == 1, 1,0)) %>%
  mutate(A_Yes_B_Yes= ifelse((df_choice_A_Y == df_choice_B_Y) & df_choice_A_Y == 1, 1,0)) %>%
  mutate(A_No_B_Yes= ifelse((df_choice_A_Y == df_choice_B_N) & df_choice_A_Y == 0, 1,0)) %>%
  mutate(A_Yes_B_No= ifelse((df_choice_A_N == df_choice_B_Y) & df_choice_A_N == 0, 1,0)) %>% 
  mutate(A_No_B_No = ifelse((df_choice_A_N == df_choice_B_N) & df_choice_A_N == 1, 1,0)) %>% 
  dplyr::select(A_Yes_B_Yes, A_No_B_No, A_Yes_B_No,A_No_B_Yes)


#################################  Detect Equilibriums  ########################
equib_check <-data.frame(apply(choices,1,sum))  # Sum by row
single_eq <-ifelse(equib_check == 1, 1,0)  ###  Opposites so 1 becomes zero, zero 1 to blank 
double_eq <-ifelse(equib_check >= 2, 1,0)
no_eq<-ifelse(equib_check == 0, 1,0)
check_sum<-sum(single_eq) + sum(double_eq) + sum(no_eq)

### Code for Zero Equib Test
no_nash_zero<-data.frame(0,0,0,0,0,0,0)  #null results
colnames(no_nash_zero)<-c("Player_A_Wins",
                          "Player_B_Wins", "Ties","A_Yes_B_Yes", "A_Yes_B_No", "A_No_B_Yes", "A_No_B_No")


################         No Equib Test Area #####

final_values_No_Nash_EQ_value<-ifelse(sum(no_eq) == 0, list(no_nash_zero), 
                                      list(get_no_equib(no_eq)))
final_values_No_Nash_EQ_value<-data.frame(final_values_No_Nash_EQ_value)  #DF

################         Single Equib Test Area #####

final_values_Single_Nash_EQ_value<-ifelse(sum(single_eq) == 0, list(no_nash_zero), 
                                      list(get_single_equib(single_eq)))
final_values_Single_Nash_EQ_value<-data.frame(final_values_Single_Nash_EQ_value)  #DF


########################   Two Nash Test Area ######

final_values_Two_Nash_EQ_values<-ifelse(sum(double_eq) == 0, list(no_nash_zero), 
                                          list(get_double_equib(double_eq)))
final_values_Two_Nash_EQ_values<-data.frame(final_values_Two_Nash_EQ_values)  #DF


############## Summarize Single, Double and Zero Nash Equib ######


###  Convert to Data Tables 
Final_values_Nash_EQ_values_DT1 <-datatable(final_values_Single_Nash_EQ_value)
Final_values_No_EQ_values_DT1<-datatable(final_values_No_Nash_EQ_value )
Final_values_Two_EQ_values_DT1 <-datatable(final_values_Two_Nash_EQ_values)

### Totals
Sub_Total <-rbind(final_values_Single_Nash_EQ_value, 
                  final_values_Two_Nash_EQ_values,
                  final_values_No_Nash_EQ_value)

Sub_Total <-data.frame(Sub_Total)#DF


###  Sum
Total<-apply(Sub_Total,2,sum)  #roll up Note:  if not equal to N-Sims then "Tie"
Summary_game_A<-rbind(Sub_Total, Total)
Summary_game_A <-data.frame(Summary_game_A)
row.names(Summary_game_A) <-c("Single Nash Eq.","Two or More Nash Eq.","No Nash Eq.","Totals")
games_won<-Summary_game_A$Player_A_Wins + Summary_game_A$Player_B_Wins  #add up player a and b wins
Summary_game1 <-round(Summary_game_A, digits = 1) #Round


Summary_game1 <- Summary_game1 %>% 
  rownames_to_column('Game_Type') %>%
  dplyr::mutate(Game_Sum =Player_A_Wins + Player_B_Wins + Ties) %>%
  dplyr::mutate(Points_Sum =A_Yes_B_Yes + A_No_B_Yes  + A_Yes_B_No + A_No_B_No) %>%
  dplyr::select_all()  # Sum Games total



# Write CSV in R
write.csv(Summary_game1, file = "Game Theory Results.csv")
  

          