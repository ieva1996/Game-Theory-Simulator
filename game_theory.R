library(dpylr)

#Payoff Game


###  Number of Runs

Runs<-1000  # number of sims

#Payoff Matrix A

UL_High_A<-5  #upper left
UL_Low_A<-1

LL_High_A<-5  #lower left
LL_Low_A<-1

UR_High_A<-5  #upper right
UR_Low_A<-1

LR_High_A<-5   #lower right
LR_Low_A<-1


#Payoff Matrix for Player B Relative to Player A
Ratio_UL<-1

UL_High_B<-UL_High_A * Ratio_UL
UL_Low_B<-UL_Low_A * Ratio_UL

Ratio_LL<-1.5 #Ratio to A 

LL_High_B<-LL_High_A * Ratio_LL  
LL_Low_B<-LL_Low_A * Ratio_LL

Ratio_UR<-1  #Ratio to A 
UR_High_B<-UR_High_A * Ratio_UR  
UR_Low_B<-UR_Low_A * Ratio_UR



Ratio_LR<-1  #Ratio to A 
LR_High_B<-LR_High_A * Ratio_LR  
LR_Low_B<-LR_Low_A * Ratio_LR


### Player A (Blank Matrix of Ones)
payoff_matrix_A_i<-matrix(1,2,ncol=2, byrow=TRUE)
payoff_matrix_A_i<-data.frame(payoff_matrix_A)
colnames(payoff_matrix_A_i)<-c("B_Yes","B_No")
rownames(payoff_matrix_A_i)<-c("A_Yes","A_No")

### Player B (Blank Matrix of Ones)
payoff_matrix_B_i<-matrix(1,2,ncol=2, byrow=TRUE)
payoff_matrix_B_i<-data.frame(payoff_matrix_B)
colnames(payoff_matrix_B_i)<-c("B_Yes","B_No")
rownames(payoff_matrix_B_i)<-c("A_Yes","A_No")

###  Player A Payoffs
#set.seed(96)  Seed repeats random results take away # to run

A_Yes_B_Yes<-runif(Runs,UL_Low_A,UL_High_A)  # Random number generator
A_No_B_Yes<-runif(Runs,LL_Low_A,LL_High_A)   # Random number generator
A_Yes_B_No<-runif(Runs,UR_Low_A,UR_High_A)  # Random number generator
A_No_B_No<-runif(Runs,LR_Low_A,LR_High_A)  # Random number generator

#UL,LL,UR,LR Populate data frame with radom values
df_test_A <-data.frame(A_Yes_B_Yes, A_No_B_Yes,A_Yes_B_No,A_No_B_No)
df_choice_A_Y<-data.frame(ifelse(df_test_A[,1] > df_test_A[,2], 1, 0))  #UL minus LL when B is "yes" = 1
df_choice_A_N<-data.frame(ifelse(df_test_A[,4] > df_test_A[,3], 1, 0))  #LR minus UR when B is "no" = 1
head(df_test_A)
names<-names(df_test_A)

#  Note convention A_Y is Player A's choice when B select's 'Yes'
#  Note convention A_N is Player A's choice when B select's 'No'


###  Player B Payoffs
#set.seed(1996)
B_Yes_A_Yes<-runif(Runs,UL_Low_B,UL_High_B)  # Random 
B_No_A_Yes<-runif(Runs,UR_Low_B,UR_High_B)  # Random 
B_Yes_A_No<-runif(Runs,LL_Low_B,LL_High_B)  # Random 
B_No_A_No<-runif(Runs,LR_Low_B,LR_High_B)  # Random 

#UL,LL,UR,LR
df_test_B <-data.frame(B_Yes_A_Yes,B_No_A_Yes,B_Yes_A_No,B_No_A_No)
df_choice_B_Y<-data.frame(ifelse(df_test_B[,1] > df_test_B[,3], 1, 0))  #When A is Yes; 1 B Yes
df_choice_B_N<-data.frame(ifelse(df_test_B[,4] > df_test_B[,2], 1, 0))   #When A is No; 1 B No
head(df_test_B)
namesB<-c("A_Yes_B_Yes","A_Yes_B_No",  "A_No_B_Yes", "A_No_B_No")
colnames(df_test_B)<-namesB


#  Note convention B_Y is Player B's choice when A select's 'Yes'
#  Note convention B_N is Player B's choice when A select's 'No'


#Choices DF

choices_A_B_df<-data.frame(df_choice_A_Y, df_choice_A_N, df_choice_B_Y, df_choice_B_N)
colnames(choices_A_B_df)<-c("df_choice_A_Y", "df_choice_A_N", "df_choice_B_Y", "df_choice_B_N")

###  Choices Builds the matrix to find nash equibs by matching AY,AN,BY,BN in appropriate quadrants

choices <- choices_A_B_df %>% #Checks Nash Equiblibrium 
  mutate(A_Yes_B_Yes= ifelse((df_choice_A_Y == df_choice_B_Y) & df_choice_A_Y == 1, 1,0)) %>%
  mutate(A_Yes_B_Yes= ifelse((df_choice_A_Y == df_choice_B_Y) & df_choice_A_Y == 1, 1,0)) %>%
  mutate(A_No_B_Yes= ifelse((df_choice_A_Y == df_choice_B_N) & df_choice_A_Y == 0, 1,0)) %>%
  mutate(A_Yes_B_No= ifelse((df_choice_A_N == df_choice_B_Y) & df_choice_A_N == 0, 1,0)) %>% 
  mutate(A_No_B_No = ifelse((df_choice_A_N == df_choice_B_N) & df_choice_A_N == 1, 1,0)) %>%
  dplyr::select(A_Yes_B_Yes,A_No_B_Yes,A_No_B_No,A_Yes_B_No)
  head(choices)
  
  
#################################  Two or more Equilibriums found   ########################
  two_equib_check <-data.frame(apply(choices ,1,sum))  # Sum by row
  two_equib_list <-ifelse(two_equib_check >= 2,1,0)  #  Only take 2 or more per row
  head(two_equib_list) 
  sum_Two_Equib<-sum(two_equib_list)   ### Number 2 equib
  nr<-nrow(choices)
  percent_Two_Equib<-sum_Two_Equib/nr  ### Percent 2 > equib
  
  two_eq<-choices * t(two_equib_list)  #finds 2 equibs
  
  
###  In cases where there is no equib, no equib detects this and assigns
no_equilibrium<- choices %>% 
  mutate(No_Equib= ifelse((A_Yes_B_Yes == A_No_B_Yes) & A_No_B_No == 0 & A_Yes_B_No == 0 , 1,0)) %>%
  dplyr::select(No_Equib)


  head(no_equilibrium) 
  sum_no_equib<-sum(no_equilibrium)  ###  Number no equilibrium
  nr<-nrow(no_equilibrium)
  percent_no_equib<-sum_no_equib/nr   ##  Percentage no equilibrium

  ### No Equilibriums adopts a max value strategy where each player goes for their max value
  
  
  ### A Values
  no_eq_A <-t(no_equilibrium) * df_test_A  #get payoffs
  no_eq_A<-dplyr::filter_all(no_eq_A, any_vars(. > 0))  # filter
  
  ### B Values
  no_eq_B <-t(no_equilibrium) * df_test_B  #get payoffs
  no_eq_B<-dplyr::filter_all(no_eq_B, any_vars(. > 0))  # filter
  
  ### A Relative Selection
  A_REL_NOEQ <-no_eq_A - no_eq_B  ###  Player A tries to pick value with highest relative diff
  row_max_A <-apply(A_REL_NOEQ,1,max)
  no_eq_A1 <-ifelse(A_REL_NOEQ == row_max_A, 1,0)
  
  
  ### B Relative Selection
  B_REL_NOEQ <-no_eq_B - no_eq_A  ###  Player B tries to pick value with highest relative diff
  row_max_B <-apply(B_REL_NOEQ ,1,max)
  no_eq_B1 <-ifelse(B_REL_NOEQ == row_max_B, 1,0)
  
  
  ### A Yes
  no_eq_A_Yes1 <-no_eq_A1[,1]
  no_eq_A_Yes3 <-no_eq_A1[,3]
  no_eq_A_Yes <-no_eq_A_Yes3 + no_eq_A_Yes1
  
  
  ### A No
  no_eq_A_No2 <-no_eq_A1[,2]
  no_eq_A_No4 <-no_eq_A1[,4]
  no_eq_A_No <-no_eq_A_No2 + no_eq_A_No4
  
  A_YN <-data.frame(no_eq_A_Yes, no_eq_A_No)  #Yes/No A
  
  
  
  ### B Yes
  no_eq_B_Yes1 <-no_eq_B1[,1]
  no_eq_B_Yes3 <-no_eq_B1[,2]
  no_eq_B_Yes <-no_eq_B_Yes3 + no_eq_B_Yes1
  
  ### B No
  no_eq_B_No2 <-no_eq_B1[,3]
  no_eq_B_No4 <-no_eq_B1[,4]
  no_eq_B_No <-no_eq_B_No2 + no_eq_B_No4
  
  B_YN <-data.frame(no_eq_A_Yes, no_eq_A_No)  #Yes/No B
  
  
  ### Combine
  
  combine <-data.frame(A_YN, B_YN)
  colnames(combine) <-c("Yes_A","No_A", "Yes_B", "No_B")
  
  ###  Payoff Matrix No Equib Raw
  
  no_equib_payoffs<- combine %>% 
    mutate(UL= ifelse((Yes_A == Yes_B) & Yes_A ==1,  1,0)) %>%
    mutate(LL= ifelse((No_A == Yes_B) & No_A ==1,  1,0)) %>%
    mutate(UR= ifelse((Yes_A == No_B) & No_B ==1,  1,0)) %>%
    mutate(LR= ifelse((No_A == No_B) & No_B ==1,  1,0)) %>%
    dplyr::select(UL,LL,UR,LR) 
  
  ###  Payoff A
  
  Player_A_No_EQ<-no_eq_A * no_equib_payoffs
  
  ###  Payoff B
  
  Player_B_No_EQ<-no_eq_B * no_equib_payoffs
  
  ### Difference
  
  difference_No_EQ <-Player_A_No_EQ - Player_B_No_EQ
  colnames(difference_No_EQ)<-c("A_Yes_B_Yes", "A_No_B_Yes", "A_Yes_B_No","A_No_B_No")
  
  wins_No_EQ<-data.frame(ifelse(apply(difference_No_EQ,1,sum) > 0, "Player_A_Win", "Player_B_Win"))
  colnames(wins_No_EQ)<-c("Wins_No_Equib")
  
  count_wins_No_EQ_Values <-table(wins_No_EQ)  #Count by player win
  
  sum_wins_No_EQ_values<-apply(difference_No_EQ,2,sum)   #Value by Quadrant
  
  final_values_No_EQ_values<-c(count_wins_No_EQ_Values, sum_wins_No_EQ_values)  #Final
  
  final_values_No_EQ_values  ###  Finally tally when no equilibrium is reached!
  
  

####################################### Player A  Payoff Matrix (Take Random Values and "Bin")

  ###  Take out no equib & 2 or Greater Equib (no already has 4 zeros)
  
  recode_two_equib <-ifelse(two_equib_list == 1, 0,1)  ###  Opposites so 1 becomes zero, zero 1 to blank 
 
  choices_l_no <-t(recode_two_equib) *   choices  #only 1 equib remain
  
  ### Build payoff matrixes
  namesB<-c("A_Yes_B_Yes","A_Yes_B_No",  "A_No_B_Yes", "A_No_B_No")
  colnames(df_test_B)<-namesB
raw_payoffs_A<- choices_l_no %>% 
  mutate(A_Value_UL= df_test_A[,1] *  A_Yes_B_Yes)  %>%
  mutate(A_Value_LL= df_test_A[,2] *  A_No_B_Yes)  %>%
  mutate(A_Value_UR= df_test_A[,3] *  A_Yes_B_No)  %>%
  mutate(A_Value_LR= df_test_A[,4] *  A_No_B_No)  %>%
  mutate(B_Value_UL= df_test_B[,1] *  A_Yes_B_Yes)  %>%
  mutate(B_Value_LL= df_test_B[,2] *  A_Yes_B_No)  %>%
  mutate(B_Value_UR= df_test_B[,3] *  A_No_B_Yes)  %>%
  mutate(B_Value_LR= df_test_B[,4] *  A_No_B_No)  %>%
  dplyr::select(A_Value_UL,A_Value_LL,A_Value_UR,A_Value_LR) 


rowA <-nrow(raw_payoffs_A)
row_max_A <-apply(raw_payoffs_A,1,max)  ### Find max per row
raw_payoffs_A <-raw_payoffs_A/row_max_A
raw_payoffs_A[is.na(raw_payoffs_A)] <-"NEQ"  ###  Get rid of NAs
raw_payoffs_A<-ifelse(raw_payoffs_A == 1, 1, 0) * round(row_max_A, digits=2)
colnames(raw_payoffs_A)<-c("A_Yes/B_Yes", "A_No/B_Yes", "A_Yes/B_No","A_No/B_No")
head(raw_payoffs_A)


###  Player B   Payoff Matrix (Take Random Values and "Bin")

raw_payoffs_B<- choices_l_no %>% 
  mutate(A_Value_UL= df_test_A[,1] *  A_Yes_B_Yes)  %>%
  mutate(A_Value_LL= df_test_A[,2] *  A_No_B_Yes)  %>%
  mutate(A_Value_UR= df_test_A[,3] *  A_Yes_B_No)  %>%
  mutate(A_Value_LR= df_test_A[,4] *  A_No_B_No)  %>%
  mutate(B_Value_UL= df_test_B[,1] *  A_Yes_B_Yes)  %>%
  mutate(B_Value_LL= df_test_B[,2] *  A_Yes_B_No)  %>%
  mutate(B_Value_UR= df_test_B[,3] *  A_No_B_Yes)  %>%
  mutate(B_Value_LR= df_test_B[,4] *  A_No_B_No)  %>%
  dplyr::select(A_Value_UL,A_Value_LL,A_Value_UR,A_Value_LR) 



rowB <-nrow(raw_payoffs_B)
row_max_B <-apply(raw_payoffs_B,1,max)
raw_payoffs_B <-raw_payoffs_B/row_max_B
raw_payoffs_B[is.na(raw_payoffs_B)] <-"NEQ"
raw_payoffs_B<-ifelse(raw_payoffs_B == 1, 1, 0) * round(row_max_B, digits=2)
colnames(raw_payoffs_B)<-c("A_Yes/B_Yes", "A_No/B_Yes", "A_Yes/B_No","A_No/B_No")



### Compare (Player A Payoff minus Player B)
difference <-data.frame(raw_payoffs_A - raw_payoffs_B)
colnames(difference)<-c("A_Yes_B_Yes", "A_No_B_Yes", "A_Yes_B_No","A_No_B_No")
difference<-dplyr::filter_all(difference, any_vars(. != 0))  # filter out zeros


wins_nash_eq<-data.frame(ifelse(apply(difference ,1,sum) > 0, "Player_A_Win", "Player_B_Win"))
colnames(wins_nash_eq)<-c("Wins_Nash_Equib")

count_wins_Nash_EQ_Values <-table(wins_nash_eq)  #Count by player win

sum_wins_Nash_EQ_values<-apply(difference,2,sum)   #Value by Quadrant

final_values_Nash_EQ_values<-c(count_wins_Nash_EQ_Values, sum_wins_Nash_EQ_values)  #Final

final_values_Nash_EQ_values  ###  Finally tally when equilibrium is reached!


#######


### A Values
two_eq_A <-t(two_equib_list) * df_test_A  #get payoffs
two_eq_A<-dplyr::filter_all(two_eq_A, any_vars(. > 0))  # filter

### B Values
two_eq_B <-t(two_equib_list) * df_test_B  #get payoffs
two_eq_B<-dplyr::filter_all(two_eq_B, any_vars(. > 0))  # filter

### A Relative Selection
TWO_REL_NOEQ <-two_eq_A - two_eq_B  ###  Player A tries to pick value with highest relative diff
row_max_A <-apply(TWO_REL_NOEQ,1,max)
two_eq_A1 <-ifelse(TWO_REL_NOEQ == row_max_A, 1,0)


### B Relative Selection
TWO_REL_NOEQ <-no_eq_B - no_eq_A  ###  Player B tries to pick value with highest relative diff
row_max_B <-apply(TWO_REL_NOEQ,1,max)
two_eq_B1 <-ifelse(TWO_REL_NOEQ == row_max_B, 1,0)


### A Yes
two_eq_A_Yes1 <-two_eq_A1[,1]
two_eq_A_Yes3 <-two_eq_A1[,3]
two_eq_A_Yes <-two_eq_A_Yes3 + two_eq_A_Yes1


### A No
two_eq_A_No2 <-two_eq_A1[,2]
two_eq_A_No4 <-two_eq_A1[,4]
two_eq_A_No <-two_eq_A_No2 + two_eq_A_No4

A_YN1 <-data.frame(two_eq_A_Yes, two_eq_A_No)  #Yes/No A



### B Yes
two_eq_B_Yes1 <-two_eq_B1[,1]
two_eq_B_Yes3 <-two_eq_B1[,2]
two_eq_B_Yes <-two_eq_B_Yes3 + two_eq_B_Yes1

### B No
two_eq_B_No2 <-two_eq_B1[,3]
two_eq_B_No4 <-two_eq_B1[,4]
two_eq_B_No <-two_eq_B_No2 + two_eq_B_No4

B_YN1 <-data.frame(two_eq_A_Yes, two_eq_A_No)  #Yes/No B


### Combine

combine1 <-data.frame(A_YN1, B_YN1)
colnames(combine1) <-c("Yes_A","No_A", "Yes_B", "No_B")

###  Payoff Matrix Two Equib Raw

two_equib_payoffs<- combine1 %>% 
  mutate(UL= ifelse((Yes_A == Yes_B) & Yes_A ==1,  1,0)) %>%
  mutate(LL= ifelse((No_A == Yes_B) & No_A ==1,  1,0)) %>%
  mutate(UR= ifelse((Yes_A == No_B) & No_B ==1,  1,0)) %>%
  mutate(LR= ifelse((No_A == No_B) & No_B ==1,  1,0)) %>%
  dplyr::select(UL,LL,UR,LR) 

###  Payoff A

Player_A_Two_EQ<-two_eq_A * two_equib_payoffs

###  Payoff B

Player_B_Two_EQ<-two_eq_B * two_equib_payoffs

### Difference

difference_Two_EQ <-Player_A_Two_EQ - Player_B_Two_EQ
colnames(difference_Two_EQ)<-c("A_Yes_B_Yes", "A_No_B_Yes", "A_Yes_B_No","A_No_B_No")

wins_Two_EQ<-data.frame(ifelse(apply(difference_Two_EQ,1,sum) > 0, "Player_A_Win", "Player_B_Win"))
colnames(wins_No_EQ)<-c("Wins_No_Equib")

count_wins_Two_EQ_Values <-table(wins_Two_EQ)  #Count by player win

sum_wins_No_Two_values<-apply(difference_Two_EQ,2,sum)   #Value by Quadrant

final_values_Two_EQ_values<-c(count_wins_Two_EQ_Values, sum_wins_No_Two_values)  #Final

final_values_Two_EQ_values  ###  Finally tally when no equilibrium is reached!


##################################### Summarize Nash, >2 Equib, & No Equib ######
library(DT)

###  Convert to Data Tables 
Final_values_Nash_EQ_values_DT <-datatable(data.frame(final_values_Nash_EQ_values))
Final_values_No_EQ_values_DT<-datatable(data.frame(  final_values_No_EQ_values ))
Final_values_Two_EQ_values_DT <-datatable(data.frame(final_values_Two_EQ_values))

### Totals
Sub_total <-rbind(final_values_Nash_EQ_values, final_values_Two_EQ_values,final_values_No_EQ_values ) # capture
Total<-apply(Sub_total,2,sum)  #roll up Note:  if not equal to N-Sims then "Tie"
Summary_game_A<-rbind(Sub_total, Total)
games_won<-Summary_game_A[4,1] + Summary_game_A[4,2]  #add up player a and b wins
Ties<-(Runs - games_won)
Ties<-c(0,0,0,Ties)
Summary_game<-cbind(Summary_game_A,Ties)

