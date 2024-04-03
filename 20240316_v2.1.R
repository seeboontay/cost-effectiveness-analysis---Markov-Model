setwd("C:/Users/User/Desktop/BMI/Sem 2/SPH5412 Economics Methods in Health Technology Assessment/Individual Assignment")
library(readxl)

parameter_excel <- read_excel("Assignment_data_sheet.xlsx")

health_state<-c("Healthy", 
                "Undiagosed Adenomas",
                "Undiagosed Early CRC",
                "Inititial Treatment Early CRC",
                "Followup Dx Early CRC",
                "Undiagosed Late CRC",
                "Inititial Treatment Late CRC",
                "Followup  Dx Late CRC",
                "Death")
n_pop_h <- 900
n_pop_a <-50
n_pop_esc <- 50

n_t <- 30
n_s <- length(health_state)
disc_r <- 0.03

# create a matrix
parameter_matrix <- matrix(data = parameter_excel$Mean_value, nrow = 1, ncol = length(parameter_excel$Mean_value))
colnames(parameter_matrix)<- parameter_excel$Parameter
pd<- data.frame(parameter_matrix)

#create the separate matrices for pop, cost and QALYS
pop_com <- array(NA, dim = c(n_t, n_s), dimnames = list(paste("Year", c(1:n_t)), health_state))
cost_com <- array(NA,dim = c(n_t, n_s), dimnames = list(paste("Year", c(1:n_t)), health_state)) 
qqly_com<- array(NA,dim = c(n_t, n_s), dimnames = list(paste("Year", c(1:n_t)), health_state))

# assign the remaining 95% 
p_re_hs <- 1-pd$p_hs
p_re_hs 
# define false negative
FN<-1-pd$sens_fit
FN
# comparator : adenoma to healthy
p_c_ade_h = pd$p_hs*(pd$sens_fit*(1-pd$p_d_allc))
# comparator : adenoma to adenoma
p_c_ade_ade = p_re_hs*(1-pd$p_d_allc-pd$p_ade_esc)+ pd$p_hs*(FN*(1-pd$p_d_allc-pd$p_ade_esc))
p_c_ade_ade
# comparator: undx early CRC to undx early CRC
p_c_esr_esr = (p_re_hs*(1-pd$p_esc_lsc-pd$p_esc_d))+ (pd$p_hs*(FN*(1-pd$p_esc_lsc-pd$p_esc_d)))
p_c_esr_esr 
# create a transiition probability for comparator
trans_mat_com <- matrix(
  c(1-(pd$p_h_ade*p_re_hs)-pd$p_d_allc, pd$p_h_ade*p_re_hs,0,0,0,0,0,0,pd$p_d_allc,
    p_c_ade_h,p_c_ade_ade,(1-p_c_ade_h-p_c_ade_ade-pd$p_d_allc),0,0,0,0,0,pd$p_d_allc,
    0,0,p_c_esr_esr,(pd$p_hs*(pd$sens_fit*(1-pd$p_esc_d))),0,(1-p_c_esr_esr-(pd$p_hs*(pd$sens_fit*(1-pd$p_esc_d)))-pd$p_esc_d),0,0,pd$p_esc_d,
    0,0,0,0,1-pd$p_esc_d,0,0,0,pd$p_esc_d,
    0,0,0,0,1-pd$p_esc_d,0,0,0,pd$p_esc_d,
    0,0,0,0,0,0,1-pd$p_lsc_d,0,pd$p_lsc_d,
    0,0,0,0,0,0,0,1-pd$p_lsc_d,pd$p_lsc_d,
    0,0,0,0,0,0,0,1-pd$p_lsc_d,pd$p_lsc_d,
    0,0,0,0,0,0,0,0,1),
  
  nrow = n_s, ncol = n_s, 
  byrow = TRUE,
  dimnames = list(health_state, health_state)
)
trans_mat_com
rowSums(trans_mat_com) # ALL row should be sum up to 1

# assign the no_screening_population
pop_com[1, c(1, 2, 3)] <- c((n_pop_h), n_pop_a, n_pop_esc)
pop_com[1, -(1:3)] <- 0  
for(i in 1:(n_t -1)){
  pop_com[i+1,]<-pop_com[i,]%*% trans_mat_com
}
pop_com

##############COST#############


# FIT + Colonoscopy
c_FC <- pd$c_fit+ pd$c_colon
c_FC

c_unit_int_vec <- c(c_FC,c_FC,c_FC,0,0,c_FC,0,0,0)
c_unit_int_vec
#Treatment cost of adenoma or cancer per person
c_unit_treat_vec <- c(0,pd$c_ade,0,pd$c_esc_n,pd$c_esc_f,0,pd$c_lsc_n,pd$c_lsc_f,0)
c_unit_treat_vec
# Total cost per person
c_unit_total_vec <- c_unit_int_vec + c_unit_treat_vec
c_unit_total_vec 

# generate total cost for intervention
#cost_com_total<- t((pop_com%*% c_unit_treat_vec))%*%(1/(1+disc_r)^c(0:(n_t-1)))#### Wrong

# Calculate the discounted factor for each year
discount_factors <- (1 / (1 + disc_r) ^ (0:(n_t - 1)))

# Calculate total discounted cost for each component - Example for the comparator scenario
total_discounted_cost_com <- rep(0, length(c_unit_treat_vec))

for(i in 1:length(c_unit_treat_vec)) {
  # Calculate the cost for each health state over time
  cost_per_state_time <- pop_com[, i] * c_unit_treat_vec[i]
  
  # Apply discounting to these costs
  discounted_cost_per_state <- cost_per_state_time * discount_factors
  
  # Sum the discounted costs across all time points for the health state
  total_discounted_cost_com[i] <- sum(discounted_cost_per_state)
}

total_discounted_cost_com



#Total cost by year
#pop_com%*%c(c_FC,(c_FC+pd$c_ade), c_FC,pd$c_esc_n,pd$c_esc_f,c_FC,pd$c_lsc_n,pd$c_lsc_f,0 )

#discounted cost
#(pop_com%*%c(c_FC,(c_FC+pd$c_ade), c_FC,pd$c_esc_n,pd$c_esc_f,c_FC,pd$c_lsc_n,pd$c_lsc_f,0 ))*(1/(1+disc_r)^c(0:(n_t-1)))

#inner product 
#t((pop_com%*%c(c_FC,(c_FC+pd$c_ade), c_FC,pd$c_esc_n,pd$c_esc_f,c_FC,pd$c_lsc_n,pd$c_lsc_f,0 )))%*%(1/(1+disc_r)^c(0:(n_t-1)))

#summation
#sum((pop_com%*%c(c_FC,(c_FC+pd$c_ade), c_FC,pd$c_esc_n,pd$c_esc_f,c_FC,pd$c_lsc_n,pd$c_lsc_f,0 )))*(1/(1+disc_r)^c(0:(n_t-1)))

# assign total cost
#cost_com_total <- t((pop_com%*%c(c_FC,(c_FC+pd$c_ade), c_FC,pd$c_esc_n,pd$c_esc_f,c_FC,pd$c_lsc_n,pd$c_lsc_f,0 )))%*%(1/(1+disc_r)^c(0:(n_t-1)))
#cost_com_total

#######################QALY############################
# Total QALY by year
pop_com%*%c(pd$u_healthy, pd$u_ade,pd$u_es,pd$u_es,pd$u_es,pd$u_ls,pd$u_ls,pd$u_ls,0)

#Total discounted QALY
t(pop_com%*%c(pd$u_healthy, pd$u_ade,pd$u_es,pd$u_es,pd$u_es,pd$u_ls,pd$u_ls,pd$u_ls,0))*(1/(1+disc_r)^c(0:(n_t-1)))

#assign total QALY
qaly_com_total <-t(pop_com%*%c(pd$u_healthy, pd$u_ade,pd$u_es,pd$u_es,pd$u_es,pd$u_ls,pd$u_ls,pd$u_ls,0))*(1/(1+disc_r)^c(0:(n_t-1)))
qaly_com_total



##############################Intervention#########################

pop_int <- array(NA, dim = c(n_t, n_s), dimnames = list(paste("Year", c(1:n_t)), health_state))
cost_int <- array(NA,dim = c(n_t, n_s), dimnames = list(paste("Year", c(1:n_t)), health_state)) 
qqly_int<- array(NA,dim = c(n_t, n_s), dimnames = list(paste("Year", c(1:n_t)), health_state))


# Transition matrix for intervention

# assign the remaining 60% 
p_re <- 1-pd$comp_fit
p_re
# define false negative
FN<-1-pd$sens_fit
FN
# intervention : adenoma to healthy
p_int_ade_h = pd$comp_fit*(pd$sens_fit*(1-pd$p_d_allc))
# intervention : adenoma to adenoma
p_int_ade_ade = ((p_re*(1-pd$p_d_allc-pd$p_ade_esc))+ (pd$comp_fit*(FN*(1-pd$p_d_allc-pd$p_ade_esc))))
p_int_ade_ade
# intervention: undx early CRC to undx early CRC
p_int_esr_esr = (p_re*(1-pd$p_esc_lsc-pd$p_esc_d))+ (pd$comp_fit*(FN*(1-pd$p_esc_lsc-pd$p_esc_d)))
p_int_esr_esr 



trans_mat_int <-matrix(
  c(1-(pd$p_h_ade*p_re)-pd$p_d_allc, pd$p_h_ade*p_re,0,0,0,0,0,0,pd$p_d_allc,
    p_int_ade_h,p_int_ade_ade,(1-p_int_ade_h-p_int_ade_ade-pd$p_d_allc),0,0,0,0,0,pd$p_d_allc,
    0,0,p_int_esr_esr,(pd$comp_fit*(pd$sens_fit*(1-pd$p_esc_d))),0,(1-p_int_esr_esr-(pd$comp_fit*(pd$sens_fit*(1-pd$p_esc_d)))-pd$p_esc_d),0,0,pd$p_esc_d,
    0,0,0,0,1-pd$p_esc_d,0,0,0,pd$p_esc_d,
    0,0,0,0,1-pd$p_esc_d,0,0,0,pd$p_esc_d,
    0,0,0,0,0,0,1-pd$p_lsc_d,0,pd$p_lsc_d,
    0,0,0,0,0,0,0,1-pd$p_lsc_d,pd$p_lsc_d,
    0,0,0,0,0,0,0,1-pd$p_lsc_d,pd$p_lsc_d,
    0,0,0,0,0,0,0,0,1),
  
  nrow = n_s, ncol = n_s, 
  byrow = TRUE,
  dimnames = list(health_state, health_state)
)  
trans_mat_int

# assign the no_screening_population
pop_int[1, c(1, 2, 3)] <- c((n_pop_h), n_pop_a, n_pop_esc)
pop_int[1, -(1:3)] <- 0  
for(i in 1:(n_t -1)){
  pop_int[i+1,]<-pop_int[i,]%*% trans_mat_int
}

# 2.	Show the number of late-stage cancer patients
pop_int[30,][c(6,7,8)]
pop_com[30,][c(6, 7,8)]
rounded_sum_int <- round(pop_int[30,][7] + pop_int[30,][8] +pop_int[30,][6])
rounded_sum_com <- round(pop_com[30,][7] + pop_com[30,][8] +pop_com[30,][6])
print(rounded_sum_int)
print(rounded_sum_com)
#################COST#######################
#Cost of intervention per person
c_unit_int_vec <- c(c_FC,c_FC,c_FC,0,0,c_FC,0,0,0)

#Treatment cost of adenoma or cancer per person
c_unit_treat_vec <- c(0,pd$c_ade,0,pd$c_esc_n,pd$c_esc_f,0,pd$c_lsc_n,pd$c_lsc_f,0)
# Total cost per person
c_unit_total_vec <- c_unit_int_vec + c_unit_treat_vec
c_unit_total_vec 

# generate total cost for intervention
cost_int_total<- t((pop_int%*% c_unit_total_vec))%*%(1/(1+disc_r)^c(0:(n_t-1))) ## Wrong
cost_int_total_1<- (pop_int%*% c_unit_total_vec)%*%(1/(1+disc_r)^c(0:(n_t-1))) 
cost_int_total_1
# Calculate the discounted factor for each year
discount_factors <- (1 / (1 + disc_r) ^ (0:(n_t - 1)))

# Calculate total discounted cost for each component - Example for the comparator scenario
total_discounted_cost_com <- rep(0, length(c_unit_treat_vec))

for(i in 1:length(c_unit_treat_vec)) {
  # Calculate the cost for each health state over time
  cost_per_state_time <- pop_int[, i] * c_unit_treat_vec[i]
  
  # Apply discounting to these costs
  discounted_cost_per_state <- cost_per_state_time * discount_factors
  
  # Sum the discounted costs across all time points for the health state
  total_discounted_cost_com[i] <- sum(discounted_cost_per_state)
}

total_discounted_cost_com




#generate total QALY for intervention
qaly_int_total <-t(pop_com%*%c(pd$u_healthy, pd$u_ade,pd$u_es,pd$u_es,pd$u_es,pd$u_ls,pd$u_ls,pd$u_ls,0))*(1/(1+disc_r)^c(0:(n_t-1)))
qaly_int_total



#comparing intervention with comparator
results <- matrix(
  c(cost_int_total, cost_com_total, cost_int_total-cost_com_total,NA,
    qaly_int_total,qaly_com_total,(qaly_int_total-qaly_com_total),NA),
  nrow = 2, ncol = 4, byrow =TRUE,
  dimnames = list (c("Cost","QALY"),
                   c("Intervention", "Comparator","Difference","ICER")))

results
a <-cost_int_total -cost_com_total
b<-qaly_int_total-qaly_com_total
a/b
