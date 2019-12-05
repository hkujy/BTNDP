"""
    contain the global variables
"""

# remark: the index from fortrain starts from 1
# exp_id = 1  # given set with given frequency
# exp_id = 1  # enumerate fleet size
exp_id = 2  # enumerate fleet size
# exp_id = 3  # bilevel abc
test_index = 0

para_dict = {
"NetworkType":0,    # simple network
# "NetworkType":1,     # sioux fall transit toy
"AssignMode":2,    # 1: tranport nwk, 2:compete nwk
"RunExe":1,
"WriteConverge":1,
"SolveMode":0,
"SolverIndex":1,
"TuneSolver":0,
"LoadIndex":0
}

# is_run_exe = True
is_run_exe = False
is_debug = True
# is_debug = False

# const_bcm_value = 20

#### Test on transit network 
"""
Case 1:
1. transit network 
2. 4 nodes network 
3. UE mode
4. solve by dp
"""
"""
Case 2: 
1. transit network 
2. sioux fall transit network toy
3. ue model 
4. solve by dp
"""
"""
Case 3
1. transit network 
2. sioux fall transit network all od
3. ue mode
4. solve by dp
"""

solve_method = 1 # 1: dp , 2: msa
# assign_mode = 1  # bpr function
assign_mode = 2  # transit network 


