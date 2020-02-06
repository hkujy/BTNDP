"""
    contain the global variables
"""
class GloParaClass:
    def __init__(self):
        self.numseed = 5
        # self.is_run_exe = True
        self.is_run_exe = False
        self.is_debug = True
        # self.is_debug = False
        self.test_index = 0   # test index for the sioux fall network
        # self.test_index = 0   # test index for the small network test
        # remark: the index from fortrain starts from 1
        # exp_id = 1  # given set with given frequency
        # exp_id = 1  # enumerate fleet size
        # exp_id = 2  # enumerate fleet size
        self.exp_id = 3  # bilevel abc
        # para for enumerate fre
        self.change_fre_line = 2
        self.numline = 4
        # para for enumerate
        self.fre_lb = 2  # lower bound of the frequency 
        self.fre_up = 15 # fre upper bound 
        self.fleetsize = 11
        self.incre = 0.01
        self.base_fre = [6,4,2,12]
        # base_demand = [200, 150]
        # para for abc
        self.abc_npop = 10
        self.abc_onlooker = 10
        self.abc_limit = 10
        self.abc_iter = 10
        self.para_dict = {
            "NetworkType":0,    # simple network
            # "NetworkType":1,     # sioux fall transit toy
            "AssignMode":2,    # 1: tranport nwk, 2:compete nwk
            "RunExe":1,
            "WriteConverge":1,
            "SolveMode":0,
            "SolverIndex":1,    # 1 , dp, 2 msa
            "TuneSolver":0,
            "LoadIndex":0,
            "Cap":30,
            "Rio":0.00,
            "Congest":1,
            "ArchiveX":10
        }

