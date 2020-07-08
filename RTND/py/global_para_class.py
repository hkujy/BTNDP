"""
    contain the global variables
"""
def prn_obj(obj):
    print ('\n'.join(['%s:%s' % item for item in obj.__dict__.items()])) 

Siou_fall_all_od_cap = 150
# test_net = 'FourNode'
test_net = 'SiouFallToyOD'
# test_net = 'SiouFallAllOD'
# is_show_fig = False
is_show_fig = True
is_write_archive_sol = True
# is_write_archive_sol = False
class GloParaClass:
    def __init__(self):
        self.numseed = 2
        # self.is_run_exe = True
        self.is_run_exe = False
        self.is_write_archive_sol = is_write_archive_sol
        # self.is_debug = True
        self.is_debug = False
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
        self.fleetsize = 15
        self.incre = 0.01
        self.base_fre = [6,4,2,12]
        # base_demand = [200, 150]
        # para for abc
        self.abc_npop = 5
        self.abc_onlooker = 5
        self.abc_limit = 50
        self.abc_iter = 10
        self.allODSiouxFall = False
        # self.allODSiouxFall = True
        # self.test_index = 0   # test index for the small network
        self.test_index = 1   # test index for the sioux fall network
        self.para_dict = {
            "AssignMode":2,    # 1: tranport nwk, 2:compete nwk
            "RunExe":1,
            "WriteConverge":0,  # 1 true, 2. false
            "SolveMode":0,
            "SolverIndex":1,    # 1 , dp, 2 msa
            "TuneSolver":0,
            "LoadIndex":0,
            "Cap":30,
            "Rio":0.1,
            "Congest":1,
            "ArchiveX":10,
            "ArchiveY":10,
            "ArchiveZ":10,
            # The following variables are not really changed
            # "NetworkType":1,     # sioux fall transit toy
            "NetworkType":0    # simple network
        }
        if test_net=="FourNode":
            self.para_dict['NetworkType'] = 0
            self.allODSiouxFall = False
            self.test_index = 0
        if test_net=="SiouFallToyOD":
            self.allODSiouxFall = False
            self.para_dict['NetworkType'] = 1
            self.test_index = 1
        if test_net=="SiouFallAllOD":
            self.allODSiouxFall = True
            self.para_dict['NetworkType'] = 2
            self.para_dict['Cap'] = Siou_fall_all_od_cap
            self.test_index = 1

    def print_para(self, _file):
        with open(_file,"w+") as f:
            for key in self.para_dict.keys():
                print('{0},{1}'.format(key, self.para_dict[key]), file=f)