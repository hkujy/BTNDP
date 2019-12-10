"""
    logical,parameter::isdebug = .true.
    parameter class
"""

import pandas as pd
import global_para as gl

class ParaClass:
   
    def __init__(self):
        self.input_folder = 'set input folder'
        self.output_folder = 'set output folder'
        self.links = 0
        self.od = 0
        self.nodes = 0
        self.is_run_exe=False
        self.assign_mode = 0
        self.solve_method = 1
        self.num_links = -1
        self.num_od = -1
        self.num_nodes = -1
        self.num_dest = -1
        self.num_cases = -1
    pass
    def set_para(self,folder):
        """
        case 0: base small network 
        case 1: sioux fall network 
        """
        nwk_file = ""
        nwk_file = folder +"\\"+"networkpara.txt"
        data = pd.read_csv(nwk_file,header=None)
        self.num_dest = data[0][0]+1
        self.num_od =  data[0][1]
        self.num_nodes = data[0][2]+1
        self.num_links = data[0][3]+1
        self.is_run_exe = gl.is_run_exe
        self.assign_mode = gl.para_dict['AssignMode']
        self.solve_method = gl.para_dict['SolverIndex']
        self.network_type = gl.para_dict['NetworkType']

    def print_para(self,file_name):
        with open(file_name, "a+") as f:
            print ("isRunExe,{0}".format(self.is_run_exe),file=f)
            print ("NumCase,{0}".format(self.num_cases),file=f)
            print ("AssignMode,{0}".format(self.assign_mode),file=f)
            print ("SolveMethod,{0}".format(self.solve_method),file=f)
            print ("NetworkType,{0}".format(self.network_type),file=f)
 
            


        

    