"""
    script for the logit assignment code 
"""
import os
import mypara
import pandas as pd
import read as rd
import myclass as mc
import myplot as mplt
from shutil import copyfile
import shutil
import pareto
import matplotlib.pyplot as plt
import bileve_tests as bilevel
import run
import global_para as gl

para_dict = {
"NetworkType":0,
"AssignMode":2,
"RunExe":1,
"WriteConverge":1,
"SolveMode":0,
"SolverIndex":1,
"TuneSolver":0,
"LoadIndex":0
}

def set_test_case_para():
    """
        set the global case para
    """
    if gl.test_index == 0:
        gl.para_dict['NetworkType'] = 0
    elif gl.test_index==1:
        gl.para_dict['NetworkType'] = 1
    elif gl.test_index==2:
        gl.para_dict['NetworkType'] = 2
    else:
        print("The overall test index is not set")

if __name__ == "__main__":

    set_test_case_para()
    mp = mypara.ParaClass()

    if gl.para_dict['NetworkType'] == 0:
        mp.input_folder = r'C:\GitCodes\BTNDP\Input\TestNetwork'
    elif gl.para_dict['NetworkType'] == 1:
        mp.input_folder = r'C:\GitCodes\OpenTransportData\SiouxFallNet\Transit_Toy'
    elif gl.para_dict['NetworkType']  == 2:
        mp.input_folder = r'C:\GitCodes\OpenTransportData\SiouxFallNet\Transport_AllOD' 
    else:
        print("network folder is not specified")
    mp.output_folder = r'C:\GitCodes\BTNDP\Results'
    
    mp.set_para(mp.input_folder)

    with open(mp.input_folder+"\\testindex.txt","w") as f:
        print(gl.exp_id,file = f)

    with open(mp.output_folder+"\\notes.txt","w") as f:
        print("Experiments Log",file=f)

    if gl.exp_id == 1:
        bilevel.test_incre_fre_case(mp)
    elif gl.exp_id == 2:
        bilevel.test_enumerate_case(mp)
    elif gl.exp_id == 3:
        bilevel.test_abc_case(mp)
    else:
        print("undefined tests")

    print("Good Luck")


