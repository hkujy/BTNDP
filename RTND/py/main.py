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
import global_para_class as gpc


def write_test_setting_file(gl:gpc.GloParaClass):
    """
        write test setting file of for fortran
    """
    with open(r"c:/GitCodes/BTNDP/Input/testsetting.txt","w+") as f:
        print("{0}".format(gl.para_dict["NetworkType"]),file = f)
        print("{0}".format(gl.para_dict["AssignMode"]),file = f)
        print("{0}".format(gl.para_dict["RunExe"]),file = f)
        print("{0}".format(gl.para_dict["WriteConverge"]),file = f)
        print("{0}".format(gl.para_dict["SolveMode"]),file = f)
        print("{0}".format(gl.para_dict["SolverIndex"]),file = f)
        print("{0}".format(gl.para_dict["TuneSolver"]),file = f)
        print("{0}".format(gl.para_dict["LoadIndex"]),file = f)


def print_paras(mp:mypara.ParaClass,gl:gpc.GloParaClass):
    """
    main program to print parameters uses
    """
    py_para_file= mp.output_folder + "\\python_para.txt"
    with open(py_para_file,"w+") as f:
        for i in gl.para_dict:
           print("{0},{1}".format(i,gl.para_dict[i]),file=f)
        print("ExpId,{0}".format(gl.exp_id),file=f)
        print("TestIndex,{0}".format(gl.test_index),file=f)
        print("ChangeFreLin,{0}".format(gl.change_fre_line),file=f)
        print("FreLb,{0}".format(gl.fre_lb),file=f)
        print("FreUb,{0}".format(gl.fre_up),file=f)
        print("FleetSize,{0}".format(gl.fleetsize),file=f)
        print("IncreFreStep,{0}".format(gl.incre),file=f)           
        print("AbcPop,{0}".format(gl.abc_npop),file=f)
        print("AbcOnlooker,{0}".format(gl.abc_onlooker),file=f)
        print("AbcLimit,{0}".format(gl.abc_limit),file = f)
        print("AbcIter,{0}".format(gl.abc_iter),file = f)
        print("Rio,{0}".format(gl.para_dict["Rio"]),file=f)
        print("BaseFre=[{0},{1},{2},{3}]".format(gl.base_fre[0],gl.base_fre[1],gl.base_fre[2],gl.base_fre[3]),file=f)

def set_test_case_para(gl:gpc.GloParaClass):
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

def SmallTests(gl:gpc.GloParaClass):
    """
        Test on the four node network
    """
    mp = mypara.ParaClass()
    set_test_case_para(gl)
    mp.input_folder = r'C:\GitCodes\BTNDP\Input\TestNetwork'
    mp.output_folder = r'C:\GitCodes\BTNDP\Results'
    mp.set_para(mp.input_folder,gl)
    with open(mp.input_folder+"\\testindex.txt","w") as f:
        print(gl.exp_id,file = f)
    with open(mp.output_folder+"\\Exp_"+str(gl.exp_id)+"_notes.txt","w") as f:
        print("Experiments Log",file=f)

    # set parameters related to congestion..    
    with open(mp.input_folder+"\\Para.txt","w") as f:
        print(gl.para_dict["Congest"],file=f)
        print("1",file = f)    # not really used for Bs
        print(gl.para_dict["Cap"],file = f)
        print(gl.para_dict["Rio"],file = f)

    write_test_setting_file(gl)
    if gl.exp_id == 1:
        bilevel.test_incre_fre_case(mp,gl)
    elif gl.exp_id == 2:
        bilevel.test_enumerate_case(mp,gl)
    elif gl.exp_id == 3:
        bilevel.test_abc_case(mp,gl)
    else:
        print("undefined tests")

    print_paras(mp,gl)


def TestSiouxFall(gl:gpc.GloParaClass):
    """
    """
 
    mp = mypara.ParaClass()
    mp.input_folder = r'C:\GitCodes\OpenTransportData\SiouxFallNet\Transit_Toy'
    # mp.input_folder = r'C:\GitCodes\OpenTransportData\SiouxFallNet\Transport_AllOD' 
    mp.output_folder = r'C:\GitCodes\BTNDP\Results'
    mp.set_para(mp.input_folder,gl)
    with open(mp.input_folder+"\\testindex.txt","w") as f:
        print(gl.exp_id,file = f)
    with open(mp.output_folder+"\\SFExp_"+str(gl.exp_id)+"_notes.txt","w") as f:
        print("Experiments Log",file=f)
    with open(mp.input_folder+"\\Para.txt","w") as f:
        print(gl.para_dict["Congest"],file=f)
        print("1",file = f)    # not really used for Bs
        print(gl.para_dict["Cap"],file = f)
        print(gl.para_dict["Rio"],file = f)

    write_test_setting_file(gl)
    bilevel.test_abc_case(mp,gl)


if __name__ == "__main__":
    gl = gpc.GloParaClass()
    if gl.test_index == 0:
        gl.para_dict['NetworkType'] = 0
        SmallTests(gl)
    elif gl.test_index == 1:
        gl.exp_id = 3    # this is fro setting the input for the python program
        gl.para_dict['NetworkType'] = 1
        TestSiouxFall(gl)
    else:
        print("Test paramters is not set")
    print("Good Luck")
