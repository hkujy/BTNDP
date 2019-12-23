"""
    code for bilevel
"""

import os
import mypara
import pandas as pd
import read as rd
import myclass as mc
# import global_para as gl
import global_para_class as gpc
import myplot as mplt
from shutil import copyfile
import shutil
import pareto
import matplotlib.pyplot as plt
import run

def create_case(mp:mypara.ParaClass(),gl:gpc.GloParaClass):
    """
        create all frequency cases at a predefined fixed frequency interval
        write the case file for the frequency 
    """ 
    l =  gl.change_fre_line - 1
    fre_list = []

    num_case = int((gl.fre_up - gl.fre_lb)/gl.incre)
    for i in range(0, num_case):
        tf = gl.fre_lb + i * gl.incre
        ff = []
        for j in range(0,4):
            if j == l:
                ff.append(tf)
            else:
                ff.append(gl.base_fre[j])
        fre_list.append(ff)            

    # print("All the frequency cases") 
    # for fl in fre_list:
    #     print(fl)

    mp.num_cases = len(fre_list)    
    write_case_files(mp,fre_list)
    # base_case_id = -1
    mc.CaseClass.base_case_id = 0
    cases = []
    for i in range(0,mp.num_cases):
        cases.append(mc.CaseClass(mp))
        cases[-1].id = i
        for j in fre_list[i]:
            cases[-1].fre.append(j)
        if (cases[-1].fre[0] == gl.base_fre[0] and cases[-1].fre[1] == gl.base_fre[1] 
        and cases[-1].fre[2] == gl.base_fre[2] and cases[-1].fre[3] == gl.base_fre[3]):
            mc.CaseClass.base_case_id = i
            print("BaseCaseId = {0}".format(mc.CaseClass.base_case_id))
    return cases 

def write_case_files(mp:mypara.ParaClass(),fre_list):
    of = mp.input_folder+"\\setfre.txt"
    if len(fre_list) > 4:
        print('Warning: The demention of the fre list is set fixed to be 4')
    # The following is to set input for the fortran
    with open(of, 'w') as f:
       for fre in fre_list:
               print("{0} {1} {2} {3}".format(fre[0],fre[1],fre[2],fre[3]),file=f)

    of = mp.input_folder+"\\numcases.txt"
    with open(of, 'w') as f:
        print("{0}".format(len(fre_list)), file=f)

def test_incre_fre_case(mp:mypara.ParaClass(),gl:gpc.GloParaClass):
    
    cases = create_case(mp,gl)
    print("Base case id is {0}".format(mc.CaseClass.base_case_id))
    print("Test Case: Enumerate Frequency at a predefined")

    run.run_exe(mp,gl)
    rd.main(mp, cases)
    mplt.main(mp, cases,gl)
    notes = mp.output_folder+"\\Exp_1_notes.txt"
    with open(notes, "a") as f:
        print("**********Test Increase Frequency**********",file=f)
        print("Base Frequency: ",end=" ",file=f)
        print(gl.base_fre,file=f)
        print("Fre lower bound = {0}".format(gl.fre_lb),file=f)
        print("Fre upper bound = {0}".format(gl.fre_up),file=f)
        print("Change Fre Line = {0}".format(gl.change_fre_line),file=f)

    copyinputdir = mp.output_folder+"\\Input_exp_"+str(gl.exp_id)
    if os.path.isdir(copyinputdir):
        shutil.rmtree(copyinputdir)
    shutil.copytree(mp.input_folder,copyinputdir)

def test_enumerate_case(mp:mypara.ParaClass(),gl:gpc.GloParaClass):

    with open(mp.input_folder+"\\testfleetpara.txt","w") as f:
        print("{0}".format(gl.fre_lb),file = f)
        print("{0}".format(gl.fre_up),file = f) 
        print("{0}".format(gl.fleetsize),file = f)

    print("Test Case: Enumerate all based on fleet")

    with open(mp.input_folder+"\\inifre.txt","w") as f:
        for fre in gl.base_fre:
            print(fre,file=f)

    run.run_exe(mp,gl)
    df = pd.read_csv(mp.input_folder+"\\setfre.txt",header=None)
    cases = []
    # rd.main(mp, cases)
    caseid = 0
    mc.CaseClass.base_case_id = 0
    for row in range(0, df.shape[0]):
        cases.append(mc.CaseClass(mp))
        cases[-1].id = caseid
        cases[-1].fre.append(df[0][row])
        cases[-1].fre.append(df[1][row])
        cases[-1].fre.append(df[2][row])
        cases[-1].fre.append(df[3][row])
        caseid =  caseid + 1

    rd.main(mp, cases)
    mplt.main(mp, cases,gl)        

    notes = mp.output_folder+"\\Exp_2_notes.txt"
    with open(notes, "a") as f:
        print("**********Test Enumerate All Feasible Fleet Cases******",file=f)
        print("Base Frequency: ",end=" ",file=f)
        print(gl.base_fre,file=f)
        print("Fre lower bound = {0}".format(gl.fre_lb),file = f)
        print("Fre upper bound = {0}".format(gl.fre_up),file = f)
        print("Fleet Size = {0}".format(gl.fleetsize),file = f)

    copyinputdir = mp.output_folder+"\\Input_exp_"+str(gl.exp_id)
    if os.path.isdir(copyinputdir):
        shutil.rmtree(copyinputdir)
    shutil.copytree(mp.input_folder,copyinputdir)

    xval = []
    yval = []
    id_index = []
    for c in cases:
        if c.id!=mc.CaseClass.base_case_id:
            xval.append(c.ttc)
            yval.append(c.fair)
    
    (px,py) = pareto.pareto_frontier(xval,yval,maxX=False,maxY=True)
    for c in range(0, len(px)):
        for cc in cases:
            if cc.ttc == px[c] and cc.fair == py[c]:
                id_index.append(cc.id)

    plt.figure("pareto")     
    plt.scatter(px,py)
    plt.savefig(mp.output_folder+"\\Exp_2_pareto.png")

    plt.show(block=False)
    plt.pause(2)
    plt.close()
    pp = mp.output_folder+"\\Exp_2_pareto.txt"

    if cases[1].id != 1:
        print("need to check the case id, which are is not equal")
        input()

    fleet_df= pd.read_csv(mp.output_folder+"\\enumeratefleet.txt",header=None)

    with open(pp, "w") as f:
        for i in range(0,len(px)):
            print("{0},{1}".format(px[i],py[i]), end ="\t",file=f)
            for j in range(0, 3):
                col = j
                row = id_index[i]-1
                fl = fleet_df[col][row]
                print("{0},".format(fl),file=f,end="\t")
            col = 3
            row = id_index[i]-1
            fl = fleet_df[col][row]
            print("{0}".format(fl),file=f)


def test_abc_case(mp:mypara.ParaClass(),gl:gpc.GloParaClass):

    with open(mp.input_folder+"\\testfleetpara.txt","w") as f:
        print("{0}".format(gl.fre_lb),file = f)
        print("{0}".format(gl.fre_up),file = f) 
        print("{0}".format(gl.fleetsize),file = f)

    with open(mp.input_folder + "\\inifre.txt","w") as f:
        for fre in gl.base_fre:
            print(fre, file = f)

    with open(mp.input_folder +"\\abcpara.txt","w") as f:
        print("{0}".format(gl.abc_npop),file = f)
        print("{0}".format(gl.abc_onlooker),file = f)
        print("{0}".format(gl.abc_limit),file = f)
        print("{0}".format(gl.abc_iter),file = f)

    first = list(range(1,100))
    second = list(range(101,200))
    if gl.numseed > 99:
        print("Warnning: the required number of seed is too large")
        print("Checkfile: bileve_test.py")
    with open(mp.input_folder+"\\Seeds.txt","w") as f:
        print(gl.numseed,file=f)  
        for i in range(0,gl.numseed):
            print("{0}  {1}".format(first[i],second[i]),file=f)

    run.run_exe(mp,gl)
    notes = mp.output_folder+"\\Exp_3_notes.txt"
    with open(notes, "a") as f:
        print("**********Test ABC**********",file=f)
        print("Base Frequency: ",end=" ",file=f)
        print(gl.base_fre,file=f)
        print("Fre lower bound = {0}".format(gl.fre_lb),file=f)
        print("Fre upper bound = {0}".format(gl.fre_up),file=f)
        print("AbcPop = {0}".format(gl.abc_npop),file = f)
        print("AbcOnlooker = {0}".format(gl.abc_onlooker),file = f)
        print("AbcLimit = {0}".format(gl.abc_limit),file = f)
        print("AbcIter = {0}".format(gl.abc_iter),file = f)
    
    copyinputdir = mp.output_folder+"\\Input_exp_"+str(gl.exp_id)
    if os.path.isdir(copyinputdir):
        shutil.rmtree(copyinputdir)
    shutil.copytree(mp.input_folder,copyinputdir)
 

    #TODO: print and read archive solutions from Frotran output