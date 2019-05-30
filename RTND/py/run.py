"""
    script for the logit assignment code 
"""
import os
import para
import pandas as pd
import read as rd
import myclass as mc
import myplot as mplt
from shutil import copyfile
import shutil


# remark: the index from fortrain starts from 1

# exp_id = 1    # given set with given frequency
exp_id = 2  # enumerate fleet size
# exp_id = 3  # bilevel abc
# is_run_exe = True
is_run_exe = False

# para for enumerate fre
change_fre_line = 2
# base_fre = 4   # basic frequency for computing fair

# para for enumerate
fre_lb = 5  # lower bound of the frequency 
fre_up = 12 # fre upper bound 
fleetsize = 10
incre = 0.1
base_fre = [6, fre_lb, 4, 4]
# para for abc
abc_npop = 5
abc_onlooker = 5
abc_limit = 5
abc_iter = 5

# para for rio
rio = 0.15



def run_exe():
    debug_exe = r'C:\GitCodes\BTNDP\RTND\RTND\Debug\RTND.exe'
    # debug_exe = r'C:\GitCodes\BTNDP\RTND\RTND\Release\RTND.exe'
    os.system(debug_exe)

def create_case(mp:para.ParaClass()):
    """
        write the case file for the frequency 
    """ 
    l =  change_fre_line - 1
    fre_list = []
    
    num_case =int((fre_up - fre_lb)/incre)
    for i in range(0, num_case):
        tf = fre_lb + i*incre
        ff = []
        for j in range(0,4):
            if j==l:
                ff.append(tf)
            else:
                ff.append(base_fre[j])
        fre_list.append(ff)            
    
    for fl in fre_list:
        print(fl)

    para.ParaClass.num_cases = len(fre_list)    
    write_case_files(mp,fre_list)

    base_case_id = -1
    cases = []
    for i in range(0,para.ParaClass.num_cases):
        cases.append(mc.CaseClass())
        cases[-1].id = i
        for j in fre_list[i]:
            cases[-1].fre.append(j)
        if (cases[-1].fre[0]==base_fre[0] and cases[-1].fre[1]==base_fre[1] 
        and cases[-1].fre[2]==base_fre[2] and cases[-1].fre[3]==base_fre[3]):
            mc.CaseClass.base_case_id = i
    
    return cases 

def write_case_files(mp:para.ParaClass(),fre_list):
    of = mp.input_folder+"\\setfre.txt"
    print('the demention of the fre list is set fixed to be 4')
    with open(of, 'w') as f:
       for fre in fre_list:
               print("{0} {1} {2} {3}".format(fre[0],fre[1],fre[2],fre[3]),file=f)
    of = mp.input_folder+"\\numcases.txt"
    with open(of, 'w') as f:
        print("{0}".format(len(fre_list)), file=f)

def test_incre_fre_case(mp:para.ParaClass()):

    cases = create_case(mp)
    print("Base case id is {0}".format(mc.CaseClass.base_case_id))
    print("Test Case: Enumerate Freqeuncy")

    if is_run_exe:
        run_exe()
    rd.main(mp, cases)
    mplt.main(mp, cases)
    notes = mp.output_folder+"\\notes.txt"
    with open(notes, "a") as f:
        print("**********Test Increase Irequency**********",file=f)
        print("Base Frequency: ",end=" ",file=f)
        print(base_fre,file=f)
        print("Fre lower bound = {0}".format(fre_lb),file=f)
        print("Fre upper bound = {0}".format(fre_up),file=f)


    copyinputdir = mp.output_folder+"\\exp_"+str(exp_id)
    if os.path.isdir(copyinputdir):
        shutil.rmtree(copyinputdir)
    shutil.copytree(mp.input_folder,copyinputdir)


def test_enumerate_case(mp:para.ParaClass()):

    with open(mp.input_folder+"\\testfleetpara.txt","w") as f:
        print("{0}".format(fre_lb),file = f)
        print("{0}".format(fre_up),file = f) 
        print("{0}".format(fleetsize),file = f)
    print("Test Case: Enumerate all based on fleet")

    if is_run_exe:
        run_exe()

    df = pd.read_csv(mp.input_folder+"\\setfre.txt",header=None)
    cases = []
    # rd.main(mp, cases)
    caseid=0
    mc.CaseClass.base_case_id = 0
    for row in range(0, df.shape[0]):
        cases.append(mc.CaseClass())
        cases[-1].id = caseid
        cases[-1].fre.append(df[0][row])
        cases[-1].fre.append(df[1][row])
        cases[-1].fre.append(df[2][row])
        cases[-1].fre.append(df[3][row])
        caseid =  caseid + 1

    rd.main(mp, cases)
    mplt.main(mp, cases)        

    notes = mp.output_folder+"\\notes.txt"
    with open(notes, "a") as f:
        print("**********Test Enumerate All Feasible Fleet Cases******",file=f)
        print("Base Frequency: ",end=" ",file=f)
        print(base_fre,file=f)
        print("Fre lower bound = {0}".format(fre_lb),file=f)
        print("Fre upper bound = {0}".format(fre_up),file=f)

    copyinputdir = mp.output_folder+"\\exp_"+str(exp_id)
    if os.path.isdir(copyinputdir):
        shutil.rmtree(copyinputdir)
    shutil.copytree(mp.input_folder,copyinputdir)

    pass


def test_abc_case(mp:para.ParaClass()):

    with open(mp.input_folder +"\\abc.txt","w") as f:
        print("{0}".format(abc_npop),file = f)
        print("{0}".format(abc_onlooker),file = f)
        print("{0}".format(abc_limit),file = f)
        print("{0}".format(abc_iter),file = f)
    pass


if __name__ == "__main__":

    mp = para.ParaClass()
    mp.input_folder = r'C:\GitCodes\BTNDP\Input\TestNetwork'
    mp.output_folder = r'C:\GitCodes\BTNDP\Results'
    with open(mp.input_folder+"\\testindex.txt","w") as f:
        print(exp_id,file=f)

    with open(mp.output_folder+"\\notes.txt","w") as f:
        print("Experiments Log")

    if exp_id == 1:
        test_incre_fre_case(mp)
    elif exp_id == 2:
        test_enumerate_case(mp)
    elif exp_id == 3:
        test_abc_case(mp)
    else:
        print("undefined tests")

    print("Good Luck")


