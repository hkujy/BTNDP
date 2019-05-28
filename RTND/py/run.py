"""
    script for the logit assignment code 
"""
import os
import para
import read as rd
import myclass as mc
import myplot as mplt


# remark: the index from fortrain starts from 1

is_run_exe = True
# is_run_exe = False
change_fre_line = 2
base_fre = 4   # basic frequency for computing fare
upper_fre = 12  # fre upper bound 
lower_fre = 3  # lower bound of the frequency 
rio = 0.15
incre = 1

TestIndex = 1

def run_exe():
    debug_exe = r'C:\GitCodes\BTNDP\RTND\RTND\Debug\RTND.exe'
    os.system(debug_exe)


def create_case(mp:para.ParaClass()):
    """
        write the case file for the frequency 
    """ 
    base_fre = [6, 4, 6, 6]
    l =  change_fre_line - 1
    fre_list = []
    
    num_case =int((upper_fre - lower_fre)/incre)
    for i in range(0, num_case):
        tf = lower_fre + i*incre
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

    cases = []
    for i in range(0,para.ParaClass.num_cases):
        cases.append(mc.CaseClass())
        cases[-1].id = i
        for j in fre_list[i]:
            cases[-1].fre.append(j)
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
    if is_run_exe:
        run_exe()
    
    rd.main(mp,cases)
    mplt.main(mp, cases)


if __name__ == "__main__":

    # step 0: set general paramters
    mp = para.ParaClass()
    mp.input_folder = r'C:\GitCodes\BTNDP\Input\TestNetwork'
    mp.output_folder = r'C:\GitCodes\BTNDP\Results'
    # step 1: set cases
    # step 2: run exe files
        # nwk = mc.NwkClass()
    # TODO: set the desitnation index  
    # nwk.dest.append(1)
    if TestIndex == 1:
        test_incre_fre_case(mp)
    else: 
        print("undefined tests")
    

    print("Good Luck")