"""
    script for the logit assignment code 
"""
import os
import para
import read as rd
import myclass as mc

# remark: the index from fortrain starts from 1

def run_exe():
    debug_exe = r'C:\GitCodes\BTNDP\RTND\RTND\Debug\RTND.exe'
    os.system(debug_exe)


if __name__ == "__main__":
    # run_exe()
    mp = para.ParaClass()
    # nwk = mc.NwkClass()
    # TODO: set the desitnation index  
    # nwk.dest.append(1)
    mp.input_folder = r'C:\GitCodes\BTNDP\Input\TestNetwork'
    mp.output_folder = r'C:\GitCodes\BTNDP\Results'
    
    cases = []
    for i in range(0,para.ParaClass.num_cases):
        cases.append(mc.CaseClass())

    rd.main(mp,cases)
