"""
    script for the logit assignment code 
"""
import os
import para
import read as rd
import myclass as mc

# remark: the index from fortrain starts from 1
num_links = 6 + 1
num_od = 1 + 1
num_nodes = 4 + 1
num_dest = 1 + 1
def run_exe():
    debug_exe = r'C:\GitCodes\BTNDP\RTND\RTND\Debug\RTND.exe'
    os.system(debug_exe)


if __name__ == "__main__":
    run_exe()
    # mp = para.ParaClass(num_links,num_od,num_nodes)
    # nwk = mc.NwkClass(num_links,num_nodes,num_od,num_dest)
    # TODO: set the desitnation index  
    # nwk.dest.append(1)
    # mp.input_folder = r'C:\GitCodes\LogitAssign\Input\TestNetwork'
    # mp.output_folder = r'C:\GitCodes\LogitAssign\Results'
    
    # rd.main(mp,nwk)
