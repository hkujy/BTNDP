"""
    script for the logit assignment code 
"""
import os
import mypara
import global_para as gl

def run_exe(mp:mypara.ParaClass()):
    debug_exe = r'c:/GitCodes/BTNDP/RTND/RTND/Debug/RTND.exe'
    release_exe = r'c:/GitCodes/BTNDP/RTND/RTND/Release/RTND.exe'
    if mp.is_run_exe:
        # os.system(debug_exe)
        print("*********Start To Run Exe File****************")
        if gl.is_debug:
            os.system(debug_exe)
        else:
            os.system(release_exe)

