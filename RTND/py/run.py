"""
    script for the logit assignment code 
"""
import os
import mypara
import global_para_class as gpc

def run_exe(mp:mypara.ParaClass(),gl:gpc.GloParaClass):
    # debug_exe = r'c:/GitCodes/BTNDP/RTND/RTND/Debug/RTND.exe'
    debug_exe = r'c:/GitCodes/BTNDP/RTND/RTND/x64/Debug/RTND.exe'
    # release_exe = r'c:/GitCodes/BTNDP/RTND/RTND/Release/RTND.exe'
    release_exe = r'c:/GitCodes/BTNDP/RTND/RTND/x64/Release/RTND.exe'
    if mp.is_run_exe:
        # os.system(debug_exe)
        print("*********Start To Run Exe File****************")
        if gl.is_debug:
            os.system(debug_exe)
        else:
            os.system(release_exe)

