"""
    plot graphs
"""
import pandas as pd
import mypara
import matplotlib.pyplot as plt
import global_para_class as gpc


def count_changes(vals):
    """
        input a list of values and check the trend 
    """
    counter = 0
    num = len(vals)
    for i in range(1, num-1):
        if (vals[i]-vals[i-1])*(vals[i]-vals[i+1])<0:
            counter =  counter + 1
    
    return counter



def plt_od_cost(mp:mypara.ParaClass(),cases,gl:gpc.GloParaClass):

    # all_od = []
    
    for w in range(0, mp.num_od):
        od = []
        x = []
        for s in cases:
            if len(s.od)==0:
                continue
            map_od =[sw for sw in s.od if sw.id == w][0] 
            od.append(map_od.demand*map_od.mincost)
            x.append(s.id)
        plt.figure("OD "+str(w))
        plt.plot(x, od)
        plt.savefig(mp.output_folder+"\\Exp_"+str(gl.exp_id)+"OD_"+str(w)+".png")

        plt.show(block=False)
        plt.pause(2)
        plt.close()
 


def main(mp:mypara.ParaClass(), cases,gl:gpc.GloParaClass):

    plt_od_cost(mp, cases,gl)
    ttc = []
    fare = []
    with open(mp.output_folder+"\\objects.txt", "w") as f:
        print("id,tc,fair",file=f)
        for c in cases:
            print("{0},{1},{2}".format(c.id,c.ttc,c.fair),file=f)
            ttc.append(c.ttc)
            fare.append(c.fair)


    plt.figure("ttc")
    plt.plot(ttc)
    plt.savefig(mp.output_folder+"\\Exp_"+str(gl.exp_id)+"_ttc.png")
    plt.show(block=False)
    plt.pause(2)
    plt.close()

    plt.plot(fare)
    plt.savefig(mp.output_folder+"\\Exp_"+str(gl.exp_id)+"_fare.png")
    plt.show(block=False)
    plt.pause(2)
    plt.close()
        

    pass