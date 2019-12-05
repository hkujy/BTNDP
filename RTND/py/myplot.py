"""
    plot graphs
"""
import pandas as pd
import mypara
import matplotlib.pyplot as plt


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



def plt_od_cost(mp:mypara.ParaClass(),cases):

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
        # all_od.append(od)
        plt.show()
        # tpt.plot(od)


def main(mp:mypara.ParaClass(), cases):

    plt_od_cost(mp, cases)
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
    plt.show()
    plt.figure("fare")
    plt.plot(fare)
    plt.show()
        

    pass