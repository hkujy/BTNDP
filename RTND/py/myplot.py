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
        if gl.exp_id==1:
            x_fre = []
            num = int((gl.fre_up - gl.fre_lb)/gl.incre)
            for i in range(0, num):
                x_fre.append(gl.fre_lb + i * gl.incre)
            plt.plot(x_fre, od)
            axes=plt.gca()
            plt.xlabel("Frequency of Line 2", fontsize=10,fontname='Times New Roman')
            plt.ylabel("TTC",fontsize=10,fontname='Times New Roman')
            xtick = axes.get_xticks()
            ytick = axes.get_yticks()
            axes.set_xticklabels(xtick, fontsize=10,fontname='Times New Roman')
            axes.set_yticklabels(ytick, fontsize=10,fontname='Times New Roman')
            plt.savefig(mp.output_folder+"\\Exp_"+str(gl.exp_id)+"_OD_"+str(w)+".png",bbox_inches='tight',dpi=600)
            if gpc.is_show_fig:
                plt.show(block=False)
                plt.pause(2)
            plt.close()
            filename = mp.output_folder+"\\Exp_TTC_"+str(gl.exp_id)+"OD_"+str(w)+".txt"
            with open(filename,"w+") as f:
                for oc in od:
                    print("{0}".format(oc), file=f)

        else:
            plt.plot(x, od)
            axes=plt.gca()
            plt.xlabel("Frequency of Line 2", fontsize=10,fontname='Times New Roman')
            plt.ylabel("TTC",fontsize=10,fontname='Times New Roman')
            xtick = axes.get_xticks()
            ytick = axes.get_yticks()
            axes.set_xticklabels(xtick, fontsize=10,fontname='Times New Roman')
            axes.set_yticklabels(ytick, fontsize=10,fontname='Times New Roman')

            plt.savefig(mp.output_folder+"\\Exp_TTC_"+str(gl.exp_id)+"_OD_"+str(w)+".png",bbox_inches='tight',dpi=600)
            if gpc.is_show_fig:
                plt.show(block=False)
                plt.pause(2)
            plt.close()
            filename = mp.output_folder+"\\Exp_"+str(gl.exp_id)+"OD_"+str(w)+".txt"
            with open(filename,"w+") as f:
                for oc in od:
                    print("{0}".format(oc), file=f)


def main(mp:mypara.ParaClass(), cases,gl:gpc.GloParaClass):

    plt_od_cost(mp, cases,gl)
    ttc = []
    fair = []
    with open(mp.output_folder+"\\objects.txt", "w") as f:
        print("id,tc,fair",file=f)
        for c in cases:
            print("{0},{1},{2}".format(c.id,c.ttc,c.fair),file=f)
            ttc.append(c.ttc)
            fair.append(c.fair)

    if gl.exp_id ==1:
        plt.figure("ttc")
        x_fre = []
        num = int((gl.fre_up - gl.fre_lb)/gl.incre)
        for i in range(0, num):
            x_fre.append(gl.fre_lb + i * gl.incre)
        plt.plot(x_fre,ttc)
        plt.xlabel("Frequency of Line 2",fontsize=10, fontname='Times New Roman')
        plt.ylabel('TTC',fontsize=10,fontname='Times New Roman')
        axes=plt.gca()
        xtick = axes.get_xticks()
        ytick = axes.get_yticks()
        
        axes.set_xticklabels(xtick, fontsize=10,fontname='Times New Roman')
        axes.set_yticklabels(ytick, fontsize=10,fontname='Times New Roman')
        xmajorFormatter = plt.FormatStrFormatter('%.1f')
        ymajorFormatter = plt.FormatStrFormatter('%.1f')
        axes.xaxis.set_major_formatter(xmajorFormatter)
        axes.yaxis.set_major_formatter(ymajorFormatter)
 
        plt.savefig(mp.output_folder+"\\Exp_"+str(gl.exp_id)+"_ttc.png",bbox_inches='tight',dpi=600)
        if gpc.is_show_fig:
            plt.show(block=False)
            plt.pause(2)
        plt.close()

        plt.plot(x_fre,fair)
        plt.xlabel("Frequency of Line 2",fontsize=10, fontname='Times New Roman')
        plt.ylabel('Fair',fontsize=10,fontname='Times New Roman')
        axes=plt.gca()
        xtick = axes.get_xticks()
        ytick = axes.get_yticks()
        axes.xaxis.set_major_formatter(xmajorFormatter)
        axes.yaxis.set_major_formatter(ymajorFormatter)
        axes.set_xticklabels(xtick, fontsize=10,fontname='Times New Roman')
        axes.set_yticklabels(ytick, fontsize=10,fontname='Times New Roman')
        xmajorFormatter = plt.FormatStrFormatter('%.1f')
        plt.gca().set_yticklabels(['{0:.3f}'.format(x) for x in ytick], fontsize=10,fontname='Times New Roman')
        ymajorFormatter = plt.FormatStrFormatter('%.3f')
        plt.savefig(mp.output_folder+"\\Exp_"+str(gl.exp_id)+"_fair.png",bbox_inches='tight',dpi=600)
        if gpc.is_show_fig:
            plt.show(block=False)
            plt.pause(2)
        plt.close()
    else:
        plt.plot(ttc)
        plt.xlabel("Case Index",fontsize=10, fontname='Times New Roman')
        plt.ylabel('TTC',fontsize=10,fontname='Times New Roman')
        axes=plt.gca()
        xtick = axes.get_xticks()
        ytick = axes.get_yticks()
        axes.set_xticklabels(xtick, fontsize=10,fontname='Times New Roman')
        axes.set_yticklabels(ytick, fontsize=10,fontname='Times New Roman')

        plt.savefig(mp.output_folder+"\\Exp_"+str(gl.exp_id)+"_ttc.png",bbox_inches='tight',dpi=600)
        if gpc.is_show_fig:
            plt.show(block=False)
            plt.pause(2)
        plt.close()

        plt.plot(fair)
        plt.xlabel("Case Index",fontsize=10, fontname='Times New Roman')
        plt.ylabel('Fair',fontsize=10,fontname='Times New Roman')
        axes=plt.gca()
        xtick = axes.get_xticks()
        ytick = axes.get_yticks()
        axes.set_xticklabels(xtick, fontsize=10,fontname='Times New Roman')
        axes.set_yticklabels(ytick, fontsize=10,fontname='Times New Roman')
        xmajorFormatter = plt.FormatStrFormatter('%.1f')
        ymajorFormatter = plt.FormatStrFormatter('%.1f')
        axes.xaxis.set_major_formatter(xmajorFormatter)
        axes.yaxis.set_major_formatter(ymajorFormatter)
        plt.savefig(mp.output_folder+"\\Exp_"+str(gl.exp_id)+"_fare.png",bbox_inches='tight',dpi=600)
        if gpc.is_show_fig:
            plt.show(block=False)
            plt.pause(2)
        plt.close()


    pass