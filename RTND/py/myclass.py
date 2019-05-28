"""
    define my class
"""
import para

class PathClass:
    """
       define the path class 
    """
    def __init__(self):
        self.id = -1
        self.links = []  # set of links path
        self.flow = 0

    def process(self):
        pass

class ODClass:
    """
        define od class
    """
    def __init__(self):
        self.id = -1
        self.origin = -1
        self.dest = -1
        self.paths = []
        self.demand = 0
        self.mincost = -99
        self.ttc = 0
    pass
    def get_ttc(self):
        self.ttc = self.demand*self.mincost

    def process(self):
        self.get_ttc

class NodeClass:
    def __init__(self):
        self.id = -1
        self.outlinks = []
        self.inlinks = []
        self.label = []
    def process(self):
        pass

class LinkClass:

    def __init__(self):
        self.id = -1
        self.tail = -1
        self.head = -1
        self.cost = -1
        self.flow = -1
    pass
    def process(self):
        pass


class SolClass:
    def __init__(self):
        self.id = -1
        self.link = LinkClass()
        self.dest = -1
        self.fx = -1
        self.x = -1
        self.logit = -1

    def process(self):
        pass

class NwkClass:

    def __init__(self):

        self.name = ""
        self.links = []
        self.nodes = []
        self.od = []
        self.dest = []

        for i in range(para.ParaClass.num_links):
            l = LinkClass()
            self.links.append(l)
            self.links[-1].id = i 
        for i in range(para.ParaClass.num_nodes):
            n = NodeClass()
            self.nodes.append(n)
            self.nodes[-1].id = i
            for j in range(para.ParaClass.num_dest):
                self.nodes[-1].label.append(-1)
        for i in range(para.ParaClass.num_od):
            d = ODClass()
            self.od.append(d)
            self.od[-1].id = i


    def process(self):
        pass


class CaseClass(object):
    """
        class for the summary
    """
    def __init__(self):
        self.id = -1
        self.fre = []
        self.od = []
        self.ttc= -1.0
        self.fair= -1.0
        self.err = -999.0
        self.sols = []
        self.nwk = NwkClass()
        # self.approach_set_flow = []
        # self.approach_set_cost = []

    def process(self):
        """
            procedure to post procees the case data 
        """
        self.get_ttc

    def get_ttc(self):
        """
            get the total travel time for each case 
        """ 
        self.ttc = 0
        for w in self.od:
            self.ttc += w.demand*w.mincost
    



