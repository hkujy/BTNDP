"""
    paramter class
"""
num_links = 6 +  1 
num_od = 2
num_nodes = 4 + 1
num_dest = 1 + 1
num_cases = 10

class ParaClass:
    num_links = num_links
    num_od = num_od
    num_nodes =  num_nodes
    num_dest = num_dest
    num_cases = num_cases
    def __init__(self):
        self.input_folder = 'set input folder'
        self.output_folder = 'set output folder'
        self.links = 0
        self.od = 0
        self.nodes = 0
    pass
