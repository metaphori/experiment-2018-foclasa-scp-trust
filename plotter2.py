#!/usr/bin/python

################################################################################
########################### Script background info #############################
################################################################################

# - https://stackoverflow.com/questions/4455076/how-to-access-the-ith-column-of-a-numpy-multidimensional-array

###############################################################################
############################## Script imports #################################
###############################################################################

import sys
import numpy as np
import matplotlib.pyplot as plt
import os
from os import listdir
from os.path import isfile, join
import re
import pylab as pl                   # frange
import math                          # isnan, isinf, ceil
import pprint
#import copy  # copy.deepcopy(myDict)

#################################################################################
############################## Script functions #################################
#################################################################################

# returns: list of full paths of files (under directory `basedir`) filtered through a file prefix (`basefn`)
def get_data_files(basedir, basefn):
  return [join(basedir,p) for p in listdir(basedir) if isfile(join(basedir,p)) and p.startswith(basefn)]

def remove_tuple_item_at_index(tpl,i):
  return tpl[0:i]+tpl[(i+1):len(tpl)]

# configs: list where each config is an N-dim tuple of (dim,val) tuples   
# returns: dict where keys are (dims-dim) and values are lists of configs
def group_by_varying_values_of(dim, configs):
  res = dict()
  for config in configs:
    configWithoutDim = tuple([d for d in config if d[0]!=dim])
    lsts = res.get(configWithoutDim, [])
    lsts.append(config)
    res[configWithoutDim] = lsts
  return res
  
# Globals used
#   - t_start: beginning of time
#   - t_step: length of a bucket
#def bucket_pos(value):
#  math.ceil((value-t_start)/t_step)-1

# Returns a dict
def process_files(filepaths):
  return dict([process_file(fp) for fp in filepaths])
  
# Returns a pair (id,matrix) for each parsed file
def process_file(filepath):
  print("\n>>> Processing file: " + filepath)
  # Open file handle
  fh = open(filepath, "r")
  
  # Deduce some info from file name
  parts = re.findall('_*([^-]*)-(\d+\.\d+)', filepath.lstrip(join(basedir,basefn)))
  parts = map(lambda p: (p[0],format(float(p[1]),'.2f')), parts)
  parts = tuple(parts) # this must be hashable (and lists are not)
  print("Dimensions: " + "; ".join(map(lambda x: str(x), parts)))
  parts_suffix = "_".join(map("-".join,parts))
  title = "; ".join(map("=".join,parts))
  
  # Gets the matrix (time X exports) from file content
  # | time | export1 | ... | exportN |
  # ----------------------------------
  # |  t1  |    .    | ... |    .    |
  # | .... |   ...   | ... |   ...   |
  # |  tK  |    .    | ... |    .    |
  # ----------------------------------
  matrix = process_file_content(fh)
  dimMatrix = matrix.transpose()
  
  # Closes file handle
  fh.close()
  
  return (parts, dimMatrix)
  
def process_file_content(filehandle):
  # Read data
  lines = filehandle.readlines()
  # Removes empty and comment lines and maps to float
  data_rows = np.array([map(float, s.strip().split(" ")) for s in lines if len(s)>0 and s[0]!="#"], dtype='float')
  return data_rows

def do_bucketize(contents, nbuckets=100, start=None, end=None):
  res = dict()
  for config, content in contents.items():
    time = content[0]
    if start==None:
      start = time[0]
    if end==None:
      end = time[-1]
    time_bins = np.linspace(start,end,nbuckets)
    hist = np.histogram(time, time_bins)
    # for ncol,data in enumerate(content):
  # INCOMPLETE DEFINITION
  return res
  
def merge_samples(contents, configs):
  res = dict()
  for config,sconfigs in configs.items():
    nsamples = len(sconfigs)
    print("CONFIGURATION: " + str(config) + " has " + str(nsamples) + " samples.")
    matrices = [contents[sample_config] for sample_config in sconfigs]
    time = map(lambda x: round(x), matrices[0][0]) # time should be the same for all
    matrices = map(lambda l: l[1:], matrices) # skips the time dimension for each sample
    merged = reduce(lambda a,b: a+b, matrices)
    merged = map(lambda x: x/nsamples, merged)
    merged.insert(0,time)
    res[config] = merged
  return res
  
def plot(config,content):
  title = "\n".join(map("=".join,config))
  parts_suffix = "_".join(map("-".join,config))
  for nf, pformat in enumerate(the_plots_formats):
    plt.figure() # (figsize=(10,10), dpi=80)
    plt.xlabel(the_plots_labels[pformat[0]])
    maxy = float("-inf")
    for k in range(1,len(pformat)): # skip x-axis which is at pos 0
      plt.plot(content[pformat[0]], content[pformat[k]], color=the_plots_colors[nf][k], label=the_plots_labels[pformat[k]])
      maxy = max(maxy, np.nanmax(content[pformat[k]]))
    maxy = min(maxy, limitPlotY)
    axes = plt.gca()
    axes.set_ylim(ymax = maxy)  
    plt.legend(loc='upper left', bbox_to_anchor=(0.0, 1.1), )
    t = plt.title(title) 
    plt.subplots_adjust(top=.84) 
    savefn = outdir+basefn+"_"+parts_suffix+ "_" + "".join(map(str,pformat)) +".png"
    print("SAVE: " + savefn)
    plt.tight_layout()
    plt.savefig(savefn)  
    plt.close() 

pp = pprint.PrettyPrinter(indent=4) # for logging purposes

#######################################################################################
############################## Script configuration ###################################
#######################################################################################

sampling = True  # tells if there is a 'random' dimension for sampling
bucketize = False # tells if there is a 'time' dimension to be split into buckets
do_aggr_plotting = True
#sample_dim = 0
#nbuckets = 100
#t_start = 0
#t_end = 200
#ts = np.linspace(t_start, t_end, nbuckets)
#t_step = (t_end-t_start)/buckets
#ts = [x*t_step+t_step/2 for x in range(0,buckets)]

# TRUSTED AC
the_plots_labels = ["Time",
 "err-fakes", "err-trust", "n-errs-fakes", "n-err-trust", "distrusted"]
the_plots_formats = [[0,1,2]] # [[0,1,2],[0,3,4],[0,5]]
default_colors = ["black","blue", "red"]
the_plots_colors = [default_colors] * 3
nplots = 6

limitPlotY = 2000 # float("inf")

###################################################################################
############################## Script preparation #################################
###################################################################################

script = sys.argv[0]
basedir = sys.argv[1]
basefn = sys.argv[2]
outdir = os.path.join(basedir, "imgs/")
if not os.path.exists(outdir):
  os.makedirs(outdir)
  
files = get_data_files(basedir,basefn)

print("Executing script: basedir=" + basedir + "\t basefilename=" + basefn)
print("Files to be processed: " + str(files))
print("Output directory for graphs: " + str(outdir))

############################# Script logic

print('*************************')
print('*** PER FILE PLOTTING ***')
print('*************************')

# CONTENTS: {key => matrix}
#   file1 [d1=A  d2=B ] => export1=[...], ..., exportK=[...]
#   file2 [d1=A' d2=B ] => export1=[...], ..., exportK=[...]  
#   file3 [d1=A  d2=B'] => export1=[...], ..., exportK=[...]
#   file4 [d1=A' d2=B'] => export1=[...], ..., exportK=[...]
contents = process_files(files)

# CONFIGURATIONS
#   file1_2 [d1=*, d2=B ] => export1=[...], ..., exportK=[...]
#   file3_4 [d1=*, d2=B'] => export1=[...], ..., exportK=[...]
configs = contents.keys() # List of configs, where each config is an N-dim tuple of (k,v) tuples
if sampling:
  sconfigs = group_by_varying_values_of('random', configs)
  merged_contents = merge_samples(contents, sconfigs)
  for title,content in merged_contents.items():
    plot(title,content)

if bucketize:
  contents = do_bucketize(contents)
  

