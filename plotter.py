#!/usr/bin/python

import sys
import numpy as np
import matplotlib.pyplot as plt
import os
from os import listdir
from os.path import isfile, join
import re
import pylab as pl                   # frange
import math                          # isnan, isinf, ceil

def get_data_files(basedir, basefn):
  return [join(basedir,p) for p in listdir(basedir) if isfile(join(basedir,p)) and p.startswith(basefn)]

def remove_tuple_item_at_index(tpl,i):
  return tpl[0:i]+tpl[(i+1):len(tpl)]
  
def bucket_pos(value):
  math.ceil((value-t_start)/t_step)-1

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

####### Configuration #######
do_aggr_plotting = False
sample_dim = 0
buckets = 100
t_start = 0
t_end = 200
t_step = (t_end-t_start)/buckets
ts = [x*t_step+t_step/2 for x in range(0,buckets)]

# TRUSTED AC
the_plots_labels = ["Time",
 "err-fakes", "err-trust", "n-errs-fakes", "n-err-trust", "distrusted"]
the_plots_formats = [[0,1,2],[0,3,4],[0,5]]
default_colors = ["black","blue", "red"]
the_plots_colors = [default_colors] * 3
nplots = 6

limitPlotY = 2000 # float("inf")

#############################

allGraphs = dict()
aggregatedGraphs = dict()

print('*** PER FILE PLOTTING ***')
print('*************************')

for fileCount, inputFilePath in enumerate(files):
  fh = open(inputFilePath, "r")
  
  parts = tuple(re.findall('_*([^-]*)-(\d+\.\d+)', inputFilePath.lstrip(join(basedir,basefn))))
  parts_suffix = "_".join(map("-".join,parts))
  title = "\n".join(map("=".join,parts))
  
  print('###########################################')
  print(inputFilePath)
  print(parts)
  
  sampleLines = fh.readlines()
  samples = [s.strip() for s in sampleLines if len(s)>0 and s[0]!="#"]
  nsamples = len(samples)

  the_plots = [np.array([], dtype=float)] * nplots

  # Loads the plots for the current file (i.e., the current config of dimensions)
  for line in samples:
    nums = map(float, line.split(" "))
    for i,pdata in enumerate(the_plots):
      the_plots[i] = np.append(pdata, nums[i])
      
  # Saves the plots for the current file (i.e., the current config of dimensions,
  #   as identified by the 'parts' tuple)
  allGraphs[parts] = the_plots
  
  # Builds a graph for the current file (i.e., the current config of dimensions)
  for nf, pformat in enumerate(the_plots_formats):
    plt.figure(figsize=(10,10), dpi=80)
    plt.xlabel(the_plots_labels[pformat[0]])
    maxy = float("-inf")
    for k in range(1,len(pformat)): # skip x-axis which is at pos 0
      plt.plot(the_plots[pformat[0]], the_plots[pformat[k]], color=the_plots_colors[nf][k], label=the_plots_labels[pformat[k]])
      maxy = max(maxy, np.nanmax(the_plots[pformat[k]]))
    maxy = min(maxy, limitPlotY)
    axes = plt.gca()
    axes.set_ylim(ymax = maxy)  
    plt.legend(loc='upper left', frameon=False)
    t = plt.title(title) 
    plt.subplots_adjust(top=.84) 
    savefn = outdir+basefn+"_"+parts_suffix+ "_" + "".join(map(str,pformat)) +".png"
    print("SAVE: " + savefn)
    plt.savefig(savefn)  
    plt.close()  

  fh.close()
  
if not do_aggr_plotting:
  sys.exit(0)
  
print("*** AGGREGATE PLOTTING ***")
print('**************************')


for j in range(1,nplots): # skip x-axis which is at pos 0
  plt.figure()
  plt.title(the_plots_labels[j])
  for k,v in allGraphs.items():
    plt.plot(v[0], v[j], label=str(k)) 
  plt.legend(loc='upper left', frameon=False)
  savefn = outdir+basefn+the_plots_labels[j].replace(" ","")+".png"
  print("SAVE: " + savefn)
  plt.savefig(savefn)