# Aggregate Computing and Computational Trust

This is the project repository for the experiments of

* Casadei, Roberto, Alessandro Aldini, and Mirko Viroli. "Combining trust and aggregate computing." 15th Int. Workshop on Foundations of Coordination Languages and Self-Adaptative Systems (FOCLASA’17), Trento, 2017;
* and the extended paper "Towards Attack-Resistant Aggregate Computing Using Trust Mechanisms" submitted to the Elsevier journal Science of Computer Programming (SCP), Special Issue "Foclasa 2017". 

### Experiments: how-to

#### Simulation framework

Clone the repository and prepare the working directory:

```
$ git clone https://bitbucket.org/metaphori/trusted-ac-experiments
$ cd trusted-ac-experiments
$ mkdir data # create dir to host experiment data
```

Source code can be compiled with:

```
./gradlew build
```

Experiments can launched with a command such as the following:

```
$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -b -var random errorLB distanceFromSource \
       -y src/main/yaml/gradient.yml -e data/mydata -t 200 -p 3 -v &> exec.txt &
```

where

* the first line is responsible of launching Alchemist and setting the classpath (the directory where class files are generated during the build process and the JAR of Alchemist);
* the second line specifies that Alchemist is to be run in batch mode (`-b`) for all combinations of the variables indicated after (`-var`);
* the third line specifies the YAML configuration file for the simulation (`-y`), the directory and file prefix (there will be one output file per run) where data have to be exported (`-e`), the total 'simulated time' (i.e., not the actual time) of the simulation (`-t`), the number of parallel runs (`-p`), and verbose mode with output redirection to file `exec.txt` (might be useful in case of errors); the last & is used to run the command in background (keep track of the PID in case you may want to kill it).

Data files are in CSV format, where columns represents the *exports* specified in the YAML file (section `exports:`);
data files are named after a variable combination, so, e.g., for the command above you'll have files of the format `data/mydata_random-XXXX_errorLB-YYYY_distanceFromSource-ZZZZ.txt`.

Further details about Alchemist can be found [here](https://alchemistsimulator.github.io/).

After data is available, diagrams can be generated using the Python script `plotter2.py`:

`USAGE: plotter2 <basedir> <basefn> <plotConfigFile>`
  
So, for the example above, data can be generated as follows:

`./plotter2 data mydata plots/plot_gradient.yml`

where the last argument is the plot configuration file and is used to drive the plotting script.
Plots will be generated under `data/imgs/` with proper suffixes.

#### Simulations and code

There are 4 relevant simulation configurations:

* `svalues.yml`
* `gradient.yml`
* `mobility.yml`
* `channel.yml` (cf., Section 7 "Case Study: Attack-Resistant Channel")

#### Simulations in the paper

Note: the following instructions might produce simulations which are slightly different from the exact versions used in the paper.
E.g., in the following subsection, the qualitative simulation which shows that the gradient field correctly recovers from attacks uses 1 attacker instead of 4.

#### Qualitative: gradient field (cf., Figure 5)

You need to launch Alchemist with `gradient.yml` simulation, not in batch mode (which automatically runs the simulator in headless mode), and with the effects file `src/main/resources/base_effects.aes` in order to have visual clues of the working.
Once the GUI has opened, you need to press `P` to start the simulation.

```
$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -y src/main/yaml/gradient.yml -g src/main/resources/base_effects.aes -v &> exec.txt &
```

#### Response of `s` to attacks (cf., Figure 6)

```
$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -b -var random \
       -y src/main/yaml/svalues.yml -e data/fig6 -t 200 -p 3 -v &> exec.txt &
$ ./plotter2.py data fig6 plots/plot_svalues.yml
```

#### Calibration of errorLB (cf., Figure 7)



#### Error and `trustThreshold` (cf., Figure 8)

#### Error and `observationWinwowSize` (cf., Figure 9)

#### Number of distrusted nodes (cf., Figure 10)

#### Error and `commRadius` (cf., Figure 11)

#### Error and mobile attackers (cf., Figure 12)

#### Channel error (cf., Figure 14)

#### Qualitative: attack-resistant channel (cf., Figure 13)

```
$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -y src/main/yaml/channel.yml -g src/main/resources/channel_effects.aes -v &> exec.txt &
``` 