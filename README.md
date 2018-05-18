# Aggregate Computing and Computational Trust

This is the project repository for the experiments of

* Casadei, Roberto, Alessandro Aldini, and Mirko Viroli. "Combining trust and aggregate computing." 15th Int. Workshop on Foundations of Coordination Languages and Self-Adaptative Systems (FOCLASA’17), Trento, 2017;
* and the extended paper "Towards Attack-Resistant Aggregate Computing Using Trust Mechanisms" submitted to the Elsevier journal Science of Computer Programming (SCP), Special Issue "Foclasa 2017". 

## Experiments: how-to

### Simulation framework

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

### Simulations and code

There are 4 relevant simulation configurations (`src/main/yaml`):

* `svalues.yml`
* `gradient.yml`
* `mobility.yml`
* `channel.yml` (cf., Section 7 "Case Study: Attack-Resistant Channel")

There are 2 relevant source code files (`src/main/scala`):

* `GradientProgram.scala`
* `ChannelProgram.scala`

Please notice that the paper shows a clean version of them (without simulation-related stuff).
Moreover, the paper shows a refactored versions of the sources kept in remote branch `trust-algo-refactoring`.

### Simulations in the paper

Note: due to potential complications in the simulation- or plotting-related procedures, the following instructions might produce simulations which are slightly different from the exact versions used in the paper.
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

* `distanceFromSource = 9`
* `trustThreshold = 0.75`

```
$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -b -var random errorLB \
       -y src/main/yaml/gradient.yml -e data/fig7 -t 200 -p 3 -v &> exec.txt &
$ ./plotter2.py data fig7 plots/plot_fig7.yml
```

#### Error and `trustThreshold` (cf., Figure 8)

* `startTrustAlgorithmAt = 40`
* `errorLB = 3.50`
* `distanceFromSource = 9`
* `turnIntoFakeAt = 50`
* `random = 0 | 1`
* `trustThreshold = 0.50 | 0.55 | 0.60 | 0.65 | 0.70 | 0.75 | 0.80 | 0.85 | 0.90| 0.95`

```
$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -b -var random trustThreshold errorLB \
       -y src/main/yaml/gradient.yml -e data/fig8 -t 250 -p 3 -v &> exec.txt &

$ ./plotter2.py data fig8 plots/plot_trusterror.yml
```

#### Error and `observationWindowSize` (cf., Figure 9)

* `observationWindowSize = 3 | 8 | 13`

```
$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -b -var random distanceFromSource startTrustAlgorithmAt observationWindowSize \
       -y src/main/yaml/gradient.yml -e data/fig9 -t 250 -p 3 -v &> exec.txt &

$ ./plotter2.py data fig9 plots/plot_trusterror.yml
```

#### Number of distrusted nodes (cf., Figure 10)

* `startTrustAlgorithmAt = 30`
* `turnIntoFakeAt = 20`
* `errorLB = 3.00`
* `trustThreshold = 0.75`
* `distanceFromSource= 5 | 9 | 13`
```

$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -b -var distanceFromSource \
       -y src/main/yaml/gradient.yml -e data/fig10 -t 250 -p 3 -v &> exec.txt &

$ ./plotter2.py data fig10 plots/plot_trusterror.yml
```

#### Error and `commRadius` (cf., Figure 11)

* `startTrustAlgorithmAt = 0`
* `turnIntoFakeAt = 20`
* `errorLB = 4.00`
* `trustThreshold = 0.75`
* `distanceFromSource = 9`
* `commRadius = 1.20 | 1.95 | 2.70`

```
$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -b -var distanceFromSource \
       -y src/main/yaml/gradient.yml -e data/fig11 -t 250 -p 3 -v &> exec.txt &

$ ./plotter2.py data fig11 plots/plot_trusterror.yml
```

#### Error and mobile attackers (cf., Figure 12)

* `startTrustAlgorithmAt = 0`
* `turnIntoFakeAt = 20`
* `errorLB = 5.00`
* `trustThreshold = 0.80`
* `fakeSpeed = 0.0 | 0.5 | 0.15 | 0.25 | 0.45 | 0.65`

```
$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -b -var fakeSpeed \
       -y src/main/yaml/mobility.yml -e data/fig12 -t 250 -p 3 -v &> exec.txt &

$ ./plotter2.py data fig12 plots/plot_trusterror.yml
```

#### Qualitative: attack-resistant channel (cf., Figure 13)

```
$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -y src/main/yaml/channel.yml -g src/main/resources/channel_effects.aes -v &> exec.txt &
```

#### Channel error (cf., Figure 14)

* `startTrustAlgorithmAt = 50`
* `turnIntoFakeAt = 80`
* `errorLB = 8.00`
* `trustThreshold = 0.90`
* `random = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8`

```
$ java -Xmx5024m -cp "build/classes/main:libs/alchemist-redist-7.0.2.jar" it.unibo.alchemist.Alchemist \
       -b -var fakeSpeed \
       -y src/main/yaml/channel.yml -e data/fig14 -t 250 -p 3 -v &> exec.txt &

$ ./plotter2.py data fig14 plots/plot_channel.yml
```

