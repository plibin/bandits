# scala-bandits

Build the code with:
```sh
sbt pack
```

To run the prevaccine, for example with Top-two Thompson sampling, prepare a script play.sh that accepts a work_dir, a seed and an arm configuration (see https://github.com/plibin-vub/flute/):
```sh
./target/pack/bin/prevaccine --seed=123 --budget=200 --flutescript=/Users/plibin/tmp/play.sh --workdir=/Users/plibin/tmp/ --algo=ttts --censor-threshold=.995
```
