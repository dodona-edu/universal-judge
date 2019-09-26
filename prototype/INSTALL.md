# Steps for installation of dependencies

Make sure you have a working Java and R install. `pip` is also required. `zeroMQ` will need to be installed as a dependency for the R kernel.

In a normal shell, run:

```
pip install -r requirements.txt
beakerx-install
```

Then in the R console, run:
```
install.packages(c('crayon', 'pbdZMQ', 'devtools'))
devtools::install_github(paste0('IRkernel/', c('repr', 'IRdisplay', 'IRkernel')))
IRkernel::installspec()
```
