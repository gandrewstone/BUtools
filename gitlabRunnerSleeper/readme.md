# Gitlab Runner Sleeper/Waker

## Create a read gitlab token 

Save it to a file in this directory called gitlabToken.txt

## Copy

Copy this directory to both your gitlab runner AND to the monitoring computer

## Sleeper Side (your gitlab runner)

### Set up wake-on-lan

You may have to turn it on in your BIOS.

And you must turn it on in your ethernet card after every bootup:

Create this file:

emacs wol.service # change ethernet interface name if needed
sudo cp wol.service /etc/systemd/system/wol.service 
sudo systemctl daemon-reload
sudo systemctl enable wol

### update runnerSleeper.py 
 ** with your project name
 ** with any changed sleep timeouts
 
cat gitlabRunnerSleeper.service and follow the instructions to install it to run automatically


## Waker Side (your Raspberry PI or other always-on machine)

### Set up wake-on-lan

* Install
apt-get install etherwake

* Allow non-root to issue wake-on-lan packets
chmod a+s /usr/sbin/etherwake

* Test

Sleep your runner. And execute this on the RPI:

etherwake 01:02:03:04:05:06 (your runner's MAC address)

Note that probably both computers need to be on the same LAN.

### update runnerWaker.py 
 * with your gitlab project name
 * with any changed sleep timeouts
 * with the MAC address of your gitlab runner
 
cat gitlabRunnerSleeper.service and follow the instructions to install it to run automatically



