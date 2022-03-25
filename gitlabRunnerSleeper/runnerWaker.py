#!/usr/bin/python3

import pdb
import subprocess
import shlex
import requests
import json
import time

runnerMAC = "04:d9:f5:7e:39:b2"
runnerIP = "192.168.1.167"

asleepPollInterval = 60
awakePollInterval = 60

reqHdr = {
    'PRIVATE-TOKEN': open("gitlabToken.txt").read().replace('\n','')
}


def bash(cmd):
    ret = subprocess.run(shlex.split(cmd), capture_output=True, shell=False, check=True)
    return ret.stdout.decode()

def gitlabJobs():
    try:
        response = requests.get('https://gitlab.com/api/v4/projects/19725714/jobs?scope[]=pending', headers=reqHdr)
    except ConnectionError:
        # gitlab down or more likely I'm just starting up and have no DNS
        time.sleep(15)
        return json.loads("[]")
    response.raise_for_status()
    respText = response.content.decode()
    return json.loads(respText)

def ping(hostname):
    try:
        ret = bash("/bin/ping -c 1 " + hostname)
        return True
    except subprocess.CalledProcessError:
        return False


def Test():
    awake = True

    while True:
        while awake:
            awake = ping(runnerIP)
            if awake:
                print("awake")
                time.sleep(awakePollInterval)
            else:
                print("asleep")

        while not awake:
            jobs = gitlabJobs()
            print("Pending Jobs: ")
            print(json.dumps(jobs, indent=4))
            if len(jobs) > 0:
                print("WAKING runner!")
                bash("/usr/sbin/etherwake %s" % runnerMAC)
                time.sleep(30) # I have to give it time to come up
                awake = True
            else:
                time.sleep(asleepPollInterval)

if __name__ == "__main__":
    Test()
