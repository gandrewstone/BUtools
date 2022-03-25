#!/usr/bin/python3

import pdb
import subprocess
import shlex
import requests
import json
import time

reqHdr = {
    'PRIVATE-TOKEN': open("gitlabToken.txt").read().replace('\n','')
}

# Don't sleep right after waking, allow time to log in
wakeHysteresis = 300

awakePollInterval=60

def bash(cmd):
    ret = subprocess.run(shlex.split(cmd), stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=False, check=True)
    return ret.stdout.decode() if ret.stdout != None else ""

def who():
    return bash("/usr/bin/who")

def sleepMe():
    bash("/bin/systemctl suspend")
    time.sleep(wakeHysteresis)
    
def gitlabAnyJobs():
    # don't sleep if any jobs waiting or running
    response = requests.get('https://gitlab.com/api/v4/projects/19725714/jobs?scope[]=pending&scope[]=running', headers=reqHdr)
    response.raise_for_status()
    respText = response.content.decode()
    return json.loads(respText)


def Test():
    awake = True

    while True:
        time.sleep(awakePollInterval)
        using = who()
        if using.strip() != "":  # someone is logged in
            continue

        jobs = gitlabAnyJobs()
        print("Jobs: ")
        print(json.dumps(jobs, indent=4))
        if len(jobs) > 0:
            continue

        # ok no log-in no jobs, time to sleep
        sleepMe()

        # just go once for now
        return

if __name__ == "__main__":
    Test()
