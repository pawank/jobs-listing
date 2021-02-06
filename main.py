from __future__ import print_function
import os
import os.path
import sys
import requests
import traceback
import simplejson as json
import re
import time
headers = {
    'user-agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.47 Safari/537.36'}


class dict2object(dict):
    def __getattr__(self, name):
        if name in self:
            return self[name]
        else:
            raise AttributeError("No such attribute: " + name)

    def __setattr__(self, name, value):
        self[name] = value

    def __delattr__(self, name):
        if name in self:
            del self[name]
        else:
            raise AttributeError("No such attribute: " + name)


class objdict(dict):
    def __getattr__(self, name):
        if name in self:
            return self[name]
        else:
            raise AttributeError("No such attribute: " + name)

    def __setattr__(self, name, value):
        self[name] = value

    def __delattr__(self, name):
        if name in self:
            del self[name]
        else:
            raise AttributeError("No such attribute: " + name)

def call(sample):
    from requests.auth import HTTPBasicAuth
    payload = {'sample': sample}
    url = 'http://www.sarkarijoblisting.com/backoffice/ads'
    #url = 'http://localhost:9988/backoffice/ads'
    pwd = None
    try:
        if 'SAJOLI_PWD' in os.environ.keys():
            pwd = str(os.environ['SAJOLI_PWD'])
        r = requests.post(url, auth=HTTPBasicAuth('admin', pwd), headers=headers, params=payload)
        print('Status code = ', r.status_code)
        if r.status_code == 200:
            data = r.json()
            print("Output = ", data)
            j = data
            #j = json.loads(data)
            cnt = j["message"]
            return int(cnt)
    except Exception as ex:
        error = str(traceback.format_exc())
        print("ERROR = ", error, " in url = ", url)
    return 999999

def crawl_sites(sample):
    from requests.auth import HTTPBasicAuth
    payload = {'sample': sample, 'filename': 'crawl-all-parallel', 'concurrency': 15, 'offsetNo': sample, 'pageNo': 0}
    url = 'http://www.sarkarijoblisting.com/backoffice/testing'
    try:
        pwd = None
        if 'SAJOLI_PWD' in os.environ.keys():
            pwd = str(os.environ['SAJOLI_PWD'])
        #print('pwd', pwd)
	#return 999999
        r = requests.get(url, auth=HTTPBasicAuth('admin', pwd), headers=headers, params=payload)
        print('Status code = ', r.status_code)
        if r.status_code == 200:
            data = r.json()
            print("Output = ", data)
            j = data
            #j = json.loads(data)
            cnt = j["message"]
            return int(cnt)
    except Exception as ex:
        error = str(traceback.format_exc())
        print("ERROR = ", error, " in url = ", url)
    return 999999

if __name__ == '__main__':
    i = 0
    pages = 9999
    while i < pages: 
        result = None
        if sys.argv[1] == 'call':
            result = call(50)
        else:
            result = crawl_sites(15)
        print('COUNT = ', result)
        if result <= 0:
            break
        i += 1
        time.sleep(300)
        #i = 99999
    print("DONE")
    pass
