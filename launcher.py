from subprocess import Popen, PIPE
import time
import sys

relaunched = False
print("Initial process")
if len(sys.argv) > 2:
    bonus = 'b'
else:
    bonus = ''

p = Popen(["bin/icfp13", "solve"+bonus, sys.argv[1]],stderr=PIPE)
_, err = p.communicate()
id, size, nops = open('tests.txt').readline().strip().split()
size = int(size)
print("Forking %d" % size)
ps = []

start = time.time()
countincr = 1
count = 5
for i in range(4):
    p = Popen(["bin/icfp13", "follow" + bonus, str(count)],stderr=PIPE)
    count += countincr
    ps.append(p)

while True:
    for p in ps:
        status = p.poll()
        if status is not None:
            print("Process ended running:%d/%d" % (len(ps)-1,4))
            _, err = p.communicate()
            ps.remove(p)
            err = err.decode('ascii')
            print(err)
            if "WIN" in err:
                print('SOLUTION FOUND')
                print('WIN %f' % (time.time() - start))
                for p in ps:
                    p.terminate()
                exit(0)
            else:
                p = Popen(["bin/icfp13", "follow"+bonus, str(count)],stderr=PIPE)
                count += countincr
                if count > size:
                    count = size
                ps.append(p)
    time.sleep(0.2)
    elapsed = time.time() - start
    if False and elapsed > 30 and (int(elapsed)-30) % 20 == 0:
        print('RELAUNCHING FULL')
        for p in ps:
            p.terminate()
        ps = []
        for i in range(4):
            p = Popen(["bin/icfp13", "follow"+bonus, str(size), "random"],stderr=PIPE)
            ps.append(p)
    if elapsed > 60:
        print('TOO LATE')
        for p in ps:
            p.terminate()
        exit(0)

