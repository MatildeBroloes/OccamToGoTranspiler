import time
import os

mean1 = 0
for x in range(0, 10):
  start = time.time()
  os.system('echo "4\n" | ./commstime')
  end = time.time()
  mean1 = mean1 + (end - start)

mean2 = 0
for y in range(0, 10):
  start = time.time()
  os.system('echo "12345\n" | ./commstime')
  end = time.time()
  mean2 = mean2 + (end - start)

mean3 = 0
for z in range(0, 10):
  start = time.time()
  os.system('echo "123456789\n" | ./commstime')
  end = time.time()
  mean3 = mean3 + (end - start)

print (mean1/10)
print (mean2/10)
print (mean3/10)
