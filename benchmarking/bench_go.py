import time
import os

mean1 = 0
for x in range(0, 100):
  start = time.time()
  os.system('echo "40\n" | go run ../go_programs/simple.go')
  end = time.time()
  mean1 = mean1 + (end - start)

mean2 = 0
for x in range(0, 100):
  start = time.time()
  os.system('echo "4\n" | go run ../go_programs/fancy.go')
  end = time.time()
  mean2 = mean2 + (end - start)

mean3 = 0
for x in range(0, 100):
  start = time.time()
  os.system('echo "400\n" | go run ../go_programs/commstime.go')
  end = time.time()
  mean3 = mean3 + (end - start)

print (mean1/100)
print (mean2/100)
print (mean3/100)
