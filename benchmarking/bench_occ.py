import time
import os

mean1 = 0
for x in range(0, 100):
  start = time.time()
  os.system('../occam-programs/count')
  end = time.time()
  mean1 = mean1 + (end - start)

mean2 = 0
for x in range(0, 100):
  start = time.time()
  os.system('echo "40\n" | ../occam-programs/simple')
  end = time.time()
  mean2 = mean2 + (end - start)

mean3 = 0
for x in range(0, 100):
  start = time.time()
  os.system('echo "400\n" | ../occam-programs/commstime')
  end = time.time()
  mean3 = mean3 + (end - start)

print (mean1/100)
print (mean2/100)
print (mean3/100)
