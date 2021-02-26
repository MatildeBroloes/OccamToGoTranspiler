import time
import os

for x in range(0,10):
  os.system('../occam-programs/count')

mean1 = 0
for x in range(0, 100):
  start = time.time()
  os.system('../occam-programs/count')
  end = time.time()
  mean1 = mean1 + (end - start)

for x in range(0,10):
  os.system('echo "40\n" | ../occam-programs/extended')

mean2 = 0
for x in range(0, 100):
  start = time.time()
  os.system('echo "40\n" | ../occam-programs/extended')
  end = time.time()
  mean2 = mean2 + (end - start)

for x in range(0,10):
  os.system('echo "400\n" | ../occam-programs/commstime')

mean3 = 0
for x in range(0, 100):
  start = time.time()
  os.system('echo "400\n" | ../occam-programs/commstime')
  end = time.time()
  mean3 = mean3 + (end - start)

print (mean1/100)
print (mean2/100)
print (mean3/100)
