package main

import "fmt"

func bubble(out chan byte) {
  var swapped bool
  var tmp byte
  var nums [5]int

  swapped = true
  nums = [...]int{2, 5, 4, 1, 3}

  for swapped == true {
    swapped = false
    for i := 0; i < 4; i++ {
      if nums[i] > nums[i+1] {
        nums[i], nums[i+1] = nums[i+1], nums[i]
        swapped = true
      } else { continue }
    }
  }

  for j := 0; j < 5; j++ {
    tmp = byte(nums[j])
    out <- (tmp + 48)
  }
  out <- 10

  /* The last thing that happens is that the channel is closed */
  close(out)
}

func main() {
	msg := make(chan byte, 10) /* 10 is just a random value to give the buffered channel? */

  go bubble(msg)

	for i := range msg {
		fmt.Print(string(i))
	}
}
