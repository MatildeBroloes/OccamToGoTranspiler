package main

import "fmt"

func hello(out chan byte) {
  i := 0
  for i < 14 {
    out <- "Hello, world!\n"[i]
    i = i + 1
  }

  /* The last thing that happens is that the channel is closed */
  close(out)
}

func main() {
  msg := make(chan byte, 10) /* 10 is just a random value to give the buffered channel? */

  go hello(msg)

  for i := range msg {
    fmt.Print(string(i))
  }
}
