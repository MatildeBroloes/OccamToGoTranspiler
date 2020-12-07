package main

import "fmt"

func count(out chan byte) {
  var c byte
  c = 48
  for i := 0; i < 10; i++ {
    out <- c
    c = c + 1
  }
  out <- 10

  /* The last thing that happens is that the channel is closed */
  close(out)
}

func main() {
  msg := make(chan byte, 10) /* 10 is just a random value to give the buffered channel? */

  go count(msg)

  for i := range msg {
    fmt.Print(string(i))
  }
}

