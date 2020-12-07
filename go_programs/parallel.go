package main

import "fmt"

func multiply(in, out chan int) {
  var value int
  for i := 0; i < 5; i++ {
    value = <-in
    value = value*2
    out <- value
  }
}

func incr(in, out chan int) {
  var v, r int
  for i := 0; i< 5; i++ {
    v = <-in
    r = v + 1
    out <- r
  }
}

func parallel(out chan byte) {
  var tmp int
  var c1, c2, c3 chan int = make(chan int), make(chan int), make(chan int)

  go multiply(c1, c2)
  go incr(c2, c3)

  for i := 0; i < 5; i++ {
    c1 <- i
    tmp = <-c3
    out <- byte(tmp) + 48
    out <- 10
  }

  /* The last thing that happens is that the channel is closed */
  close(out)
}

func main() {
  msg := make(chan byte, 10) /* 10 is just a random value to give the buffered channel? */

  go parallel(msg)

  for i := range msg {
    fmt.Print(string(i))
  }
}
