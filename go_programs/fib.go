package main

import "fmt"

func fib(screen chan byte) {
  var a byte = 1
  screen <- a
  var b byte = 1
  screen <- b
  var i int = 0
  for i < 10 {
    var tmp byte = a + b
    a = b
    b = tmp
    screen <- tmp
    i = i + 1
  }

  close(screen)
}

func main() {
  msg := make(chan byte, 10) /* 10 is just a random value to give the buffered channel? */

  go fib(msg)

  for i := range msg {
    fmt.Print(string(i))
  }
}
