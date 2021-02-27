package main

import "os"
import "fmt"
import "sync"
import "bufio"

func fib(in, out, err chan byte) {
  defer close(out)
  defer close(err)

  var fib0 int
  var fib1 int
  var res int
  var n int
  var m int
  var msg int
  var read byte
  var d bool
  d = true

  n = 0

  out <- 'I'
  out <- 'n'
  out <- 'p'
  out <- 'u'
  out <- 't'
  out <- 58
  out <- 32
  for d {
    read = <-in
    if read == 10 {
      d = false

    } else if true {
      n = n * 10 + int(read) - 48

    } else { os.Exit(1) }
    out <- read
  }
  fib0 = 0

  fib1 = 1

  if n == 0 {
    out <- byte(fib0 + 48)
    out <- 10
  } else if n == 1 {
    out <- byte(fib1 + 49)
    out <- 10
  } else if n > 46 || n < 0 {
    err <- 'I'
    err <- 'n'
    err <- 'p'
    err <- 'u'
    err <- 't'
    err <- 32
    err <- 'e'
    err <- 'r'
    err <- 'r'
    err <- 'o'
    err <- 'r'
    err <- 10
  } else if true {
    res = 0

    for i := 0; i < 0 + n - 1; i++ {
      res = fib0 + fib1

      fib0, fib1 = fib1, res

    }
    m = 1000000000

    for m >= 1 {
      msg = res / m

      res = res % m

      m = m / 10

      out <- byte(msg + 48)
    }
    out <- 10
  } else { os.Exit(1) }
}

func main() {
  in, out, err := make(chan byte, 10), make(chan byte, 10), make(chan byte, 10)

  var wg_main sync.WaitGroup

  wg_main.Add(1)
  go func() {
    fib(in, out, err)

    wg_main.Done()
  }()

  go func() {
    input := bufio.NewReader(os.Stdin)
    for {
      char, _, err := input.ReadRune()
      if err == nil {in <- byte(char)}
    }
  }()

  wg_main.Add(1)
  go func() {
    for i := range out {
      fmt.Print(string(i))
    }
    wg_main.Done()
  }()

  wg_main.Add(1)
  go func() {
    for i := range err {
      fmt.Print(string(i))
    }
    wg_main.Done()
  }()

  wg_main.Wait()
}
