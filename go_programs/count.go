package main

import "fmt"
import "sync"
import "os"
import "bufio"

func count(in, out, err chan byte) {
  defer close(out)
  defer close(err)

  var c byte
  c = 48

  for i := 0; i < 10; i++ {
    out <- c
    c = c + 1

  }
  out <- 10
}

func main() {
  in, out, err := make(chan byte, 10), make(chan byte, 10), make(chan byte, 10)

  var wg_main sync.WaitGroup

  wg_main.Add(1)
  go func() {
    count(in, out, err)

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
