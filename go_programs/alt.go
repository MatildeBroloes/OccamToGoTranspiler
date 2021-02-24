package main

import "sync"
import "fmt"
import "os"
import "bufio"

func alt(in, out, err chan byte) {
  defer close(out)
  defer close(err)

  var tmp byte
  var c1 = make(chan byte)
  var c2 = make(chan byte)
  out <- 'H'
  out <- 'e'
  out <- 'j'
  out <- 10
  var wg_0 sync.WaitGroup

  wg_0.Add(1)
  go func() {
    defer wg_0.Done()
    for j := 0; j < 0 + 2; j++ {
      c1 <- '?'
    }
  }()

  wg_0.Add(1)
  go func() {
    defer wg_0.Done()
    for k := 0; k < 0 + 2; k++ {
      c2 <- '!'
    }
  }()

  wg_0.Add(1)
  go func() {
    defer wg_0.Done()
    for i := 0; i < 0 + 4; i++ {
      select {
      case tmp = <-func() chan byte {if i < 2 {return c1} else {return nil}}() :
        out <- tmp
      case tmp = <-func() chan byte {if i >= 2 {return c2} else {return nil}}() :
        out <- tmp
      }
    }
  }()

  wg_0.Wait()

  out <- 10
  out <- 'D'
  out <- 'o'
  out <- 'n'
  out <- 'e'
  out <- 10
}

func main() {
  in, out, err := make(chan byte, 10), make(chan byte, 10), make(chan byte, 10)

  var wg_main sync.WaitGroup

  wg_main.Add(1)
  go func() {
    alt(in, out, err)

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
