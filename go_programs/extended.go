package main

import "sync"
import "fmt"
import "os"
import "bufio"

func read0s(in chan byte, out chan int) {
  var tmp byte
  tmp = <-in
  out <- int(tmp) - 47
}

func read1s(in chan byte, out chan int) {
  var tmp byte
  tmp = <-in
  out <- int(tmp) - 46
}

func complex(in, out, err chan byte) {
  defer close(out)
  defer close(err)

  var in1 = make(chan byte)
  var in2 = make(chan byte)
  var c1 = make(chan int)
  var c2 = make(chan int)
  var v byte
  var tmp int
  v = <-in
  var wg_0 sync.WaitGroup

  wg_0.Add(1)
  go func() {
    defer wg_0.Done()
    in2 <- v
    in1 <- v
  }()

  wg_0.Add(1)
  go func() {
    defer wg_0.Done()
    read0s(in1, c1)
  }()

  wg_0.Add(1)
  go func() {
    defer wg_0.Done()
    read1s(in2, c2)
  }()

  wg_0.Add(1)
  go func() {
    defer wg_0.Done()
    for i := 0; i < 0 + 2; i++ {
      select {
      case tmp = <-func() chan int {if true {return c1} else {return nil}}() :
        out <- byte(tmp + 48)
        out <- 10
      case tmp = <-func() chan int {if true {return c2} else {return nil}}() :
        out <- byte(tmp + 48)
        out <- 10
      }
    }
  }()

  wg_0.Wait()

}

func main() {
  in, out, err := make(chan byte, 10), make(chan byte, 10), make(chan byte, 10)

  var wg_main sync.WaitGroup

  wg_main.Add(1)
  go func() {
    complex(in, out, err)

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
