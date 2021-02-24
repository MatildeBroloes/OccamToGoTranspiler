package main

import "os"
import "sync"
import "fmt"
import "bufio"

func prefix(cin, cout chan int, n int) {
  var v int
  v = 0

  for i := 0; i < 0 + n; i++ {
    cout <- v
    v = <-cin
  }
}

func delta(cin, cout1, cout2 chan int, n int) {
  var v int
  for i := 0; i < 0 + n; i++ {
    v = <-cin
    cout1 <- v
    cout2 <- v
  }
}

func succ(cin, cout chan int, n int) {
  var v int
  for i := 0; i < 0 + n; i++ {
    v = <-cin
    cout <- v + 1
  }
}

func consume(cin chan int, n int) {
  for i := 0; i < 0 + n; i++ {
    <-cin
  }
}

func commstime(in, out, err chan byte) {
  defer close(out)
  defer close(err)

  var a = make(chan int)
  var b = make(chan int)
  var c = make(chan int)
  var d = make(chan int)
  var read byte
  var n int
  var n1 int
  var n2 int
  var n3 int
  var n4 int
  var reading bool
  reading = true

  n = 0

  for reading {
    read = <-in
    if read == 10 {
      reading = false

    } else if true {
      n = n * 10 + int(read) - 48

    } else { os.Exit(1) }
    out <- read
  }
  n1, n2, n3, n4 = n, n, n, n

  var wg_0 sync.WaitGroup

  wg_0.Add(1)
  go func() {
    defer wg_0.Done()
    prefix(c, a, n1)
  }()

  wg_0.Add(1)
  go func() {
    defer wg_0.Done()
    delta(a, b, d, n2)
  }()

  wg_0.Add(1)
  go func() {
    defer wg_0.Done()
    succ(b, c, n3)
  }()

  wg_0.Add(1)
  go func() {
    defer wg_0.Done()
    consume(d, n4)
  }()

  wg_0.Wait()

  os.Exit(1)
}

func main() {
  in, out, err := make(chan byte, 10), make(chan byte, 10), make(chan byte, 10)

  var wg_main sync.WaitGroup

  wg_main.Add(1)
  go func() {
    commstime(in, out, err)

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
