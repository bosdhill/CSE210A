load ../../harness

@test "b49aa26a389b" {
  check 'if (x -   z     < -4-    4∨ y   -     3  <-4 -    3)     then y  :=    z  * z  else  SX    :=  x    -   R' '⇒ SX := (x-R), {}
⇒ skip, {SX → 0}'
}
