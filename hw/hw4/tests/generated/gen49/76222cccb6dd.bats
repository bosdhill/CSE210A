load ../../harness

@test "76222cccb6dd" {
  check 'while x     *    y  =     -2 + y   ∨ x-   y =-1- y    do y:=    x    - z ' '⇒ skip, {}'
}
