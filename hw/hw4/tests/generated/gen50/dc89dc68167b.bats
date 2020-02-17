load ../../harness

@test "dc89dc68167b" {
  check 'j   :=   P0+   -3    ;
 
 y   :=    -2-   x   ' '⇒ skip; y := (-2-x), {j → -3}
⇒ y := (-2-x), {j → -3}
⇒ skip, {j → -3, y → -2}'
}
