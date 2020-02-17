load ../../harness

@test "9599f8f17e99" {
  check 'y :=-4-   y  ;
y    :=h  +  2' '⇒ skip; y := (h+2), {y → -4}
⇒ y := (h+2), {y → -4}
⇒ skip, {y → 2}'
}
