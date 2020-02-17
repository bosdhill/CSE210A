load ../../harness

@test "1b82c0ec36f4" {
  check 'z  := -1  *     -4     ;




z :=    x    +    z     ' '⇒ skip; z := (x+z), {z → 4}
⇒ z := (x+z), {z → 4}
⇒ skip, {z → 4}'
}
