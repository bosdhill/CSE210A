load ../../harness

@test "0a9b20ecad29" {
  check 'B  :=     X   + 1    ;  
z     :=     0-   -4     ' '⇒ skip; z := (0--4), {B → 1}
⇒ z := (0--4), {B → 1}
⇒ skip, {B → 1, z → 4}'
}
