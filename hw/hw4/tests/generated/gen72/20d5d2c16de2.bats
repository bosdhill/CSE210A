load ../../harness

@test "20d5d2c16de2" {
  check 'x  :=     z  -   3 ;

MQ   :=     x   -    p7' '⇒ skip; MQ := (x-p7), {x → -3}
⇒ MQ := (x-p7), {x → -3}
⇒ skip, {MQ → -3, x → -3}'
}
