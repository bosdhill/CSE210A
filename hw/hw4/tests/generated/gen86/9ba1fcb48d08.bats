load ../../harness

@test "9ba1fcb48d08" {
  check 'z := -1 *     3;  


z   := -3    +  tn     ' '⇒ skip; z := (-3+tn), {z → -3}
⇒ z := (-3+tn), {z → -3}
⇒ skip, {z → -3}'
}
