load ../../harness

@test "7a66e8af5113" {
  check 'if (¬(1  < 0  +    z))    then skip     else  Q :=  1     *    Q  ' '⇒ skip, {}'
}
