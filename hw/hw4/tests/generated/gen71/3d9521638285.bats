load ../../harness

@test "3d9521638285" {
  check 'while true    ∧     false    do Z  := z    +    -4     ' '⇒ skip, {}'
}
