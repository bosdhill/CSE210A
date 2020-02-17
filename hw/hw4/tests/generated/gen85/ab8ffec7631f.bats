load ../../harness

@test "ab8ffec7631f" {
  check 'if (x     -   y     < z     -     1  ∧   false)    then 

s  :=     z    +    3   else skip    ' '⇒ skip, {}'
}
