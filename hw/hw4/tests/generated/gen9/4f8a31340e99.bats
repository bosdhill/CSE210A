load ../../harness

@test "4f8a31340e99" {
  check 'if (¬(z +   0 =   -4    +   y))     then    skip   else  

y:=     y     * -4 ' '⇒ skip, {}'
}
