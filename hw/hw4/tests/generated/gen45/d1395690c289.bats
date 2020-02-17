load ../../harness

@test "d1395690c289" {
  check 'if (2  +2 <    1 - 0)  then 
 y   :=-3    +  zd      else z    :=-4  ' '⇒ z := -4, {}
⇒ skip, {z → -4}'
}
