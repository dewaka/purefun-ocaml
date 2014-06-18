let binary_search x arr =
  let n = Array.length arr - 1 in
  let rec find i j =
    if i > j then None
    else
      let m = (i + j) / 2 in
      let mid = Array.get arr m in
      if mid=x then Some(m)
      else
        if x<mid then find i (m-1)
        else find (m+1) j
  in
  find 0 n
