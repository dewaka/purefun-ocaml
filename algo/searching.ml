let binary_search x arr =
	let n = Array.length arr in
	let rec find i j =
		if i = j then false
		else
			let m = (i + j) / 2 in
			let mid = Array.get arr m in
			if mid=x then true
			else 
				if x<mid then find i m
				else find m j
	in
	find 0 n

