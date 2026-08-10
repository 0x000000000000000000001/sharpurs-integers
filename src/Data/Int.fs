let fromNumberImpl = 
    fun (just: obj) -> fun (nothing: obj) -> fun (nVal: obj) ->
        let n = nVal :?> float
        if n = float (int n) then
            let just' = just :?> (obj -> obj)
            just' (box (int n))
        else
            nothing

let toNumber = 
    fun (n: obj) -> box (float (n :?> int))

let fromStringAsImpl = 
    fun (just: obj) -> fun (nothing: obj) -> fun (radixVal: obj) ->
        let radix = radixVal :?> int
        let just' = just :?> (obj -> obj)
        fun (sVal: obj) ->
            let s = sVal :?> string
            try
                let i = System.Convert.ToInt32(s, radix)
                just' (box i)
            with
            | _ -> nothing

let toStringAs = 
    fun (radixVal: obj) -> fun (iVal: obj) ->
        let radix = radixVal :?> int
        let i = iVal :?> int
        box (System.Convert.ToString(i, radix))

let quot = 
    fun (xVal: obj) -> fun (yVal: obj) ->
        let x = xVal :?> int
        let y = yVal :?> int
        box (x / y)

let rem = 
    fun (xVal: obj) -> fun (yVal: obj) ->
        let x = xVal :?> int
        let y = yVal :?> int
        box (x % y)

let pow = 
    fun (xVal: obj) -> fun (yVal: obj) ->
        let x = xVal :?> int
        let y = yVal :?> int
        box (int (System.Math.Pow(float x, float y)))
