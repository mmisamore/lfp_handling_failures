namespace FailureTypes
    
module Maybe =

    open System.IO

    // A type for values that can fail
    type Maybe<'a> =
        | Nothing
        | Just of 'a

    // Attempt to open a file and read all bytes as a string. Calling with "blah"
    // throws a System.IO.FileNotFoundException, which is not accounted for
    // in the type signature!
    let openForRead: string -> string =
        fun fp -> File.ReadAllText fp

    // Calling with "blah" catches the exception and hands back a Maybe<string>
    // of "Nothing", meaning the operation failed. Calling on "/etc/password" returns
    // a Maybe<string> of "Just" the string we got.
    let openForReadMaybe: string -> Maybe<string> =
        fun fp ->
            try Just (File.ReadAllText fp)
            with | e -> Nothing

    // Notation for left-to-right function composition: (>>) :: (a -> b) -> (b -> c) -> (a -> c)
    // composeExample 17 = 100
    let composeExample = (fun x -> x + 3) >> (fun x -> x * 5)

    // We want to be able to compose computations that can fail: (>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
    let composeMaybes: ('a -> Maybe<'b>) -> ('b -> Maybe<'c>) -> ('a -> Maybe<'c>) =
        fun amb bmc a ->
            match amb a with
            | Nothing -> Nothing
            | Just b -> bmc b

    // Now we can introduce our operator:
    let (>=>) = composeMaybes

    // Another silly computation that can fail
    let divide10ByStringLength: string -> Maybe<int> =
        fun s ->
            try Just (10 / s.Length)
            with | e -> Nothing

    // Composing with potential failures. Calling with "blah" and "/dev/null" give Nothing as expected,
    // and calling with "/etc/passwd" gives Just 0 since it is longer than 10 characters 
    let possibleFailures = openForReadMaybe >=> divide10ByStringLength

    // Another way to look at the same thing: given an output value that could have failed, we can pretend
    // that it didn't fail!
    // Boring details:
    // Here, we turn the "ma" of type Maybe<'a> into a little function of type unit -> Maybe<'a>
    // Then we can composeMaybe this with f: 'a -> Maybe<'b> to get a function unit -> Maybe<'b>, and finally
    // we evaluate at (), which is the only value of the type "unit", to get our Maybe<'b>
    let maybeBind: Maybe<'a> -> ('a -> Maybe<'b>) -> Maybe<'b> =
        fun ma f ->
            let unitMa = fun _ -> ma
            composeMaybes unitMa f ()
    
    // The bind operator has a famous name
    let (>>=) = maybeBind

    // An example of using bind: we can pretend that the open and read went fine
    let openForReadAndDivide fp = openForReadMaybe fp >>= divide10ByStringLength
         
    // Easy at this point, but we can map over possibly failed values
    let mapMaybe: ('a -> 'b) -> Maybe<'a> -> Maybe<'b> =
        fun f ma ->
            match ma with
            | Nothing -> Nothing
            | Just a -> Just (f a)
    
    // In Haskell this would be <$>, but this syntax isn't allowed in F#
    let (<!>) = mapMaybe

    // Taking the length of the result, only if we don't fail
    let readAndGetLength3: string -> Maybe<int> =
        fun s -> String.length <!> openForReadMaybe s

    // Eventually we want to handle Maybes. Here's a generic function to help:
    let maybe: 'b -> ('a -> 'b) -> Maybe<'a> -> 'b =
        fun b f ma -> match ma with
                      | Nothing -> b
                      | Just a  -> f a

    // Safely handle failures to open the file and get the length
    let readFileLengthSafe: string -> int = 
        fun fp -> maybe 0 String.length (openForReadMaybe fp)


module Result =

    open System.IO

    // A richer failure type than Maybe<'a>. This type captures not only *that* something failed, but also *why* it failed. 
    type Result<'a> =
        | Failure of string
        | Success of 'a

    // Try to open and read any file, and capture message of any resulting failure
    // Trying with "blah" gives "Could not find file 'blah'"
    let openForReadResult: string -> Result<string> =
        fun fp ->
            try Success (File.ReadAllText fp)
            with | e -> Failure (e.Message)

    // Composing computations that can fail will give us back the first failure message
    let composeResults: ('a -> Result<'b>) -> ('b -> Result<'c>) -> ('a -> Result<'c>) =
        fun f g a -> match (f a) with
                         | Failure s -> Failure s
                         | Success b -> g b
    
    // Separate module, so we can also call this >=>
    let (>=>) = composeResults

    // We can map pure functions over these types
    let mapResult: ('a -> 'b) -> (Result<'a> -> Result<'b>) =
        fun f ra -> match ra with
                      | Failure s -> Failure s
                      | Success a -> Success (f a)
        
    // The usual infix notation for mapping
    let (<!>) = mapResult

    // Mapping a pure function over a Result
    let readResultLength: string -> Result<int> =
        fun fp -> mapResult String.length (openForReadResult fp)
    
    // Generic function for safely handling any Result
    let result: 'b -> ('a -> 'b) -> Result<'a> -> 'b =
        fun b f ra ->
            match ra with
            | Failure s -> b
            | Success a -> f a
    
    // Safely handle failures to open the file and get the length
    let readFileLengthSafe fp = result 0 String.length (openForReadResult fp)
