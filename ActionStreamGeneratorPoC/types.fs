module types

type result = { 
    page : string
    ms : float 
}

type action = {
    //func : System.Net.WebClient -> result
    percentage : float
    name : string
}

type knob = {
    rpm : int
    actions : action list
}

type run = {
    knobs : knob list
}

type tableEntry = {
    action : action
    high : int
    low : int    
}

type table = {
    entries : tableEntry list
}

//randomness strategies
let predictableRandoms () = System.Random(1234)
let realRandoms () = System.Random()