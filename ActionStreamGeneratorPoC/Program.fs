open runner
open streamGenerator
open types

context "suite 1"

let knobWithNoOp = 
    { 
        rpm = 100
        actions = 
            [
                ////func = fun webClient -> { page = "test 1"; ms = 25.0 }
                { name = "test 1"; percentage = 66.1 }
                { name = "test 2"; percentage = 33.2 }
            ] 
    }

let knobNoNoOp = 
    { 
        rpm = 100
        actions = 
            [
                ////func = fun webClient -> { page = "test 1"; ms = 25.0 }
                { name = "test 1"; percentage = 70.0 }
                { name = "test 2"; percentage = 30.0 }
            ] 
    }

"given a knob it returns the right table with no-op" &&& fun _ ->
    let expected =
        {
            entries =
                [
                    { high = 10000; low = 3390; action =  { name = "test 1"; percentage = 66.1 } }
                    { high = 3389; low = 70; action = { name = "test 2"; percentage = 33.2 } }
                    { high = 69; low = 0; action = { name = "no-op"; percentage = 0.7 } }
                ]
        }
    
    buildTable knobWithNoOp == expected

"given a knob it returns the right table with out no-op" &&& fun _ ->
    let expected =
        {
            entries =
                [
                    { high = 10000; low = 3000; action =  { name = "test 1"; percentage = 70.0 } }
                    { high = 2999; low = 0; action = { name = "test 2"; percentage = 30.0 } }                    
                ]
        }
    
    buildTable knobNoNoOp == expected

"given a knob and number it returns the right table entry" &&& fun _ ->
    let table = buildTable knobWithNoOp
    let namefor number = (getCorrectTableEntry number table).action.name
    namefor 0 == "no-op"
    namefor 1 == "no-op"
    namefor 69 == "no-op"
    namefor 70 == "test 2"
    namefor 71 == "test 2"
    namefor 3389 == "test 2"
    namefor 3390 == "test 1"
    namefor 3391 == "test 1"
    namefor 9999 == "test 1"
    namefor 10000 == "test 1"    

"given knob to get a stream, predictable seed random gives consistent results" &&& fun _ ->    
    let stream = getStream knobWithNoOp predictableRandoms
    let validateResults number name =
        let result, number' = stream() |> Seq.take 1 |> Seq.head 
        number == number'
        name == result.name
    
    validateResults 3990 "test 1"    
    validateResults 8958 "test 1"    
    validateResults 3192 "test 2"    
    validateResults 9467 "test 1"    
    validateResults 3394 "test 1"    
    validateResults 9487 "test 1"

run ()

System.Console.ReadKey() |> ignore