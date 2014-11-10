module streamGenerator

open types

let buildTable knob =
    let max = 10000
    let max2 = ref max
    let results = 
        knob.actions
        |> List.map  (fun action -> 
            let oldMax = !max2
            let percentage = action.percentage * 100.0 |> System.Math.Round |> int
            max2 := oldMax - percentage
            {
                high = if oldMax = max then max else oldMax - 1
                low = !max2
                action = action
            })
      
    //create no-op if needed
    let sumOfPercentages = results |> List.sumBy (fun tableEntry -> tableEntry.action.percentage)
    if int sumOfPercentages <> 100 then
        let min = results |> List.minBy (fun tableEntry -> tableEntry.low)
        let newHigh = min.low - 1
        let newLow = 0
        let tableEntry = { high = newHigh; low = newLow; action = { name = "no-op"; percentage = (min.low |> float) / 100.0; }}
        let results = results @ [tableEntry]
        {
            entries = results
        }
    else
        {
            entries = results
        }
    
let getCorrectTableEntry number table =
    table.entries |> List.find (fun tableEntry -> tableEntry.low <= number && number <= tableEntry.high)

let getStream knob (randomStrategy : unit -> System.Random) =
    let table = buildTable knob
    let random = randomStrategy()
    fun () -> seq { 
        let number = random.Next(0, 10000)
        let entry = getCorrectTableEntry number table
        yield (entry.action, number); 
    }