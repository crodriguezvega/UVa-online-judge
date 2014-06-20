namespace UVaOnlineJudge

module Problem_10154 =
  type Turtle (weight: uint16, strength: uint16) = class
    let mutable spareStrength = strength - weight

    member this.weight = weight
    member this.strength = strength
    member this.SpareStrength
        with get() = spareStrength
        and set(value) =  spareStrength <- value
  end

  let sortFunction (left: Turtle) (right: Turtle) =
    if (left.SpareStrength > right.SpareStrength) then -1
    else if (left.SpareStrength < right.SpareStrength) then 1
    else
      if (left.weight < right.weight) then -1
      else if (left.weight > right.weight) then 1
      else 0

  let maximumNumberOfStackedTurtles turtles =
    let sortedTurtles = List.sortWith sortFunction turtles
    let firstTurtle = List.head sortedTurtles
    let rec loop (turtles: Turtle list) nrOfStackedTurtles =
      match turtles with
      | [] -> nrOfStackedTurtles
      | head :: tail when head.weight < firstTurtle.SpareStrength ->
        firstTurtle.SpareStrength <- firstTurtle.SpareStrength - head.weight
        loop tail (nrOfStackedTurtles + 1)
      | head :: tail -> loop tail nrOfStackedTurtles
    loop (List.tail sortedTurtles) 1

  [new Turtle (300us, 1000us);
   new Turtle (400us, 1000us);
   new Turtle (300us, 800us);
   new Turtle (200us, 300us);
   new Turtle (100us, 200us);
   new Turtle (50us, 100us)] |> maximumNumberOfStackedTurtles
                             |> printfn "Maximum number of turtles that can be stacked: %d"