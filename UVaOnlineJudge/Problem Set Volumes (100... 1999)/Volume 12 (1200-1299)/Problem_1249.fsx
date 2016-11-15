module Problem_1249 =
  type Point = { x: float; y: float }

  // Inputs
  let A = { x = 1.3 ; y = 2.6 } 
  let B = { x = 12.1 ; y = 4.5 } 
  let C = { x = 8.1 ; y = 13.7 } 
  let D = { x = 2.2 ; y = 0.1 } 
  let E = { x = 9.8 ; y = 6.6 } 
  let F = { x = 1.9 ; y = 6.7 }

  let distance a b = sqrt ((a.x - b.x) ** 2.0 + (a.y - b.y) ** 2.0)
  let vector a b = { x = b.x - a.x; y = b.y - a.y }

  let areaTriangle D E F =
    let a = distance D E
    let b = distance E F
    let c = distance F D
    let s = (a + b + c) / 2.0
    sqrt (s * (s - a) * (s - b) * (s - c))

  let angle A B C =
    let u = vector A B
    let v = vector A C
    acos ((u.x * v.x + u.y * v.y) / (sqrt (u.x ** 2.0 + u.y ** 2.0) * sqrt (v.x ** 2.0 + v.y ** 2.0)))

  let calculatePoints A B theta elevation height =
    let distance = height / sin theta
    let deltaX = distance * cos (theta + elevation)
    let deltaY = distance * sin (theta + elevation)
    let H = { x = A.x + deltaX; y = A.y + deltaY}
    let G = { x = B.x + deltaX; y = B.y + deltaY}
    G, H

  let area = areaTriangle D E F
  let theta = angle A B C
  let elevation = angle A { x = A.x + 1.0; y = A.y } B
  let width = distance A B
  let height = area / width
  let G, H = calculatePoints A B theta elevation height

  printfn "%.3f %.3f %.3f %.3f" G.x G.y H.x H.y