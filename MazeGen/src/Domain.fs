module Domain

type Point = Point of x: int * y:int
    with member this.isValid() =
        let (Point(x,y)) = this
        x % 2 <> 1 || y % 2 <> 0
type Connection = Connection of x: int * y: int
    with member this.isValid() =
        let (Connection(x,y)) = this
        x % 2 <> y % 2
type MazeElement = Open | Closed
type Maze = {
    size: int * int
    grid: MazeElement[][]
    }

type Direction = Up | Down | Left | Right

let moveTo direction (Point(x, y)) =
    match direction with
        | Up -> x, y+2
        | Down -> x, y-2
        | Left -> x-2, y
        | Right -> x+2, y
    |> Point

let connectionTo direction (Point(x, y)) =
    match direction with
        | Up -> x, y+1
        | Down -> x, y-1
        | Left -> x-1, y
        | Right -> x+1, y
    |> Connection

let newMaze (width, height, initialConnection) =
    let grid = Array.init (width*2+1) (fun _ -> Array.create (height*2+1) Closed)
    // tunnel out all of the "rooms"
    for x in [1..2..width*2] do
        for y in [1..2..height*2] do
            grid[x][y] <- Open
    if initialConnection then
        // tunnel out all of the left/right corridors (but not the outside walls)
        for x in [2..2..width*2-2] do
            for y in [1..2..height*2] do
                grid[x][y] <- Open
        // tunnel out all of the up/down corridors (but not the outside walls)
        for x in [1..2..width*2] do
            for y in [2..2..height*2-2] do
                grid[x][y] <- Open
    { size = (width, height); grid = grid }

