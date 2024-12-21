import Algorithms

struct Day15: AdventDay {
    var data: String
    
    struct Coord: Hashable {
        let x: Int
        let y: Int

        init(_ x: Int, _ y: Int) {
            self.x = x
            self.y = y
        }
        
        static func + (lhs: Coord, rhs: Coord) -> Coord {
            return Coord(lhs.x + rhs.x, lhs.y + rhs.y)
        }
        
        static func - (lhs: Coord, rhs: Coord) -> Coord {
            return Coord(lhs.x - rhs.x, lhs.y - rhs.y)
        }
    }
    
    struct Warehouse {
        var state: [Coord: Character]
        let bounds: Coord
        var robotPos: Coord
        let directions: [Character: Coord] = [
            "^": Coord(0, -1),
            "v": Coord(0, 1),
            "<": Coord(-1, 0),
            ">": Coord(1, 0),
        ]
        
        init(_ initialState: [Coord: Character], _ initialRobotPos: Coord, _ bounds: Coord) {
            self.state = initialState
            self.robotPos = initialRobotPos
            self.bounds = bounds
        }
        
        func inBounds(_ coord: Coord) -> Bool {
            return coord.x > 0 && coord.x <= bounds.x && coord.y > 0 && coord.y <= bounds.y
        }
        
        mutating func step(_ direction: Character) {
            if canMove(robotPos, direction) {
                let nextPos = directions[direction]! + robotPos
                state[nextPos] = "@"
                state[robotPos] = "."
                robotPos = nextPos
            }
        }
        
        mutating func canMove(_ coord: Coord, _ direction: Character) -> Bool {
            let vec = directions[direction]!
            let nextCoord = coord + vec
            if state[nextCoord] == "#" {
                return false
            }
            var ret = false
            if state[nextCoord] == "." {
                ret = true
            }
            if state[nextCoord] == "O" {
                ret = canMove(nextCoord, direction)
            }
            if state[nextCoord] == "[" || state[nextCoord] == "]" {
                return canMoveWideBox(nextCoord, direction)
            }
            if ret {
                if state[coord] == "O" {
                    state[nextCoord] = state[coord]
                    state[coord] = "."
                }
            }
            return ret
        }
        
        mutating func canMoveWideBox(_ coord: Coord, _ direction: Character) -> Bool {
            // Handle lateral
            var canMove = false
            let complementOffset = state[coord] == "[" ? directions[">"]! : directions["<"]!
            // Lateral movement
            if direction == ">" || direction == "<" {
                let posToCheck = if direction == "<" {
                    (state[coord] == "[" ? coord : coord + complementOffset) + directions["<"]!
                } else {
                    (state[coord] == "]" ? coord : coord + complementOffset) + directions[">"]!
                }
                canMove = state[posToCheck] != "#" && (state[posToCheck] == "." || canMoveWideBox(posToCheck, direction))
            } else {
                // Vertical Movement
                let nextCoord1 = coord + directions[direction]!, nextCoord2 = coord + complementOffset + directions[direction]!
                if state[nextCoord1] == "." && state[nextCoord2] == "." {
                    canMove = true
                } else if state[nextCoord1] == "#" || state[nextCoord2] == "#" {
                    canMove = false
                } else {
                    if state[nextCoord1] == state[coord] {
                        canMove = canMoveWideBox(nextCoord1, direction)
                    } else {
                        let clause1 = switch state[nextCoord1] {
                        case "#":
                            false
                        case ".":
                            true
                        default:
                            canMoveWideBox(nextCoord1, direction)
                        }
                        let clause2 = switch state[nextCoord2] {
                        case "#":
                            false
                        case ".":
                            true
                        default:
                            canMoveWideBox(nextCoord2, direction)
                        }
                        canMove = clause1 && clause2
                    }
                }
            }
            if canMove {
                let nextOffset = directions[direction]!
                let current = state[coord]
                let complement = state[coord + complementOffset]
                state[coord + nextOffset] = current
                state[coord + complementOffset + nextOffset] = complement
                state[coord] = "."
                if direction == "^" || direction == "v" {
                    state[coord + complementOffset] = "."
                }
            }
            return canMove
        }
        
        func getBoxGPSCoords() -> [Int] {
            var ret: [Int] = []
            for x in 0..<bounds.x {
                for y in 0..<bounds.y {
                    switch state[Coord(x,y)] {
                    case "O", "[":
                        ret.append(100 * y + x)
                    default:
                        break
                    }
                }
            }
            return ret
        }
        
        func draw() {
            var drawStr = ""
            for y in 0..<bounds.y {
                for x in 0..<bounds.x {
                    drawStr.append(state[Coord(x, y)]!)
                }
                drawStr.append("\n")
            }
            print(drawStr)
        }
    }

    var entities: (Warehouse, Warehouse, String) {
        let inputParts = data.split(separator: "\n\n")
        let map = inputParts[0].split(separator: "\n")
        let bounds = Coord(map[0].count, map.count)
        let part2Bounds = Coord(2 * map[0].count, map.count)
        let directions = inputParts[1]
        var state: [Coord: Character] = [:]
        var part2State: [Coord: Character] = [:]
        var part2RobotPos: Coord? = nil
        state.reserveCapacity(bounds.x * bounds.y)
        part2State.reserveCapacity(bounds.x * bounds.y * 2)
        var robotPos: Coord? = nil
        
        for (y, line) in map.enumerated() {
            for (x, char) in line.enumerated() {
                let pos = Coord(x, y)
                let pos2 = Coord(2 * x, y)
                state[pos] = char
                part2State[pos2] = switch char {
                case "O":
                    "["
                default:
                    char
                }
                part2State[pos2 + Coord(1, 0)] = switch char {
                case "#":
                    "#"
                case "O":
                    "]"
                case ".", "@":
                    "."
                default:
                    "."
                }
                if char == "@" {
                    robotPos = pos
                    part2RobotPos = pos2
                }
            }
        }
        
        let directionsList = directions.trimmingCharacters(in: .whitespacesAndNewlines).replacingOccurrences(of: "\n", with: "")
        return (Warehouse(state, robotPos!, bounds), Warehouse(part2State, part2RobotPos!, part2Bounds), directionsList)
    }

    func part1() -> Int {
        var (warehouse, _, directions) = entities
        for direction in directions {
            warehouse.step(direction)
        }
        let ret: Int = warehouse.getBoxGPSCoords().reduce(0, +)
        warehouse.draw()
        return ret
    }

    func part2() -> Int {
        var (_, warehouse, directions) = entities
        for direction in directions {
            warehouse.step(direction)
        }
        warehouse.draw()
        return warehouse.getBoxGPSCoords().reduce(0, +)
    }
}
