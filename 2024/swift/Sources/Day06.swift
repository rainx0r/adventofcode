import Foundation
import Algorithms
import Parsing

struct Day06: AdventDay {
    // Save your data in a corresponding text file in the `Data` directory.
    var data: String

    struct Coord: Hashable {
        let x: Int
        let y: Int

        init(_ x: Int, _ y: Int) {
            self.x = x
            self.y = y
        }
    }

    struct GuardGallivant {
        var map: [Coord: Character]
        var renderingDirections: [Coord: Character]
        var actualDirections: [Coord: Character]
        var positionOfGuard: Coord
        let bounds: Coord
        var visited: Set<Coord>
        var finished: Bool
        var looping: Bool

        init(_ map: [Coord: Character], _ positionOfGuard: Coord, _ bounds: Coord) {
            self.map = map
            self.renderingDirections = [:]
            self.renderingDirections.reserveCapacity(map.count)
            self.actualDirections = [:]
            self.actualDirections.reserveCapacity(map.count)
            self.bounds = bounds
            self.positionOfGuard = positionOfGuard
            self.visited = Set<Coord>()
            self.visited.reserveCapacity(map.count)
            self.visited.insert(positionOfGuard)
            self.finished = false
            self.looping = false
        }

        mutating func step() {
            let (x, y) = (self.positionOfGuard.x, self.positionOfGuard.y)
            var characterDirection = self.map[self.positionOfGuard]!

            let nextPosition: Coord = switch characterDirection {
            case "^":
                Coord(x, y - 1)
            case ">":
                Coord(x + 1, y)
            case "v":
                Coord(x, y + 1)
            case "<":
                Coord(x - 1, y)
            default:
                fatalError("Unexpected character position value: \(characterDirection)")
            }

            if nextPosition.x >= self.bounds.x || nextPosition.y >= self.bounds.y || nextPosition.x < 0 || nextPosition.y < 0 {
                // If OOB, exit
                self.visited.insert(self.positionOfGuard)
                self.finished = true
            } else {
                // Else, check collision with wall
                if self.map[nextPosition]! == "#" {
                    // On wall collision, rotate 90deg clockwise
                    characterDirection = switch characterDirection {
                    case "^":
                        ">"
                    case ">":
                        "v"
                    case "v":
                        "<"
                    case "<":
                        "^"
                    default:
                        fatalError("Unexpected character position value: \(characterDirection)")
                    }
                    self.map.updateValue(characterDirection, forKey: self.positionOfGuard)
                    // NOTE: Seems to be how the notation is intended
                    self.renderingDirections[self.positionOfGuard] = "+"
                } else {
                    // Step forward in currently facing direction
                    if !self.renderingDirections.keys.contains(self.positionOfGuard) {
                        // We haven't gone past the current tile before, store facing direction
                        self.renderingDirections[self.positionOfGuard] = switch characterDirection {
                        case "^":
                            "|"
                        case "v":
                            "|"
                        case ">":
                            "-"
                        case "<":
                            "-"
                        default:
                            fatalError("Unexpected character position value: \(characterDirection)")
                        }
                    } else {
                        // We have gone past the previous tile before
                        let pastDirection = self.renderingDirections[self.positionOfGuard]!
                        if !(pastDirection == "-" && (characterDirection == ">" || characterDirection == "<"))
                            && !(pastDirection == "|" && (characterDirection == "^" || characterDirection == "v")) {
                            self.renderingDirections[self.positionOfGuard] = "+"
                        }
                    }
                
                    if !self.actualDirections.keys.contains(self.positionOfGuard) {
                        self.actualDirections[self.positionOfGuard] = characterDirection
                    } else {
                        self.looping = self.actualDirections[self.positionOfGuard] == characterDirection
                    }

                    self.visited.insert(self.positionOfGuard)
                    self.map.updateValue(characterDirection, forKey: nextPosition)
                    self.map.updateValue(".", forKey: self.positionOfGuard)
                    self.positionOfGuard = nextPosition
                }
            }
        }
    }

    // Splits input data into its component parts and convert from string.
    var entities: GuardGallivant {
        let lines = self.data.split(separator: "\n")
        let firstLine = lines.first!
        var map: [Coord: Character] = [:]
        map.reserveCapacity(lines.count * firstLine.count)
        var positionOfCharacter: Coord? = nil

        for (j, line) in self.data.split(separator: "\n").enumerated() {
            for (i, char) in line.enumerated() {
                map[Coord(i, j)] = char
                if char == "^" {
                    positionOfCharacter = Coord(i, j)
                }
            }
        }

        return GuardGallivant(map, positionOfCharacter!, Coord(lines.count, firstLine.count))
    }

    // Replace this with your solution for the first part of the day's challenge.
    func part1() async throws -> Int {
        var gg = self.entities

        while !gg.finished {
            gg.step()
        }

        return gg.visited.count
    }
    
    func printMap(_ gg: GuardGallivant) {
        var retStr = ""
        
        for j in 0...(gg.bounds.y - 1) {
            for i in 0...(gg.bounds.x - 1) {
                var mapValue = gg.map[Coord(i, j)]!
                if mapValue == "." {
                    mapValue = gg.renderingDirections[Coord(i, j)] ?? "."
                }
                retStr.append(mapValue)
            }
            retStr.append("\n")
        }

        print(retStr)
    }
    
    func simulateObstacle(_ gg: GuardGallivant, _ obstaclePos: Coord) -> Bool {
        var gg2 = gg
        gg2.map[obstaclePos] = "#"
        while !gg2.finished && !gg2.looping {
            gg2.step()
        }
        return gg2.looping
    }

    // Replace this with your solution for the second part of the day's challenge.
    func part2() async throws -> Int {
        let start = Date()
        let gg = self.entities

        let possibleObstacleCount = await withTaskGroup(of: Bool.self) { group in
            for i in 0...(gg.bounds.x - 1) {
                for j in 0...(gg.bounds.y - 1) {
                    let candidatePosition = Coord(i, j)
                    let candidatePositionValue = gg.map[candidatePosition]
                    if candidatePositionValue != "#" && candidatePositionValue != "^" {
                        group.addTask {
                            return simulateObstacle(gg, candidatePosition)
                        }
                    }
                }
            }
            
            var sum = 0
            
            for await result in group {
                sum += result ? 1 : 0
            }
            return sum
        }

        let end = Date()
        
        print("Time elapsed: \(end.timeIntervalSince(start))s.")

        return possibleObstacleCount
    }
}
