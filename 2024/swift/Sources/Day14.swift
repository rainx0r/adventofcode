import Algorithms
import Parsing

struct Day14: AdventDay {
    var data: String
    
    struct RobotParser: Parser {
        var body: some Parser<Substring, (Int, Int, Int, Int)> {
            "p="
            Int.parser()
            ","
            Int.parser()
            " v="
            Int.parser()
            ","
            Int.parser()
        }
    }
    
    struct InputParser: Parser {
        var body: some Parser<Substring, [(Int, Int, Int, Int)]> {
            Many {
                RobotParser()
            } separator: {
                "\n"
            } terminator: {
                End()
            }
        }
    }
    
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

    struct Robot {
        var pos: Coord
        let velocity: Coord
        
        init(_ pos: Coord, _ velocity: Coord) {
            self.pos = pos
            self.velocity = velocity
        }
        
        mutating func step(_ bounds: Coord) {
            var newX = self.pos.x + self.velocity.x
            if newX >= bounds.x {
                newX = newX - bounds.x
            } else if newX < 0 {
                newX = bounds.x + newX
            }
            var newY = self.pos.y + self.velocity.y
            if newY >= bounds.y {
                newY = newY - bounds.y
            } else if newY < 0 {
                newY = bounds.y + newY
            }
            self.pos = Coord(newX, newY)
        }
    }
    
    func getQuadrantRobots(_ robots: [Robot], _ bounds: Coord) -> (Int, Int, Int, Int) {
        var topLeftCount = 0
        var topRightCount = 0
        var bottomLeftCount = 0
        var bottomRightCount = 0
        
        for robot in robots {
            let left = robot.pos.x < bounds.x / 2
            let right = robot.pos.x > bounds.x / 2
            let top = robot.pos.y < bounds.y / 2
            let bottom = robot.pos.y > bounds.y / 2
            
            if top && left {
                topLeftCount += 1
            } else if top && right {
                topRightCount += 1
            } else if bottom && left {
                bottomLeftCount += 1
            } else if bottom && right {
                bottomRightCount += 1
            }
        }
        
        return (topLeftCount, topRightCount, bottomLeftCount, bottomRightCount)
    }
    
    func drawRobots(_ robots: [Robot], _ bounds: Coord) {
        var robotCounts: [Coord: Int] = [:]
        
        for robot in robots {
            robotCounts[robot.pos, default: 0] += 1
        }
        
        
        var drawStr = ""
        for y in 0..<bounds.y {
            for x in 0..<bounds.x {
                if let count = robotCounts[Coord(x, y)] {
                    drawStr.append(String(count))
                } else {
                    drawStr.append(".")
                }
            }
            drawStr.append("\n")
        }
        print(drawStr)
    }

    var entities: [Robot] {
        do {
            return try InputParser().parse(self.data.trimmingCharacters(in: .whitespacesAndNewlines)).map { Robot(Coord($0.0, $0.1), Coord($0.2, $0.3)) }
        } catch {
            fatalError("Unable to parse data: \(error)")
        }
    }
    
    func findLargestContigiousSegment(_ arr: [Coord]) -> Int {
        var currentLen = 1
        var maxLen = 0
        
        for i in 1..<arr.count {
            if arr[i].x != arr[i-1].x + 1 {
                maxLen = max(maxLen, currentLen)
                currentLen = 1
            } else {
                currentLen += 1
            }
        }
        
        return max(maxLen, currentLen)
    }
    
    func treeBaseDetected(_ robots: [Robot], _ bounds: Coord) -> Bool {
        var ret = false
        var robotCoordsPerRow: [Int: [Coord]] = [:]
        for robot in robots {
            robotCoordsPerRow[robot.pos.y, default: []].append(robot.pos)
        }
        for key in robotCoordsPerRow.keys {
            robotCoordsPerRow[key]!.sort(by: { $0.x < $1.x })
            let largestSegment = findLargestContigiousSegment(robotCoordsPerRow[key]!)
            // HACK: arbitrary value
            if largestSegment >= 14 {
                return true
            }
        }
        return false
    }

    func part1() -> Int {
        var robots = self.entities
        let bounds = Coord(101, 103)
        for _ in 0..<100 {
            for i in 0..<robots.count {
                robots[i].step(bounds)
            }
        }
//        drawRobots(robots, bounds)
        let counts = self.getQuadrantRobots(robots, bounds)
        return counts.0 * counts.1 * counts.2 * counts.3
    }

    func part2() -> Int {
        var robots = self.entities
        let bounds = Coord(101, 103)
        var timestep = 0
        while true {
            while !treeBaseDetected(robots, bounds) {
                for i in 0..<robots.count {
                    robots[i].step(bounds)
                }
                timestep += 1
            }
            print("Candidate tree detected at \(timestep)")
            drawRobots(robots, bounds)
            print("chat is this real")
            let response = readLine()!
            if response == "yes" {
                return timestep
            }
        }
    }
}
