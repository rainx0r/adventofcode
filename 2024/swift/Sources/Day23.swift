import Parsing

struct Day23: AdventDay {
    var data: String
    
    struct Edge {
        let source: Node
        let target: Node
    }
    
    struct Node: Hashable {
        let name: String
        
        init(_ name: String.SubSequence) {
            self.name = String(name)
        }
    }
    
    struct NodePair: Hashable {
        let n1: Node
        let n2: Node
        
        init(_ n1: Node, _ n2: Node) { self.n1 = n1; self.n2 = n2 }
    }
    
    struct Graph {
        let nodes: Set<Node>
        let edges: [Node: Set<Node>]
        let rawEdges: [Edge]
        
        init(_ edges: [Edge], directed: Bool = false) {
            self.rawEdges = edges
            var nodes = Set<Node>()
            var newEdges: [Node: Set<Node>] = [:]
            for edge in edges {
                if !nodes.contains(edge.source) { nodes.insert(edge.source) }
                if !nodes.contains(edge.target) { nodes.insert(edge.target) }
                newEdges[edge.source, default: Set()].insert(edge.target)
                if !directed {
                    newEdges[edge.target, default: Set()].insert(edge.source)
                }
            }
            self.nodes = nodes
            self.edges = newEdges
        }
    }
    
    struct NodeTriangle: Hashable {
        let n1: Node
        let n2: Node
        let n3: Node
        
        init(_ n1: Node, _ n2: Node, _ n3: Node) { self.n1 = n1; self.n2 = n2; self.n3 = n3 }
        
        static func == (lhs: NodeTriangle, rhs: NodeTriangle) -> Bool {
            return lhs.hashValue == rhs.hashValue
        }
        
        func hash(into hasher: inout Hasher) {
            hasher.combine(n1.name.hashValue ^ n2.name.hashValue ^ n3.name.hashValue)
        }
    }

    var entities: Graph {
        do {
            let edges = try Many {
                Parse(Edge.init(source:target:)) {
                    PrefixUpTo("-").map(Node.init); "-"; Prefix { $0 != "\n" }.map(Node.init)
                }
            } separator: { "\n" } terminator: { End() }.parse(data.trimmingCharacters(in: .whitespacesAndNewlines))
            return Graph(edges)
        } catch {
            fatalError("Failed to parse input \(error)")
        }
    }

    func part1() -> Any {
        let graph = entities
        var triangles: Set<NodeTriangle> = []
        for edge in graph.rawEdges { // O(|V| * |E|)
            for node in graph.nodes.filter({ $0 != edge.source && $0 != edge.target }) {
                if graph.edges[edge.source]!.contains(node) && graph.edges[edge.target]!.contains(node) {
                    triangles.insert(NodeTriangle(edge.source, edge.target, node))
                }
            }
        }
        triangles = triangles.filter { t in t.n1.name.hasPrefix("t") || t.n2.name.hasPrefix("t") || t.n3.name.hasPrefix("t") }
//        print(triangles.map { "\($0.n1.name)-\($0.n2.name)-\($0.n3.name)" })
        return triangles.count
    }
    
    func bronKerbosch(R: Set<Node> = Set(), P: Set<Node>, X: Set<Node> = Set(), graph: Graph) -> [Set<Node>] {
        if P.isEmpty && X.isEmpty {
            return [R]
        }
        var maximalCliques: [Set<Node>] = [], P = P, X = X
        for node in P {
            maximalCliques.append(contentsOf: bronKerbosch(
                R: R.union([node]), P: P.intersection(graph.edges[node]!), X: X.intersection(graph.edges[node]!),
                graph: graph
            ))
            P = P.subtracting([node])
            X = X.union([node])
        }
        return maximalCliques
    }
    
    func getPassword(_ nodes: Set<Node>) -> String {
        return nodes.map { $0.name }.sorted().joined(separator: ",")
    }

    func part2() -> Any {
        let cliques = bronKerbosch(P: entities.nodes, graph: entities)
        return getPassword(cliques.max { $0.count < $1.count }!)
    }
}
