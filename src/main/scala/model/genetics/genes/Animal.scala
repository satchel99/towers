package towers.model.genetics.genes

class Animal(val genes: List[Gene]) {

  var fitness: Double = 0.0

  def random(): Animal = {
    new Animal(for (gene <- genes) yield {
      gene.random()
    })
  }

  def mate(other: Animal): Animal = {
    new Animal((for (i <- genes.indices) yield {
      genes.apply(i).mate(other.genes.apply(i))
    }).toList)
  }

  def mutate(): Animal = {
    new Animal(for (gene <- genes) yield {
      gene.mutate()
    })
  }

}
