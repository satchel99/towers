package towers.model.genetics

import towers.model.genetics.genes.{Animal, Gene}

object GeneticAlgorithm {


  def geneticAlgorithm[T](fitnessFunction: T => Double, incubator: List[Gene] => T, sampleGenes: List[Gene]): T = {

    val n = 20

    // generate n random animals based on the sample
    var animals: List[Animal] = (for (i <- 0 until n) yield {
      new Animal(for (g <- sampleGenes) yield {
        g.random()
      })
    }).toList

    val generations = 1000

    var best = animals.head

    for (i <- 0 until generations) {

      // find fitness of each
      for (animal <- animals) {
        animal.fitness = fitnessFunction(incubator(animal.genes))
      }

      animals = animals.sortBy((a: Animal) => -a.fitness)

      var nextGeneration: List[Animal] = List()

      // 1 most fit advances
      nextGeneration = animals.head :: nextGeneration
      best = animals.head

      // 2 mutations of most fit
      nextGeneration = animals.head.mutate() :: nextGeneration
      nextGeneration = animals.head.mutate() :: nextGeneration

      // 1 mutation of second most fit
      nextGeneration = animals.apply(1) :: nextGeneration

      // 6 children of top 4 (all combinations)
      nextGeneration = animals.head.mate(animals.apply(1)) :: nextGeneration
      nextGeneration = animals.head.mate(animals.apply(2)) :: nextGeneration
      nextGeneration = animals.head.mate(animals.apply(3)) :: nextGeneration

      nextGeneration = animals.apply(1).mate(animals.apply(2)) :: nextGeneration
      nextGeneration = animals.apply(1).mate(animals.apply(3)) :: nextGeneration

      nextGeneration = animals.apply(2).mate(animals.apply(3)) :: nextGeneration

      // 10 purely random
      for (i <- 0 until 10) {
        nextGeneration = animals.head.random() :: nextGeneration
      }

      animals = nextGeneration
    }

    incubator(best.genes)
  }


}
