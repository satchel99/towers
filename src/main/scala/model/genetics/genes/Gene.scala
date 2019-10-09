package towers.model.genetics.genes

class Gene(val geneValue: Double) {

  def random(): Gene = {
    new Gene(Math.random())
  }

  def mate(gene: Gene): Gene = {
    new Gene((this.geneValue + gene.geneValue) / 2.0)
  }

  def mutate(): Gene = {
    val mutationPercent = 0.01
    new Gene(1.0.min(0.0.max(this.geneValue + (Math.random() - 0.5) * mutationPercent)))
  }

}
