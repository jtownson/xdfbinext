package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.XdfTable

object TableAndCategoryFilter {

  def filterTables(
      tables: Seq[XdfTable],
      tableExclusions: Set[String],
      categoryExclusions: Set[String]
  ): Seq[XdfTable] = {

    tables
      .filterNot(t => categoryIn(categoryExclusions)(t))
      .filterNot(t => tableExclusions.exists(filter => t.title.contains(filter)))
  }

  private def categoryIn(categories: Set[String])(xdfTable: XdfTable): Boolean = {
    val categoryNames = xdfTable.categoryMems.map(_.category.name).toSet
    categories.intersect(categoryNames).nonEmpty
  }
}
