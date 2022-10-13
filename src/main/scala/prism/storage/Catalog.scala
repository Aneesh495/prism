package prism.storage

import scala.collection.mutable

final case class ColumnDef(name: String, typeName: String)

final case class RelationSchema(
  name: String,
  columns: List[ColumnDef],
  isEdb: Boolean = true
) {
  def arity: Int = columns.length
}

/**
 * System catalog managing registered relation schemas and domain constraints.
 */
final class Catalog {
  private val relations = mutable.Map[String, RelationSchema]()

  def registerRelation(name: String, columnDefs: List[ColumnDef], isEdb: Boolean = true): RelationSchema = {
    val schema = RelationSchema(name, columnDefs, isEdb)
    relations.put(name, schema)
    schema
  }

  def getSchema(name: String): Option[RelationSchema] = relations.get(name)

  def allRelations: Seq[RelationSchema] = relations.values.toSeq

  def hasRelation(name: String): Boolean = relations.contains(name)

  def clear(): Unit = relations.clear()
}
