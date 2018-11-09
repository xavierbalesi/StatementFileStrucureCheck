package processes.records

sealed trait RecordType

trait OpeningRecordType extends RecordType {
  def shouldContain(recordType: RecordType): Boolean
  val closingRecordType: ClosingRecordType
  val isMandatory: Boolean
}

object OpeningRecordType {
  case object HD extends OpeningRecordType {
    override def shouldContain(recordType: RecordType): Boolean =
      (recordType == BH)
    override val closingRecordType = ClosingRecordType.TR
    override val isMandatory = true
  }
  case object BH extends OpeningRecordType {
    override def shouldContain(recordType: RecordType): Boolean =
      (recordType == PH)
    override val closingRecordType = ClosingRecordType.BT
    override val isMandatory = true
  }
  case object PH extends OpeningRecordType {
    override def shouldContain(recordType: RecordType): Boolean =
      (recordType == GS ||
       recordType == GC ||
       recordType == TS)
    override val closingRecordType = ClosingRecordType.PT
    override val isMandatory = true
  }
  case object GS extends OpeningRecordType {
    override def shouldContain(recordType: RecordType): Boolean =
      (recordType == GC ||
       recordType == TS)
    override val closingRecordType = ClosingRecordType.GT
    override val isMandatory = false
  }
  case object GC extends OpeningRecordType {
    override def shouldContain(recordType: RecordType): Boolean =
      (recordType == TS)
    override val closingRecordType = ClosingRecordType.CT
    override val isMandatory = false
  }
  case object TS extends OpeningRecordType {
    import processes.records.UniqueRecordType._
    override def shouldContain(recordType: RecordType): Boolean =
      (recordType == AB ||
       recordType == AD ||
       recordType == TD ||
       recordType == QD ||
       recordType == ST ||
       recordType == LS ||
       recordType == RP)
    override val closingRecordType = ClosingRecordType.TT
    override val isMandatory = true
  }
}


trait ClosingRecordType extends RecordType
object ClosingRecordType {
  case object TT extends ClosingRecordType
  case object CT extends ClosingRecordType
  case object GT extends ClosingRecordType
  case object PT extends ClosingRecordType
  case object BT extends ClosingRecordType
  case object TR extends ClosingRecordType
}

trait UniqueRecordType extends RecordType { val isMandatory: Boolean }
object UniqueRecordType {
  case object AB extends UniqueRecordType { override val isMandatory = true }
  case object AD extends UniqueRecordType { override val isMandatory = true }
  case object TD extends UniqueRecordType { override val isMandatory = false }
  case object QD extends UniqueRecordType { override val isMandatory = false }
  case object ST extends UniqueRecordType { override val isMandatory = false }
  case object LS extends UniqueRecordType { override val isMandatory = false }
  case object RP extends UniqueRecordType { override val isMandatory = true }
}