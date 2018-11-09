package processes

import processes.records._

object StructureChecker {
  def verify(listOfRecordTypes: List[RecordType]): Either[String, Unit] = {
    def loop(records: List[RecordType], stack: List[OpeningRecordType]): Either[String, Unit] =
      records match {
        case Nil => Right(())
        case (opening: OpeningRecordType) :: restOfRecordType => {
          stack match {
            case Nil =>
              loop(restOfRecordType, opening :: stack)
            case previousOpeningRecordType :: _ =>
              if (previousOpeningRecordType.shouldContain(opening))
                loop(restOfRecordType, opening :: stack)
              else
                Left(s"opening ${previousOpeningRecordType} should not contain ${opening}")
          }
        }
        case (closing: ClosingRecordType) :: restOfRecordType => {
          stack match {
            case Nil =>
              Left(s"Closing Record Type ${closing} has no Opening Record Type.")
            case previousOpeningRecordType :: restOfStack =>
              if(previousOpeningRecordType.closingRecordType == closing)
                loop(restOfRecordType, restOfStack)
              else
                Left(s"expected ${previousOpeningRecordType.closingRecordType} but encoutered ${closing}.")
          }
        }
        case (unique: UniqueRecordType) :: restOfRecordType => {
          stack match {
            case Nil =>
              Left(s"Record Type ${unique} must be enclosed.")
            case previousOpeningRecordType :: _ =>
              if(previousOpeningRecordType.shouldContain(unique))
                loop(restOfRecordType, stack)
              else
                Left(s"Opening Record Type ${previousOpeningRecordType} couldn't contain ${unique}.")
          }
        }
      }
    loop(listOfRecordTypes, Nil)
  }
}