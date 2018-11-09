package processes

import processes.records._

object StructureChecker {
  def verify(listOfRecordTypes: List[RecordType]): Either[String, Unit] = {
    def loop(records: List[RecordType], stack: List[OpeningRecordType], nbTab: Int = 0): Either[String, Unit] =
      records match {
        case Nil => Right(())
        case (opening: OpeningRecordType) :: restOfRecordType => {
          var i: Int = 0;
          while(i <= nbTab) { print("| "); i+=1 }
          println(s"${opening.toString}")
          stack match {
            case Nil =>
              loop(restOfRecordType, opening :: stack, nbTab+1)
            case previousOpeningRecordType :: _ =>
              if (previousOpeningRecordType.shouldContain(opening))
                loop(restOfRecordType, opening :: stack, nbTab+1)
              else
                Left(s"opening ${previousOpeningRecordType} should not contain ${opening}")
          }
        }
        case (closing: ClosingRecordType) :: restOfRecordType => {
          var i: Int = 0;
          while(i < nbTab) {print("| "); i+=1 }
          println(s"${closing.toString}")
          stack match {
            case Nil =>
              Left(s"Closing Record Type ${closing} has no Opening Record Type.")
            case previousOpeningRecordType :: restOfStack =>
              if(previousOpeningRecordType.closingRecordType == closing)
                loop(restOfRecordType, restOfStack, nbTab-1)
              else
                Left(s"expected ${previousOpeningRecordType.closingRecordType} but encoutered ${closing}.")
          }
        }
        case (unique: UniqueRecordType) :: restOfRecordType => {
          var i: Int = 0;
          while(i <= nbTab) {print("| "); i+=1 }
          println(s"${unique.toString}")
          stack match {
            case Nil =>
              Left(s"Record Type ${unique} must be enclosed.")
            case previousOpeningRecordType :: _ =>
              if(previousOpeningRecordType.shouldContain(unique))
                loop(restOfRecordType, stack, nbTab)
              else
                Left(s"Opening Record Type ${previousOpeningRecordType} couldn't contain ${unique}.")
          }
        }
      }
    loop(listOfRecordTypes, Nil)
  }
}