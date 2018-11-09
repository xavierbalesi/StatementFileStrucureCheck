package processes.records

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import processes.records.ClosingRecordType._
import processes.records.UniqueRecordType._


class OpeningRecordTypeSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  import OpeningRecordType._

  val UniqueRecordType = Table(
    "recordTypes",
    AB, AD, TD, QD, ST, LS, RP
  )

  "HD.shouldContain" should "return true for record type BH" in { HD.shouldContain(BH) shouldBe true }
  "BH.shouldContain" should "return true for record type PH" in { BH.shouldContain(PH) shouldBe true }
  "PH.shouldContain" should "return true for record type GS" in { PH.shouldContain(GS) shouldBe true }
  "PH.shouldContain" should "return true for record type GC" in { PH.shouldContain(GC) shouldBe true }
  "PH.shouldContain" should "return true for record type TS" in { PH.shouldContain(TS) shouldBe true }
  "GS.shouldContain" should "return true for record type GC" in { GS.shouldContain(GC) shouldBe true }
  "GS.shouldContain" should "return true for record type TS" in { GS.shouldContain(TS) shouldBe true }
  "GC.shouldContain" should "return true for record type TS" in { GC.shouldContain(TS) shouldBe true }
  "TS.shouldContain" should "return true for all record type Unique" in forAll(UniqueRecordType) {
    recordType => TS.shouldContain(recordType) shouldBe true
  }
}