package interfaces.input

import processes.records.RecordType
import processes.records.OpeningRecordType._
import processes.records.UniqueRecordType._
import processes.records.ClosingRecordType._

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}


object ReadFile {
  def open(fileName: String): Either[String, Iterator[String]] = {
    Try(Source.fromFile(fileName)(io.Codec.ISO8859)) match {
      case Success(b) =>
        Right(b.getLines())
      case Failure(throwable) =>
        Left(s"Impossible d'ouvrir le fichier ${fileName} : ${throwable.getMessage}")
    }
  }
  def readNextLine(bufferedSource: BufferedSource): Either[String, Iterator[String]] = {
    Try(bufferedSource.getLines) match {
      case Success(l) =>
        Right(l)
      case Failure(throwable) =>
        Left(s"Impossible de lire la ligne : ${throwable.getMessage}")
    }
  }
  def getNextRecordType(iterator: Iterator[String], rList: List[RecordType]): Either[String, List[RecordType]] = {
    if(iterator.isEmpty){
      Right(rList)
    } else {
      val s: String = iterator.next()
      getRecordType(s) match {
        case Right(r) => getNextRecordType(iterator, r :: rList)
        case Left(s) => Left(s)
      }
    }
  }
  def getRecordType(line: String): Either[String, RecordType] = {
    // println(s"line : ${line}")
    if(line.length < 2)
      Left("La ligne contient moins de 2 caractères.")
    else
      line.substring(0, 2) match {
        case "HD" => Right(HD)
        case "BH" => Right(BH)
        case "PH" => Right(PH)
        case "GS" => Right(GS)
        case "GC" => Right(GC)
        case "TS" => Right(TS)

        case "AB" => Right(AB)
        case "AD" => Right(AD)
        case "TD" => Right(TD)
        case "QD" => Right(QD)
        case "ST" => Right(ST)
        case "LS" => Right(LS)
        case "RP" => Right(RP)

        case "TT" => Right(TT)
        case "CT" => Right(CT)
        case "GT" => Right(GT)
        case "PT" => Right(PT)
        case "BT" => Right(BT)
        case "TR" => Right(TR)

        case _ => Left(s"Impossible de matcher le RecordType : ${line.substring(0, 2)}.")
      }
  }
  def getListRecordType(fileName: String) : Either[String, List[RecordType]] = {
    open(fileName) match {
      case Right(b) =>
        getNextRecordType(b, Nil) match {
          case Right(rList) => Right(rList)
          case Left(s) => Left(s)
        }
      case Left(s) =>
        println(s"Impossible d'ouvrir le fichier ${fileName}.")
        Left(s)
    }
  }
}