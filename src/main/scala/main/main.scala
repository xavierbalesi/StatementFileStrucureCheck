package main

import interfaces.input.ReadFile
import processes.StructureChecker




object StatementFileStructureChecker {
  def main(args: Array[String]) = {
    if(args.length != 1) {
      println("Usage : StatementFileStructureChecker STATMENTFILE")
      System.exit(1)
    }

    ReadFile.getListRecordType(args(0)) match {
      case Right(rList) =>
        println(s"liste : ${rList.toString()}")
        StructureChecker.verify(rList.reverse) match {
          case Right(_) =>
            println(s"Le fichier ${args(0)} a une structure correcte.")
          case Left(s) =>
            println(s"Le fichier ${args(0)} n'as pas une structure correcte :")
            println(s)
        }
      case Left(s) =>
        println(s"Impossible de charger la liste : ${s}")
    }


  }
}