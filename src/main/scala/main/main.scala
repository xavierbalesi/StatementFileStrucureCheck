package main

import interfaces.input.ReadFile




object StatementFileStructureChecker {
  def main(args: Array[String]) = {
    if(args.length != 1) {
      println("Usage : StatementFileStructureChecker STATMENTFILE")
      System.exit(1)
    }

    ReadFile.getListRecordType(args(0)) match {
      case Right(rList) =>
        println(s"liste : ${rList.toString()}")
      case Left(s) =>
        println(s"Impossible de charger la liste : ${s}")
    }

  }
}