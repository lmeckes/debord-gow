package com.lms.gow.model

import scala.collection.Seq

object Coordinates {

  def indexFromCoordinates(x: Int, y: Int) = (x + (y * Rules.terrainWidth))

}

object IO {

  def loadTilesFromFile(file: String, tileRepository: Seq[Tile]): Seq[Tile] = {
    io.Source.fromFile(file)
      .mkString
      .filter(_ > ' ')
      .map((tileRepository.map(_.char) zip tileRepository)
        .toMap
        .get(_)
        .getOrElse(VoidTile))
  }

}
