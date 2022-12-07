package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.{SimpleCommonPuzzle, SimpleMultiPuzzle}
import com.lmat.adventofcode.year2022.Day07Definitions.{Command, Filesystem}
import com.lmat.util.Files.readResource

import scala.util.matching.Regex


object Day07Definitions {

  trait Filelike {
    def size: Long

    def mkString: String
  }

  case class File(name: String, size: Long, level: Int) extends Filelike {
    override def mkString: String = "\n" + (" " * (level * 2)) + s"- $name (file, size=$size)"

    override def toString: String = mkString
  }

  case class Directory(name: String, parent: Directory = null, level: Int = 0, var files: Seq[Filelike] = Seq()) extends Filelike {
    def size: Long = files.map(_.size).sum

    override def mkString: String = "\n" + ((" " * (level * 2)) + s"- $name (dir, size=$size)") :++ files.map(_.mkString).mkString

    def listDir: Seq[Directory] = this +: files.collect { case f: Directory => f }.flatMap(_.listDir)

    override def toString: String = "\n" + ((" " * (level * 2)) + s"- $name (dir, size=$size)")
  }

  class Filesystem {
    val root: Directory = Directory("/")

    var current: Directory = root

    def mkString: String = root.mkString

    def dirs: Seq[Directory] = root.listDir

    def move(path: String): Unit = path match {
      case "/" => current = root
      case ".." => current = current.parent
      case _ => current = current.files.collect { case f: Directory => f }.find(_.name.equals(path)).orNull
    }

    def addFile(file: File): Unit = {
      current.files = current.files :+ file
    }

    def addDirectory(newDir: Directory): Unit = {
      current.files = current.files :+ newDir
    }

    def execCmd(cmd: Command): Unit = cmd match {
      case ChangeDirectory(arg) => move(arg)
      case MakeDir(arg) => addDirectory(Directory(arg, current, current.level + 1))
      case MakeFile(name, size) => addFile(File(name, size, current.level + 1))
      case _ => throw new IllegalStateException()
    }
  }

  trait Command

  case class ChangeDirectory(arg: String) extends Command

  case class MakeDir(arg: String) extends Command

  case class MakeFile(name: String, size: Long) extends Command

  object Command {
    def apply(line: String): Option[Command] = {
      val CD_PATTERN: Regex = """\$ cd (.*)""".r
      val LS_PATTERN: Regex = """\$ ls""".r
      val MKDIR_PATTERN: Regex = """dir (.*)""".r
      val MKFILE_PATTERN: Regex = """(.*) (.*)""".r
      line match {
        case CD_PATTERN(name) => Some(ChangeDirectory(name))
        case LS_PATTERN() => None
        case MKDIR_PATTERN(name) => Some(MakeDir(name))
        case MKFILE_PATTERN(size, name) => Some(MakeFile(name, size.toLong))
      }
    }
  }
}

object Day07 extends SimpleCommonPuzzle[Filesystem, Long, Long] {

  override def parse(resource: String): Filesystem = {
    val fs = new Filesystem()
    readResource(resource).flatMap(Command(_)).foreach(fs.execCmd)
    fs
  }

  override def part1(input: Filesystem): Long = {
    input.dirs.map(_.size).filter(_ <= 100000).sum
  }

  override def part2(input: Filesystem): Long = {
    val initialAvailableSpace = 70000000
    val minimumSpace = 30000000
    val unusedSpace = initialAvailableSpace - input.dirs.map(_.size).max
    val targetSpace = minimumSpace - unusedSpace

    input.dirs.map(_.size).filter(_ >= targetSpace).min
  }
}
