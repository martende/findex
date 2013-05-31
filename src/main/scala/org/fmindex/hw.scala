package org.fmindex

object MmlAlnApp extends optional.Application {
  def main(dir: String) {
    IndexMaker.make(dir)
  }
}
