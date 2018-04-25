package nl.biopet.utils

case class Build(build: String) extends Ordered[Build] {
  override def toString: String = this.build

  def compare(that: Build): Int = {
    // Empty builds should always be greatest.
    // Example 0.8.0-alpha < 0.8.0 and 1.0.2-SNAPSHOT < 1.0.2
    if (this.build.isEmpty && !that.build.isEmpty) Int.MaxValue
    else if (!this.build.isEmpty && that.build.isEmpty) Int.MinValue
    else this.build compare that.build
  }
}

object Build {
  def fromString(string: String): Build = new Build(string)
}
