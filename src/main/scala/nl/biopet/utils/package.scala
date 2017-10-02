package nl.biopet

package object utils {

  /** Package version */
  // needs the Option here since the value is `null` when we run from an unpackaged JAR
  val Version: String =
    Option(getClass.getPackage.getImplementationVersion)
      .getOrElse("unpackaged")

}
