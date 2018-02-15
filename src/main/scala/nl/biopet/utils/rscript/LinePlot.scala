/*
 * Copyright (c) 2014 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package nl.biopet.utils.rscript

import java.io.File

/**
  * Extension for en general line plot with R
  *
  * Created by pjvan_thof on 4/29/15.
  */
case class LinePlot(input: File,
                    output: File,
                    width: Int = 1200,
                    height: Int = 1200,
                    xlabel: Option[String] = None,
                    ylabel: Option[String] = None,
                    llabel: Option[String] = None,
                    title: Option[String] = None,
                    hideLegend: Boolean = false,
                    removeZero: Boolean = false,
                    xLog10: Boolean = false,
                    yLog10: Boolean = false,
                    xLog10AxisTicks: Seq[String] = Seq(),
                    xLog10AxisLabels: Seq[String] = Seq())
    extends Rscript {
  protected def scriptPath: String = "plotXY.R"

  override def cmd: Seq[String] =
    super.cmd ++
      Seq("--input", input.getAbsolutePath) ++
      Seq("--output", output.getAbsolutePath) ++
      Seq("--width", width.toString) ++
      Seq("--height", height.toString) ++
      xlabel.map(Seq("--xlabel", _)).getOrElse(Seq()) ++
      ylabel.map(Seq("--ylabel", _)).getOrElse(Seq()) ++
      llabel.map(Seq("--llabel", _)).getOrElse(Seq()) ++
      title.map(Seq("--title", _)).getOrElse(Seq()) ++
      (if (hideLegend) Seq("--hideLegend", "true") else Seq()) ++
      (if (removeZero) Seq("--removeZero", "true") else Seq()) ++
      (if (xLog10) Seq("--xLog10", "true") else Seq()) ++
      (if (yLog10) Seq("--yLog10", "true") else Seq()) ++
      (if (xLog10AxisTicks.nonEmpty) xLog10AxisTicks.+:("--xLog10Breaks")
       else Seq()) ++
      (if (xLog10AxisLabels.nonEmpty) xLog10AxisLabels.+:("--xLog10Labels")
       else Seq())
}
