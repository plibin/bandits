package vub.bandits.postprocessing

import java.awt.geom.Rectangle2D
import java.io.{File, FileOutputStream}

import com.lowagie.text.pdf.{DefaultFontMapper, PdfWriter}
import com.lowagie.text.{Document, Rectangle}
import org.jfree.chart.{ChartFrame, JFreeChart}
import org.jfree.graphics2d.svg.{SVGGraphics2D, SVGUtils}

object Plot {
  def show(title: String, chart: JFreeChart) = {
    val frame = new ChartFrame(title, chart)
    frame.pack()
    frame.setVisible(true)
  }

  def exportToSVG(svg: File, chart: JFreeChart) = {
    val g2 = new SVGGraphics2D(500, 300);
    val r = new java.awt.Rectangle(0, 0, 500, 300);
    chart.draw(g2, r);
    SVGUtils.writeToSVG(svg, g2.getSVGElement());
  }

  def exportToPDF(chart: JFreeChart, pdf: File, width: Float, height: Float)
  {
    try
    {
      val pagesize = new Rectangle( width, height );
      val document = new Document( pagesize, 50, 50, 50, 50 );
      val writer = PdfWriter.getInstance( document, new FileOutputStream(pdf) );
      document.open();
      val cb = writer.getDirectContent();
      val tp = cb.createTemplate( width, height);
      val g2 = tp.createGraphics( width, height, new DefaultFontMapper() );
      val r2D = new Rectangle2D.Double(0.0, 0.0, width, height);
      chart.draw(g2, r2D);
      g2.dispose();
      cb.addTemplate(tp, 0, 0);
      document.close();
    }
    catch {
      case e:Exception => print(e)
    }
  }

}
