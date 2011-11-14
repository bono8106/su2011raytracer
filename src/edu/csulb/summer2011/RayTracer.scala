package edu.csulb.summer2011

import scala.swing._
import java.awt.image.BufferedImage
import java.awt.Color

object RayTracer extends SimpleSwingApplication {

  // Scene dimensions
  val width = 512;
  val height = 512;

  //
  // The tracer
  //

  case class Point(x: Double, y: Double, z: Double) {

    def magnitude = math.sqrt(this ** this)

    def normal = {
      val n = magnitude
      Point(x / n, y / n, z / n)
    }

    def -(other: Point) = Point(x - other.x, y - other.y, z - other.z)

    def +(other: Point) = Point(x + other.x, y + other.y, z + other.z)
    
    /**
     * Dot product
     */
    def **(other: Point) = x*other.x + y*other.y + z*other.z

    /**
     * Cross product
     */
    def ^^(other: Point) = Point(y*other.z - other.y*z, x*other.z - other.x*z, x*other.y - other.x*y)

  }

  class DoubleWithPoint(d: Double) {
    def *(p: Point) = Point(d * p.x, d * p.y, d * p.z)
  }
  implicit def double2DoubleWithPoint(d: Double) = new DoubleWithPoint(d)
  
  case class Line(P: Point, D: Point) {
    def at(t: Double) = P + t * D
  }

  case object Line {
    def apply(a: Point, b: Point, start: Point): Line = apply(start, (b - a) normal)
  }

  abstract class Objekt {
    def intersect(line: Line): Array[Double]
    
    def normal(point: Point): Point
  }

  case class Sphere(C: Point, r: Double) extends Objekt {
    def intersect(line: Line): Array[Double] = {
      val M = line.P - C;

      val b =  (line.D ** M)
      val d = b*b - ((M**M) - r*r)
      if (d > 0) {
        // If d > 0, the line and sphere intersect at two points.        val d_root = math.sqrt(d)
        return Array[Double](-b - d_root, -b + d_root)      } else if (d == 0) {
        // If d = 0, the line and sphere intersect at one point.
        return Array[Double](-b)
      } else {
        // If d < 0, the line and sphere don't intersect.
        return Array[Double]()
      }
    }
    
    def normal(point: Point): Point = point - C
  }

  case class Triangle(p0: Point, p1: Point, p2: Point) extends Objekt {
    def intersect(line: Line): Array[Double] = {
      val e0 = p1 - p0
      val e1 = p2 - p1

      // The normal to the plane
      val n = e0 ^^ e1
      val k = p0 ** n
      
      // Find t
      val Dn = line.D ** n
      if (Dn == 0) {
        // The ray is parallel to the plane
        return Array[Double]()
      }
      
      val t = (k - line.P ** n) / Dn

      // The intersection point with the plane
      val R = line at t
      
      // Find out if R is inside the triangle
      val e2 = p0 - p2
      
      val x0 = (e0 ^^ (R - p0)) ** n
      val x1 = (e1 ^^ (R - p1)) ** n
      val x2 = (e2 ^^ (R - p2)) ** n
      
      if (x0 > 0 && x1 > 0 && x2 > 0) {
        return Array[Double](t)
      } else {
        return Array[Double]()
      }
    }
    
    def normal(point: Point): Point = (p1 - p0) ^^ (point - p1)
    
  }

  case class Kolor(r: Double, g: Double, b: Double) {
    def *(d: Double) = Kolor(r * d, g * d, b * d)
    def *(k: Kolor) = Kolor(r * k.r, g * k.g, b * k.b)
    def +(k: Kolor) = Kolor(r + k.r, g + k.g, b + k.b)
    
    def getRGB: Int = {
      val (rr, gg, bb) = ((r*255+0.5).asInstanceOf[Int], (g*255+0.5).asInstanceOf[Int], (b*255+0.5).asInstanceOf[Int])

      val aa = 255
      return ((aa & 0xFF) << 24) |
                ((rr & 0xFF) << 16) |
                ((gg & 0xFF) << 8)  |
                ((bb & 0xFF) << 0);
    }
  }

  implicit def awtColor2Kolor(c: Color) = Kolor(c.getRed / 255d, c.getGreen / 255d, c.getBlue / 255d)

  // Scene configuration
  val objects = Array[Tuple2[Objekt, Kolor]](
      (Sphere(Point(0, 0, 1000), 50), Color.BLUE)
      ,(Triangle(Point(0, 0, 1500), Point(0, 100, 1500), Point(100, 100, 1000)), Color.RED)
  )
  val cameraPosition = Point(0, 0, -10000)
  val viewportTopLeft = Point(-width/2, -height/2, 0)
  val I_ambient = Color.WHITE
  val I_light = Color.WHITE
  val P_light = Point(0, 100, -1000)

  def shootRaygun(x: Double, y: Double): Kolor = {
    // Ray line definition
    val P = Point(viewportTopLeft.x + x, viewportTopLeft.y + y, viewportTopLeft.z)
    val ray = Line(cameraPosition, P, P)

    // Find all intersections
    var hits = new collection.immutable.TreeMap[Double, Tuple2[Objekt, Kolor]]
    for (ob <- objects) {
      for (t <- ob._1.intersect(ray)) {
        if (t > 0) {
          hits += (t -> ob)
        }
      }
    }

    // Compute color
    if (hits.size > 0) {
      val t = hits.head._1
      val obj = hits.head._2._1
      val k = hits.head._2._2

      val P = ray at t
      val N = obj.normal(P) normal
      val L = (P_light - P) normal

      return k*I_ambient + k*I_light*(N**L)
    } else {
      return Color.BLACK
    }
  }

  //
  // Render the scene
  //
  
  val buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

  for (x <- 0 to width - 1) {
    for (y <- 0 to height - 1) {
      buffer.setRGB(x, y, shootRaygun(x, y).getRGB)
    }
  }

  //
  // The main loop
  //

  def top = new MainFrame {
    title = "Ray Tracer"
    preferredSize = new Dimension(width, height)
    contents = new Panel {
      override def paint(g: Graphics2D) {
        super.paint(g)
        g.drawImage(buffer, null, 0, 0)
      }
    }
  }

}
