// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package util

import Chisel._
import config._
import scala.math._

class ParameterizedBundle(implicit p: Parameters) extends Bundle {
  override def cloneType = {
    try {
      this.getClass.getConstructors.head.newInstance(p).asInstanceOf[this.type]
    } catch {
      case e: java.lang.IllegalArgumentException =>
        throwException("Unable to use ParamaterizedBundle.cloneType on " +
                       this.getClass + ", probably because " + this.getClass +
                       "() takes more than one argument.  Consider overriding " +
                       "cloneType() on " + this.getClass, e)
    }
  }
}
