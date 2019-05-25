// See README.md for license details.

package generator

import chisel3._

object Main extends App {
  chisel3.Driver.execute(args, () => new gcd.Top(10)) 
}
