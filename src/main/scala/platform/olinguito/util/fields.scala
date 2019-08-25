// See LICENSE for license details.

package olinguitochip.platform.olinguito.fields

import chisel3._
import chisel3.util._
import olinguitochip.config._
import olinguitochip.util._

case object XLen extends Field[Int]
case object FastMulDiv extends Field[Boolean]
case object useMExt extends Field[Boolean]
case object useEExt extends Field[Boolean]
case object useCExt extends Field[Boolean]
case object useAExt extends Field[Boolean]
case object useFormal extends Field[Boolean]
case object olinguitoID extends Field[Int]
case object useDM extends Field[Boolean]
case object IncludeSPIDTM extends Field[Boolean]
case object IncludeI2CDTM extends Field[Boolean]
case object NTiles extends Field[Int]
case object useAESExt extends Field[Boolean]
case object ADC_DAC extends Field[Boolean]
case object resetVector extends Field[String]
case object TileKey extends Field[TileParams]
case object ResetVectorBits extends Field[Int]
case object MaxHartIdBits extends Field[Int]
case object ArbiterKey extends Field[arbiterConfig]
case object PAddrBits extends Field[Int]
case object PgLevels extends Field[Int]
case object ASIdBits extends Field[Int]


case class arbiterConfig
(
 val offset_scratch_pad: BigInt = BigInt("40000000", 16),
 val size_scratch_pad: Int = 2048
)
{
  val has_scratch_pad: Boolean = size_scratch_pad != 0
}

case class MulDivParams
(
  mulUnroll: Int = 1,
  divUnroll: Int = 1,
  mulEarlyOut: Boolean = false,
  divEarlyOut: Boolean = false
)

trait CoreParams {
  val useVM: Boolean
  val useUser: Boolean
  val useDebug: Boolean
  val useAtomics: Boolean
  val useCompressed: Boolean
  val mulDiv: Option[MulDivParams]
  val fetchWidth: Int
  val decodeWidth: Int
  val retireWidth: Int
  val instBits: Int
  val nLocalInterrupts: Int
}

trait TileParams {
  val core: CoreParams
  /*val icache: Option[ICacheParams]
  val dcache: Option[DCacheParams]
  val rocc: Seq[RoCCParams]
  val btb: Option[BTBParams]*/
}

case class OlinguitoCoreParams
(
  bootFreqHz: BigInt = 0,
  useVM: Boolean = true,
  useUser: Boolean = false,
  useDebug: Boolean = true,
  useAtomics: Boolean = true,
  useCompressed: Boolean = true,
  nLocalInterrupts: Int = 0,
  nBreakpoints: Int = 1,
  nPMPs: Int = 8,
  nPerfCounters: Int = 0,
  nCustomMRWCSRs: Int = 0,
  mtvecInit: Option[BigInt] = Some(BigInt(0)),
  mtvecWritable: Boolean = true,
  fastLoadWord: Boolean = true,
  fastLoadByte: Boolean = false,
  fastJAL: Boolean = false,
  mulDiv: Option[MulDivParams] = Some(MulDivParams())
) extends CoreParams {
  val fetchWidth: Int = if (useCompressed) 2 else 1
  //  fetchWidth doubled, but coreInstBytes halved, for RVC:
  val decodeWidth: Int = fetchWidth / (if (useCompressed) 2 else 1)
  val retireWidth: Int = 1
  val instBits: Int = if (useCompressed) 16 else 32
}

trait HasTileParameters {
  implicit val p: Parameters
  val tileParams: TileParams = p(TileKey)

  val usingVM = tileParams.core.useVM
  val usingUser = tileParams.core.useUser || usingVM
  val usingDebug = tileParams.core.useDebug
  val usingRoCC = false //!tileParams.rocc.isEmpty
  val usingBTB = false //tileParams.btb.isDefined && tileParams.btb.get.nEntries > 0
  val usingPTW = usingVM
  //val usingDataScratchpad = tileParams.dcache.flatMap(_.scratch).isDefined
  val hartIdLen = p(MaxHartIdBits)

  //def dcacheArbPorts = 1 + usingVM.toInt + usingDataScratchpad.toInt + tileParams.rocc.size
}

trait HasCoreParameters extends HasTileParameters {
  val coreParams: CoreParams = tileParams.core

  val xLen = p(XLen)
  val ILEN = p(XLen)
  val fLen = xLen // TODO relax this
  require(xLen == 32 || xLen == 64)

  val usingMulDiv = coreParams.mulDiv.nonEmpty
  val usingFPU = false //coreParams.fpu.nonEmpty
  val usingAtomics = coreParams.useAtomics
  val usingCompressed = coreParams.useCompressed

  val retireWidth = coreParams.retireWidth
  val fetchWidth = coreParams.fetchWidth
  val decodeWidth = coreParams.decodeWidth

  val coreInstBits = coreParams.instBits
  val coreInstBytes = coreInstBits/8
  val coreDataBits = xLen max fLen
  val coreDataBytes = coreDataBits/8

  //val coreDCacheReqTagBits = 6
  //val dcacheReqTagBits = coreDCacheReqTagBits + log2Ceil(dcacheArbPorts)

  def pgIdxBits = 12
  def pgLevelBits = 10 - log2Ceil(xLen / 32)
  def vaddrBits = pgIdxBits + pgLevels * pgLevelBits
  val paddrBits = p(PAddrBits)
  def ppnBits = paddrBits - pgIdxBits
  def vpnBits = vaddrBits - pgIdxBits
  val pgLevels = p(PgLevels)
  val asIdBits = p(ASIdBits)
  val vpnBitsExtended = vpnBits + (vaddrBits < xLen).toInt
  val vaddrBitsExtended = vpnBitsExtended + pgIdxBits
  val coreMaxAddrBits = paddrBits max vaddrBitsExtended
  val maxPAddrBits = xLen match { case 32 => 34; case 64 => 56 }
  require(paddrBits <= maxPAddrBits)

  // Print out log of committed instructions and their writeback values.
  // Requires post-processing due to out-of-order writebacks.
  val enableCommitLog = false
}

trait HasOlinguitoCoreParameters extends HasCoreParameters {
  val ruberParams: OlinguitoCoreParams = tileParams.core.asInstanceOf[OlinguitoCoreParams]

  val fastLoadWord = ruberParams.fastLoadWord
  val fastLoadByte = ruberParams.fastLoadByte
  val fastJAL = ruberParams.fastJAL
  val nBreakpoints = ruberParams.nBreakpoints
  val nPMPs = ruberParams.nPMPs
  val nPerfCounters = ruberParams.nPerfCounters
  val nCustomMrwCsrs = ruberParams.nCustomMRWCSRs
  val mtvecInit = ruberParams.mtvecInit
  val mtvecWritable = ruberParams.mtvecWritable

  val mulDivParams = ruberParams.mulDiv.getOrElse(MulDivParams()) // TODO ask andrew about this

  require(!fastLoadByte || fastLoadWord)
}

trait HasOlinguitoParameters extends HasOlinguitoCoreParameters {
  val fastMulDiv = p(FastMulDiv)

  // these should become parameters, rather than constants
  val haveMExt = p(useMExt)
  val haveEExt = p(useEExt)
  val haveCExt = p(useCExt)

  // ID stuff
  val hartID = p(olinguitoID)

  val haveSbox = p(useAESExt)

  val resetVal = p(resetVector)

  val haveADC = p(ADC_DAC)
  val haveFormal = p(useFormal)
}

// This class is only used for create our CSR/ALU
case class CSRALUTileParams(mtveci: BigInt) extends TileParams {
  val icache = None
  val dcache = None
  val btb = None
  val rocc = Nil
  val core = OlinguitoCoreParams(
    useDebug = true,
    nLocalInterrupts = 0,
    mtvecInit = Some(mtveci),
    mtvecWritable = true,
    fastJAL = false,
    bootFreqHz = 0, // Unsupported
    useVM = false, // Unsupported
    useUser = true, // Unsupported
    useAtomics = false, // Unsupported
    useCompressed = false, // Unsupported
    nBreakpoints = 1, // Unsupported
    nPMPs = 0, // Unsupported
    nPerfCounters = 0, // Unsupported
    nCustomMRWCSRs = 0, // Unsupported
    fastLoadWord = false, // Unsupported
    fastLoadByte = false, // Unsupported
    mulDiv = Some(MulDivParams()) // Unsupported (Parameter are given inside Olinguito)
  )
}
