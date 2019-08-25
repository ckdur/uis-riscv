package olinguitochip.platform.configs

import chisel3.util._
import olinguitochip.config._
import olinguitochip.platform.olinguito.fields._

class BaseCoreplexConfig extends Config ((site, here, up) => {
  // Seems that are only used by CSR/ALU (tile and core params)
  case PAddrBits => 32
  case PgLevels => if (site(XLen) == 64) 3 /* Sv39 */ else 2 /* Sv32 */
  case ASIdBits => 0
  case NTiles => 1  // TODO: only 1 tile
  case MaxHartIdBits => Math.max(log2Ceil(site(NTiles)), 1)
  // General
  case XLen => 64
  case `useMExt` => true
  case `useEExt` => false
  case `FastMulDiv` => false
  case `olinguitoID` => 0
  case `TileKey` => CSRALUTileParams(0x10000)
  case `useAESExt` => false
  case `ADC_DAC` => false
  case `useFormal` => false
  case `resetVector` => "h10000000"
  case `ArbiterKey` => new arbiterConfig
  // TODO: Unsupported, put always in false
  case `useCExt` => false
  case `useAExt` => false
})

class BaseConfig extends Config(new BaseCoreplexConfig)

// Just a placeholder
class Default64Config extends Config(new BaseConfig)
class Default32Config extends Config(
  new BaseConfig().alter((site, here, up) => {
    case XLen => 32
  }))
// Config with AES in 32 bits
class AES32Config extends Config(
  new BaseConfig().alter((site, here, up) => {
    case XLen => 32
    case `useAESExt` => true
  }))

// Tiniest config possible
class TinyConfig extends Config(
  new BaseConfig().alter((site, here, up) => {
    case XLen => 32
    case `useEExt` => true
    case `useMExt` => false
    case `FastMulDiv` => false
  }))

// Config with ADC_DAC instruction in 32 bits
class ADC_DAC32Config extends Config(
  new BaseConfig().alter((site, here, up) => {
    case XLen => 32
    case `ADC_DAC` => true
  }))
