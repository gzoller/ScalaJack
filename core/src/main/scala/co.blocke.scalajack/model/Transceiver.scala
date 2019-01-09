package co.blocke.scalajack
package model

trait Transceiver extends Reader with Writer {
  type WIRE
}
