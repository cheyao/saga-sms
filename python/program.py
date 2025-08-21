import sys
import board
import digitalio
from struct import unpack

class ld_nes:
  def __init__(self,spi,cs):
    self.spi=spi
    self.cs=cs
    self.cs.value = False
    #self.rom="/sd/zxspectrum/roms/opense.rom"

  # LOAD/SAVE and CPU control

  # read from file -> write to SPI RAM
  def load_stream(self, filedata, addr=0, maxlen=0x110000, blocksize=1024):
    block = bytearray(blocksize)
    # Request load
    self.cs.value = True
    self.spi.write(bytearray([0,(addr >> 24) & 0xFF, (addr >> 16) & 0xFF, (addr >> 8) & 0xFF, addr & 0xFF]))
    bytes_loaded = 0
    while bytes_loaded < maxlen:
      if filedata.readinto(block):
        self.spi.write(block)
        bytes_loaded += blocksize
      else:
        break
    self.cs.value = False

  # read from SPI RAM -> write to file
  def save_stream(self, filedata, addr=0, length=1024, blocksize=1024):
    bytes_saved = 0
    block = bytearray(blocksize)
    # Request save
    self.cs.value = True
    self.spi.write(bytearray([1,(addr >> 24) & 0xFF, (addr >> 16) & 0xFF, (addr >> 8) & 0xFF, addr & 0xFF, 0]))
    while bytes_saved < length:
      self.spi.readinto(block)
      filedata.write(block)
      bytes_saved += len(block)
    self.cs.value = False

  def ctrl(self,i):
    self.cs.value = True
    self.spi.write(bytearray([0, 0xFF, 0xFF, 0xFF, 0xFF, i]))
    self.cs.value = False

  def cpu_halt(self):
    self.ctrl(2)

  def cpu_continue(self):
    self.ctrl(0)

filename = sys.argv[1]

spi=board.SPI()
while not spi.try_lock():
    pass
spi.configure(baudrate=3000000, polarity=0, phase=0)

cs = digitalio.DigitalInOut(board.CE0)
cs.direction = digitalio.Direction.OUTPUT
cs.value = False

s=ld_nes(spi,cs)
s.ctrl(1)
s.ctrl(0)
with open(filename, mode='rb') as file:
    s.load_stream(file)
s.ctrl(2)
s.ctrl(0)

