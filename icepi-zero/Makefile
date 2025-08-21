VERILOG = ../src/sms.v ../src/ram.v ../src/vram.v ../src/rom.v \
	  ../src/Z80/tv80_core.v ../src/Z80/tv80n.v ../src/Z80/tv80_mcode.v \
	  ../src/Z80/tv80_reg.v ../src/Z80/tv80_alu.v \
	  ../src/video.v ../src/hdmi.v \
	  ../src/sn76489.v ../src/tone_generator.v ../src/sigmadelta.v\
	  ../src/lattice/ecp5pll.sv \
	  ../src/sdram.v \
      ../src/spi_display/lcd_video.v \
      ../src/spi_display/hex_decoder_v.v \
	  ../src/osd/osd.v ../src/osd/spi_osd.v \
	  ../src/osd/spi_ram_btn.v ../src/osd/spirw_slave_v.v \
	  ../src/usb/usb_hid_host.v ../src/usb/usb_hid_host_rom.v

OUTPUT = toplevel

all: debug

%.json: $(VERILOG)
	rm -f $(OUTPUT).bit $(OUTPUT).config $(OUTPUT).json
	yosys -p 'synth_ecp5 -top top -json $@' $^

%.config: %.json icepi-zero.lpf
	nextpnr-ecp5 --25k --package CABGA256 --lpf icepi-zero.lpf --json $< --textcfg $@ # 2> log.txt
	# cat log.txt | grep Device -A 28

%.bit: %.config
	ecppack $< $@

build: $(OUTPUT).bit

debug: build
	openFPGALoader -cft231X --pins=7:3:5:6 $(OUTPUT).bit

install: build
	openFPGALoader -cft231X --pins=7:3:5:6 $(OUTPUT).bit --write-flash

install-bitstream:
	openFPGALoader -cft231X --pins=7:3:5:6 $(OUTPUT).bit --write-flash

lint:
	verilator --lint-only -Wall -Wno-DECLFILENAME -Wno-WIDTHEXPAND $(VERILOG_SOURCES)

help:
	echo "Usage: make [option]"
	echo "Options:"
	echo "- install: install to flash"
	echo "- debug: install to chip's temp memory (bitstream lost on power loss)"
	echo "- build: builds the bitstream"
	echo "- clean: delete all temparary files"

clean:
	rm -f $(OUTPUT).bit $(OUTPUT).config $(OUTPUT).json

