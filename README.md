# Sega Master System for Ulx3s ECP5 FPGA

## Introduction

This is a version of the Sega Master System 8-bit games console for the Ulx3s board, written entirely in Verilog.

It has HDMI and optional VGA output. The resolution is 640x480 at 60MHz.

Joypad 1 is implemented by using the buttons.

There is audio output, but it needs improving.

It currently uses the Europe/USA 1.3 BIOS, which, on power-up,  displays the Sega trademark and then looks for a game cartridge.

To load games, copy `python/program.py` to your Rpi Zero installed with [circuitpython](https://learn.adafruit.com/circuitpython-on-raspberrypi-linux/installing-circuitpython-on-raspberry-pi), and run `python3 program.py <rom>.sms`.

There are lots of games on the [planetemu](https://www.planetemu.net/roms/sega-master-system) site.

Controls are implemented on `usb 0` (Left port) using [usb_hid_host](https://github.com/nand2mario/usb_hid_host). You can control it via wasd keys on a keyboard, and the xy keys or kl. Press P to restart. Alternativly mouse control is supported if you don't have a keyboard, left button goes left, right one right, middle button is jump (fire1). The onboard buttons are for reset and up when a mouse is plugged in. USB gamepads (not xbox controllers) are supported, their key mapping is pretty obvious :)

## Implementation

It is written entirely in Verilog. The top level and VDP implementation are new.

The rom cartridge is loaded into the SDRAM and executed from there, using the Sega mapper. The few games that do not use the Sega mapper will not work.

The VDP implementation generates VGA output, which is then converted to HDMI. It does not use the timing of the original VDP chip. Legacy modes compatible with Texas Instruments TMS9918 chip are implemented for compatibility with the Sega SG 1000.

Console memory (8KB) uses BRAM, as does the video ram (16KB), and the bios rom (32KB).

Top-level parameters allow the VGA output to be selected, and also LCD and LED diagnostics.

## Installation

You need recent versions of oss cad suite.

To build do:

```
cd icepi-zero
make install
```

## Bugs

Audio needs improving.

Only joypad 1 is supported and seems to have some problems.

There is a vertical colored bar down the left of some games.

Various edge cases in the VDP are not correct.

Not all games run.

These are some of the games that seem to have problems:

- Asterix - hangs
- Baku Baku Animal - does not respond to start button
- Chop Lifter - hangs
- Dracula - screen corruption
- Fantastic Dizzy - Software Error
- Jungle Book - screen corruption
- Lemmings - screen corruption
- Lion King - crashes
- Miracle World - hangs
- Ms Pacman - hangs
- Outrun - screen corruption
- Space Harrier - hangs
- Spell Caster - hangs
- Wanted - screen corruption
- Zaxxon 3D - screen corruption
