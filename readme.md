  [SCPI]:https://en.wikipedia.org/wiki/Standard_Commands_for_Programmable_Instruments

# A Rigol DS1054Z command-line tool

It turns out my DS1054Z scope has a relatively simple [SCPI]
interface. This is super neat! You can change controls, take
screenshots, capture the current waveform data (when stopped), and do
most of the things that the physical buttons let you. I did not,
however, find a way to relabel the channels. That would have been
useful as the on-screen keyboard isn't fantastic. A minor detail,
though.

This project tries to enable some of these features through a simple
to use command line tool.

## Implementation

The command set is documented
[here](https://rigol.com.pl/pl/p/file/4b4d1a97273d6a3ddc301181fa34fb11/MSO1000ZDS1000Z_ProgrammingGuide_EN.pdf). I
hook it up with the LAN connector and find its IP. After that, you can
simply use `nc` to start testing. For example, to see if CH1 is
currenly on you can do this:

     ~> rlwrap nc -vv 10.0.0.160 5555
    Connection to 10.0.0.160 5555 port [tcp/personal-agent] succeeded!
    :CHAN1:DISP?
    0

There are also [SCPI] commands to take a screenshot. I use this quite
a lot so I made a script for it. That way, I don't have to fiddle
around with a USB stick every time I want to share the screen. I often
run it like this:

    rigolette screenshot | oxipng - | tee ss.png | feh -

That lets me see the image immediately, and if it's a keeper I'll
rename `ss.png` to something more descripting. I also use `oxipng` to
reduce the `png` file size by half, I guess the Rigol's on-board png
encoder isn't the best one.

## Installing

I don't expect anyone else to use this but me. There are probably
hundreds of better alternatives and in more well-known programming
environments. But just in case this is useful to other, here's
something to get you started.

    apt install chicken-bin # or your distro equivalent
    git clone rigolette && cd rigolette
    chicken-install -s

# Supported devices

I have only tested this on my DS1054Z, but the same manual covers all
of these:

- MSO1104Z-S
- MSO1074Z-S
- MSO1104Z
- MSO1074Z
- DS1104Z-S Plus
- DS1074Z-S Plus
- DS1104Z Plus
- DS1074Z Plus
- DS1054Z

I suspect some of the functionality here may work on a number of the
models above.

# TODO

- Improve frame-boundary robustness (a rogue command can break subsequent ones)
