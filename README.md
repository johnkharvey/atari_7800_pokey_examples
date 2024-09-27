# Atari 7800 POKEY demo compilable archive

----

This project contains a list of POKEY demos released to [atariage.com](https://www.atariage.com) between July 16, 2019 and December 8, 2020.  The discussion goes on from there, but the purpose of this project is to ensure that all provided assembly files can be compiled into functioning ROMS on a7800.  The author is Atari Age user [@Synthpopalooza](https://forums.atariage.com/profile/23798-synthpopalooza/); these are all their compositions and experiments.

----

**NOTE:** A few of these files have been tweaked in order to get them to work correctly:

#### 1. Bad Bytes

The following files were tweaked due to an errant byte '966' that likely should have been '96'.  Newer compilers (such as modern versions of DASM) trigger on this error and fail immediately:

 * metroid-brinstar-bass-perc.asm 
 * metroid-brinstar-lead-harmony.asm

#### 2. Incorrect Headers

The Header files were incorrect; in this case, I am not using specific headers but generic POKEY headers where the source code can be seen and compiled along with the source code.  Atari Age user [@Trebor](https://forums.atariage.com/profile/18-trebor/) has done a fantastic job of fixing the headers in these and those versions can be found in his ProPack releases.

#### 3. Missing POKEY initilization calls

All of the code required a "JSR RSTPOKEY" call injected in order to work with modern versions of a7800.  These modern versions more closely align with physical Atari 7800 hardware when used with POKEY support.

----

### How to compile these demos:

 * `make setup`
 * `make`
 * `cd run/4000` or `cd run/450`
 * Find the demo you would like to play and execute it as a shell script, e.g. `./test16k.bash`

### Caveats:

 * I have only gotten the compiler working on Mac.  Windows / Linux users will likely require updates to the Makefile.

### Additional resources:

 * [https://forums.atariage.com/topic/293427-pokey-16-bit-mode-and-other-audctl-settings-a7800-emulation/](https://forums.atariage.com/topic/293427-pokey-16-bit-mode-and-other-audctl-settings-a7800-emulation/)
 * [https://forums.atariage.com/topic/295770-my-pokey-experiments-using-nonstandard-settings/page/3/#comment-4488175](https://forums.atariage.com/topic/295770-my-pokey-experiments-using-nonstandard-settings/page/3/#comment-4488175)
 * [https://forums.atariage.com/topic/312414-pokey-explorer-v10-release/](https://forums.atariage.com/topic/312414-pokey-explorer-v10-release/)
 * [https://forums.atariage.com/topic/338679-atari-7800-speech/](https://forums.atariage.com/topic/338679-atari-7800-speech/)
 * [https://forums.atariage.com/topic/337460-cant-get-more-than-a-beep-with-pokey/?do=findComment&comment=5080905](https://forums.atariage.com/topic/337460-cant-get-more-than-a-beep-with-pokey/?do=findComment&comment=5080905)

### Questions?

 * Feel free to reach out to [me](https://forums.atariage.com/profile/151-propane13/) on [atariage.com](https://www.atariage.com)

