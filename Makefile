# Makefile for "Pokey Examples"

#==================
# Global Variables
#==================
# Latest as of 12/24/2022 - https://github.com/dasm-assembler/dasm/tags
DASM_VERSION=2.20.14.1

# Latest as of 12/24/2022 - https://github.com/7800-devtools/a7800/tags
A7800_VERSION=v5.2

# For a7800sign; latest as of 12/24/2022 - http://7800.8bitdev.org/index.php/7800AsmDevKit
DEVKIT_VERSION=0.20.1
DEVKIT_URL=http://7800.8bitdev.org/images/6/60/7800AsmDevKit-0.20.1-osx-x64.tar.gz

#=========
# Aliases
#=========
# 7800-specific build tools
DASM=./bin/dasm/dasm
7800SIGN=./bin/devkit_tools/7800basic/7800sign

# 7800-specific runtime tools
A7800=./bin/a7800/a7800

# OS-level Tools
WGET=/opt/homebrew/bin/wget
TAR=/usr/bin/tar
UNZIP=/usr/bin/unzip
SLEEP=/bin/sleep

#===============
# Build Targets
#===============
.PHONY: all run clean megaclean setup vars

default:	all

# Special variable to only initialize tools.  Not generally used.
setup:	${WGET} ${7800SIGN} ${DASM} ${A7800}

${WGET}:
	brew install wget

${7800SIGN}:	${WGET} ${TAR}
	mkdir -p bin/devkit_tools
	wget -O bin/devkit_tools/7800AsmDevKit.tar.gz ${DEVKIT_URL}
	cd bin/devkit_tools && tar xvf 7800AsmDevKit.tar.gz
	# Do a touch so it doesn't keep appearing out-of-date
	find bin/devkit_tools/ -type f -exec touch {} +
	rm -f bin/devkit_tools/7800AsmDevKit.tar.gz

${DASM}:	${WGET} ${TAR}
	mkdir -p bin/dasm
	wget -O bin/dasm/dasm.tar.gz https://github.com/dasm-assembler/dasm/releases/download/${DASM_VERSION}/dasm-${DASM_VERSION}-osx-x64.tar.gz
	tar xvf bin/dasm/dasm.tar.gz --directory bin/dasm/
	# Do a touch so it doesn't keep appearing out-of-date
	find bin/dasm/ -type f -exec touch {} +
	rm -f bin/dasm/dasm.tar.gz

#====================
SOURCEDIR_4000=src/4000
SOURCES_4000 := $(shell (find $(SOURCEDIR_4000) -type f -name '*.asm'))
OUTDIR_4000=out/4000
BINS_4000 := $(addprefix $(OUTDIR_4000)/, $(notdir $(SOURCES_4000:.asm=.bin)))
A78_4000 := $(BINS_4000:.bin=.a78)
RUNDIR_4000=run/4000
RUN_4000 := $(addprefix $(RUNDIR_4000)/, $(notdir $(SOURCES_4000:.asm=.bash)))
#====================
SOURCEDIR_450=src/450
SOURCES_450 := $(shell (find $(SOURCEDIR_450) -type f -name '*.asm'))
OUTDIR_450=out/450
BINS_450 := $(addprefix $(OUTDIR_450)/, $(notdir $(SOURCES_450:.asm=.bin)))
A78_450 := $(BINS_450:.bin=.a78)
RUNDIR_450=run/450
RUN_450 := $(addprefix $(RUNDIR_450)/, $(notdir $(SOURCES_450:.asm=.bash)))
#====================
.PRECIOUS: $(BINS_4000) $(A78_4000) $(RUN_4000) $(BINS_450) $(A78_450) $(RUN_450)
#====================

vars:
	$(info ==============================================)
	$(info SOURCEDIR_4000 is $(SOURCEDIR_4000))
	$(info SOURCES_4000 is $(SOURCES_4000))
	$(info OUTDIR_4000 is $(OUTDIR_4000))
	$(info BINS_4000 is $(BINS_4000))
	$(info A78_4000 is $(A78_4000))
	$(info RUNDIR_4000 is $(RUNDIR_4000))
	$(info RUN_4000 is $(RUN_4000))
	$(info ==============================================)
	$(info SOURCEDIR_450 is $(SOURCEDIR_450))
	$(info SOURCES_450 is $(SOURCES_450))
	$(info OUTDIR_450 is $(OUTDIR_450))
	$(info BINS_450 is $(BINS_450))
	$(info A78_450 is $(A78_450))
	$(info RUNDIR_450 is $(RUNDIR_450))
	$(info RUN_450 is $(RUN_450))
	$(info ==============================================)

${OUTDIR_4000}:
	mkdir -p $(OUTDIR_4000)

${OUTDIR_450}:
	mkdir -p $(OUTDIR_450)

#========
# Header
#========
${OUTDIR_4000}/16k_pokey_cartridge_header_pokey_4000.bin:	./src/16k_pokey_cartridge_header_pokey_4000.asm	${OUTDIR_4000}
	echo "\n###### Creating ${OUTDIR_4000}/16k_pokey_cartridge_header_pokey_4000.bin"
	${DASM} ./src/16k_pokey_cartridge_header_pokey_4000.asm -f3 -o${OUTDIR_4000}/16k_pokey_cartridge_header_pokey_4000.bin

${OUTDIR_450}/16k_pokey_cartridge_header_pokey_450.bin:	./src/16k_pokey_cartridge_header_pokey_450.asm	${OUTDIR_450}
	echo "\n###### Creating ${OUTDIR_450}/16k_pokey_cartridge_header_pokey_450.bin"
	${DASM} ./src/16k_pokey_cartridge_header_pokey_450.asm -f3 -o${OUTDIR_450}/16k_pokey_cartridge_header_pokey_450.bin

#===============
# Build targets
#===============
all:	${RUN_4000} ${RUN_450}

${OUTDIR_4000}/%.bin:	${SOURCEDIR_4000}/%.asm ${OUTDIR_4000} ${DASM} ${7800SIGN}
	echo "\n###### Creating $@ from source file $<... ######"
	${DASM} $< -f3 -o$@
	echo "\n###### Signing $@... ######"
	${7800SIGN} -w $@

${OUTDIR_4000}/%.a78:	${OUTDIR_4000}/%.bin ${OUTDIR_4000} ${OUTDIR_4000}/16k_pokey_cartridge_header_pokey_4000.bin
	echo "\n\n###### Creating $@ from bin file $<... ######"
	cat ${OUTDIR_4000}/16k_pokey_cartridge_header_pokey_4000.bin $< > $@

${OUTDIR_450}/%.bin:	${SOURCEDIR_450}/%.asm ${OUTDIR_450} ${DASM} ${7800SIGN}
	echo "\n###### Creating $@ from source file $<... ######"
	${DASM} $< -f3 -o$@
	echo "\n###### Signing $@... ######"
	${7800SIGN} -w $@

${OUTDIR_450}/%.a78:	${OUTDIR_450}/%.bin ${OUTDIR_450} ${OUTDIR_450}/16k_pokey_cartridge_header_pokey_450.bin
	echo "\n\n###### Creating $@ from bin file $<... ######"
	cat ${OUTDIR_450}/16k_pokey_cartridge_header_pokey_450.bin $< > $@

#===============
# Run Targets
#===============
${RUNDIR_4000}:
	mkdir -p $(RUNDIR_4000)

${RUNDIR_450}:
	mkdir -p $(RUNDIR_450)

${RUNDIR_4000}/%.bash:	${OUTDIR_4000}/%.a78 ${RUNDIR_4000} ${A7800}
	echo "\n###### Creating $@ from source file $<... ######"
	echo "#!/bin/bash" > $@
	echo "cd ../../bin/a7800 && ./a7800 a7800 -cart ../../$<" >> $@
	chmod 755 $@

$(RUNDIR_450)/%.bash:	${OUTDIR_450}/%.a78 ${RUNDIR_450} ${A7800}
	echo "\n###### Creating $@ from source file $<... ######"
	echo "#!/bin/bash" > $@
	echo "cd ../../bin/a7800 && ./a7800 a7800 -cart ../../$<" >> $@
	chmod 755 $@

${A7800}:	${WGET} ${TAR}
	echo "Checking a7800 emulator..."
	# Download and install
	mkdir -p tmp/
	wget -O tmp/a7800.tgz https://github.com/7800-devtools/a7800/releases/download/${A7800_VERSION}/a7800-osx-${A7800_VERSION}.tgz
	cd tmp && tar zxf a7800.tgz
	mkdir -p bin/a7800
	cp tmp/a7800-osx/a7800 bin/a7800/a7800
	rm -rf tmp
	# Configure our ini file
	mkdir -p ~/.a7800
	./bin/a7800/a7800 -cc
	mv a7800.ini ~/.a7800
	#rm -f ui.ini plugin.ini
	# We need the 7800.rom file (found here: https://atariage.com/forums/topic/268458-a7800-the-atari-7800-emulator/)
	wget -O bin/a7800/Additional_Files_a7800_v0188-01.zip https://atariage.com/forums/applications/core/interface/file/attachment.php?id=521608
	cd bin/a7800 && unzip -o Additional_Files_a7800_v0188-01.zip
	rm -f bin/a7800/Additional_Files_a7800_v0188-01.zip

#===============
# Clean Targets
#===============
clean:
	rm -rf out
	rm -rf run

megaclean:	clean
	#brew uninstall wget
	rm -rf bin/devkit_tools
	rm -rf bin/dasm
	rm -rf bin/a7800
	#rm -rf ~/.a7800
	#sudo rm -rf /Volumes/SDL2/SDL2.framework
