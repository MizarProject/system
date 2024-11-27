# Makefile for Mizar 

# Directory for sources
MIZSRC=.

# Directories for binaries
FPCBIN=bin

# Directories for object and unit files
FPCOBJ=obj

# Path to Mizar units
MIZ_UNITS =base kernel usrtools libtools addtools 

# Compilers
FPC = fpc -Sdg -FE$(FPCBIN) -FU$(FPCOBJ) \
	$(addprefix -Fu$(MIZSRC)/,$(MIZ_UNITS)) 

FPCFLAGS =
#FPCFLAGS = -g -gc -gg -pg 

ADDDPR = $(wildcard $(MIZSRC)/addtools/*.dpr)

BASEDPR = $(wildcard $(MIZSRC)/base/*.dpr)

KERNELDPR = $(wildcard $(MIZSRC)/kernel/*.dpr)

LIBDPR = $(wildcard $(MIZSRC)/libtools/*.dpr)

USRDPR = $(wildcard $(MIZSRC)/usrtools/*.dpr)

ADD_EXE = $(basename $(ADDDPR))
BASE_EXE = $(basename $(BASEDPR))
KERNEL_EXE = $(basename $(KERNELDPR))
LIB_EXE = $(basename $(LIBDPR))
USR_EXE = $(basename $(USRDPR))

.PHONY: all
all: dirs fpc_base fpc_kernel fpc_add fpc_lib fpc_usr

clean: fpc_clean
fpc_clean: fpc_obj_clean fpc_bin_clean

dirs:
	mkdir -p $(FPCBIN) $(FPCOBJ)

fpc_base: $(addprefix $(FPCBIN)/,$(BASE_EXE))
fpc_kernel: $(addprefix $(FPCBIN)/,$(KERNEL_EXE))
fpc_add: $(addprefix $(FPCBIN)/,$(ADD_EXE))
fpc_lib: $(addprefix $(FPCBIN)/,$(LIB_EXE))
fpc_usr: $(addprefix $(FPCBIN)/,$(USR_EXE))

fpc_obj_clean:
	rm -r -f $(FPCOBJ)/*
fpc_bin_clean:
	rm -r -f $(FPCBIN)/*

# Rules for FPC
$(addprefix $(FPCBIN)/,$(BASE_EXE)): 
	$(FPC) $(FPCFLAGS) $(MIZSRC)/base/$(basename $(@F)).dpr
$(addprefix $(FPCBIN)/,$(KERNEL_EXE)): 
	$(FPC) $(FPCFLAGS) $(MIZSRC)/kernel/$(basename $(@F)).dpr
$(addprefix $(FPCBIN)/,$(ADD_EXE)): 
	$(FPC) $(FPCFLAGS) $(MIZSRC)/addtools/$(basename $(@F)).dpr
$(addprefix $(FPCBIN)/,$(filter-out $(SPECIAL_LIB),$(LIB_EXE))): 
	$(FPC) $(FPCFLAGS) $(MIZSRC)/libtools/$(basename $(@F)).dpr
$(addprefix $(FPCBIN)/,$(USR_EXE)): 
	$(FPC) $(FPCFLAGS) $(MIZSRC)/usrtools/$(basename $(@F)).dpr


