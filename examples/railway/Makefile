# $Id$
# 
# 20080407 T. Bourke
#    Original file.

OBJS=train-gate-mod.xml \
     train-gate-fairtest.xml \
     train-gate-test.xml \
     train-gate-test.q \
     train-gate-mod.q

XSLTPROC=xsltproc
URPAL=./urpal

##
all: $(OBJS)

train-gate-mod.xml: train-gate-fair.xml
	@$(XSLTPROC) --stringparam system "Gate=QGate(); system Train, Gate, Silent;" \
		    --novalid --output train-gate-mod.xml \
		    change_uppaal_system.xml train-gate-fair.xml

train-gate-fairtest.xml: train-gate-fair.xml
	@$(URPAL) -e 'FlexGateTest=tabulate(maketest(scale(FlexGate, 2.0)), {Err})' \
         	  -i train-gate-fair.xml -o train-gate-fairtest-int.xml
	@$(XSLTPROC) --stringparam system "system FlexGate, FlexGateTest;" \
		     --novalid --output train-gate-fairtest.xml \
		     change_uppaal_system.xml train-gate-fairtest-int.xml
	@rm train-gate-fairtest-int.xml

train-gate-test.xml: train-gate-fairtest.xml
	@$(XSLTPROC) --stringparam system "system QGate, FlexGateTest;" \
		     --novalid --output train-gate-test.xml \
		     change_uppaal_system.xml train-gate-fairtest.xml

train-gate-test.q: train-gate-fairtest.q
	@cp train-gate-fairtest.q train-gate-test.q

train-gate-mod.q: train-gate-fair.q
	@cp train-gate-fair.q train-gate-mod.q

clean:

clobber: clean
	-@rm $(OBJS)

