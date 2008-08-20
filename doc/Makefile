# $Id$
#
#  Copyright (c) 2008 Timothy Bourke (University of NSW and NICTA)
#  All rights reserved.
#
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of the "BSD License" which is distributed with the
#  software in the file LICENSE.
#
#  This program is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the BSD
#  License for more details.

urpal.1.pdf: urpal.1.ps
	ps2pdf urpal.1.ps

urpal.1.ps: urpal.1
	sed '/^.Dt/d' urpal.1 | groff -Tps -mdoc > urpal.1.ps

clean:
	-@rm urpal.1.ps 2>/dev/null

clobber: clean
	-@rm urpal.1.pdf 2>/dev/null

