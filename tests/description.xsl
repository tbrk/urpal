<?xml version="1.0" encoding="utf-8"?>
<!-- $Id$

     Copyright (c) 2008 Timothy Bourke (University of NSW and NICTA)
     All rights reserved.

     This program is free software; you can redistribute it and/or modify it
     under the terms of the "BSD License" which is distributed with the
     software in the file LICENSE.

     This program is distributed in the hope that it will be useful, but
     WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the BSD
     License for more details.
  -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="text" encoding="utf-8" />

    <xsl:template match="nta">
	<xsl:value-of select="declaration/text()"/>
    </xsl:template>

</xsl:stylesheet>
