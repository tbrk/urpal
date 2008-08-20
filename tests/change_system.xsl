<?xml version="1.0" encoding="utf-8"?>
<!--$Id

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
    <xsl:output method="xml"
		encoding="utf-8"
		doctype-public="-//Uppaal Team//DTD Flat System 1.1//EN"
		doctype-system="http://www.it.uu.se/research/group/darts/uppaal/flat-1_1.dtd"
    />

    <xsl:template match="@* | node()">
	<xsl:copy>
	    <xsl:apply-templates select="@* | node()"/>
	</xsl:copy>
    </xsl:template>

    <xsl:template match="system">
	<xsl:copy>
	    <xsl:value-of select="$system"/>
	</xsl:copy>
    </xsl:template>

</xsl:stylesheet>
