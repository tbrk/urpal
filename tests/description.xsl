<?xml version="1.0" encoding="utf-8"?>
<!-- $Id$ -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="text" encoding="utf-8" />

    <xsl:template match="nta">
	<xsl:value-of select="declaration/text()"/>
    </xsl:template>

</xsl:stylesheet>
