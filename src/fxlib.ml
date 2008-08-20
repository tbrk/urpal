(* $Id$
 *
 * Copyright (c) 2008 Timothy Bourke (University of NSW and NICTA)
 * All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the "BSD License" which is distributed with the
 * software in the file LICENSE.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the BSD
 * License for more details.
 *)
local
val root = "/usr/local/lib/sml-fxp/src"
val root = "/home/tbourke/tmp/fxp-2.0/src"

val fxplib = ["config.sml",
"Util/utilCompare.sml",
"Util/utilString.sml",
"Util/utilError.sml",
"Util/utilHash.sml",
"Util/utilInt.sml",
"Util/utilList.sml",
"Util/utilTime.sml",
"Util/intLists.sml",
"Util/intSets.sml",
"Util/options.sml",
"Util/SymDict/key.sml",
"Util/SymDict/dict.sml",
"Util/SymDict/symbolTable.sml",
"Util/SymDict/intSetDict.sml",
"Util/SymDict/intDict.sml",
"Util/SymDict/intListDict.sml",
"Util/SymDict/stringDict.sml",
"Unicode/Chars/uniChar.sml",
"Unicode/Chars/charClasses.sml",
"Unicode/Chars/charVecDict.sml",
"Unicode/Chars/dataDict.sml",
"Unicode/Chars/uniRanges.sml",
"Unicode/Chars/uniClasses.sml",
"Unicode/Chars/testClasses.sml",
"Unicode/Uri/uriDecode.sml",
"Unicode/Uri/uriEncode.sml",
"Unicode/Uri/uri.sml",
"Unicode/Uri/uriDict.sml",
"Unicode/encoding.sml",
"Unicode/Encode/encodeBasic.sml",
"Unicode/Encode/encodeError.sml",
"Unicode/Encode/encodeMisc.sml",
"Unicode/Encode/encode.sml",
"Unicode/Decode/decodeFile.sml",
"Unicode/Decode/decodeError.sml",
"Unicode/Decode/decodeMisc.sml",
"Unicode/Decode/decodeUtil.sml",
"Unicode/Decode/decodeUcs2.sml",
"Unicode/Decode/decodeUcs4.sml",
"Unicode/Decode/decodeUtf16.sml",
"Unicode/Decode/decodeUtf8.sml",
"Unicode/Decode/decode.sml",
"Parser/version.sml",
"Parser/Dfa/dfaData.sml",
"Parser/Dfa/dfaError.sml",
"Parser/Dfa/dfaOptions.sml",
"Parser/Dfa/dfaUtil.sml",
"Parser/Dfa/dfaPassOne.sml",
"Parser/Dfa/dfaPassTwo.sml",
"Parser/Dfa/dfaPassThree.sml",
"Parser/Dfa/dfaString.sml",
"Parser/Dfa/dfa.sml",
"Parser/Error/errorData.sml",
"Parser/Error/errorString.sml",
"Parser/Error/errorMessage.sml",
"Parser/Error/errorUtil.sml",
"Parser/Error/expected.sml",
"Parser/Error/errors.sml",
"Parser/Base/baseData.sml",
"Parser/Base/baseString.sml",
"Parser/Base/base.sml",
"Parser/Params/dtd.sml",
"Parser/Params/hookData.sml",
"Parser/Params/hooks.sml",
"Parser/Params/ignore.sml",
"Parser/Params/parserOptions.sml",
"Parser/Params/resolve.sml",
"Parser/entities.sml",
"Parser/Dtd/dtdDeclare.sml",
"Parser/Dtd/dtdAttributes.sml",
"Parser/Dtd/dtdManager.sml",
"Parser/Parse/parseBase.sml",
"Parser/Parse/parseNames.sml",
"Parser/Parse/parseMisc.sml",
"Parser/Parse/parseXml.sml",
"Parser/Parse/parseRefs.sml",
"Parser/Parse/parseLiterals.sml",
"Parser/Parse/parseTags.sml",
"Parser/Parse/parseDecl.sml",
"Parser/Parse/parseDtd.sml",
"Parser/Parse/parseContent.sml",
"Parser/Parse/parseDocument.sml",
"Catalog/catData.sml",
"Catalog/catDtd.sml",
"Catalog/catError.sml",
"Catalog/catParams.sml",
"Catalog/catFile.sml",
"Catalog/catHooks.sml",
"Catalog/catOptions.sml",
"Catalog/socatParse.sml",
"Catalog/catParse.sml",
"Catalog/catalog.sml",
"Catalog/catResolve.sml",
"genRandom.sml"]
in
val _ = List.app (fn s=>use (root^"/"^s)) fxplib
end

