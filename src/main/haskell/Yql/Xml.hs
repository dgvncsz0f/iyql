-- Copyright (c) 2010, Diego Souza
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--   * Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright notice,
--     this list of conditions and the following disclaimer in the documentation
--     and/or other materials provided with the distribution.
--   * Neither the name of the <ORGANIZATION> nor the names of its contributors
--     may be used to endorse or promote products derived from this software
--     without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module Yql.Xml
       ( -- *  Types 
         XML()
         -- * Parsing
       , xmlParse
         -- * Querying
       , findElement
       , findElements
       , childNodes
       , attributes
       , attribute
       , tagName
       , verbatim
       , element
       , pcdata
         -- * Printing
       , xmlPrint
       ) where

import qualified Text.XML.Light as X

newtype XML = XML X.Content

unpackElement :: XML -> X.Element
unpackElement (XML (X.Elem e)) = e
unpackElement _                = error "element expected"

element :: XML -> Bool
element (XML (X.Elem _)) = True
element _                = False

pcdata :: XML -> Bool
pcdata (XML (X.Text _)) = True
pcdata _                = False

xmlParse :: String -> Maybe XML
xmlParse = fmap (XML . X.Elem) . X.parseXMLDoc 

xmlPrint :: XML -> String
xmlPrint = X.ppTopElement . unpackElement

findElement :: String -> XML -> Maybe XML
findElement tag = fmap (XML . X.Elem) . X.findElement (X.QName tag Nothing Nothing) . unpackElement

findElements :: String -> XML -> [XML]
findElements tag = map (XML . X.Elem) . X.findElements (X.QName tag Nothing Nothing) . unpackElement

childNodes :: XML -> [XML]
childNodes (XML (X.Elem (X.Element _ _ cs _))) = map XML cs
childNodes _                                   = []

verbatim :: XML -> String
verbatim (XML (X.Text e))  = X.cdData e
verbatim xml               = concatMap verbatim (childNodes xml)

tagName :: XML -> String
tagName (XML (X.Elem (X.Element q _ _ _))) = X.qName q
tagName _                                  = error "element was expected"

attributes :: XML -> [(String,String)]
attributes (XML (X.Elem (X.Element _ attrs _ _))) = map (\attr -> (X.qName (X.attrKey attr), X.attrVal attr)) attrs
attributes _                                      = []

attribute :: String -> XML -> Maybe String
attribute k = lookup k . attributes
