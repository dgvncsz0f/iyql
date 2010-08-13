{-# LANGUAGE CPP #-}
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

module Test.Yql.Core.Types where

#define eq assertEqual (__FILE__ ++":"++ show __LINE__)
#define ok assertBool (__FILE__ ++":"++ show __LINE__)

import Data.Maybe
import Yql.Core.Types
import Yql.Xml
import Control.Monad
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

suite :: [Test]
suite = [ testGroup "Types.hs" [ test0
                              , test1
                              , test2
                              , test3
                              , test4
                              , test5
                              , test6
                              , test7
                              , test8
                              , test9
                              , test10
                              , test11
                              , test12
                              , test13
                              , test14
                              , test15
                              , test16
                              , test17
                              , test18
                              , test19
                              , test20
                              , test21
                              , test22
                              , test23
                              , test24
                              , test25
                              , test26
                              , test27
                              ]
        ]

test0 = testCase "show select * without where produces correct stmt" $
        eq "SELECT * FROM iyql;" (show $ SELECT ["*"] "iyql" Nothing [])

test1 = testCase "show select foo,bar without where produces correct stmt" $
        eq "SELECT foo,bar FROM iyql;" (show $ SELECT ["foo","bar"] "iyql" Nothing [])

test2 = testCase "show select foo with single where clause [txt]" $
        eq "SELECT foo FROM iyql WHERE foo=\"bar\";" (show $ SELECT ["foo"] "iyql" (Just $ "foo" `OpEq` TxtValue "bar") [])

test3 = testCase "show select pi with single where clause [num]" $
        eq "SELECT pi FROM iyql WHERE pi=3.14;" (show $ SELECT ["pi"] "iyql" (Just $ "pi" `OpEq` NumValue "3.14") [])

test4 = testCase "show select foo with single IN clause" $
        eq "SELECT foo FROM iyql WHERE foo IN (\"b\",\"a\",\"r\");" (show $ SELECT ["foo"] "iyql" (Just $ "foo" `OpIn` [TxtValue "b",TxtValue "a",TxtValue "r"]) [])

test5 = testCase "show select with where expression with and/or" $
        do eq "SELECT foo FROM iyql WHERE foo=\"bar\" OR bar=\"foo\" AND pi=3.14;" (show $ SELECT ["foo"] "iyql" (Just $ ("foo" `OpEq` TxtValue "bar") `OpOr` ("bar" `OpEq` TxtValue "foo") `OpAnd` ("pi" `OpEq` NumValue "3.14")) [])

test6 = testCase "read string creates correct type" $
        do eq (SELECT ["foo","bar"] "iyql" (Just $ ("pi" `OpEq` NumValue "3.14") `OpOr` ("foo" `OpIn` [TxtValue "b",TxtValue "a",TxtValue "r"])) []) (read "select foo,bar from iyql where pi=3.14 or foo in (\"b\",\"a\",\"r\");")

test7 = testCase "show select escapes strings" $
        do eq "SELECT * FROM iyql WHERE foo=\"foo\\\"bar\";" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "foo\"bar") [])
           eq "SELECT * FROM iyql WHERE foo=\"foo'bar\";" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "foo'bar") [])

test8 = testCase "show select with remote functions" $
        do eq "SELECT * FROM iyql | iyql() | iyql(a=1) | iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" Nothing [Remote "iyql" [],Remote "iyql" [("a",NumValue "1")],Remote "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])
           eq "SELECT * FROM iyql WHERE foo=\"bar\" | iyql() | iyql(a=1) | iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "bar") [Remote "iyql" [],Remote "iyql" [("a",NumValue "1")],Remote "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])

test9 = testCase "show select with local functions" $
        do eq "SELECT * FROM iyql | .iyql() | .iyql(a=1) | .iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" Nothing [Local "iyql" [],Local "iyql" [("a",NumValue "1")],Local "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])
           eq "SELECT * FROM iyql WHERE foo=\"bar\" | .iyql() | .iyql(a=1) | .iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "bar") [Local "iyql" [],Local "iyql" [("a",NumValue "1")],Local "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])

test10 = testCase "pipeline [execTransform] is in correct order" $
         do eq (Just $ ("bar"++).("foo"++) $ ">") (fmap (($ ">") . execTransform) (pipeline myLinker [Local "foo" [],Local "bar" []]))
            eq (Just $ ("bar"++).("foo"++) $ ">") (fmap (($ ">") . execTransform) (pipeline myLinker [Local "foobar1" []]))
            eq (Just $ ("bar"++).(++"foo") $ ">") (fmap (($ ">") . execTransform) (pipeline myLinker [Local "foobar2" []]))
            eq (Just $ (++"bar").(++"foo") $ ">") (fmap (($ ">") . execTransform) (pipeline myLinker [Local "foobar3" []]))
            eq (Just $ (++"bar").("foo"++) $ ">") (fmap (($ ">") . execTransform) (pipeline myLinker [Local "foobar4" []]))
  where myLinker = [ ("foo", const (Just (Transform ("foo"++))))
                   , ("bar", const (Just (Transform ("bar"++))))
                   , ("foobar1", const (Just $ Transform ("foo"++) `Seq` Transform ("bar"++)))
                   , ("foobar2", const (Just $ Transform (++"foo") `Seq` Transform ("bar"++)))
                   , ("foobar3", const (Just $ Transform (++"foo") `Seq` Transform (++"bar")))
                   , ("foobar4", const (Just $ Transform ("foo"++) `Seq` Transform (++"bar")))
                   ] :: [(String,[(String,Value)] -> Maybe Exec)]

test11 = testCase "pipeline generates error when function is not found" $
         do ok (isNothing $ pipeline () [Local "foo" []])
            ok (isJust $ pipeline () [])

test12 = testCase "ld [transform] is in correct order" $
         do eq (Just $ ("bar"++).("foo"++) $ ">") (fmap (($ ">") . execTransform) (ld myLinker (SELECT ["*"] "foobar" Nothing [Local "foo" [],Local "bar" []])))
  where myLinker = [ ("foo", const (Just (Transform ("foo"++))))
                   , ("bar", const (Just (Transform ("bar"++))))
                   ] :: [(String,[(String,Value)] -> Maybe Exec)]

test13 = testCase "ld generates error when function is not found" $
         do ok (isNothing $ ld () (SELECT ["*"] "foobar" Nothing [Local "foo" []]))
            ok (isJust $ ld () (SELECT ["*"] "foobar" Nothing []))

test14 = testCase "show desc produces correct result" $
         do eq ("DESC foobar;") (show $ DESC "foobar" [])
            eq ("DESC a;") (show $ DESC "a" [])
            eq ("DESC a | .yql();") (show $ DESC "a" [Local "yql" []])

test15 = testCase "read desc statements produces correct type" $
         do eq (DESC "foobar" []) (read "desc foobar;")
            eq (DESC "foobar" [Local "tables" []]) (read "desc foobar | .tables();")

test16 = testCase "test ord implementation of security level [User > App > Any]" $
         do ok (Any < App)
            ok (Any < User)
            ok (App < User)
            ok (App > Any)
            ok (User > App)
            ok (User > Any)
            ok (User /= App)
            ok (User /= Any)
            ok (User == User)
            ok (App == App)
            ok (Any == Any)

test17 = testCase "test readDescXml extracts attributes" $
         do ok $ (Just (Table "meme.info" Any False)) == (join $ fmap readDescXml (xmlParse xml0))
            ok $ (Just (Table "meme.info" App False)) == (join $ fmap readDescXml (xmlParse xml1))
            ok $ (Just (Table "meme.info" User False)) == (join $ fmap readDescXml (xmlParse xml2))
            ok $ (Just (Table "meme.info" Any True)) == (join $ fmap readDescXml (xmlParse xml3))
  where xml0 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><query xmlns:yahoo=\"http://www.yahooapis.com/v1/base.rng\"    yahoo:count=\"1\" yahoo:created=\"2010-08-09T04:08:39Z\" yahoo:lang=\"en-US\">    <results>        <table name=\"meme.info\" security=\"ANY\">            <meta>                <author>Yahoo! Inc.</author>                <documentationURL>http://developer.yahoo.com/meme/</documentationURL>                <sampleQuery>SELECT * FROM meme.info WHERE owner_guid=me</sampleQuery>            </meta>            <request>                <select>                    <key name=\"owner_guid\" type=\"xs:string\"/>                    <key name=\"name\" type=\"xs:string\"/>                </select>            </request>        </table>    </results></query>"
        xml1 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><query xmlns:yahoo=\"http://www.yahooapis.com/v1/base.rng\"    yahoo:count=\"1\" yahoo:created=\"2010-08-09T04:08:39Z\" yahoo:lang=\"en-US\">    <results>        <table name=\"meme.info\" security=\"APP\">            <meta>                <author>Yahoo! Inc.</author>                <documentationURL>http://developer.yahoo.com/meme/</documentationURL>                <sampleQuery>SELECT * FROM meme.info WHERE owner_guid=me</sampleQuery>            </meta>            <request>                <select>                    <key name=\"owner_guid\" type=\"xs:string\"/>                    <key name=\"name\" type=\"xs:string\"/>                </select>            </request>        </table>    </results></query>"
        xml2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><query xmlns:yahoo=\"http://www.yahooapis.com/v1/base.rng\"    yahoo:count=\"1\" yahoo:created=\"2010-08-09T04:08:39Z\" yahoo:lang=\"en-US\">    <results>        <table name=\"meme.info\" security=\"USER\">            <meta>                <author>Yahoo! Inc.</author>                <documentationURL>http://developer.yahoo.com/meme/</documentationURL>                <sampleQuery>SELECT * FROM meme.info WHERE owner_guid=me</sampleQuery>            </meta>            <request>                <select>                    <key name=\"owner_guid\" type=\"xs:string\"/>                    <key name=\"name\" type=\"xs:string\"/>                </select>            </request>        </table>    </results></query>"
        xml3 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><query xmlns:yahoo=\"http://www.yahooapis.com/v1/base.rng\"    yahoo:count=\"1\" yahoo:created=\"2010-08-09T04:08:39Z\" yahoo:lang=\"en-US\">    <results>        <table name=\"meme.info\" security=\"ANY\" https=\"true\">            <meta>                <author>Yahoo! Inc.</author>                <documentationURL>http://developer.yahoo.com/meme/</documentationURL>                <sampleQuery>SELECT * FROM meme.info WHERE owner_guid=me</sampleQuery>            </meta>            <request>                <select>                    <key name=\"owner_guid\" type=\"xs:string\"/>                    <key name=\"name\" type=\"xs:string\"/>                </select>            </request>        </table>    </results></query>"

test18 = testCase "show produces the correct stmt for updates" $ 
         do eq ("UPDATE foobar SET foo=\"bar\";") (show $ UPDATE [("foo",TxtValue "bar")] "foobar" Nothing [])
            eq ("UPDATE foobar SET f=0,o=2,o=3 WHERE a=0 AND b=1;") (show $ UPDATE [("f",NumValue "0"),("o",NumValue "2"),("o",NumValue "3")] "foobar" (Just $ ("a" `OpEq` NumValue "0") `OpAnd` ("b" `OpEq` NumValue "1")) [])
            eq ("UPDATE foobar SET foo=\"bar\" WHERE bar=\"foo\" | .json();") (show $ UPDATE [("foo",TxtValue "bar")] "foobar" (Just $ "bar" `OpEq` TxtValue "foo") [Local "json" []])

test19 = testCase "read update statements produces the correct type" $ 
         do eq (UPDATE [("foo",TxtValue "bar")] "foobar" Nothing []) (read "update foobar set foo='bar';")
            eq (UPDATE [("foo",TxtValue "bar"),("bar",TxtValue "foo")] "foobar" (Just $ "guid" `OpEq` MeValue) []) (read "update foobar set foo='bar', bar='foo' where guid=me;")
            eq (UPDATE [("foo",TxtValue "bar")] "foobar" (Just $ "guid" `OpEq` MeValue) [Local "json" []]) (read "update foobar set foo='bar' where guid=me | .json();")

test20 = testCase "update returns true for update stmts" $
         do ok (update $ UPDATE [] "" Nothing [])

test21 = testCase "read insert statements produces the correct type" $ 
         do eq (INSERT [("foo",TxtValue "bar")] "foobar" []) (read "insert into foobar (foo) VALUES ('bar');")
            eq (INSERT [("foo",NumValue "7")] "foobar" [Remote "iyql" []]) (read "insert into foobar (foo) values (7) | iyql();")
            eq (INSERT [("a",TxtValue "0"),("b",TxtValue "1")] "foobar" []) (read "insert into foobar (a,b) values ('0','1');")

test22 = testCase "show produces the correct stmt for updates" $
         do eq ("INSERT INTO foobar (foo) VALUES (\"bar\");") (show $ INSERT [("foo",TxtValue "bar")] "foobar" [])
            eq ("INSERT INTO foobar (a,b) VALUES (0,1);") (show $ INSERT [("a",NumValue "0"),("b",NumValue "1")] "foobar" [])

test23 = testCase "show produces the correct stmt for deletes" $ 
         do eq ("DELETE FROM foobar WHERE guid=me;") (show $ DELETE "foobar" (Just $ "guid" `OpEq` MeValue) [])
            eq ("DELETE FROM foobar;") (show $ DELETE "foobar" Nothing [])
            eq ("DELETE FROM foobar WHERE guid=me | .diagnostics();") (show $ DELETE "foobar" (Just $ "guid" `OpEq` MeValue) [Local "diagnostics" []])

test24 = testCase "read delete statements produces the correct type" $
         do eq (DELETE "foobar" Nothing []) (read "delete from foobar;")
            eq (DELETE "foobar" (Just $ "guid" `OpEq` MeValue) []) (read "delete from foobar where guid=me;")
            eq (DELETE "foobar" (Just $ "guid" `OpEq` MeValue) [Local "diagnostics" []]) (read "delete from foobar where guid=me | .diagnostics();")

test25 = testCase "delete returns true for delete stmts" $
         do ok (delete $ DELETE "" Nothing [])

test26 = testCase "read show tables statements produces the correct type" $
         do eq (SHOWTABLES []) (read "show tables;")
            eq (SHOWTABLES [Local "iyql" []]) (read "show tables | .iyql();")
            eq (SHOWTABLES [Local "iyql" [],Remote "iyql" []]) (read "show tables | .iyql() | iyql();")

test27 = testCase "show produces the correct stmt for show tables" $
         do eq ("SHOW TABLES;") (show $ SHOWTABLES [])
            eq ("SHOW TABLES | .iyql();") (show $ SHOWTABLES [Local "iyql" []])