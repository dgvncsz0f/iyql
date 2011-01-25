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
import qualified Data.Map as M
import Yql.Core.Types
import Yql.Core.LocalFunction
import Yql.Data.Xml
import Control.Monad
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

eqM aM bM = do { a <- aM
               ; b <- bM
               ; eq a b
               }


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
                               , test28
                               , test29
                               , test30
                               , test31
                               , test32
                               , test33
                               , test34
                               , test35
                               , test36
                               , test37
                               , test38
                               , test39
                               , test40
                               , test41
                               , test42
                               , test43
                               , test44
                               , test45
                               , test46
                               , test47
                               , test48
                               ]
        ]

test0 = testCase "show select * without where produces correct stmt" $
        eq "SELECT * FROM iyql;" (show $ SELECT ["*"] "iyql" Nothing Nothing Nothing [])

test1 = testCase "show select foo,bar without where produces correct stmt" $
        eq "SELECT foo,bar FROM iyql;" (show $ SELECT ["foo","bar"] "iyql" Nothing Nothing Nothing [])

test2 = testCase "show select foo with single where clause [txt]" $
        eq "SELECT foo FROM iyql WHERE foo = \"bar\";" (show $ SELECT ["foo"] "iyql" (Just $ "foo" `OpEq` TxtValue "bar") Nothing Nothing[])

test3 = testCase "show select pi with single where clause [num]" $
        eq "SELECT pi FROM iyql WHERE pi = 3.14;" (show $ SELECT ["pi"] "iyql" (Just $ "pi" `OpEq` NumValue "3.14") Nothing Nothing [])

test4 = testCase "show select foo with single IN clause" $
        eq "SELECT foo FROM iyql WHERE foo IN (\"b\",\"a\",\"r\");" (show $ SELECT ["foo"] "iyql" (Just $ "foo" `OpIn` [TxtValue "b",TxtValue "a",TxtValue "r"]) Nothing Nothing [])

test5 = testCase "show select with where expression with and/or" $
        do eq "SELECT foo FROM iyql WHERE foo = \"bar\" OR bar = \"foo\" AND pi = 3.14;" (show $ SELECT ["foo"] "iyql" (Just $ ("foo" `OpEq` TxtValue "bar") `OpOr` ("bar" `OpEq` TxtValue "foo") `OpAnd` ("pi" `OpEq` NumValue "3.14")) Nothing Nothing [])

test6 = testCase "read string creates correct type" $
        do eq (SELECT ["foo","bar"] "iyql" (Just $ ("pi" `OpEq` NumValue "3.14") `OpOr` ("foo" `OpIn` [TxtValue "b",TxtValue "a",TxtValue "r"])) Nothing Nothing []) (read "select foo,bar from iyql where pi=3.14 or foo in (\"b\",\"a\",\"r\");")

test7 = testCase "show select escapes strings" $
        do eq "SELECT * FROM iyql WHERE foo = \"foo\\\"bar\";" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "foo\"bar") Nothing Nothing [])
           eq "SELECT * FROM iyql WHERE foo = \"foo'bar\";" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "foo'bar") Nothing Nothing [])

test8 = testCase "show select with remote functions" $
        do eq "SELECT * FROM iyql | iyql() | iyql(a=1) | iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" Nothing Nothing Nothing [Remote "iyql" [],Remote "iyql" [("a",NumValue "1")],Remote "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])
           eq "SELECT * FROM iyql WHERE foo = \"bar\" | iyql() | iyql(a=1) | iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "bar") Nothing Nothing [Remote "iyql" [],Remote "iyql" [("a",NumValue "1")],Remote "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])

test9 = testCase "show select with local functions" $
        do eq "SELECT * FROM iyql | .iyql() | .iyql(a=1) | .iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" Nothing Nothing Nothing [Local "iyql" [],Local "iyql" [("a",NumValue "1")],Local "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])
           eq "SELECT * FROM iyql WHERE foo = \"bar\" | .iyql() | .iyql(a=1) | .iyql(a=1,b=\"2\");" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpEq` TxtValue "bar") Nothing Nothing [Local "iyql" [],Local "iyql" [("a",NumValue "1")],Local "iyql" [("a",NumValue "1"),("b",TxtValue "2")]])

test10 = testCase "pipeline [execTransform] is in correct order" $
         do { eqM (return $ ("bar"++).("foo"++) $ ">") (join $ fmap (flip execTransformM_ ">") (pipeline myLinker [Local "foo" [],Local "bar" []]))
            ; eqM (return $ ("bar"++).("foo"++) $ ">") (join $ fmap (flip execTransformM_ ">") (pipeline myLinker [Local "foobar1" []]))
            ; eqM (return $ ("bar"++).(++"foo") $ ">") (join $ fmap (flip execTransformM_ ">") (pipeline myLinker [Local "foobar2" []]))
            ; eqM (return $ (++"bar").(++"foo") $ ">") (join $ fmap (flip execTransformM_ ">") (pipeline myLinker [Local "foobar3" []]))
            ; eqM (return $ (++"bar").("foo"++) $ ">") (join $ fmap (flip execTransformM_ ">") (pipeline myLinker [Local "foobar4" []]))
            }
  where myLinker = M.fromList [ ("foo", Transform id (const ("foo"++)))
                              , ("bar", Transform id (const ("bar"++)))
                              , ("foobar1", Transform id (const ("foo"++)) `Seq` Transform id (const ("bar"++)))
                              , ("foobar2", Transform id (const (++"foo")) `Seq` Transform id (const ("bar"++)))
                              , ("foobar3", Transform id (const (++"foo")) `Seq` Transform id (const (++"bar")))
                              , ("foobar4", Transform id (const ("foo"++)) `Seq` Transform id (const (++"bar")))
                              ]

test11 = testCase "pipeline generates error when function is not found" $
         do ok (isNothing $ pipeline M.empty [Local "foo" []])
            ok (isJust $ pipeline M.empty [])

test12 = testCase "ld [transform] is in correct order" $
         do eqM (return $ ("bar"++).("foo"++) $ ">") (join $ fmap (flip execTransformM_ ">") (ld' myLinker (SELECT ["*"] "foobar" Nothing Nothing Nothing [Local "foo" [],Local "bar" []])))
  where myLinker = M.fromList [ ("foo", Transform id (const ("foo"++)))
                              , ("bar", Transform id (const ("bar"++)))
                              ]

test13 = testCase "ld generates error when function is not found" $
         do ok (isNothing $ ld' M.empty (SELECT ["*"] "foobar" Nothing Nothing Nothing [Local "foo" []]))
            ok (isJust $ ld' M.empty (SELECT ["*"] "foobar" Nothing Nothing Nothing []))

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
            eq ("UPDATE foobar SET f=0,o=2,o=3 WHERE a = 0 AND b = 1;") (show $ UPDATE [("f",NumValue "0"),("o",NumValue "2"),("o",NumValue "3")] "foobar" (Just $ ("a" `OpEq` NumValue "0") `OpAnd` ("b" `OpEq` NumValue "1")) [])
            eq ("UPDATE foobar SET foo=\"bar\" WHERE bar = \"foo\" | .json();") (show $ UPDATE [("foo",TxtValue "bar")] "foobar" (Just $ "bar" `OpEq` TxtValue "foo") [Local "json" []])

test19 = testCase "read update statements produces the correct type" $ 
         do eq (UPDATE [("foo",TxtValue "bar")] "foobar" Nothing []) (read "update foobar set foo='bar';")
            eq (UPDATE [("foo",TxtValue "bar"),("bar",TxtValue "foo")] "foobar" (Just $ "guid" `OpEq` MeValue) []) (read "update foobar set foo='bar', bar='foo' where guid=me;")
            eq (UPDATE [("foo",TxtValue "bar")] "foobar" (Just $ "guid" `OpEq` MeValue) [Local "json" []]) (read "update foobar set foo='bar' where guid=me | .json();")

test20 = testCase "update returns true for update stmts" $
         do ok (update $ UPDATE [] "" Nothing [])
            ok (update $ USE "" "" (UPDATE [] "" Nothing []))

test21 = testCase "read insert statements produces the correct type" $ 
         do eq (INSERT [("foo",TxtValue "bar")] "foobar" []) (read "insert into foobar (foo) VALUES ('bar');")
            eq (INSERT [("foo",NumValue "7")] "foobar" [Remote "iyql" []]) (read "insert into foobar (foo) values (7) | iyql();")
            eq (INSERT [("a",TxtValue "0"),("b",TxtValue "1")] "foobar" []) (read "insert into foobar (a,b) values ('0','1');")

test22 = testCase "show produces the correct stmt for updates" $
         do eq ("INSERT INTO foobar (foo) VALUES (\"bar\");") (show $ INSERT [("foo",TxtValue "bar")] "foobar" [])
            eq ("INSERT INTO foobar (a,b) VALUES (0,1);") (show $ INSERT [("a",NumValue "0"),("b",NumValue "1")] "foobar" [])

test23 = testCase "show produces the correct stmt for deletes" $ 
         do eq ("DELETE FROM foobar WHERE guid = me;") (show $ DELETE "foobar" (Just $ "guid" `OpEq` MeValue) [])
            eq ("DELETE FROM foobar;") (show $ DELETE "foobar" Nothing [])
            eq ("DELETE FROM foobar WHERE guid = me | .diagnostics();") (show $ DELETE "foobar" (Just $ "guid" `OpEq` MeValue) [Local "diagnostics" []])

test24 = testCase "read delete statements produces the correct type" $
         do eq (DELETE "foobar" Nothing []) (read "delete from foobar;")
            eq (DELETE "foobar" (Just $ "guid" `OpEq` MeValue) []) (read "delete from foobar where guid=me;")
            eq (DELETE "foobar" (Just $ "guid" `OpEq` MeValue) [Local "diagnostics" []]) (read "delete from foobar where guid=me | .diagnostics();")

test25 = testCase "delete returns true for delete stmts" $
         do ok (delete $ DELETE "" Nothing [])
            ok (delete $ USE "" "" (DELETE "" Nothing []))

test26 = testCase "read show tables statements produces the correct type" $
         do eq (SHOWTABLES []) (read "show tables;")
            eq (SHOWTABLES [Local "iyql" []]) (read "show tables | .iyql();")
            eq (SHOWTABLES [Local "iyql" [],Remote "iyql" []]) (read "show tables | .iyql() | iyql();")

test27 = testCase "show produces the correct stmt for show tables" $
         do eq ("SHOW TABLES;") (show $ SHOWTABLES [])
            eq ("SHOW TABLES | .iyql();") (show $ SHOWTABLES [Local "iyql" []])

test28 = testCase "show produces the correct stmt using local filter [like]" $
         do eq "SELECT * FROM iyql WHERE foo LIKE \"iyql%\";" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpLike` TxtValue "iyql%") Nothing Nothing [])
            eq "SELECT * FROM iyql WHERE foo NOT LIKE \"iyql%\";" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpNotLike` TxtValue "iyql%") Nothing Nothing [])
            
test29 = testCase "show produces the correct stmt using local filter [matches]" $
         do eq "SELECT * FROM iyql WHERE foo MATCHES \"iyql%\";" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpMatches` TxtValue "iyql%") Nothing Nothing [])
            eq "SELECT * FROM iyql WHERE foo NOT MATCHES \"iyql%\";" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpNotMatches` TxtValue "iyql%") Nothing Nothing [])

test30 = testCase "show produces the correct stmt using local filters [is null]" $
         do eq "SELECT * FROM iyql WHERE foo IS NULL;" (show $ SELECT ["*"] "iyql" (Just $ OpIsNull "foo") Nothing Nothing [])
            eq "SELECT * FROM iyql WHERE foo IS NOT NULL;" (show $ SELECT ["*"] "iyql" (Just $ OpIsNotNull "foo") Nothing Nothing [])

test31 = testCase "show produces the correct stmt using local filters [!=,<,>,<=,>=]" $
         do eq "SELECT * FROM iyql WHERE foo != 7;" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpNe` NumValue "7") Nothing Nothing [])
            eq "SELECT * FROM iyql WHERE foo > 7;" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpGt` NumValue "7") Nothing Nothing [])
            eq "SELECT * FROM iyql WHERE foo < 7;" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpLt` NumValue "7") Nothing Nothing [])
            eq "SELECT * FROM iyql WHERE foo >= 7;" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpGe` NumValue "7") Nothing Nothing [])
            eq "SELECT * FROM iyql WHERE foo <= 7;" (show $ SELECT ["*"] "iyql" (Just $ "foo" `OpLe` NumValue "7") Nothing Nothing [])

test32 = testCase "read parses the correct statement using local filters [like]" $
         do eq (SELECT ["*"] "iyql" (Just $ "foo" `OpLike` TxtValue "iyql%") Nothing Nothing []) (read "SELECT * FROM iyql WHERE foo LIKE \"iyql%\";")
            eq (SELECT ["*"] "iyql" (Just $ "foo" `OpNotLike` TxtValue "iyql%") Nothing Nothing []) (read "SELECT * FROM iyql WHERE foo NOT LIKE \"iyql%\";" )
            
test33 = testCase "read parses the correct stmt using local filters [is null]" $
         do eq (SELECT ["*"] "iyql" (Just $ OpIsNull "foo") Nothing Nothing []) (read "SELECT * FROM iyql WHERE foo IS NULL;")
            eq (SELECT ["*"] "iyql" (Just $ OpIsNotNull "foo") Nothing Nothing []) (read "SELECT * FROM iyql WHERE foo IS NOT NULL;")

test34 = testCase "read parses the correct stmt using local filters [!=,<,>,<=,>=]" $
         do eq (SELECT ["*"] "iyql" (Just $ "foo" `OpNe` NumValue "7") Nothing Nothing []) (read "SELECT * FROM iyql WHERE foo != 7;")
            eq (SELECT ["*"] "iyql" (Just $ "foo" `OpGt` NumValue "7") Nothing Nothing []) (read "SELECT * FROM iyql WHERE foo > 7;")
            eq (SELECT ["*"] "iyql" (Just $ "foo" `OpLt` NumValue "7") Nothing Nothing []) (read "SELECT * FROM iyql WHERE foo < 7;")
            eq (SELECT ["*"] "iyql" (Just $ "foo" `OpGe` NumValue "7") Nothing Nothing []) (read "SELECT * FROM iyql WHERE foo >= 7;")
            eq (SELECT ["*"] "iyql" (Just $ "foo" `OpLe` NumValue "7") Nothing Nothing []) (read "SELECT * FROM iyql WHERE foo <= 7;")

test35 = testCase "show functions with remote limits produces the proper stmt" $
         do eq ("SELECT * FROM iyql (0,10);") (show $  SELECT ["*"] "iyql" Nothing (Just (0,10)) Nothing [])

test36 = testCase "show functions with local limits produces the proper stmt" $
         do eq ("SELECT * FROM iyql LIMIT 10 OFFSET 0;") (show $ SELECT ["*"] "iyql" Nothing Nothing (Just (0,10)) [])

test37 = testCase "read parses the correct stmt using remote limits" $
         do eq (SELECT ["*"] "iyql" Nothing (Just (0,10)) Nothing []) (read "SELECT * FROM iyql (0,10);")
            eq (SELECT ["*"] "iyql" Nothing (Just (0,10)) Nothing []) (read "SELECT * FROM iyql (10);")

test38 = testCase "read parses the correct stmt using local limits" $
         do eq (SELECT ["*"] "iyql" Nothing Nothing (Just (13,7)) []) (read "SELECT * FROM iyql LIMIT 7 OFFSET 13;")
            eq (SELECT ["*"] "iyql" Nothing Nothing (Just (0,17)) []) (read "SELECT * FROM iyql LIMIT 17;");

test39 = testCase "read parses the correct stmt using remote limits" $
         do eq (SELECT ["*"] "iyql" Nothing (Just (0,10)) Nothing []) (read "SELECT * FROM iyql(0,10);")
            eq (SELECT ["*"] "iyql" Nothing (Just (0,17)) Nothing []) (read "SELECT * FROM iyql (17);")

test40 = testCase "read parses the correct stmt for use statements" $
         do eq (USE "foobar" "fb" (SELECT ["*"] "iyql" Nothing Nothing Nothing [])) (read "USE \"foobar\" as fb; SELECT * FROM iyql;")
            eq (USE "foobar" "fb" (UPDATE [("foo",TxtValue "bar")] "iyql" Nothing [])) (read "USE \"foobar\" as fb; UPDATE iyql set foo='bar';")
            eq (USE "foobar" "fb" (INSERT [("foo",TxtValue "bar")] "foobar" [])) (read "USE \"foobar\" as fb; INSERT INTO foobar (foo) VALUES ('bar');")
            eq (USE "foobar" "fb" (DELETE "foobar" Nothing [])) (read "USE \"foobar\" as fb; DELETE FROM foobar;")
            eq (USE "foo" "f" (USE "bar" "b" (SELECT ["*"] "iyql" Nothing Nothing Nothing []))) (read "USE \"foo\" as f; USE \"bar\" as b; SELECT * FROM iyql;")

test41 = testCase "select returns true for select statements" $
         do ok (select $ SELECT [] "foobar" Nothing Nothing Nothing [])
            ok (select $ USE "" "" (SELECT [] "foobar" Nothing Nothing Nothing []))

test42 = testCase "insert returns true for insert statements" $
         do ok (insert $ INSERT [] "" [])
            ok (insert $ USE "" "" (INSERT [] "" []))

test43 = testCase "showTables returns true for show tables statements" $
         do ok (showTables $ SHOWTABLES [])
            ok (showTables $ USE "" "" (SHOWTABLES []))

test44 = testCase "desc returns true for desc statements" $
         do ok (desc $ DESC "" [])
            ok (desc $ USE "" "" (DESC "" []))

test45 = testCase "showStmt shows subselects properly" $
         do eq ("SELECT * FROM iyql WHERE foo IN (SELECT id FROM iyql WHERE bar > 7);") (showStmt $ SELECT ["*"] "iyql" (Just $ "foo" `OpIn` [SubSelect $ SELECT ["id"] "iyql" (Just $ "bar" `OpGt` (NumValue "7")) Nothing Nothing []]) Nothing Nothing [])

test46 = testCase "read select with subqueries" $
         do eq (SELECT ["*"] "iyql" (Just $ "foo" `OpIn` [SubSelect $ SELECT ["id"] "iyql" (Just $ "bar" `OpGt` (NumValue "7")) Nothing Nothing []]) Nothing Nothing []) (read "select * from iyql where foo in (select id from iyql where bar>7);")

test47 = testCase "readShowTablesXml reads all tables from xml" $
         do eq (Just ["foo","bar","baz"]) (fmap (readShowTablesXml) (xmlParse "<query><results><table>foo</table><table>bar</table><table>baz</table></results></query>"))

test48 = testCase "usingMe is able to identify me identifiers in simple cases" $
         do ok (usingMe (SELECT ["*"] "iyql" (Just $ "foo" `OpEq` MeValue) Nothing Nothing []))

test49 = testCase "usingMe is able to identify me identifier in complex cases" $
         do ok (usingMe (SELECT ["*"] "iyql" (Just $ "foo" `OpIn` [SubSelect $ SELECT ["guid"] "iyql" (Just $ "bar" `OpEq` MeValue) Nothing Nothing []]) Nothing Nothing []))

