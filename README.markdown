iYQL - Interactive YQL
======================
This project provides a CLI (command-line interface) for [YQL](http://developer.yahoo.com/yql/).

Installing
----------

### Install dependencies - MacOS

    $ sudo port install haskell-platform
    $ sudo port install hs-haskeline
    $ sudo port install curl
    $ cabal install hoauth>=0.2.5 xml

### Install dependencies - Debian

    $ sudo apt-get install cabal-install libghc6-curl-dev libghc6-parsec2-dev libghc6-haskeline-dev
    $ cabal install hoauth>=0.2.5 xml

### Compile and Install

    $ make
    $ sudo make PREFIX=/usr/local/bin install

### Install using cabal

    $ cabal install iyql

Getting Started
---------------

### Statements

To start iyql, type _iyql_. After the program is started, it waits the user to input queries, that will be sent to yql. Each query must be terminated by a semicolon and LF. The semicolon is required in order to know when the statement terminates. However, you may split up a long query into multiple lines, as follows:

    $ iyql
    iyql version 0.0.1-alpha
    This is free software. Enter :license to read it
    Enter :help for instructions
    Enter YQL statements terminated with a ";"
    iyql> SELECT
     ...> guid
     ...> FROM
     ...> meme.info
     ...> WHERE
     ...> name="meme"
     ...> ;
     <?xml version='1.0' ?>
     <query xmlns:yahoo="http://www.yahooapis.com/v1/base.rng" yahoo:count="1" yahoo:created="2010-08-29T12:57:24Z" yahoo:lang="en-US">
       <results>
         <meme>
           <guid>S5R44PGJRBLKNEE5GYSRQPTXLQ</guid>
         </meme>
       </results>
     </query>

To terminate the program, either type EOF (usually C-d) or type :quit. Currently C-c also terminates the program, however, this will change in future versions. The following statements are supported:

   * SELECT _(except WHERE clauses with different precedence [=expressions with parenthesis])_;
   * UPDATE;
   * INSERT;
   * DELETE;
   * SHOW TABLES;
   * USE _(differently from YQL, `AS' clause is not optional)_;
   * DESC;

#### OAuth

Iyql will automatically identify queries that requires either 2-legged or 3-legged oauth authentication. This is done by inspecting the statement. Queries that contains a _me_ symbol requires 3-legged. Queries without this, a _DESC TABLE_ is performed and the security level is get from the result of this operation.

Allowing authenticated requests requires the configuration file to be properly defined. Iyql will look for the file _$HOME/.iyql/cfg_ upon initialization. This file should contain your credentials:

    $ cat $HOME/.iyql/cfg
    -- Lines starting with -- and empty lines are ignored

    -- This should contain your consumer key, as given by http://developer.yahoo.com/dashboard/
    oauth_consumer_key: ...

    -- This is your consumer secret, as given by http://developer.yahoo.com/dashboard/
    oauth_consumer_sec: ...

When this file is defined, you should be able to perform authenticated requests. Example:

    $ iyql
    iyql version 0.0.1-alpha
    This is free software. Enter :license to read it
    Enter :help for instructions
    Enter YQL statements terminated with a ";"
    iyql> SELECT * FROM social.profile WHERE guid=me;
    <?xml version='1.0' ?>
    <query xmlns:yahoo="http://www.yahooapis.com/v1/base.rng" yahoo:count="1" yahoo:created="2010-08-29T01:17:42Z" yahoo:lang="en-US">
      <results>
        <profile xmlns="http://social.yahooapis.com/v1/schema.rng">
          <guid>6BY52OMEJVITJSBZJCZPB22JZA</guid>
        </profile>
      </results>
    </query>

#### Local Functions

Yql has support for functions. For instance, suppose you want to display the results in a different order, you may use _sort_ function to accomplish this. Iyql extends a bit this idea introducing local functions. Local functions are similar to YQL functions but are interpreted by the program itself. These functions might either a) change the request before that is sent, b) change the response before the output is generated. Next you will find a list of such functions:

   * .request     -- add parameters in the request (for instance diagnostics or env);
   * .json        -- alias to .request(format="json");
   * .diagnostics -- alias to .request(diagnostics="true");
   * .endpoint    -- use a different yql endpoint, one other than `query.yahooapis.com' [.endpoint(host="query.yahooapis.com", port=80)]
   * .tables      -- turns the output into a tabular format;

and its use:

       $ iyql> SELECT * FROM social.profile WHERE guid=me;
       SELECT guid,nickname FROM social.profile WHERE guid=me | .tables();
       |*guid*                    |*nickname*|
       |6BY52OMEJVITJSBZJCZPB22JZA|dsouza    |
       iyql> SELECT guid,nickname FROM social.profile WHERE guid=me | .json();
       {"query":{"count":"1","created":"2010-08-29T01:34:21Z","lang":"en-US","results":{"profile":{"nickname":"dsouza","guid":"6BY52OMEJVITJSBZJCZPB22JZA"}}}}

### Commands 

Most of the input is sent to the YQL. However, lines starting with : (colon) are considered to be commands and interpreted by iyql itself. These commands can modify/inspect the state of iyql program. Following a list of available commands:

    * :whoami  -- The guid of the authenticated user;
    * :logout  -- Purge the saved oauth token (if any);
    * :login   -- Perform the oauth authorization process (if necessary);
    * :env     -- Modifies the env list that is sent to yql;
    * :help    -- The available commands;
    * :quit    -- Terminates the program;
    * :man     -- Help/Listing of available local functions;

### Completion

Hitting the tab key attemps to complete to word previous to the cursor. Possible candidates include commands, local functions and tables. The table names are read in upon startup, which means :env command has no effect on completion. Only -e switch and `env' entries in .iyql/cfg affect the tables suggestion. Notice that this suggestion is not contextual. In other words, it suggests everything that contains a given prefix, even though that doesn't make sense (e.g. table name prior a SELECT/UPDATE/DELETE/INSERT keyword).

