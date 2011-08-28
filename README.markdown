## Erlang Github API

Wrapper for github.com API written in Erlang.

## Status
The erlang-github-api project in very very early stage, it's only started to be developed.

## Features

  * Support Gists
  * Git api
  * Support Issues 
  * Organizations
  * Repos
  * Users
  * Full support github API v.3
  
## Build
```
make
```
  
## Usage
Example for usage, Get gist description:

```erlang
1> gist:get_gist_description(1).

["the meaning of gist"]

2> gist:get_gist_pull_url(1).

["git://gist.github.com/1.git"]

3> gist:is_public(1).

true
```
  
## Dependency

  * Ibrowse (https://github.com/cmullaparthi/ibrowse)
  
## erlang-github-api TODO and Issues
Issues - <https://github.com/onlyshk/erlang-github-api>
  
## Contribute
 1) Fork epmail
 
 2) Write some new features or fix bug
 
 3) Test it
 
 4) Pull request
  
## LICENSE
License in LICENSE file

## More info
More info at kuleshovmail@gmail.com
